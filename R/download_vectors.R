#' Download pre-trained GloVe word vectors
#'
#' Downloads GloVe word vectors from Stanford NLP and caches them on disk.
#' The download happens **once**; subsequent calls detect the cached file and
#' return immediately without re-downloading.
#'
#' @param model Character scalar. Which GloVe model to download. One of:
#'   * `"glove.6B.50d"`  (default) — 50 dimensions, ~160 MB extracted
#'   * `"glove.6B.100d"` — 100 dimensions, ~330 MB extracted
#'   * `"glove.6B.200d"` — 200 dimensions, ~660 MB extracted
#'   * `"glove.6B.300d"` — 300 dimensions, ~990 MB extracted
#'
#'   All four models share the same source zip (~822 MB). The zip is
#'   downloaded once, the requested file is extracted, and the zip is
#'   deleted to reclaim disk space.
#'
#' @param cache_dir Character scalar. Directory in which to cache the
#'   extracted vector file. Defaults to a user-level application data
#'   directory returned by [rappdirs::user_data_dir()].
#' @param overwrite Logical. If `TRUE`, re-download even when a cached file
#'   exists. Default `FALSE`.
#' @param timeout Integer. Download timeout in seconds. The default R timeout
#'   (60 s) is far too short for an 822 MB file; this argument temporarily
#'   raises it for the duration of the download and restores the original
#'   value afterwards. Default `3600` (one hour), which is comfortable even
#'   on a slow connection. Set lower if you want an earlier failure.
#'
#' @return Invisibly returns the path to the cached `.txt` vector file.
#'   Use this path with [load_vectors()] or pass it directly to other tools.
#'
#' @seealso [load_vectors()], [expand_dict()]
#'
#' @examples
#' \dontrun{
#' # Download the compact 50-dimension model (recommended starting point)
#' download_vectors("glove.6B.50d")
#'
#' # Explicit timeout (seconds) on a slow connection
#' download_vectors("glove.6B.50d", timeout = 7200)
#'
#' # Download a higher-quality model
#' download_vectors("glove.6B.100d")
#'
#' # Force re-download
#' download_vectors("glove.6B.50d", overwrite = TRUE)
#' }
#'
#' @export
download_vectors <- function(
    model     = "glove.6B.50d",
    cache_dir = rappdirs::user_data_dir("lexiExpand"),
    overwrite = FALSE,
    timeout   = 3600L
) {
  model <- match.arg(
    model,
    c("glove.6B.50d", "glove.6B.100d", "glove.6B.200d", "glove.6B.300d")
  )

  if (!is.numeric(timeout) || timeout < 1L) {
    cli::cli_abort("{.arg timeout} must be a positive number of seconds.")
  }

  # ── Ensure cache directory exists ─────────────────────────────────────────
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    cli::cli_inform("Created cache directory: {.path {cache_dir}}")
  }

  txt_file <- .vector_cache_path(model, cache_dir)
  zip_file <- file.path(cache_dir, "glove.6B.zip")

  # ── Short-circuit if already cached ──────────────────────────────────────
  if (file.exists(txt_file) && !overwrite) {
    cli::cli_inform(c(
      "v" = "Vectors already cached.",
      " " = "Path: {.path {txt_file}}",
      "i" = "Pass {.code overwrite = TRUE} to force a re-download."
    ))
    return(invisible(txt_file))
  }

  # ── Raise download timeout for the duration of this call -----------------
  # R's default timeout is 60 s — nowhere near enough for 822 MB.
  # We restore the original value on exit (even if an error occurs).
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = as.integer(timeout))

  # ── Download ──────────────────────────────────────────────────────────────
  glove_url <- "https://downloads.cs.stanford.edu/nlp/data/glove.6B.zip"

  cli::cli_inform(c(
    "!" = "Downloading the GloVe 6B source archive (~822 MB).",
    " " = "This is a one-time download; the zip is deleted after extraction.",
    "i" = "Timeout set to {timeout} seconds. Use {.arg timeout} to adjust.",
    "i" = "URL: {.url {glove_url}}"
  ))

  dl_ok <- tryCatch({
    utils::download.file(
      url      = glove_url,
      destfile = zip_file,
      mode     = "wb",
      quiet    = FALSE
    )
    TRUE
  }, warning = function(w) {
    # download.file raises a *warning* (not an error) for incomplete downloads
    msg <- conditionMessage(w)
    if (grepl("Timeout|downloaded length", msg, ignore.case = TRUE)) {
      cli::cli_abort(c(
        "Download timed out or was incomplete.",
        "x" = msg,
        "i" = paste0(
          "Try increasing the timeout: ",
          "{.code download_vectors(timeout = 7200)}."
        ),
        "i" = paste0(
          "Alternatively, download the file manually from ",
          "{.url https://downloads.cs.stanford.edu/nlp/data/glove.6B.zip}, ",
          "place it in {.path {cache_dir}}, then re-run this function."
        )
      ))
    }
    # Other warnings: re-raise so they are still visible
    warning(w)
    TRUE
  }, error = function(e) {
    cli::cli_abort(c(
      "Download failed.",
      "x" = conditionMessage(e),
      "i" = paste0(
        "Check your internet connection. ",
        "You can also place a pre-downloaded {.file glove.6B.zip} ",
        "in {.path {cache_dir}} and re-run this function."
      )
    ))
  })

  if (!isTRUE(dl_ok)) return(invisible(NULL))

  # ── Verify the zip is not a partial download ──────────────────────────────
  zip_size_mb <- file.size(zip_file) / 1024^2
  if (zip_size_mb < 800) {
    cli::cli_abort(c(
      "Downloaded zip looks incomplete ({round(zip_size_mb, 1)} MB; expected ~822 MB).",
      "i" = "Delete the partial file and re-run, or download manually.",
      " " = "Partial file: {.path {zip_file}}"
    ))
  }

  # ── Extract only the requested dimension file ─────────────────────────────
  target_filename <- paste0(model, ".txt")
  cli::cli_inform("Extracting {.file {target_filename}} from archive...")

  tryCatch(
    utils::unzip(
      zipfile   = zip_file,
      files     = target_filename,
      exdir     = cache_dir,
      junkpaths = TRUE
    ),
    error = function(e) {
      cli::cli_abort(c(
        "Extraction failed.",
        "x" = conditionMessage(e),
        "i" = "The zip file may be corrupt. Delete it and re-run."
      ))
    }
  )

  if (!file.exists(txt_file)) {
    cli::cli_abort(c(
      "Extracted file not found at expected path: {.path {txt_file}}",
      "i" = "Check whether the zip contains {.file {target_filename}}."
    ))
  }

  # ── Clean up zip to save disk space ──────────────────────────────────────
  if (file.exists(zip_file)) {
    unlink(zip_file)
    cli::cli_inform("Deleted source zip to save disk space.")
  }

  cli::cli_inform(c(
    "v" = "Done! Vectors cached at:",
    " " = "{.path {txt_file}}"
  ))

  invisible(txt_file)
}
