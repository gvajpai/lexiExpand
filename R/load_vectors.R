#' Load cached word vectors into memory
#'
#' Reads a cached GloVe `.txt` file from disk and returns an L2-normalised
#' numeric matrix (words × dimensions). The matrix is stored in a
#' session-level cache so repeated calls within the same R session are
#' instant.
#'
#' @param model Character scalar. Which cached model to load. Must match
#'   the `model` you passed to [download_vectors()]. Default `"glove.6B.50d"`.
#' @param cache_dir Character scalar. Cache directory used by
#'   [download_vectors()]. Default: `rappdirs::user_data_dir("lexiExpand")`.
#' @param vocab_size Integer or `NULL`. Maximum number of words to load.
#'   GloVe files are sorted by corpus frequency (most common first), so
#'   loading the top-N words captures the vast majority of real text while
#'   keeping memory use manageable. `NULL` loads the full vocabulary
#'   (~400 000 words). Default `50000L`.
#' @param force_reload Logical. Re-read from disk even if the matrix is
#'   already in the session cache? Useful after updating the cached file.
#'   Default `FALSE`.
#'
#' @return A numeric matrix with:
#'   * **rows** — words (row names set to the vocabulary terms)
#'   * **columns** — embedding dimensions
#'   * **values** — L2-normalised so that cosine similarity between two
#'     words equals the dot product of their rows.
#'
#' @details
#' If the vector file is not found, the function aborts with a clear
#' message directing the user to run [download_vectors()] first.
#'
#' The session cache is stored in an internal environment
#' (`.lexiexpand_env$vectors`) keyed by `"<model>_<vocab_size>"`.
#'
#' @seealso [download_vectors()], [expand_dict()]
#'
#' @examples
#' \dontrun{
#' download_vectors("glove.6B.50d")
#'
#' # Load top 50 000 words (default)
#' vecs <- load_vectors("glove.6B.50d")
#' dim(vecs)        # 50000 x 50
#' rownames(vecs)[1:5]  # most frequent words
#'
#' # Load the full vocabulary
#' vecs_full <- load_vectors("glove.6B.50d", vocab_size = NULL)
#' }
#'
#' @export
load_vectors <- function(
    model        = "glove.6B.50d",
    cache_dir    = rappdirs::user_data_dir("lexiExpand"),
    vocab_size   = 50000L,
    force_reload = FALSE
) {
  model <- match.arg(
    model,
    c("glove.6B.50d", "glove.6B.100d", "glove.6B.200d", "glove.6B.300d")
  )

  # ── Session cache key ─────────────────────────────────────────────────────
  cache_key <- paste0(model, "_", vocab_size %||% "all")

  if (!force_reload && !is.null(.lexiexpand_env$vectors[[cache_key]])) {
    cli::cli_inform(
      "Using session-cached {.val {model}} ({cache_key})."
    )
    return(.lexiexpand_env$vectors[[cache_key]])
  }

  # ── Locate file ───────────────────────────────────────────────────────────
  txt_file <- .vector_cache_path(model, cache_dir)

  if (!file.exists(txt_file)) {
    cli::cli_abort(c(
      "Vector file not found: {.path {txt_file}}",
      "i" = "Run {.run lexiExpand::download_vectors('{model}')} first."
    ))
  }

  # ── Read file ─────────────────────────────────────────────────────────────
  n_rows <- if (is.null(vocab_size)) Inf else as.integer(vocab_size)

  cli::cli_inform(c(
    "i" = "Loading {.val {model}} from disk",
    " " = "(top {.val {vocab_size %||% 'all'}} words)..."
  ))

  dt <- tryCatch(
    data.table::fread(
      file         = txt_file,
      header       = FALSE,
      nrows        = n_rows,
      quote        = "",            # words may contain quote characters
      encoding     = "UTF-8",
      showProgress = interactive()
    ),
    error = function(e) {
      cli::cli_abort(c(
        "Failed to read vector file.",
        "x" = conditionMessage(e),
        "i" = "The file may be corrupt. Re-download with {.code download_vectors(overwrite = TRUE)}."
      ))
    }
  )

  # ── Convert to named matrix ───────────────────────────────────────────────
  words <- as.character(dt[[1L]])
  mat   <- as.matrix(dt[, -1L, with = FALSE])  # drop word column
  storage.mode(mat) <- "double"
  rownames(mat) <- words

  # ── L2 normalise ─────────────────────────────────────────────────────────
  mat <- .l2_normalise(mat)

  # ── Store in session cache ────────────────────────────────────────────────
  .lexiexpand_env$vectors[[cache_key]] <- mat

  cli::cli_inform(c(
    "v" = "Loaded {.val {nrow(mat)}} words \u00d7 {.val {ncol(mat)}} dimensions."
  ))

  mat
}


#' Load GloVe vectors via the textdata package
#'
#' An alternative to [download_vectors()] + [load_vectors()] that delegates
#' the download and caching to the \pkg{textdata} package. On first call,
#' `textdata` prompts the user to confirm the download; subsequent calls use
#' the `textdata` cache automatically.
#'
#' This approach is convenient when \pkg{textdata}
#' is already installed and you prefer its consent-and-cache flow over the
#' direct Stanford download in [download_vectors()].
#'
#' @param dimensions Integer. Embedding dimensions to download. One of
#'   `50`, `100`, `200`, or `300`. Default `300`.
#' @param vocab_size Integer or `NULL`. Maximum number of words to load.
#'   Default `50000L`.
#'
#' @return An L2-normalised numeric matrix (words × dimensions) identical in
#'   structure to the output of [load_vectors()]. Compatible with all
#'   [expand_dict()] calls via the `vectors` argument.
#'
#' @seealso [load_vectors()], [expand_dict()]
#'
#' @examples
#' \dontrun{
#' # Requires the textdata package
#' install.packages("textdata")
#'
#' vecs <- load_vectors_textdata(dimensions = 300, vocab_size = 30000)
#' result <- expand_dict(c("sad", "angry"), vectors = vecs, interactive = FALSE)
#' }
#'
#' @export
load_vectors_textdata <- function(dimensions = 300L, vocab_size = 50000L) {
  if (!requireNamespace("textdata", quietly = TRUE)) {
    cli::cli_abort(c(
      "{.pkg textdata} is required for this function.",
      "i" = "Install it with {.run install.packages('textdata')}.",
      "i" = "Alternatively, use {.fn download_vectors} + {.fn load_vectors} \\
             for the direct Stanford download."
    ))
  }

  dimensions <- as.integer(dimensions)
  if (!dimensions %in% c(50L, 100L, 200L, 300L)) {
    cli::cli_abort(
      "{.arg dimensions} must be one of 50, 100, 200, or 300."
    )
  }

  cli::cli_inform(
    "Loading GloVe 6B {dimensions}-dimensional vectors via {.pkg textdata}..."
  )

  raw <- tryCatch(
    textdata::embedding_glove6b(dimensions = dimensions),
    error = function(e) {
      cli::cli_abort(c(
        "Failed to retrieve vectors via {.pkg textdata}.",
        "x" = conditionMessage(e)
      ))
    }
  )

  n_rows <- if (is.null(vocab_size)) nrow(raw) else min(as.integer(vocab_size), nrow(raw))
  raw    <- raw[seq_len(n_rows), ]

  mat              <- as.matrix(raw[, -1L])
  storage.mode(mat) <- "double"
  rownames(mat)    <- raw[[1L]]   # first column is the token

  mat <- .l2_normalise(mat)

  cli::cli_inform(c(
    "v" = "Loaded {.val {nrow(mat)}} words \u00d7 {.val {ncol(mat)}} dimensions \\
           via {.pkg textdata}."
  ))

  mat
}
