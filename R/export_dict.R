#' Export an expanded dictionary to analysis-ready formats
#'
#' Converts the output of [review_candidates()] (or a plain character vector
#' of words) into a format ready for use with \pkg{quanteda}, \pkg{tidytext},
#' or any other text-analysis workflow.
#'
#' @param result One of:
#'   * A `lexiexpand_result` object returned by [expand_dict()] /
#'     [review_candidates()] (interactive workflow), **or**
#'   * A `data.frame` returned by [expand_dict()] with
#'     `interactive = FALSE` — the `$word` column is used automatically, **or**
#'   * A plain character vector of accepted words (in which case `seed`
#'     must also be supplied).
#' @param seed Character vector of seed words. Ignored when `result` is an
#'   `lexiexpand_result`; required when `result` is a character vector.
#' @param name Character scalar. Category name for the dictionary entry.
#'   Default `"category"`. Example: `"negative_emotion"`, `"innovation"`.
#' @param format Character scalar. Output format. One of:
#'   \describe{
#'     \item{`"list"`}{(Default) A named list, e.g.
#'       `list(negative_emotion = c("sad", "angry", "miserable", ...))`.
#'       Drop-in compatible with \pkg{quanteda} `dictionary()` and
#'       \pkg{tidytext} `match_any()`.}
#'     \item{`"data.frame"`}{A two-column `data.frame` with columns `word`
#'       and `category`. Easy to write to CSV or join with other data.}
#'     \item{`"quanteda"`}{A `quanteda::dictionary` object. Requires the
#'       \pkg{quanteda} package.}
#'   }
#' @param include_seed Logical. Include the original seed words in the
#'   exported dictionary? Default `TRUE`.
#'
#' @return Depends on `format`:
#'   * `"list"`: named `list`.
#'   * `"data.frame"`: `data.frame`.
#'   * `"quanteda"`: `quanteda::dictionary`.
#'
#' @seealso [expand_dict()], [review_candidates()]
#'
#' @examples
#' \dontrun{
#' download_vectors("glove.6B.50d")
#'
#' # Fully scripted pipeline (no interactive wizard)
#' result <- expand_dict(c("sad", "angry"), threshold = 0.70,
#'                       interactive = FALSE)
#'
#' # Export top candidates automatically (e.g. top 10 by similarity)
#' top_words <- head(result$word, 10)
#'
#' export_dict(top_words,
#'             seed   = c("sad", "angry"),
#'             name   = "negative_emotion",
#'             format = "list")
#'
#' # From an interactive result object
#' result <- expand_dict(c("sad", "angry"))    # launches wizard
#' export_dict(result, name = "negative_emotion", format = "quanteda")
#' }
#'
#' @export
export_dict <- function(
    result,
    seed         = NULL,
    name         = "category",
    format       = "list",
    include_seed = TRUE
) {
  format <- match.arg(format, c("list", "data.frame", "quanteda"))

  if (!is.character(name) || length(name) != 1L || nchar(name) == 0L) {
    cli::cli_abort("{.arg name} must be a single non-empty character string.")
  }

  # ── Normalise input to vectors of seed + accepted words ───────────────────
  if (inherits(result, "lexiexpand_result")) {
    seed_words     <- result$seed
    accepted_words <- result$accepted
  } else if (is.character(result)) {
    if (is.null(seed)) {
      cli::cli_abort(c(
        "{.arg seed} is required when {.arg result} is a character vector.",
        "i" = "Provide the original seed words so they can be included in the export."
      ))
    }
    seed_words     <- unique(trimws(tolower(seed)))
    accepted_words <- unique(trimws(tolower(result)))
    accepted_words <- setdiff(accepted_words, seed_words)  # avoid duplication
  } else if (is.data.frame(result)) {
    # Accept a data.frame from expand_dict(interactive = FALSE) directly
    if (!"word" %in% names(result)) {
      cli::cli_abort(
        "When {.arg result} is a data.frame it must have a {.code word} column \
         (as returned by {.fn expand_dict}).")
    }
    if (is.null(seed)) {
      cli::cli_abort(c(
        "{.arg seed} is required when passing a data.frame.",
        "i" = "Supply the original seed words used to produce the candidates."
      ))
    }
    seed_words     <- unique(trimws(tolower(seed)))
    accepted_words <- unique(trimws(tolower(result[["word"]])))
    accepted_words <- setdiff(accepted_words, seed_words)
  } else {
    cli::cli_abort(c(
      "{.arg result} must be a {.cls lexiexpand_result}, a {.cls data.frame}, \
       or a character vector.",
      "i" = "See {.help lexiExpand::export_dict} for details."
    ))
  }

  # ── Assemble final word list ──────────────────────────────────────────────
  all_words <- if (include_seed) {
    unique(c(seed_words, accepted_words))
  } else {
    unique(accepted_words)
  }

  if (length(all_words) == 0L) {
    cli::cli_warn("The exported dictionary is empty (no accepted words).")
  }

  # ── Format output ─────────────────────────────────────────────────────────
  switch(format,

    "list" = {
      stats::setNames(list(all_words), name)
    },

    "data.frame" = {
      data.frame(
        word     = all_words,
        category = name,
        stringsAsFactors = FALSE
      )
    },

    "quanteda" = {
      if (!requireNamespace("quanteda", quietly = TRUE)) {
        cli::cli_abort(c(
          "{.pkg quanteda} is not installed.",
          "i" = "Install it with {.run install.packages('quanteda')}."
        ))
      }
      dict_list <- stats::setNames(list(all_words), name)
      quanteda::dictionary(dict_list)
    }
  )
}
