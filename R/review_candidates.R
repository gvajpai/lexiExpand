#' Interactive candidate review wizard
#'
#' Presents expansion candidates one by one and asks the researcher whether
#' to accept each word into the expanded dictionary. Called automatically by
#' [expand_dict()] when `interactive = TRUE`, but can also be called directly
#' on a candidate data frame.
#'
#' @param candidates A `data.frame` with at minimum columns `word`,
#'   `similarity`, `seed`, and `pct_match`, as returned by
#'   [expand_dict()] with `interactive = FALSE`.
#' @param seed Character vector of the original seed words (for display).
#'
#' @return A `lexiexpand_result` object (invisibly) with elements:
#'   \describe{
#'     \item{`accepted`}{Character vector of words the user accepted.}
#'     \item{`seed`}{Original seed words.}
#'     \item{`candidates`}{Full candidate data frame (all words, not just accepted).}
#'   }
#'
#' @section Key bindings during the wizard:
#' \tabular{ll}{
#'   `y` \tab Accept this word \cr
#'   `n` \tab Skip this word  \cr
#'   `a` \tab Accept this word and **all remaining** candidates \cr
#'   `q` \tab Quit; keep words accepted so far \cr
#' }
#'
#' @seealso [expand_dict()], [export_dict()]
#'
#' @examples
#' \dontrun{
#' # Run non-interactively first to get the data frame, then review manually
#' cands  <- expand_dict(c("sad", "angry"), interactive = FALSE)
#' result <- review_candidates(cands, seed = c("sad", "angry"))
#' }
#'
#' @export
review_candidates <- function(candidates, seed) {
  # ── Guards ────────────────────────────────────────────────────────────────
  if (!is.data.frame(candidates) || nrow(candidates) == 0L) {
    cli::cli_abort("{.arg candidates} must be a non-empty data frame.")
  }
  required_cols <- c("word", "similarity", "seed", "pct_match")
  missing_cols  <- setdiff(required_cols, names(candidates))
  if (length(missing_cols) > 0L) {
    cli::cli_abort(
      "{.arg candidates} is missing column{?s}: {.val {missing_cols}}."
    )
  }

  # ── Header ────────────────────────────────────────────────────────────────
  cli::cli_h1("Semantic Dictionary Expander \u2014 Review Wizard")
  cli::cli_text("Seed word{?s}: {.val {seed}}")
  cli::cli_text(
    "{nrow(candidates)} candidate{?s} to review, sorted by similarity."
  )
  cli::cli_text(
    "Keys: {.kbd y} accept \u00b7 {.kbd n} skip \u00b7 \\
     {.kbd a} accept all remaining \u00b7 {.kbd q} quit\n"
  )

  accepted <- character(0L)
  i        <- 1L
  n_cands  <- nrow(candidates)

  while (i <= n_cands) {
    row <- candidates[i, ]

    cli::cli_rule(
      left  = paste0("[", i, "/", n_cands, "]"),
      right = paste0("similarity: ", row$pct_match,
                     "  |  seed: ", row$seed)
    )
    cli::cli_text("{.strong {row$word}}")

    resp <- .prompt_user("  > ")

    if (is.null(resp) || resp == "q") {
      cli::cli_inform(c("i" = "Stopped at candidate {i}."))
      break
    }

    switch(resp,
      y = {
        accepted <- c(accepted, row$word)
        cli::cli_inform(c("v" = "Added {.val {row$word}}"))
        i <- i + 1L
      },
      n = {
        cli::cli_inform(c("x" = "Skipped {.val {row$word}}"))
        i <- i + 1L
      },
      a = {
        remaining  <- candidates$word[i:n_cands]
        accepted   <- c(accepted, remaining)
        cli::cli_inform(c(
          "v" = "Accepted all {length(remaining)} remaining candidate{?s}."
        ))
        break
      }
    )
  }

  # ── Summary ───────────────────────────────────────────────────────────────
  cli::cli_rule()
  if (length(accepted) == 0L) {
    cli::cli_inform(c("!" = "No words were accepted."))
  } else {
    cli::cli_inform(c(
      "v" = "Accepted {length(accepted)} word{?s}: {.val {accepted}}"
    ))
  }
  cli::cli_text(
    "Next step: {.run lexiExpand::export_dict(result, name = 'my_category')}"
  )

  # ── Return S3 result object ───────────────────────────────────────────────
  result <- structure(
    list(
      accepted   = accepted,
      seed       = seed,
      candidates = candidates
    ),
    class = "lexiexpand_result"
  )

  invisible(result)
}


# ── Internal prompt helper ────────────────────────────────────────────────────

#' Read a single-character response from the user
#'
#' Loops until the user enters one of the valid keys (`y`, `n`, `a`, `q`).
#' Returns `NULL` immediately in non-interactive sessions so callers can
#' handle the no-TTY case gracefully.
#'
#' @param prompt Character. Prompt string shown to the user.
#' @return One of `"y"`, `"n"`, `"a"`, `"q"`, or `NULL`.
#' @keywords internal
.prompt_user <- function(prompt = "> ") {
  if (!base::interactive()) return(NULL)

  repeat {
    resp <- tolower(trimws(readline(prompt)))
    if (resp %in% c("y", "n", "a", "q")) return(resp)
    cli::cli_inform("Please enter {.kbd y}, {.kbd n}, {.kbd a}, or {.kbd q}.")
  }
}


# ── S3 methods for lexiexpand_result ────────────────────────────────────────────

#' Print an lexiexpand_result
#'
#' @param x A `lexiexpand_result` object.
#' @param ... Ignored.
#' @return `x` invisibly.
#' @export
print.lexiexpand_result <- function(x, ...) {
  cli::cli_h2("lexiExpand result")
  cli::cli_dl(c(
    "Seed words" = paste(x$seed, collapse = ", "),
    "Accepted"   = if (length(x$accepted) == 0L)
                     "(none)"
                   else paste(x$accepted, collapse = ", "),
    "Candidates" = as.character(nrow(x$candidates))
  ))
  invisible(x)
}


#' Coerce an lexiexpand_result to a data.frame
#'
#' Returns a data frame of accepted words with their similarity scores,
#' combining seed words (similarity = 1) and accepted candidates.
#'
#' @param x A `lexiexpand_result` object.
#' @param ... Ignored.
#' @return A `data.frame` with columns `word`, `similarity`, `seed`,
#'   `pct_match`, and a logical `is_seed` column.
#' @export
as.data.frame.lexiexpand_result <- function(x, ...) {
  seed_df <- data.frame(
    word       = x$seed,
    similarity = 1,
    seed       = x$seed,
    pct_match  = "100%",
    is_seed    = TRUE,
    stringsAsFactors = FALSE
  )

  if (length(x$accepted) == 0L) {
    accepted_df <- x$candidates[0L, , drop = FALSE]
    accepted_df$is_seed <- logical(0L)
  } else {
    accepted_df          <- x$candidates[x$candidates$word %in% x$accepted, ,
                                          drop = FALSE]
    accepted_df$is_seed  <- FALSE
  }

  rbind(seed_df, accepted_df)
}
