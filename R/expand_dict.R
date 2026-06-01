#' Expand a seed dictionary using semantic similarity
#'
#' Given a character vector of seed words, finds semantically similar words
#' from pre-trained GloVe word embeddings, filters by a similarity threshold,
#' and either returns the candidate data frame directly or launches an
#' interactive review wizard.
#'
#' @param seed Character vector of seed words. Example: `c("sad", "angry")`.
#' @param n Integer. Number of candidate neighbours to retrieve per seed word
#'   (or per centroid in `"centroid"` mode) before applying the threshold.
#'   Default `20L`.
#' @param threshold Numeric in \[0, 1\]. Minimum cosine similarity required
#'   to include a candidate. Default `0.65`.
#' @param seed_mode Character. Multi-seed expansion strategy:
#'   * `"individual"` *(default)* — finds neighbours for each seed word
#'     separately, then deduplicates keeping the highest similarity.
#'     Preserves which seed word drove each match (`$seed` column).
#'   * `"centroid"` — averages all seed vectors into a single centroid and
#'     finds neighbours of that centroid. Returns words nearest the semantic
#'     *centre* of all seeds jointly; better when seeds define a coherent
#'     concept rather than a loose theme. (`$seed` column is `"centroid"`.)
#' @param model Character. Which GloVe model to use. Must already have been
#'   downloaded via [download_vectors()]. Default `"glove.6B.50d"`.
#' @param cache_dir Character. Cache directory. Default:
#'   `rappdirs::user_data_dir("lexiExpand")`.
#' @param vocab_size Integer or `NULL`. Maximum vocabulary loaded by
#'   [load_vectors()]. Default `50000L`.
#' @param interactive Logical. If `TRUE` (default when R is interactive),
#'   launches the word-by-word review wizard via [review_candidates()].
#'   If `FALSE`, returns the full ranked data frame silently.
#' @param vectors Numeric matrix or `NULL`. Supply a pre-loaded,
#'   L2-normalised word-vector matrix (as returned by [load_vectors()] or
#'   [load_vectors_textdata()]) to skip the loading step.
#'
#' @return
#' * When `interactive = FALSE`: a `data.frame` with columns:
#'   \describe{
#'     \item{`word`}{Candidate word.}
#'     \item{`similarity`}{Cosine similarity to the nearest seed word or
#'       centroid. Ranges from -1 to 1; values below {.arg threshold} are excluded.}
#'     \item{`seed`}{Seed word that drove this match, or `"centroid"` in
#'       centroid mode.}
#'     \item{`pct_match`}{Similarity as a percentage string, e.g. `"87%"`.}
#'   }
#' * When `interactive = TRUE`: a `lexiexpand_result` object (invisibly)
#'   with `$accepted`, `$seed`, and `$candidates`.
#'
#' @seealso [download_vectors()], [load_vectors()], [load_vectors_textdata()],
#'   [review_candidates()], [export_dict()]
#'
#' @examples
#' \dontrun{
#' download_vectors("glove.6B.50d")
#'
#' # Default: per-seed neighbours
#' candidates <- expand_dict(c("sad", "angry"),
#'                           n = 15, threshold = 0.70,
#'                           interactive = FALSE)
#'
#' # Centroid mode: find words near the semantic centre of both seeds
#' candidates_c <- expand_dict(c("sad", "angry"),
#'                             seed_mode   = "centroid",
#'                             n           = 15,
#'                             threshold   = 0.70,
#'                             interactive = FALSE)
#'
#' # Using textdata as the vector source
#' vecs <- load_vectors_textdata(dimensions = 300, vocab_size = 30000)
#' result <- expand_dict(c("innovation", "invention"),
#'                       vectors = vecs, interactive = FALSE)
#' }
#'
#' @export
expand_dict <- function(
    seed,
    n           = 20L,
    threshold   = 0.65,
    seed_mode   = c("individual", "centroid"),
    model       = "glove.6B.50d",
    cache_dir   = rappdirs::user_data_dir("lexiExpand"),
    vocab_size  = 50000L,
    interactive = base::interactive(),
    vectors     = NULL
) {
  # ── Input validation ──────────────────────────────────────────────────────
  if (!is.character(seed) || length(seed) == 0L) {
    cli::cli_abort(
      "{.arg seed} must be a non-empty character vector, e.g. {.code c('sad', 'angry')}."
    )
  }
  if (!is.numeric(threshold) || length(threshold) != 1L ||
      threshold < 0 || threshold > 1) {
    cli::cli_abort(
      "{.arg threshold} must be a single number between 0 and 1."
    )
  }
  if (!is.numeric(n) || length(n) != 1L || n < 1L) {
    cli::cli_abort("{.arg n} must be a positive integer.")
  }

  seed_mode <- match.arg(seed_mode)
  seed      <- unique(trimws(tolower(seed)))
  n         <- as.integer(n)

  # ── Load or validate vectors ──────────────────────────────────────────────
  if (is.null(vectors)) {
    model   <- match.arg(
      model,
      c("glove.6B.50d", "glove.6B.100d", "glove.6B.200d", "glove.6B.300d")
    )
    vectors <- load_vectors(
      model      = model,
      cache_dir  = cache_dir,
      vocab_size = vocab_size
    )
  } else {
    .check_vector_matrix(vectors)
    vectors <- .l2_normalise(vectors)
  }

  # ── Find nearest neighbours ───────────────────────────────────────────────
  candidates <- if (seed_mode == "centroid") {
    .cosine_centroid(seeds = seed, mat = vectors, n = n, exclude = seed)
  } else {
    .cosine_neighbours(seeds = seed, mat = vectors, n = n, exclude = seed)
  }

  # ── Apply threshold ───────────────────────────────────────────────────────
  candidates <- candidates[candidates$similarity >= threshold, , drop = FALSE]

  if (nrow(candidates) == 0L) {
    cli::cli_warn(c(
      "!" = "No candidates found above threshold {.val {threshold}}.",
      "i" = "Try lowering {.arg threshold} or increasing {.arg n}."
    ))
    return(invisible(candidates))
  }

  # Deduplicate (relevant in individual mode when a word appears near multiple seeds)
  candidates <- candidates[!duplicated(candidates$word), , drop = FALSE]
  candidates$pct_match <- paste0(round(candidates$similarity * 100), "%")
  rownames(candidates) <- NULL

  # ── Dispatch ──────────────────────────────────────────────────────────────
  if (!interactive) {
    return(candidates)
  }

  review_candidates(candidates, seed)
}


#' Expand a lexicon-style data frame by dimension
#'
#' Accepts a two-column data frame where the first column contains words and
#' the second column contains their dimension labels (e.g. the `mde_words`
#' object from \pkg{mdeinR}). Runs [expand_dict()] on each unique dimension
#' group and returns all candidate expansions in a single tidy data frame.
#'
#' @param lexicon A data frame with at least two columns. The first column
#'   is treated as words; the second as dimension labels. Column names are
#'   detected automatically, or you can specify them explicitly via
#'   `word_col` and `dim_col`.
#' @param word_col Character. Name of the word column. If `NULL` (default),
#'   the first column is used.
#' @param dim_col Character. Name of the dimension column. If `NULL`
#'   (default), the second column is used.
#' @param n Integer. Candidates per seed word per dimension. Default `20L`.
#' @param threshold Numeric. Minimum cosine similarity. Default `0.65`.
#' @param seed_mode Character. `"individual"` (default) or `"centroid"`.
#'   See [expand_dict()].
#' @param vectors Numeric matrix or `NULL`. Pre-loaded vector matrix from
#'   [load_vectors()]. Strongly recommended when calling this function to
#'   avoid reloading vectors for each dimension.
#' @param model,cache_dir,vocab_size Passed to [load_vectors()] if `vectors`
#'   is `NULL`.
#'
#' @return A data frame with columns `word`, `similarity`, `seed`,
#'   `pct_match`, and `dimension` — one row per candidate across all
#'   dimensions, sorted by dimension then descending similarity. Words
#'   already present in the lexicon are excluded from results.
#'
#' @seealso [expand_dict()], [export_dict()]
#'
#' @examples
#' \dontrun{
#' # Typical use with mdeinR
#' library(mdeinR)
#' vecs <- load_vectors("glove.6B.50d")
#'
#' # mde_words is a data frame: columns 'word' and 'dimension'
#' candidates <- expand_lexicon(mde_words, vectors = vecs)
#' head(candidates)
#'
#' # Works with any two-column data frame
#' my_lexicon <- data.frame(
#'   term  = c("delicious", "fragrant", "joyful", "warm"),
#'   group = c("sensory",   "sensory",  "affect", "affect")
#' )
#' expand_lexicon(my_lexicon, vectors = vecs, threshold = 0.70)
#' }
#'
#' @export
expand_lexicon <- function(
    lexicon,
    word_col   = NULL,
    dim_col    = NULL,
    n          = 20L,
    threshold  = 0.65,
    seed_mode  = c("individual", "centroid"),
    vectors    = NULL,
    model      = "glove.6B.50d",
    cache_dir  = rappdirs::user_data_dir("lexiExpand"),
    vocab_size = 50000L
) {
  # ── Validate lexicon ───────────────────────────────────────────────────────
  if (!is.data.frame(lexicon) || ncol(lexicon) < 2L) {
    cli::cli_abort(
      "{.arg lexicon} must be a data frame with at least two columns \\
       (words and dimension labels)."
    )
  }

  wc <- word_col %||% names(lexicon)[1L]
  dc <- dim_col  %||% names(lexicon)[2L]

  if (!wc %in% names(lexicon)) {
    cli::cli_abort("Word column {.val {wc}} not found in {.arg lexicon}.")
  }
  if (!dc %in% names(lexicon)) {
    cli::cli_abort("Dimension column {.val {dc}} not found in {.arg lexicon}.")
  }

  words      <- trimws(tolower(as.character(lexicon[[wc]])))
  dimensions <- as.character(lexicon[[dc]])
  dim_levels <- unique(dimensions)

  # ── Load vectors once ──────────────────────────────────────────────────────
  if (is.null(vectors)) {
    vectors <- load_vectors(model, cache_dir, vocab_size)
  } else {
    .check_vector_matrix(vectors)
    vectors <- .l2_normalise(vectors)
  }

  all_existing <- unique(words)
  seed_mode    <- match.arg(seed_mode)

  cli::cli_inform(
    "Expanding {length(dim_levels)} dimension{?s}: \\
     {.val {dim_levels}}"
  )

  # ── Expand per dimension ───────────────────────────────────────────────────
  results <- lapply(dim_levels, function(dim) {
    seeds <- unique(words[dimensions == dim])
    cli::cli_inform("  {dim}: {length(seeds)} seed word{?s}")

    candidates <- tryCatch(
      expand_dict(
        seed        = seeds,
        n           = n,
        threshold   = threshold,
        seed_mode   = seed_mode,
        vectors     = vectors,
        interactive = FALSE
      ),
      warning = function(w) {
        cli::cli_warn("  {dim}: {conditionMessage(w)}")
        suppressWarnings(expand_dict(
          seed        = seeds,
          n           = n,
          threshold   = threshold,
          seed_mode   = seed_mode,
          vectors     = vectors,
          interactive = FALSE
        ))
      }
    )

    if (nrow(candidates) == 0L) return(NULL)

    # Remove words already anywhere in the lexicon
    candidates <- candidates[!candidates$word %in% all_existing, , drop = FALSE]
    if (nrow(candidates) == 0L) return(NULL)

    candidates$dimension <- dim
    candidates
  })

  results <- Filter(Negate(is.null), results)

  if (length(results) == 0L) {
    cli::cli_warn("No candidates found above threshold {.val {threshold}}.")
    return(data.frame(word = character(), similarity = numeric(),
                      seed = character(), pct_match = character(),
                      dimension = character(), stringsAsFactors = FALSE))
  }

  df <- do.call(rbind, results)
  df <- df[order(df$dimension, -df$similarity), , drop = FALSE]
  rownames(df) <- NULL
  df
}
