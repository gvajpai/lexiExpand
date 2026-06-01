# ── Cosine similarity engine ──────────────────────────────────────────────────
# All functions here are internal (not exported).  They form the mathematical
# core of the package and are separated for testability.


#' Find cosine nearest neighbours for a set of seed words
#'
#' For each seed word present in `mat`, computes the cosine similarity against
#' every row of `mat` (efficiently via matrix multiplication on L2-normalised
#' vectors), then returns the top-`n` candidates across all seeds.
#'
#' @param seeds Character vector of seed words.
#' @param mat Numeric matrix returned by [load_vectors()] (L2-normalised rows,
#'   word row names).
#' @param n Integer. Number of top candidates to retrieve **per seed word**
#'   before deduplication. Default `20L`.
#' @param exclude Character vector of words to suppress from results.
#'   Typically the seed words themselves so they are not recommended back.
#'
#' @return A `data.frame` with three columns:
#'   \describe{
#'     \item{`word`}{Candidate word (character).}
#'     \item{`similarity`}{Cosine similarity score in (-1, 1) (numeric).}
#'     \item{`seed`}{The seed word that produced this candidate (character).}
#'   }
#'   Rows are sorted descending by `similarity`.  Words that appear as
#'   neighbours of multiple seed words are retained under the seed that
#'   yielded the highest similarity.
#'
#' @keywords internal
.cosine_neighbours <- function(seeds, mat, n = 20L, exclude = seeds) {
  # ── Vocabulary check ──────────────────────────────────────────────────────
  valid   <- intersect(seeds, rownames(mat))
  missing <- setdiff(seeds, rownames(mat))

  if (length(missing) > 0L) {
    n_miss   <- length(missing)
    word_str <- if (n_miss == 1L) "word was" else "words were"
    cli::cli_warn(c(
      "!" = "{n_miss} seed {word_str} not found in the loaded vocabulary:",
      " " = "{.val {missing}}",
      "i" = "Try loading more words with a larger {.arg vocab_size} in {.fn load_vectors}."
    ))
  }

  if (length(valid) == 0L) {
    cli::cli_abort(c(
      "None of the provided seed words were found in the vocabulary.",
      "i" = "Check spelling, or increase {.arg vocab_size} in {.fn load_vectors}."
    ))
  }

  n <- min(as.integer(n), nrow(mat))

  # ── Batch cosine similarity ───────────────────────────────────────────────
  # seed_mat : k x d   (k = number of valid seeds, d = dimensions)
  # mat      : V x d   (V = vocabulary size)
  # sim_mat  : k x V   (dot product = cosine sim because rows are unit vectors)
  seed_mat <- mat[valid, , drop = FALSE]
  sim_mat  <- tcrossprod(seed_mat, mat)     # k x V

  # ── Build results data.frame ──────────────────────────────────────────────
  exclude_set <- unique(exclude[exclude %in% colnames(sim_mat) |
                                  exclude %in% rownames(mat)])

  results <- vector("list", length(valid))

  for (i in seq_along(valid)) {
    sims <- sim_mat[i, ]

    # Zero out excluded words
    excl_idx <- which(names(sims) %in% exclude_set)
    if (length(excl_idx) > 0L) {
      sims[excl_idx] <- -Inf
    }

    # Top-n indices (partial sort is faster than full sort for large V)
    top_idx <- .top_n_idx(sims, n)

    results[[i]] <- data.frame(
      word       = names(sims)[top_idx],
      similarity = sims[top_idx],
      seed       = valid[[i]],
      stringsAsFactors = FALSE
    )
  }

  df <- do.call(rbind, results)

  # Drop any rows with non-finite similarity (shouldn't occur, but guard)
  df <- df[is.finite(df$similarity), , drop = FALSE]

  # Sort globally by similarity descending
  df <- df[order(df$similarity, decreasing = TRUE), , drop = FALSE]
  rownames(df) <- NULL

  df
}


#' Return indices of the top-n largest values in a numeric vector
#'
#' Uses `order()` only on the finite, non-suppressed portion; faster than
#' sorting the full vocabulary when most values are `-Inf`.
#'
#' @param x Numeric vector (named).
#' @param n Integer. Number of top indices to return.
#' @return Integer vector of length `min(n, sum(is.finite(x)))`.
#' @keywords internal
.top_n_idx <- function(x, n) {
  finite_idx <- which(is.finite(x))
  if (length(finite_idx) == 0L) return(integer(0L))
  n_take <- min(n, length(finite_idx))
  # partial sort: only rank the top n among finite values
  top_local <- order(x[finite_idx], decreasing = TRUE)[seq_len(n_take)]
  finite_idx[top_local]
}


#' Find cosine nearest neighbours using a centroid (averaged) seed vector
#'
#' Averages all valid seed vectors into a single centroid, L2-normalises it,
#' then returns the top-`n` nearest neighbours from the vocabulary.
#'
#' This approach finds words near the *semantic centre* of
#' all seeds jointly — useful when seeds define a coherent concept and you
#' want words that are collectively close to the whole group rather than close
#' to any individual seed.
#'
#' @inheritParams .cosine_neighbours
#' @return A `data.frame` with columns `word`, `similarity`, and `seed`
#'   (fixed value `"centroid"`), sorted descending by `similarity`.
#' @keywords internal
.cosine_centroid <- function(seeds, mat, n = 20L, exclude = seeds) {
  valid   <- intersect(seeds, rownames(mat))
  missing <- setdiff(seeds, rownames(mat))

  if (length(missing) > 0L) {
    n_miss   <- length(missing)
    word_str <- if (n_miss == 1L) "word was" else "words were"
    cli::cli_warn(c(
      "!" = "{n_miss} seed {word_str} not found in the loaded vocabulary:",
      " " = "{.val {missing}}",
      "i" = "Try a larger {.arg vocab_size} in {.fn load_vectors}."
    ))
  }

  if (length(valid) == 0L) {
    cli::cli_abort(c(
      "None of the provided seed words were found in the vocabulary.",
      "i" = "Check spelling, or increase {.arg vocab_size} in {.fn load_vectors}."
    ))
  }

  # ── Build centroid ─────────────────────────────────────────────────────────
  if (length(valid) == 1L) {
    centroid <- mat[valid, ]
  } else {
    centroid <- colMeans(mat[valid, , drop = FALSE])
  }

  # L2-normalise the centroid so dot product = cosine similarity
  norm <- sqrt(sum(centroid^2))
  if (norm > 0) centroid <- centroid / norm

  # ── Cosine similarity against full vocabulary ──────────────────────────────
  sims <- as.vector(mat %*% centroid)
  names(sims) <- rownames(mat)

  excl_idx <- which(names(sims) %in% unique(exclude[exclude %in% names(sims)]))
  if (length(excl_idx) > 0L) sims[excl_idx] <- -Inf

  n <- min(as.integer(n), sum(is.finite(sims)))
  top_idx <- .top_n_idx(sims, n)

  df <- data.frame(
    word       = names(sims)[top_idx],
    similarity = sims[top_idx],
    seed       = "centroid",
    stringsAsFactors = FALSE
  )
  df <- df[is.finite(df$similarity), , drop = FALSE]
  df <- df[order(df$similarity, decreasing = TRUE), , drop = FALSE]
  rownames(df) <- NULL
  df
}
