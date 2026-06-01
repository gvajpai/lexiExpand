# ── Internal utilities ────────────────────────────────────────────────────────
# These functions are not exported. They are used across the package internals.

# Null-coalescing operator: returns y if x is NULL, else x.
# @noRd suppresses the malformed Rd file that operator names trigger.
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x


#' Row-wise L2 normalisation of a numeric matrix
#'
#' Divides each row by its Euclidean norm so that every row becomes a unit
#' vector. After normalisation, the dot product of two rows equals their
#' cosine similarity, enabling fast similarity computation via matrix
#' multiplication.
#'
#' Zero-norm rows are left unchanged (norms replaced with 1) to avoid
#' division-by-zero.
#'
#' @param mat A numeric matrix.
#' @return A numeric matrix of the same dimensions with unit-length rows.
#' @keywords internal
.l2_normalise <- function(mat) {
  norms <- sqrt(rowSums(mat^2))
  norms[norms == 0] <- 1L   # guard against zero vectors
  mat / norms
}


#' Validate a user-supplied word-vector matrix
#'
#' Checks that `mat` is a numeric matrix with words stored as row names.
#' Aborts with an informative message if not.
#'
#' @param mat Object to validate.
#' @return `TRUE` invisibly on success.
#' @keywords internal
.check_vector_matrix <- function(mat) {
  if (!is.matrix(mat) || !is.numeric(mat)) {
    cli::cli_abort(
      "{.arg vectors} must be a numeric matrix (rows = words, cols = dimensions)."
    )
  }
  if (is.null(rownames(mat)) || length(rownames(mat)) == 0L) {
    cli::cli_abort(
      "{.arg vectors} must have words as row names."
    )
  }
  invisible(TRUE)
}


#' Return the default on-disk path for a cached model file
#'
#' @param model  Character. Model name, e.g. `"glove.6B.50d"`.
#' @param cache_dir Character. Root cache directory.
#' @return Character scalar: the full path to the `.txt` vector file.
#' @keywords internal
.vector_cache_path <- function(
    model     = "glove.6B.50d",
    cache_dir = rappdirs::user_data_dir("lexiExpand")
) {
  file.path(cache_dir, paste0(model, ".txt"))
}
