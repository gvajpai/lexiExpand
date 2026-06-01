#' lexiExpand: Semantic Dictionary Expander for Text Analysis
#'
#' @description
#' Expands seed dictionaries used in dictionary-based text analysis by finding
#' semantically similar words through pre-trained GloVe word embeddings.
#'
#' @section Typical workflow:
#' ```r
#' # 1. Download vectors once (~160 MB for the 50-dimension model)
#' download_vectors("glove.6B.50d")
#'
#' # 2. Expand a seed dictionary interactively
#' result <- expand_dict(c("sad", "angry"), threshold = 0.70)
#'
#' # 3. Export to your preferred format
#' export_dict(result, name = "negative_emotion", format = "list")
#' ```
#'
#' @section Key functions:
#' * [download_vectors()] — Download & cache GloVe vectors (run once)
#' * [load_vectors()]     — Load cached vectors into a session matrix
#' * [load_vectors_textdata()] — Alternative: load via the textdata package
#' * [expand_dict()]      — Core expansion: seed words → ranked candidates
#' * [expand_lexicon()]   — Expand a full lexicon data frame by dimension
#' * [review_candidates()] — Interactive accept / reject wizard
#' * [export_dict()]      — Export to list / data.frame / quanteda dictionary
#'
#' @keywords internal
"_PACKAGE"

# ── Session-level vector cache ────────────────────────────────────────────────
# Stored in a dedicated environment so it persists for the R session without
# polluting the global environment. Keyed by "<model>_<vocab_size>".
.lexiexpand_env <- new.env(parent = emptyenv())
.lexiexpand_env$vectors <- list()
