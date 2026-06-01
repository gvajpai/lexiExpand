test_that("expand_dict returns data.frame in non-interactive mode", {
  set.seed(99)
  n_words <- 50L
  n_dims  <- 25L
  mat <- matrix(rnorm(n_words * n_dims), nrow = n_words, ncol = n_dims)
  rownames(mat) <- c("sad", "angry", paste0("word", seq_len(n_words - 2L)))
  mat <- lexiExpand:::.l2_normalise(mat)

  result <- expand_dict(
    seed        = c("sad", "angry"),
    n           = 10L,
    threshold   = 0.0,    # accept everything to have output
    interactive = FALSE,
    vectors     = mat
  )

  expect_s3_class(result, "data.frame")
  expect_true(all(c("word", "similarity", "seed", "pct_match") %in% names(result)))
  expect_false(any(result$word %in% c("sad", "angry")))
  expect_true(all(result$similarity >= 0.0))
})

test_that("expand_dict errors on bad seed input", {
  expect_error(expand_dict(seed = 123),   regexp = "character")
  expect_error(expand_dict(seed = c()),   regexp = "character")
})

test_that("expand_dict errors on bad threshold", {
  expect_error(
    expand_dict(seed = "sad", threshold = 2, interactive = FALSE),
    regexp = "threshold"
  )
})

test_that("expand_dict warns when no candidates pass threshold", {
  set.seed(5)
  mat <- matrix(rnorm(30), nrow = 3L, ncol = 10L)
  rownames(mat) <- c("sad", "w1", "w2")
  mat <- lexiExpand:::.l2_normalise(mat)

  expect_warning(
    expand_dict(
      seed        = "sad",
      threshold   = 0.9999,    # extremely high — nothing will pass
      n           = 2L,
      interactive = FALSE,
      vectors     = mat
    ),
    regexp = "No candidates"
  )
})

test_that("expand_dict deduplicates when word near multiple seeds", {
  # Manually construct a matrix where one word is closest to both seeds
  mat <- rbind(
    seed1    = c(1, 0),
    seed2    = c(0, 1),
    shared   = c(0.8, 0.6),    # near both seeds
    unshared = c(-1,  0)
  )
  mat <- lexiExpand:::.l2_normalise(mat)

  result <- expand_dict(
    seed        = c("seed1", "seed2"),
    n           = 2L,
    threshold   = 0.0,
    interactive = FALSE,
    vectors     = mat
  )

  expect_equal(sum(result$word == "shared"), 1L)   # only one row for "shared"
})

test_that("expand_lexicon handles two-column data frame", {
  set.seed(77)
  mat <- matrix(rnorm(600), nrow = 60, ncol = 10)
  rownames(mat) <- c(
    "delicious", "fragrant", "crispy",    # sensory
    "joyful",    "excited",  "moved",     # affect
    "friendly",  "warm",     "caring",    # social
    paste0("w", seq_len(51))
  )
  mat <- lexiExpand:::.l2_normalise(mat)

  lex <- data.frame(
    word      = c("delicious", "fragrant", "crispy",
                  "joyful",    "excited",  "moved",
                  "friendly",  "warm",     "caring"),
    dimension = c("sensory",   "sensory",  "sensory",
                  "affect",    "affect",   "affect",
                  "social",    "social",   "social"),
    stringsAsFactors = FALSE
  )

  result <- expand_lexicon(lex, vectors = mat, threshold = 0.0, n = 5L)

  expect_s3_class(result, "data.frame")
  expect_true("dimension" %in% names(result))
  expect_true(all(unique(result$dimension) %in% c("sensory", "affect", "social")))
  expect_false(any(result$word %in% lex$word))  # no seeds in output
})

test_that("expand_lexicon accepts custom column names", {
  set.seed(88)
  mat <- matrix(rnorm(200), nrow = 20, ncol = 10)
  rownames(mat) <- c("aroma", "taste", paste0("x", seq_len(18)))
  mat <- lexiExpand:::.l2_normalise(mat)

  lex <- data.frame(
    term  = c("aroma", "taste"),
    group = c("sensory", "sensory"),
    stringsAsFactors = FALSE
  )

  result <- expand_lexicon(lex, word_col = "term", dim_col = "group",
                            vectors = mat, threshold = 0.0, n = 5L)
  expect_true(nrow(result) > 0L)
  expect_true("dimension" %in% names(result))
})

test_that("expand_lexicon errors on non-data-frame input", {
  expect_error(expand_lexicon(c("sad", "angry")), regexp = "data frame")
})
