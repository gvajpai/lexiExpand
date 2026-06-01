test_that("centroid mode returns correct structure", {
  set.seed(10)
  mat <- matrix(rnorm(500), nrow = 50, ncol = 10)
  rownames(mat) <- c("sad", "angry", paste0("w", seq_len(48)))
  mat <- lexiExpand:::.l2_normalise(mat)

  result <- expand_dict(
    seed        = c("sad", "angry"),
    seed_mode   = "centroid",
    n           = 5L,
    threshold   = 0.0,
    interactive = FALSE,
    vectors     = mat
  )

  expect_s3_class(result, "data.frame")
  expect_named(result, c("word", "similarity", "seed", "pct_match"))
  expect_true(all(result$seed == "centroid"))
  expect_false(any(result$word %in% c("sad", "angry")))
})

test_that("centroid mode results are sorted descending by similarity", {
  set.seed(20)
  mat <- matrix(rnorm(300), nrow = 30, ncol = 10)
  rownames(mat) <- c("seed1", "seed2", paste0("w", seq_len(28)))
  mat <- lexiExpand:::.l2_normalise(mat)

  result <- expand_dict(
    seed        = c("seed1", "seed2"),
    seed_mode   = "centroid",
    n           = 10L,
    threshold   = 0.0,
    interactive = FALSE,
    vectors     = mat
  )

  expect_true(all(diff(result$similarity) <= 0))
})

test_that("centroid mode with a single seed behaves like individual mode", {
  set.seed(30)
  mat <- matrix(rnorm(200), nrow = 20, ncol = 10)
  rownames(mat) <- c("sad", paste0("w", seq_len(19)))
  mat <- lexiExpand:::.l2_normalise(mat)

  res_indiv <- expand_dict("sad", seed_mode = "individual",
                            n = 5L, threshold = 0.0,
                            interactive = FALSE, vectors = mat)
  res_cent  <- expand_dict("sad", seed_mode = "centroid",
                            n = 5L, threshold = 0.0,
                            interactive = FALSE, vectors = mat)

  # Top words should be the same (centroid of 1 seed == the seed itself)
  expect_equal(res_indiv$word, res_cent$word)
})

test_that(".cosine_centroid returns expected structure", {
  set.seed(5)
  mat <- matrix(rnorm(300), nrow = 30, ncol = 10)
  rownames(mat) <- paste0("word", seq_len(30))
  mat <- lexiExpand:::.l2_normalise(mat)

  result <- lexiExpand:::.cosine_centroid(
    seeds   = c("word1", "word2"),
    mat     = mat,
    n       = 8L,
    exclude = c("word1", "word2")
  )

  expect_s3_class(result, "data.frame")
  expect_named(result, c("word", "similarity", "seed"))
  expect_true(all(result$seed == "centroid"))
  expect_false(any(result$word %in% c("word1", "word2")))
  expect_true(nrow(result) <= 8L)
})

test_that("load_vectors_textdata errors gracefully when textdata not installed", {
  # Mock textdata not being present by testing the error message
  # (We can't uninstall textdata, so we test the error path via tryCatch)
  if (requireNamespace("textdata", quietly = TRUE)) {
    skip("textdata is installed — skipping 'not installed' path test")
  }
  expect_error(
    load_vectors_textdata(),
    regexp = "textdata"
  )
})
