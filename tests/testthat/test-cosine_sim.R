test_that(".cosine_neighbours returns correct structure", {
  set.seed(42)
  mat <- matrix(rnorm(200), nrow = 20, ncol = 10)
  rownames(mat) <- paste0("word", seq_len(20))
  mat <- lexiExpand:::.l2_normalise(mat)

  result <- lexiExpand:::.cosine_neighbours(
    seeds   = c("word1", "word2"),
    mat     = mat,
    n       = 5L,
    exclude = c("word1", "word2")
  )

  expect_s3_class(result, "data.frame")
  expect_named(result, c("word", "similarity", "seed"))
  expect_false(any(result$word %in% c("word1", "word2")))
  expect_true(all(result$similarity >= -1 & result$similarity <= 1))
  expect_true(nrow(result) > 0L)
})

test_that(".cosine_neighbours results are sorted descending", {
  set.seed(1)
  mat <- matrix(rnorm(300), nrow = 30, ncol = 10)
  rownames(mat) <- paste0("w", seq_len(30))
  mat <- lexiExpand:::.l2_normalise(mat)

  result <- lexiExpand:::.cosine_neighbours("w1", mat, n = 10L, exclude = "w1")
  expect_true(all(diff(result$similarity) <= 0))   # non-increasing
})

test_that(".cosine_neighbours warns on missing seed words", {
  set.seed(7)
  mat <- matrix(rnorm(50), nrow = 5, ncol = 10)
  rownames(mat) <- c("happy", "joy", "glad", "sad", "angry")
  mat <- lexiExpand:::.l2_normalise(mat)

  expect_warning(
    lexiExpand:::.cosine_neighbours(
      seeds   = c("happy", "notaword"),
      mat     = mat,
      n       = 3L,
      exclude = c("happy", "notaword")
    ),
    regexp = "not found"
  )
})

test_that(".cosine_neighbours errors when NO seeds are in vocabulary", {
  set.seed(3)
  mat <- matrix(rnorm(30), nrow = 3, ncol = 10)
  rownames(mat) <- c("apple", "orange", "grape")
  mat <- lexiExpand:::.l2_normalise(mat)

  expect_error(
    lexiExpand:::.cosine_neighbours(
      seeds   = c("zzz", "yyy"),
      mat     = mat,
      n       = 2L,
      exclude = c("zzz", "yyy")
    ),
    regexp = "None of"
  )
})
