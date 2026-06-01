test_that(".l2_normalise produces unit-length rows", {
  mat <- matrix(c(3, 4, 0, 1, 0, 0), nrow = 2, byrow = TRUE)
  rownames(mat) <- c("a", "b")
  normed <- lexiExpand:::.l2_normalise(mat)
  norms  <- sqrt(rowSums(normed^2))
  expect_equal(unname(norms), c(1, 1), tolerance = 1e-10)
})

test_that(".l2_normalise handles zero-norm rows without error", {
  mat <- matrix(0, nrow = 2, ncol = 3)
  rownames(mat) <- c("zero1", "zero2")
  # Should not error; norms replaced with 1 → rows stay zero
  expect_no_error(lexiExpand:::.l2_normalise(mat))
})

test_that("%||% returns right-hand side when left is NULL", {
  expect_equal(lexiExpand:::`%||%`(NULL, 42L), 42L)
  expect_equal(lexiExpand:::`%||%`(5L,   42L),  5L)
})

test_that(".check_vector_matrix accepts valid matrix", {
  mat <- matrix(1:9, nrow = 3)
  rownames(mat) <- c("a", "b", "c")
  expect_true(lexiExpand:::.check_vector_matrix(mat))
})

test_that(".check_vector_matrix rejects non-matrix input", {
  expect_error(
    lexiExpand:::.check_vector_matrix(data.frame(x = 1:3)),
    class = "rlang_error"
  )
})

test_that(".check_vector_matrix rejects matrix without row names", {
  mat <- matrix(1:6, nrow = 2)
  expect_error(
    lexiExpand:::.check_vector_matrix(mat),
    class = "rlang_error"
  )
})
