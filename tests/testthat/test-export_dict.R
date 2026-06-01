test_that("export_dict produces a named list from character vector", {
  out <- export_dict(
    result = c("miserable", "furious"),
    seed   = c("sad", "angry"),
    name   = "negative_emotion",
    format = "list"
  )
  expect_type(out, "list")
  expect_named(out, "negative_emotion")
  expect_true("sad"       %in% out$negative_emotion)
  expect_true("miserable" %in% out$negative_emotion)
})

test_that("export_dict produces a data.frame", {
  out <- export_dict(
    result = c("miserable", "furious"),
    seed   = c("sad", "angry"),
    name   = "emotion",
    format = "data.frame"
  )
  expect_s3_class(out, "data.frame")
  expect_named(out, c("word", "category"))
  expect_true(all(out$category == "emotion"))
})

test_that("export_dict respects include_seed = FALSE", {
  out <- export_dict(
    result       = c("miserable", "furious"),
    seed         = c("sad", "angry"),
    name         = "emotion",
    format       = "list",
    include_seed = FALSE
  )
  expect_false("sad"   %in% out$emotion)
  expect_false("angry" %in% out$emotion)
  expect_true("miserable" %in% out$emotion)
})

test_that("export_dict works with lexiexpand_result objects", {
  fake_result <- structure(
    list(
      accepted   = c("miserable", "furious"),
      seed       = c("sad", "angry"),
      candidates = data.frame(
        word       = c("miserable", "furious"),
        similarity = c(0.92, 0.88),
        seed       = c("sad", "angry"),
        pct_match  = c("92%", "88%"),
        stringsAsFactors = FALSE
      )
    ),
    class = "lexiexpand_result"
  )

  out <- export_dict(fake_result, name = "emotion", format = "list")
  expect_type(out, "list")
  expect_named(out, "emotion")
  expect_true(all(c("sad", "angry", "miserable", "furious") %in% out$emotion))
})

test_that("export_dict errors on invalid result type", {
  expect_error(export_dict(result = 42), regexp = "character")
})

test_that("export_dict errors when seed missing for character result", {
  expect_error(
    export_dict(result = c("miserable"), name = "em"),
    regexp = "seed"
  )
})

test_that("export_dict errors with invalid name", {
  expect_error(
    export_dict(c("miserable"), seed = "sad", name = ""),
    regexp = "name"
  )
})

test_that("export_dict accepts data.frame from expand_dict directly", {
  df <- data.frame(
    word       = c("miserable", "furious"),
    similarity = c(0.92, 0.88),
    seed       = c("sad", "angry"),
    pct_match  = c("92%", "88%"),
    stringsAsFactors = FALSE
  )
  out <- export_dict(df, seed = c("sad", "angry"),
                     name = "emotion", format = "list")
  expect_type(out, "list")
  expect_true("miserable" %in% out$emotion)
  expect_true("sad"       %in% out$emotion)  # seed included by default
})

test_that("export_dict data.frame path errors without seed arg", {
  df <- data.frame(word = c("miserable"), stringsAsFactors = FALSE)
  expect_error(
    export_dict(df, name = "emotion"),
    regexp = "seed"
  )
})

test_that("export_dict data.frame path errors when no word column", {
  df <- data.frame(term = c("miserable"), stringsAsFactors = FALSE)
  expect_error(
    export_dict(df, seed = "sad", name = "emotion"),
    regexp = "word"
  )
})
