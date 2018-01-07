context("restquery")

test_that("restquery returns a length 1 character vector", {
  result <- restquery(.endpoint="https://httpbin.org/get",
                      arg1 = "value1", arg2 = "value2", .encoding = "UTF-8")

  expect_length(result, 1)
  expect_is(result, "character")
})

test_that("restquery fails properly", {
  expect_error(
    restquery(.endpoint="https://not.an-address-at-all-ever-ever.org/get",
              arg1 = "value1", arg2 = "value2", .encoding = "UTF-8"),
    "Unable to connect to"
  )
})

test_that("messaging is supressed upon request", {
  expect_message(
    restquery(.endpoint="https://httpbin.org/get",
              arg1 = "value1", arg2 = "value2", .encoding = "UTF-8",
              .quiet = FALSE),
    "Retreiving information from"
  )

  expect_silent(
    restquery(.endpoint="https://httpbin.org/get",
              arg1 = "value1", arg2 = "value2", .encoding = "UTF-8",
              .quiet = TRUE)
  )
})

test_that("the parser is used when present", {
  expect_is(
    restquery(.endpoint="https://httpbin.org/get",
              arg1 = "value1", arg2 = "value2", .encoding = "UTF-8",
              .parser = jsonlite::fromJSON),
    "list"
  )
})

test_that("the cache is used when present", {
  temp_cache <- tempfile()

  # no files should have been downloaded
  expect_length(list.files(temp_cache, "\\.csv$"), 0)

  expect_message(
    restquery(.endpoint="https://httpbin.org/get",
              arg1 = "value1", arg2 = "value2", .encoding = "UTF-8",
              .quiet = FALSE, .cache = temp_cache),
    "Retreiving information from"
  )

  # one file should have been downloaded
  expect_length(list.files(temp_cache, "\\.csv$"), 1)

  expect_message(
    restquery(.endpoint="https://httpbin.org/get",
              arg1 = "value1", arg2 = "value2", .encoding = "UTF-8",
              .quiet = FALSE, .cache = temp_cache),
    "Using cached information for"
  )
})
