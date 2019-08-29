context("caches")
skip_if_offline()

test_that("the cache can be cleared", {
  temp_cache <- tempfile()

  # cache should start empty
  expect_length(list.files(temp_cache, "\\.csv$"), 0)

  # check a single instance of each monthly, daily, and hourly files
  monthly <- ec_climate_data(27141, timeframe = "monthly",
                             cache = temp_cache)
  daily <- ec_climate_data(27141, timeframe = "daily", start = "1999-01-01", end = "1999-12-31",
                           cache = temp_cache)
  hourly <- ec_climate_data(27141, timeframe = "hourly", start = "1999-07-01", end = "1999-07-31",
                            cache = temp_cache)

  # three files should have been downloaded
  expect_length(list.files(temp_cache, "\\.csv$"), 3)

  # clear the cache
  clear_cache(cache = temp_cache)

  # cache should be empty
  expect_length(list.files(temp_cache, "\\.csv$"), 0)
})

test_that("the default cache can be set", {
  current <- get_default_cache()
  new <- tempfile()
  expect_identical(set_default_cache(new), current)
  expect_identical(get_default_cache(), new)

  set_default_cache(current)
})

test_that("the default cache can be set to NULL", {
  current <- get_default_cache()
  new <- NULL
  expect_identical(set_default_cache(new), current)
  expect_identical(get_default_cache(), new)

  set_default_cache(current)
})
