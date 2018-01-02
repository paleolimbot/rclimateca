context("climate_data")

test_that("ec_climate_data_base works as intended", {
  # check a single instance of each monthly, daily, and hourly files
  monthly <- ec_climate_data_base(27141, timeframe = "monthly")
  daily <- ec_climate_data_base(27141, timeframe = "daily", year = 1999)
  hourly <- ec_climate_data_base(27141, timeframe = "hourly", year = 1999, month = 7)

  expect_is(monthly, "tbl")
  expect_is(daily, "tbl")
  expect_is(hourly, "tbl")

  monthly_flags <- attr(monthly, "flag_info")
  daily_flags <- attr(daily, "flag_info")
  hourly_flags <- attr(hourly, "flag_info")

  expect_is(monthly_flags, "tbl")
  expect_is(daily_flags, "tbl")
  expect_is(hourly_flags, "tbl")
})

test_that("ec_climate_data works as intended", {
  # check a single instance of each monthly, daily, and hourly files
  monthly <- ec_climate_data(27141, timeframe = "monthly")
  daily <- ec_climate_data(27141, timeframe = "daily", year = 1999)
  hourly <- ec_climate_data(27141, timeframe = "hourly", year = 1999, month = 7)

  expect_is(monthly, "tbl")
  expect_is(daily, "tbl")
  expect_is(hourly, "tbl")

  monthly_flags <- attr(monthly, "flag_info")
  daily_flags <- attr(daily, "flag_info")
  hourly_flags <- attr(hourly, "flag_info")

  expect_is(monthly_flags, "tbl")
  expect_is(daily_flags, "tbl")
  expect_is(hourly_flags, "tbl")
})

#test_that("the quiet flag is respected", {
#
#})

#test_that("the cache flag is respected", {
#
#})
