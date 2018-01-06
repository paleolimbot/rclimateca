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
  daily <- ec_climate_data(27141, timeframe = "daily", start = "1999-01-01", end = "1999-12-31")
  hourly <- ec_climate_data(27141, timeframe = "hourly", start = "1999-07-01", end = "1999-07-31")

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

test_that("column types for ec_climate_data() are correct", {
  # check a single instance of each monthly, daily, and hourly files
  monthly <- ec_climate_data(27141, timeframe = "monthly")
  daily <- ec_climate_data(27141, timeframe = "daily", start = "1999-01-01", end = "1999-12-31")
  hourly <- ec_climate_data(27141, timeframe = "hourly", start = "1999-07-01", end = "1999-07-31")

  # list of expected column types
  expected_types <- c(
    "dataset" = "character",
    "location" = "character",
    "year" = "integer",
    "month" = "integer",
    "day" = "integer",
    "time_lst" = "hms",
    "date_time_utc" = "POSIXct",
    "date_time_local" = "POSIXct",
    "date" = "Date"
  )

  types_monthly <- names(expected_types) %>%
    purrr::set_names() %>%
    purrr::keep(~.x %in% names(monthly)) %>%
    purrr::map_lgl(~inherits(monthly[[.x]], expected_types[.x]))

  types_daily <- names(expected_types) %>%
    purrr::set_names() %>%
    purrr::keep(~.x %in% names(daily)) %>%
    purrr::map_lgl(~inherits(daily[[.x]], expected_types[.x]))

  types_hourly <- names(expected_types) %>%
    purrr::set_names() %>%
    purrr::keep(~.x %in% names(hourly)) %>%
    purrr::map_lgl(~inherits(hourly[[.x]], expected_types[.x]))

  expect_true(all(types_monthly))
  expect_true(all(types_daily))
  expect_true(all(types_hourly))


  # all value columns should be numeric, all flag columns should be character
  val_cols_monthly <- rclimateca:::ec_climate_extract_value_columns(monthly)
  val_cols_daily <- rclimateca:::ec_climate_extract_value_columns(daily)
  val_cols_hourly <- rclimateca:::ec_climate_extract_value_columns(hourly)

  expect_true(
    all(
      purrr::map_lgl(monthly[val_cols_monthly$values], inherits, "numeric")
    )
  )

  expect_true(
    all(
      purrr::map_lgl(monthly[val_cols_monthly$flags], inherits, "character")
    )
  )

  expect_true(
    all(
      purrr::map_lgl(daily[val_cols_daily$values], inherits, "numeric")
    )
  )

  expect_true(
    all(
      purrr::map_lgl(daily[val_cols_daily$flags], inherits, "character")
    )
  )

  expect_true(
    all(
      purrr::map_lgl(hourly[val_cols_hourly$values], inherits, "numeric")
    )
  )

  expect_true(
    all(
      purrr::map_lgl(hourly[val_cols_hourly$flags], inherits, "character")
    )
  )

  # data quality colum should be character
  expect_is(daily$data_quality, "character")
  expect_is(hourly$data_quality, "character")
})

test_that("dates and times are parsed correctly", {

  # check a single instance of each monthly, daily, and hourly files
  monthly <- ec_climate_data(27141, timeframe = "monthly")
  daily <- ec_climate_data(27141, timeframe = "daily", start = "1999-01-01", end = "1999-12-31")
  hourly <- ec_climate_data(27141, timeframe = "hourly", start = "1999-07-01", end = "1999-07-31")

  expect_is(monthly$date, "Date")
  expect_is(daily$date, "Date")
  expect_is(hourly$date, "Date")

  expect_is(hourly$time_lst, "hms")
  expect_is(hourly$date_time_local, "POSIXct")
  expect_is(hourly$date_time_utc, "POSIXct")

  # check dates
  expect_equal(
    lubridate::make_date(year = monthly$year, month = monthly$month, day = 1),
    monthly$date
  )

  expect_equal(
    lubridate::make_date(year = daily$year, month = daily$month, day = daily$day),
    daily$date
  )

  expect_equal(
    lubridate::make_date(year = hourly$year, month = hourly$month, day = hourly$day),
    hourly$date
  )

  # check times
  hourly_subset <- hourly %>% dplyr::filter(month == 7, day == 1)
  expect_true(nrow(hourly_subset) == 24)

  expect_true(
    diff(range(hourly_subset$date_time_local)) == lubridate::dhours(23)
  )
  expect_true(
    diff(range(hourly_subset$date_time_utc)) == lubridate::dhours(23)
  )
  expect_true(
    diff(range(hourly_subset$time_lst)) == lubridate::dhours(23)
  )

  # min local standard time is 0:00
  expect_true(min(hourly_subset$time_lst) == lubridate::dseconds(0))
  # min local time is 1:00 because of daylight savings
  expect_equal(
    min(hourly_subset$date_time_local),
    lubridate::make_datetime(year = 1999, month = 7, day = 1, hour = 1, min = 0, tz = "America/Halifax")
  )
  expect_equal(
    min(hourly_subset$date_time_utc),
    lubridate::make_datetime(year = 1999, month = 7, day = 1, hour = 4, min = 0, tz = "UTC")
  )
})

test_that("times are parsed correctly for multiple locations", {

  # check quietness of single location download
  expect_silent(ec_climate_data(27141, timeframe = "hourly", start = "1999-07-01", end = "1999-07-31"))
  expect_silent(ec_climate_data(4337, timeframe = "hourly", start = "1999-07-01", end = "1999-07-31"))

  # check message ouput when multiple timezones are used
  expect_message(
    ec_climate_data(c(27141, 4337), timeframe = "hourly", start = "1999-07-01", end = "1999-07-31"),
    "Using time zone America/Halifax"
  )
  expect_message(
    ec_climate_data(c(4337, 27141), timeframe = "hourly", start = "1999-07-01", end = "1999-07-31"),
    "Using time zone America/Toronto"
  )

  hourly_kent <- ec_climate_data(27141, timeframe = "hourly", start = "1999-07-01", end = "1999-07-31")
  hourly_ott <- ec_climate_data(4337, timeframe = "hourly", start = "1999-07-01", end = "1999-07-31")
  hourly_kent_first <- ec_climate_data(c(27141, 4337), timeframe = "hourly",
                                       start = "1999-07-01", end = "1999-07-31")
  hourly_ott_first <- ec_climate_data(c(27141, 4337), timeframe = "hourly",
                                      start = "1999-07-01", end = "1999-07-31")

  # all local standard times should be equal between cities
  expect_true(all(hourly_kent$time_lst == hourly_ott$time_lst))

  # all time differences should be one hour
  # (kentville starts the day an hour earlier than ottawa)
  expect_true(all((hourly_kent$date_time_utc - hourly_ott$date_time_utc) == -1))

  # the (printed) local times should be the same
  expect_equal(
    as.character(hourly_kent$date_time_local),
    as.character(hourly_ott$date_time_local)
  )
})

test_that("date filtering is respected", {

  # check a single instance of each monthly, daily, and hourly files
  start <- as.Date("1999-01-15")
  end <- as.Date("1999-02-15")

  monthly <- ec_climate_data(27141, timeframe = "monthly", start = start, end = end)
  daily <- ec_climate_data(27141, timeframe = "daily", start = start, end = end)
  hourly <- ec_climate_data(27141, timeframe = "hourly", start = start, end = end)

  # monthly should only be one date (because the date column is the first of the month)
  expect_equal(monthly$date, as.Date("1999-02-01"))

  # others should have first date as start and last date as end
  expect_equal(min(daily$date), start)
  expect_equal(max(daily$date), end)
  expect_equal(min(hourly$date), start)
  expect_equal(max(hourly$date), end)
})

test_that("dates with length != 1 produce errors", {
  expect_error(
    ec_climate_data(27141, timeframe = "monthly", start = character(0), end = NA),
    "start must be length 1"
  )
  expect_error(
    ec_climate_data(27141, timeframe = "monthly", start = NA, end = character(0)),
    "end must be length 1"
  )
})

test_that("daily and hourly requests without start and end dates fail", {
  expect_error(
    ec_climate_data(27141, timeframe = "daily", start = "1999-01-01", end = NA),
    "Must have start/end dates for daily requests"
  )
  expect_error(
    ec_climate_data(27141, timeframe = "daily", start = NA, end = "1999-01-01"),
    "Must have start/end dates for daily requests"
  )
  expect_error(
    ec_climate_data(27141, timeframe = "hourly", start = "1999-01-01", end = NA),
    "Must have start/end dates for hourly requests"
  )
  expect_error(
    ec_climate_data(27141, timeframe = "hourly", start = NA, end = "1999-01-01"),
    "Must have start/end dates for hourly requests"
  )
})

test_that("no files are downloaded when the locations table indicates there is no data", {
  temp_cache <- tempfile()
  dir.create(temp_cache)

  # hourly first year is 1999, daily and monthly first year is 1996
  # monthly downloads have no constraints, and so it is not possible to avoid downloading
  # this file
  daily <- ec_climate_data(27141, timeframe = "daily", start = "1995-01-01", end = "1995-12-31",
                           cache = temp_cache)
  hourly <- ec_climate_data(27141, timeframe = "hourly", start = "1998-12-01", end = "1998-12-31",
                            cache = temp_cache)

  # no files should have been downloaded
  expect_length(list.files(temp_cache, "\\.csv$"), 0)

  # and the output should have zero rows
  expect_equal(nrow(daily), 0)
  expect_equal(nrow(hourly), 0)

  # when *some* valid dates exist, it should still work
  daily <- ec_climate_data(27141, timeframe = "daily", start = "1995-01-01", end = "1996-01-31",
                           cache = temp_cache)
  hourly <- ec_climate_data(27141, timeframe = "hourly", start = "1998-12-01", end = "1999-01-31",
                            cache = temp_cache)

  # two files should have been downloaded
  expect_length(list.files(temp_cache, "\\.csv$"), 2)

  # and the output should not have zero rows
  expect_false(nrow(daily) == 0)
  expect_false(nrow(hourly) == 0)

  unlink(temp_cache, recursive = TRUE)
})

# test_that("the quiet flag is respected", {
#   expect_true(FALSE)
# })
#
# test_that("the cache flag is respected", {
#   expect_true(FALSE)
# })
