context("climate_data")

test_that("ec_climate_data_base works as intended", {
  skip_if_offline()

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
  skip_if_offline()

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
  skip_if_offline()

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

  # data quality colum should be character (now missing from hourly output)
  expect_is(daily$data_quality, "character")
})

test_that("dates and times are parsed correctly", {
  skip_if_offline()

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

  expect_equal(
    as.numeric(diff(range(hourly_subset$date_time_local))), 23
  )
  expect_equal(
    as.numeric(diff(range(hourly_subset$date_time_utc))), 23
  )
  expect_equal(
    as.numeric(diff(range(hourly_subset$time_lst))), 23 * 60 * 60
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
  skip_if_offline()

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
  skip_if_offline()

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
  skip_if_offline()

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
  skip_if_offline()

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

test_that("no files are downloaded when the locations table indicates there was never data", {
  skip_if_offline()

  temp_cache <- tempfile()
  dir.create(temp_cache)

  hourly <- ec_climate_data(5585, timeframe = "hourly",
                            start = "2015-11-01", end = "2015-11-30",
                            cache = temp_cache)

  # no files should have been downloaded
  expect_length(list.files(temp_cache, "\\.csv$"), 0)

  # and the output should have zero rows
  expect_true(nrow(hourly) == 0)

  unlink(temp_cache, recursive = TRUE)
})

test_that("parsing errors result in a warning and a problems attribute", {
  skip_if_offline()

  expect_silent(
    ec_climate_data(
      "CHELSEA QC 5585", timeframe = "daily",
      start = "2015-11-01", end = "2015-11-30"
    )
  )

  expect_warning(
    ec_climate_data(
      "KENTVILLE CDA CS NS 27141", timeframe = "daily",
      start = "2015-11-01", end = "2015-11-30"
    ),
    "One or more parsing error"
  )

  df_good <- ec_climate_data(
    "CHELSEA QC 5585", timeframe = "daily",
    start = "2015-11-01", end = "2015-11-30"
  )

  df_bad <- suppressWarnings(ec_climate_data(
    "KENTVILLE CDA CS NS 27141", timeframe = "daily",
    start = "2015-11-01", end = "2015-11-30"
  ))

  expect_null(attr(df_good, "problems"))
  expect_is(attr(df_bad, "problems"), "data.frame")
  expect_identical(problems(df_bad), attr(df_bad, "problems"))
})

test_that("parse as character works", {
  skip_if_offline()

  expect_silent(
    ec_climate_data(
      "KENTVILLE CDA CS NS 27141", timeframe = "daily",
      start = "2015-11-01", end = "2015-11-30",
      value_parser = readr::parse_character
    )
  )

  df_chr_long <- ec_climate_data(
    "KENTVILLE CDA CS NS 27141", timeframe = "daily",
    start = "2015-11-01", end = "2015-11-30",
    value_parser = readr::parse_character
  ) %>%
    ec_climate_long()

  expect_is(df_chr_long$value, "character")
})

test_that("the quiet flag is respected", {
  skip_if_offline()

  expect_silent(ec_climate_data(27141, timeframe = "monthly"))
  expect_silent(ec_climate_data(27141, timeframe = "daily", start = "1996-01-01", end = "1996-12-31"))
  expect_silent(ec_climate_data(27141, timeframe = "hourly", start = "1999-12-01", end = "1999-12-31"))

  expect_message(
    ec_climate_data(27141, timeframe = "monthly", quiet = FALSE),
    "Using cached information for"
  )
  expect_message(
    ec_climate_data(27141, timeframe = "daily", start = "1996-01-01", end = "1996-12-31",
                    quiet = FALSE),
    "Using cached information for"
  )
  expect_message(
    ec_climate_data(27141, timeframe = "hourly", start = "1999-12-01", end = "1999-12-31",
                    quiet = FALSE),
    "Using cached information for"
  )
})

test_that("the cache flag is respected", {
  skip_if_offline()

  temp_cache <- tempfile()
  dir.create(temp_cache)

  # no files should have been downloaded
  expect_length(list.files(temp_cache, "\\.csv$"), 0)

  daily <- ec_climate_data(27141, timeframe = "daily", start = "1996-01-01", end = "1996-12-31",
                           cache = temp_cache)
  hourly <- ec_climate_data(27141, timeframe = "hourly", start = "1999-12-01", end = "1999-12-31",
                            cache = temp_cache)

  # two files should have been downloaded
  expect_length(list.files(temp_cache, "\\.csv$"), 2)

  # clean cache
  clear_cache(temp_cache)
})

test_that("ec_climate_long correctly transforms output", {
  skip_if_offline()

  # check a single instance of each monthly, daily, and hourly files
  monthly <- ec_climate_data(27141, timeframe = "monthly")
  daily <- ec_climate_data(27141, timeframe = "daily", start = "1999-01-01", end = "1999-12-31")
  hourly <- ec_climate_data(27141, timeframe = "hourly", start = "1999-07-01", end = "1999-07-31")

  monthly_long <- ec_climate_long(monthly)
  daily_long <- ec_climate_long(daily)
  hourly_long <- ec_climate_long(hourly)

  # make sure dates are kept
  expect_equal(unique(monthly_long$date), unique(monthly$date))
  expect_equal(unique(daily_long$date), unique(daily$date))
  expect_equal(unique(hourly_long$date), unique(hourly$date))

  # make sure values are numeric
  expect_is(monthly_long$value, "numeric")
  expect_is(daily_long$value, "numeric")
  expect_is(hourly_long$value, "numeric")

  # make sure flags are character
  expect_is(monthly_long$flag, "character")
  expect_is(daily_long$flag, "character")
  expect_is(hourly_long$flag, "character")
})

test_that("get mudata function for climate data works", {
  skip_if_offline()

  # check a single instance of each monthly, daily, and hourly files
  monthly <- ec_climate_mudata(27141, timeframe = "monthly")
  daily <- ec_climate_mudata(27141, timeframe = "daily", start = "1999-01-01", end = "1999-12-31")
  hourly <- ec_climate_mudata(27141, timeframe = "hourly", start = "1999-07-01", end = "1999-07-31")

  expect_is(monthly, "mudata")
  expect_equal(
    colnames(mudata2::tbl_data(monthly)),
    c("dataset", "location", "param", "date", "value", "flag")
  )

  expect_is(daily, "mudata")
  expect_equal(
    colnames(mudata2::tbl_data(daily)),
    c("dataset", "location", "param", "date", "value", "data_quality", "flag")
  )

  expect_is(hourly, "mudata")
  # data_quality is now missing from hourly data, but could show up in cached data
  expect_equal(
    colnames(mudata2::tbl_data(hourly)),
    c("dataset", "location", "param", "date", "date_time_utc", "value", "flag")
  )
})

test_that("get_mudata function works on zero-row (empty) outputs", {
  skip_if_offline()

  # hourly first year is 1999, daily and monthly first year is 1996
  # monthly downloads have no constraints, and so it is not possible to avoid downloading
  # this file
  daily <- ec_climate_mudata(27141, timeframe = "daily", start = "1995-01-01", end = "1995-12-31")
  hourly <- ec_climate_mudata(27141, timeframe = "hourly", start = "1998-12-01", end = "1998-12-31")


  expect_is(daily, "mudata")
  expect_equal(
    colnames(mudata2::tbl_data(daily)),
    c("dataset", "location", "param", "date", "value", "data_quality", "flag")
  )

  expect_is(hourly, "mudata")
  # data_quality is now missing from hourly data, but could show up in cached data
  expect_equal(
    colnames(mudata2::tbl_data(hourly)),
    c("dataset", "location", "param", "date", "date_time_utc", "value", "flag")
  )
})

test_that("ec_climate_data_read_csv works on downloaded files", {
  skip_if_offline()

  # url for monthly data from yellowknife
  url <- "http://climate.weather.gc.ca/climate_data/bulk_data_e.html?format=csv&stationID=1706&timeframe=3&submit=Download+Data"

  # download file
  temp_dst <- tempfile()
  curl::curl_download(url, temp_dst)

  result <- ec_climate_data_read_csv(temp_dst)
  expect_is(result, "tbl")
  expect_is(attr(result, "flag_info"), "tbl")

  unlink(temp_dst)
})
