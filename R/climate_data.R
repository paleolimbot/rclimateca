
#' Load Environment Canada Historical Climate Data
#'
#' @param location An unambiguous name identifier or station ID
#'   (resolved using \link{as_ec_climate_location}).
#' @param timeframe One of monthly, daily, or hourly.
#' @param year A year or vector of years
#' @param month A month or vector of months
#' @param day A day or vector of days
#' @param cache A directory in which to cache downloaded files
#' @param quiet Use FALSE for verbose output
#' @param endpoint The web address for the EC data service
#'
#' @return A data.frame (tibble) with an attribute "flag_info", containing the flag information.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
ec_climate_data <- function(location, timeframe = c("monthly", "daily", "hourly"),
                            year = NULL, month = NULL, day = NULL,
                            cache = "ec.cache", quiet = TRUE) {
  # validate arguments
  timeframe <- match.arg(timeframe)

  # validate/resolve locations (ignore duplicates)
  location <- as_ec_climate_location(unique(location))

  # check year, month, and day args while generating arguments to pass to ec_climate_data_base()
  if(timeframe == "monthly") {
    # monthly data from EC requires no arguments
    if(!is.null(day)) stop("Cannot get monthly data with a day constraint")
    args <- tibble::tibble(location = location)
  } else if(timeframe == "daily") {
    # daily data from EC is downloaded with one file per year
    if(is.null(year)) stop("Year required for daily requests")
    args <- purrr::cross_df(list(location = location, year = unique(year)))
  } else if(timeframe == "hourly") {
    # hourly data from EC is donwloaded with one file per month
    if(is.null(year)) stop("Year required for hourly requests")
    if(is.null(month)) {
      month <- 1:12
    }
    args <- purrr::cross_df(list(location = location, year = unique(year), month = unique(month)))
  } else {
    stop("Unrecognized timeframe: ", timeframe)
  }

  # warn the user if they are about to download lots of files, even if quiet = TRUE
  if(nrow(args) > 1) message(sprintf("Downloading %s files (use quiet = FALSE for details)", nrow(args)))

  # safely loop through each file
  args$data <- purrr::pmap(args, purrr::safely(ec_climate_data_base),
                           cache = cache, quiet = quiet, timeframe = timeframe)
  args$result <- purrr::map(args$data, "result")
  args$has_error <- purrr::map_lgl(args$result, is.null)
  args$flags <- purrr::map(args$result, attr, "flag_info")

  # check for errors
  if(any(args$has_error)) {
    errors <- unlist(purrr::map(args$data, ~as.character(.x$error)))
    stop("The following errors occurred while downloading/parsing climate data: ",
         paste(errors, collapse = ", "))
  }

  # with no errors, unnest data frames
  args$location <- as.character(as_ec_climate_location(args$location))
  args$dataset <- "ec_climate"

  climate_out <- args %>%
    dplyr::select("dataset", "location", "result") %>%
    tidyr::unnest(.data$result) %>%
    set_nice_names()

  # extract value columns using _flag columns
  value_cols <- ec_climate_extract_value_columns(climate_out)$values

  # coerce to double using readr's parse function, which fails quietly with warnings
  climate_out <- dplyr::mutate_at(climate_out, dplyr::vars(dplyr::one_of(value_cols)),
                                  readr::parse_double, na = "", locale = readr::locale())

  # parse datetime columns
  climate_out <- ec_climate_parse_dates(climate_out)

  # extract flag information
  flag_info <- dplyr::bind_rows(args$flags) %>% dplyr::distinct()
  attr(climate_out, "flag_info") <- flag_info

  # return climate out with datetime information
  climate_out
}

#' @rdname ec_climate_data
#' @export
ec_climate_data_base <- function(location, timeframe=c("monthly", "daily", "hourly"),
                                 year = NULL, month = NULL,
                                 cache = NULL, quiet = FALSE,
                                 endpoint = "http://climate.weather.gc.ca/climate_data/bulk_data_e.html") {
  # validate arguments
  timeframe <- match.arg(timeframe)
  stopifnot(length(year) == 1 || is.null(year), length(month) == 1 || is.null(month))

  # validate/resolve locations
  location <- as_ec_climate_location(location)

  # make sure there is enough information (but not too much information) specified in the request
  if(timeframe == "daily" && is.null(year)) stop("Year required for daily requests")
  if(timeframe == "hourly" && (is.null(year) || is.null(month) ))
    stop("Year and month required for hourly requests")

  if(timeframe == "monthly" && (!is.null(year) || !is.null(month)))
    stop("Specification of year/month not necessary for monthly data")
  if(timeframe == "daily" && (!is.null(month)))
    stop("Specification of month not necessary for daily data")

  # download the file (or not if the cache already contains the data)
  x <- restquery(endpoint, .encoding="UTF-8",
                 format = "csv", stationID = as.numeric(location), submit = "Download Data",
                 timeframe = which(timeframe == c("hourly", "daily", "monthly")),
                 Year = year, Month = month, .cache = cache, .quiet = quiet)

  # NULL x is a failed download
  if(is.null(x)) stop("Download failed")

  # use ec_climate_data_read to parse the text
  ec_climate_data_read(x)
}

#' Read a historical climate data CSV file
#'
#' @param path A path to an (unmodified) CSV file downloaded from the EC service
#'
#' @return A tibble with an attribute "flag_info", containing the flag information.
#' @export
#'
ec_climate_data_read_csv <- function(path) {
  x <- readr::read_file(path)
  ec_climate_data_read(x)
}

#' Read a historical climate data CSV from a length 1 character vector
#'
#' @param x A length one character vector (from \link[readr]{read_file} or restquery)
#'
#' @return A tibble
#' @noRd
#'
ec_climate_data_read <- function(x) {
  # find how many lines are in the header
  xlines <- readr::read_lines(x)
  empty <- which(nchar(xlines) == 0)
  empty <- empty[empty != length(xlines)]

  # read the climate data (everything after the last empty line)
  # these files have partial lines occasionally, which causes readr::read_csv() to fail
  climate_data <- utils::read.csv(textConnection(x), skip = empty[length(empty)],
                                  stringsAsFactors = F, check.names = F, na.strings = c("", " "),
                                  strip.white = TRUE, colClasses = "character")

  # read the flag data if it exists (default is an empty table)
  flag_data <- tibble::tibble(flag = character(0), description = character(0))

  # for flag data to exist, there must be two blank non-consecutive lines
  if((length(empty) == 2) && ((empty[2] - empty[1] - 2) > 0)) {
    # flag information is between the two blank lines
    possible_flag_data <- try(
      readr::read_csv(x,
                      skip = empty[1] + 1, n_max = empty[2] - empty[1] - 2,
                      col_names = c("flag", "description"), na = character(0),
                      col_types = readr::cols(.default = readr::col_character())),
      silent = TRUE
    )

    # if something went wrong, send as a warning
    if(inherits(possible_flag_data, "try-error")) {
      warning("Error reading flag information: ", as.character(possible_flag_data))
    } else {
      flag_data <- possible_flag_data
    }
  }

  # make climate_data a tibble
  climate_data <- tibble::as_tibble(climate_data)

  # attach flag data to the climate data as an attribute
  attr(climate_data, "flag_info") <- flag_data

  # return the climate data
  climate_data
}

ec_climate_parse_dates <- function(climate_df) {
  df_nice <- set_nice_names(climate_df)

  df_nice$year <- readr::parse_integer(df_nice$year)
  df_nice$month <- readr::parse_integer(df_nice$month)

  # remove date_time column (it is confusing)
  df_nice$date_time <- NULL

  if("day" %in% colnames(df_nice)) {
    # daily or hourly output
    df_nice$day <- readr::parse_integer(df_nice$day)
    df_nice$date <- as.Date(lubridate::ymd(paste(df_nice$year, df_nice$month, df_nice$day, sep = "-")))

    if("time" %in% colnames(df_nice)) {
      # hourly output
      df_nice$time_lst <- hms::parse_hm(df_nice$time)

      # it is unclear which time is refered to here, so the column should be removed
      df_nice$time <- NULL

      # get a vector of UTC offsets from ec_climate_locations_all
      tz_info <- dplyr::left_join(
        df_nice[c("dataset", "location")],
        ec_climate_locations_all[c("dataset", "location", "timezone_id", "lst_utc_offset")],
        by = c("dataset", "location")
      )

      # combine the date and local_standard_time cols to get local standard datetime
      df_nice$date_time_utc <- lubridate::make_datetime(
        year = df_nice$year,
        month = df_nice$month,
        day = df_nice$day,
        hour = 0,
        min = 0,
        sec = 0,
        tz = "UTC"
      ) + df_nice$time_lst - lubridate::dhours(tz_info$lst_utc_offset)

      # also provide local time (use first timezone_id with a warning if there's more than one)
      timezones <- unique(tz_info$timezone_id)
      if(length(timezones) > 1) {
        message(sprintf("Using time zone %s for column date_time_local", timezones[1]))
      }
      df_nice$date_time_local <- lubridate::with_tz(df_nice$date_time_utc, tzone = timezones[1])

      # move columns to the front (date columns will be moved to the front after)
      df_nice <- dplyr::select(df_nice, "time_lst", "date_time_utc", "date_time_local",
                               dplyr::everything())
    }

    # return df_nice with date columns first
    dplyr::select(df_nice,
                  "dataset", "location",
                  "year", "month", "day", "date",
                  dplyr::everything())
  } else {
    # monthly output
    df_nice$date <- as.Date(lubridate::ymd(paste(df_nice$year, df_nice$month, 1, sep = "-")))

    # return df_nice with date columns first
    dplyr::select(df_nice,
                  "dataset", "location",
                  "year", "month", "date",
                  dplyr::everything())
  }
}

ec_climate_extract_value_columns <- function(climate_df) {
  # extract names
  raw_colnames <- colnames(climate_df)
  nice_colnames <- nice_names(raw_colnames)

  # find value columns using the "_flag" suffix
  flag_columns <- stringr::str_subset(nice_colnames, "_flag$")
  value_columns <- stringr::str_replace(flag_columns, "_flag$", "")
  # the value columns also have a unit, and need to be partial-matched to find the
  # actual column name
  non_flag_colnames <- setdiff(nice_colnames, flag_columns)
  value_columns <- non_flag_colnames[pmatch(value_columns, non_flag_colnames)]

  # match back with original column names
  tibble::tibble(values = raw_colnames[match(value_columns, nice_colnames)],
                 flags = raw_colnames[match(flag_columns, nice_colnames)])
}
