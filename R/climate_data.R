
#' Load Environment Canada Historical Climate Data
#'
#' @param location A vector of unambiguous name identifiers or station IDs
#'   (resolved using \link{as_ec_climate_location}).
#' @param timeframe One of monthly, daily, or hourly.
#' @param start The first date to be included in the output as a Date object
#'   or in YYYY-MM-DD format (passed through \link{as.Date})
#' @param end The last date to be included in the output as a Date object
#'   or in YYYY-MM-DD format (passed through \link{as.Date})
#' @param value_parser A readr parse function (like
#'   \link[readr]{parse_double} or \link[readr]{parse_character}) to apply
#'   to value columns. The default is to use \link[readr]{parse_double}, but
#'   occasionally values are in the form ">30", or "<30", especially for wind speed.
#'   When this happens a warning will occur, and \link[readr]{problems}() can be used
#'   to see which values were dropped. Use \link[readr]{parse_character}) to skip parsing
#'   and extract the values yourself.
#' @param cache A directory in which to cache downloaded files
#' @param quiet Use FALSE for verbose output
#'
#' @references
#' \url{http://climate.weather.gc.ca/historical_data/search_historic_data_e.html}
#' \url{ftp://client_climate@ftp.tor.ec.gc.ca/Pub/Get_More_Data_Plus_de_donnees/Readme.txt}
#' \url{http://climate.weather.gc.ca/glossary_e.html}
#'
#' @return A data.frame (tibble) with an attribute "flag_info", containing the flag information.
#'  \code{ec_climate_mudata()} returns a \link[mudata2]{mudata} object.
#' @export
#'
#' @examples
#' \donttest{
#' # station 27141 is Kentville CDA CS
#' monthly <- ec_climate_data(27141, timeframe = "monthly")
#' daily <- ec_climate_data(27141, timeframe = "daily", start = "1999-01-01", end = "1999-12-31")
#' hourly <- ec_climate_data(27141, timeframe = "hourly", start = "1999-07-01", end = "1999-07-31")
#'
#' # get climate data in mudata format
#' library(mudata2)
#' monthly_md <- ec_climate_mudata(27141, timeframe = "monthly")
#' daily_md <- ec_climate_mudata(27141, timeframe = "daily",
#'                               start = "1999-01-01", end = "1999-12-31")
#' hourly_md <- ec_climate_mudata(27141, timeframe = "hourly",
#'                                start = "1999-07-01", end = "1999-07-31")
#'
#' # mudata objects can easily be plotted
#' autoplot(monthly_md)
#' autoplot(daily_md)
#' autoplot(hourly_md)
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
ec_climate_data <- function(location, timeframe = c("monthly", "daily", "hourly"),
                            start = NA, end = NA, value_parser = readr::parse_double,
                            cache = get_default_cache(), quiet = TRUE) {
  # validate arguments
  timeframe <- match.arg(timeframe)

  # validate/resolve locations (ignore duplicates)
  location <- as_ec_climate_location(unique(location))

  # make start and end Date objects
  start <- as.Date(start)
  end <- as.Date(end)

  # if the start and end objects are not length == 1, code below fails
  if(length(start) != 1) stop("start must be length 1")
  if(length(end) != 1) stop("end must be length 1")

  # check year, month, and day args while generating arguments to pass to ec_climate_data_base()
  if(timeframe == "monthly") {
    # monthly data from EC requires no arguments (start and end can be NA)
    args <- tibble::tibble(location = location)
  } else if(timeframe == "daily") {
    # need start and end dates for daily data
    if(is.na(start) || is.na(end)) stop("Must have start/end dates for daily requests")
    if(start > end) stop("start date must be before end date")

    # daily data from EC is downloaded with one file per year
    year <- seq(lubridate::year(start), lubridate::year(end))
    args <- purrr::cross_df(list(location = location, year = unique(year)))
  } else if(timeframe == "hourly") {
    # need start and end dates for hourly data
    if(is.na(start) || is.na(end)) stop("Must have start/end dates for hourly requests")
    if(start > end) stop("start date must be before end date")

    # hourly data from EC is downloaded with one file per month per location
    all_dates <- seq(start, end, by = 1)
    all_dates_df <- dplyr::distinct(
      tibble::tibble(
        year = lubridate::year(all_dates),
        month = lubridate::month(all_dates)
      )
    )
    args <- tibble::tibble(location = location, dates = list(all_dates_df)) %>%
      tidyr::unnest(.data$dates)
  } else {
    stop("Unrecognized timeframe: ", timeframe)
  }

  # warn the user if they are about to download lots of files, even if quiet = TRUE
  if(nrow(args) > 1) message(sprintf("Downloading %s files (use quiet = FALSE for details)", nrow(args)))

  # safely loop through each file
  args$data <- purrr::pmap(args, purrr::safely(ec_climate_data_base),
                           cache = cache, quiet = quiet, timeframe = timeframe,
                           check_dates = TRUE)
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

  # assign the dataset as the first column
  climate_out$dataset <- rep_len(paste0("ec_climate_", timeframe), nrow(climate_out))

  # parse datetime columns
  climate_out <- ec_climate_parse_dates(climate_out)

  # filter using start and end arguments
  if(!is.na(start)) {
    row_offset <- sum(climate_out$date < start)
    climate_out <- dplyr::filter(climate_out, .data$date >= start)
  }
  if(!is.na(end)) {
    climate_out <- dplyr::filter(climate_out, .data$date <= end)
  }

  # extract value columns using _flag columns
  value_cols <- ec_climate_extract_value_columns(climate_out)$values

  # make value_parser silent
  value_parser_quiet <- function(...) suppressWarnings(value_parser(...))

  # coerce to double using readr's parse function, which fails quietly with warnings
  climate_out <- dplyr::mutate_at(climate_out, dplyr::vars(dplyr::one_of(value_cols)),
                                  value_parser_quiet, na = "", locale = readr::locale())

  # create problems attribute if there are any parsing problems
  probs <- seq_along(climate_out) %>%
    purrr::map_dfr(
      function(col_number) {
        probs <- readr::problems(climate_out[[col_number]])
        probs$col <- rep(col_number, nrow(probs))
        probs
      }
    )

  if(nrow(probs) > 0) {
    warning("One or more parsing error(s) occurred. See problems(...) ",
            "or pass value_parser = readr::parse_character to diagnose.")
    attr(climate_out, "problems") <- probs
  }

  # extract flag information
  flag_info <- dplyr::bind_rows(args$flags) %>% dplyr::distinct()
  attr(climate_out, "flag_info") <- flag_info

  # return climate out with datetime information
  climate_out
}

#' @rdname ec_climate_data
#' @export
ec_climate_mudata <- function(location, timeframe = c("monthly", "daily", "hourly"),
                              start = NA, end = NA,
                              cache = get_default_cache(), quiet = TRUE) {
  # resolve timeframe arg
  timeframe <- match.arg(timeframe)

  # get data from ec_climate_data
  climate_df <- ec_climate_data(location = location, timeframe = timeframe,
                                start = start, end = end, cache = cache, quiet = quiet)

  # extract flags df from output
  climate_flags <- attr(climate_df, "flag_info")

  # get data table in long form
  md_data <- ec_climate_long(climate_df, na.rm = TRUE)

  # get the locations table, and add the dataset column
  md_locations <- tibble::as_tibble(as_ec_climate_location(unique(md_data$location)))
  md_locations$dataset <- unique(md_data$dataset)

  # get the params table
  md_params <- dplyr::distinct(md_data[c("dataset", "param")]) %>%
    dplyr::left_join(ec_climate_params_all, by = c("dataset", "param"))

  # assign the x columns
  if(timeframe == "hourly") {
    x_columns <- c("date", "date_time_utc")
    data_quality <- "data_quality"
  } else if(timeframe == "daily") {
    x_columns <- "date"
    data_quality <- "data_quality"
  } else if(timeframe == "monthly") {
    x_columns <- "date"
    data_quality <- character(0)
  }

  # finalize data table
  md_data <- md_data %>%
    dplyr::select("dataset", "location", "param", dplyr::one_of(x_columns),
                  "value", dplyr::one_of(data_quality), "flag")

  # create datasets table
  package_version <- paste(unlist(utils::packageVersion("rclimateca")[[1]]), collapse = ".")
  dataset <- unique(md_locations$dataset)
  md_datasets <- tibble::tibble(
    dataset = dataset,
    timeframe = rep(timeframe, length(dataset)),
    url = rep("http://climate.weather.gc.ca/", length(dataset)),
    source = rep(sprintf("rclimateca version %s", package_version), length(dataset))
  )

  # create mudata object
  mudata2::mudata(
    data = md_data,
    locations = md_locations,
    params = md_params,
    datasets = md_datasets,
    x_columns = x_columns,
    more_tbls = list(flag_info = climate_flags)
  )
}


#' Transform EC Climate Data to Parameter-Long Form
#'
#' @param climate_df The outuput of \link{ec_climate_data}
#' @param na.rm TRUE to remove measurements for which there is no information
#'
#' @return A data.frame (tibble) with one row per measurement
#' @export
#'
#' @examples
#' \donttest{
#' # station 27141 is Kentville CDA CS
#' monthly <- ec_climate_data(27141, timeframe = "monthly")
#' ec_climate_long(monthly)
#'
#' # or use the pipe
#' ec_climate_data(27141, timeframe = "monthly") %>%
#'   ec_climate_long()
#' }
#'
ec_climate_long <- function(climate_df, na.rm = FALSE) {

  # extract flags df from output
  climate_flags <- attr(climate_df, "flag_info")

  # if the "weather" column is present, turn it into a flag column
  # because the weather column is text and the value column should be numeric
  if("weather" %in% colnames(climate_df)) {
    # get value column type using preliminary col_info
    col_info <- ec_climate_extract_value_columns(climate_df)
    na_val <- climate_df[[col_info$values[1]]][NA_integer_]

    climate_df$weather_flag <- climate_df$weather
    climate_df$weather <- rep(na_val, nrow(climate_df))
  }

  # get numeric value columns and flag columns
  col_info <- ec_climate_extract_value_columns(climate_df)

  # get vector to transform nice column labels to param identifiers
  transformer <- ec_climate_params_all %>%
    dplyr::select("nice_label", "param") %>%
    dplyr::distinct() %>%
    tibble::deframe()

  # get data table in long form
  long_data <- mudata2::parallel_gather(
    climate_df,
    key = "nice_label",
    value = dplyr::one_of(col_info$values),
    flag = dplyr::one_of(col_info$flags)
  ) %>%
    dplyr::mutate(nice_label = transformer[.data$nice_label]) %>%
    dplyr::rename(param = "nice_label")

  if(na.rm) {
    long_data <- long_data %>%
      # filter out measurements where there is no information
      dplyr::filter(!is.na(.data$value) | !is.na(.data$flag))
  }

  # attach the flags as an attribute
  attr(long_data, "flag_info") <- climate_flags

  long_data
}


#' Low-level access to the EC Climate Bulk Data Service
#'
#' @param year The year for which to get data (required for daily requests)
#' @param month The month for which to get data (required for hourly requests)
#' @param endpoint The web address for the EC data service
#' @param check_dates Check the request data against \link{ec_climate_locations_all}
#'   to avoid downloading data that is known not to exist. Pass FALSE to circumvent
#'   this check.
#' @inheritParams ec_climate_data
#'
#' @references
#' \url{http://climate.weather.gc.ca/historical_data/search_historic_data_e.html}
#' \url{ftp://client_climate@ftp.tor.ec.gc.ca/Pub/Get_More_Data_Plus_de_donnees/Readme.txt}
#'
#' @return A data.frame (tibble) of the downloaded data frame, with all columns as
#'   character vectors
#' @export
#'
#' @examples
#' \donttest{
#' # station 27141 is Kentville CDA CS
#' monthly <- ec_climate_data_base(27141, timeframe = "monthly")
#' daily <- ec_climate_data_base(27141, timeframe = "daily", year = 1999)
#' hourly <- ec_climate_data_base(27141, timeframe = "hourly", year = 1999, month = 7)
#' }
#'
ec_climate_data_base <- function(location, timeframe = c("monthly", "daily", "hourly"),
                                 year = NULL, month = NULL,
                                 cache = NULL, quiet = FALSE,
                                 check_dates = FALSE,
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

  # check if the year is outside the range of observed dates
  # if it is, return an empty tibble with the correct columns
  if(check_dates && !is.null(year) && !ec_climate_check_date(location, timeframe, year)) {
    return(ec_climate_empty(timeframe))
  }

  # download the file (or not if the cache already contains the data)
  x <- restquery(endpoint, .encoding = "windows-1252",
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
                                  stringsAsFactors = F, check.names = F,
                                  na.strings = c("", " ", "NA"),
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

ec_climate_empty <- function(timeframe = c("monthly", "daily", "hourly")) {

  # validate timeframe argument
  timeframe <- match.arg(timeframe)

  if(timeframe == "monthly") {

    cols <- c("Date/Time", "Year", "Month", "Mean Max Temp (\u00B0C)", "Mean Max Temp Flag",
              "Mean Min Temp (\u00B0C)", "Mean Min Temp Flag", "Mean Temp (\u00B0C)",
              "Mean Temp Flag", "Extr Max Temp (\u00B0C)", "Extr Max Temp Flag",
              "Extr Min Temp (\u00B0C)", "Extr Min Temp Flag", "Total Rain (mm)",
              "Total Rain Flag", "Total Snow (cm)", "Total Snow Flag", "Total Precip (mm)",
              "Total Precip Flag", "Snow Grnd Last Day (cm)", "Snow Grnd Last Day Flag",
              "Dir of Max Gust (10's deg)", "Dir of Max Gust Flag", "Spd of Max Gust (km/h)",
              "Spd of Max Gust Flag")

  } else if(timeframe == "daily") {

    cols <- c("Date/Time", "Year", "Month", "Day", "Data Quality", "Max Temp (\u00B0C)",
              "Max Temp Flag", "Min Temp (\u00B0C)", "Min Temp Flag", "Mean Temp (\u00B0C)",
              "Mean Temp Flag", "Heat Deg Days (\u00B0C)", "Heat Deg Days Flag",
              "Cool Deg Days (\u00B0C)", "Cool Deg Days Flag", "Total Rain (mm)",
              "Total Rain Flag", "Total Snow (cm)", "Total Snow Flag", "Total Precip (mm)",
              "Total Precip Flag", "Snow on Grnd (cm)", "Snow on Grnd Flag",
              "Dir of Max Gust (10s deg)", "Dir of Max Gust Flag", "Spd of Max Gust (km/h)",
              "Spd of Max Gust Flag")

  } else if(timeframe == "hourly") {

    cols <- c("Date/Time", "Year", "Month", "Day", "Time", "Data Quality",
              "Temp (\u00B0C)", "Temp Flag", "Dew Point Temp (\u00B0C)", "Dew Point Temp Flag",
              "Rel Hum (%)", "Rel Hum Flag", "Wind Dir (10s deg)", "Wind Dir Flag",
              "Wind Spd (km/h)", "Wind Spd Flag", "Visibility (km)", "Visibility Flag",
              "Stn Press (kPa)", "Stn Press Flag", "Hmdx", "Hmdx Flag", "Wind Chill",
              "Wind Chill Flag", "Weather")

  } else {
    stop("Unrecognized timeframe")
  }

  # empty output should be zero rows
  climate_out <- tibble::as_tibble(
    lapply(purrr::set_names(cols), function(x) character(0))
  )

  attr(climate_out, "flag_info") <- tibble::tibble(flag = character(0), description = character(0))

  climate_out
}

ec_climate_check_date <- function(location, timeframe = c("monthly", "daily", "hourly"), year) {

  # validate timeframe argument
  timeframe <- match.arg(timeframe)

  # validate location argument
  location <- as_ec_climate_location(location)
  location_tbl <- as.list(tibble::as_tibble(location))

  # any year past 2017 should be treated as 2017, in the off chance that
  # ec_climate_locations_all doesn't get updated
  year <- min(year, 2017)

  timeframe_abbrev <- c("monthly" = "mly", "daily" = "dly", "hourly" = "hly")

  col_start <- paste0(timeframe_abbrev[timeframe], "_first_year")
  col_end <- paste0(timeframe_abbrev[timeframe], "_last_year")

  # NA means there was never any data for that timeframe
  if(is.na(location_tbl[[col_start]]) || is.na(location_tbl[[col_end]])) {
    return(FALSE)
  }

  (year >= location_tbl[[col_start]]) && (year <= location_tbl[[col_end]])
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
        df_nice["location"],
        ec_climate_locations_all[c("location", "timezone_id", "lst_utc_offset")],
        by = "location"
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
      } else if(length(timezones) == 0) {
        # this happens when zero-row tibbles get passed in
        timezones <- "UTC"
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
