
#' Load Environment Canada Historical Climate Data
#'
#' @param location An unambiguous name identifier or station ID (resolved using \link{as_ec_climate_location}).
#' @param timeframe One of monthly, daily, or hourly.
#' @param year A year or vector of years
#' @param month A month or vector of months
#' @param day A day or vector of days
#' @param cache A directory in which to cache downloaded files
#' @param quiet Use FALSE for verbose output
#' @param parse_dates Use parsed dates/times instead of year/month/day/hour columns. This will add
#'   columns "date", "local_time", and "datetime", which is a timezone-aware POSIXct vector.
#' @param check_dates Check dates with \link{ec_climate_locations_all} to avoid downloading empty files
#' @param nice_names Use \link{nice_names} to make column names R-friendly
#' @param endpoint The web address for the EC data service
#'
#' @return A data.frame (tibble) with an attribute "flag_info", containing the flag information.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
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
  args$data <- purrr::pmap(args, purrr::safely(ec_climate_data_base), cache = cache, quiet = quiet, timeframe = timeframe)
  args$result <- purrr::map(args$data, "result")
  args$error <- purrr::map(args$data, "error")
  args$has_error <- purrr::map_lgl(error, is.null)
  args$error_message <- ifelse()

  # check for errors
  errors <- unlist(purrr::map(args$data, ~as.character(.x$error)))
  if(length(errors) > 0) {
    stop("The following errors occurred while downloading/parsing climate data: ",
         paste(errors, collapse = ", "))
  }

  # with no errors, unnest data frames
  climate_out <- args %>%
    tidyr::unnest(result)

  # return climate out
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
  ec_climate_check_timeframe_date(timeframe, year, month)

  # download the file (or not if the cache already contains the data)
  x <- restquery(endpoint, .encoding="UTF-8",
                 format = "csv", stationID = as.numeric(location), submit = "Download Data",
                 timeframe = which(timeframe == c("hourly", "daily", "monthly")),
                 Year = year, Month = month, .cache = cache)

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
                                  colClasses = "character")

  # read the flag data if it exists (default is an empty table)
  flag_data <- tibble::tibble(flag = character(0), description = character(0))

  # for flag data to exist, there must be two blank non-consecutive lines
  if((length(empty) == 2) && ((empty[2] - empty[1] - 2) > 0)) {
    # flag information is between the two blank lines
    possible_flag_data <- try(
      readr::read_csv(x,
                      skip = empty[1] + 1, n_max = empty[2] - empty[1] - 2,
                      col_names = c("flag", "description"),
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

ec_climate_check_timeframe_date <- function(timeframe, year, month) {
  if(timeframe == "daily" && is.null(year)) stop("Year required for daily requests")
  if(timeframe == "hourly" && (is.null(year) || is.null(month) ))
    stop("Year and month required for hourly requests")

  if(timeframe == "monthly" && (!is.null(year) || !is.null(month)))
    stop("Specification of year/month not necessary for monthly data")
  if(timeframe == "daily" && (!is.null(month)))
    stop("Specification of month not necessary for daily data")
}
