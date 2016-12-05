

#' Get Data from Environment Canada
#'
#' @param stationID Vector of stationIDs to get data from
#' @param timeframe Resolution of acquired data
#' @param year Vector of years (if applicable)
#' @param month Vector of months (if applicable)
#'
#' @return A data.frame of the results
#' @export
getdata <- function(stationID, timeframe=c("monthly", "daily", "hourly"),
                    year=NULL, month=NULL, cache=NULL, quiet=TRUE,
                    progress=c("text", "none", "tk"), format=c("wide", "long")) {
  timeframe <- match.arg(timeframe)
  progress <- match.arg(progress)
  format <- match.arg(format)

  if(is.null(year)) {
    args <- data.frame(stationID=stationID, year=NA, month=NA,
                       stringsAsFactors = FALSE)
  } else if(is.null(month)) {
    args <- expand.grid(stationID=stationID, year=year, stringsAsFactors = FALSE)
    args$month <- NA
  } else {
    args <- expand.grid(stationID=stationID, year=year, month=month, stringsAsFactors = FALSE)
  }
  plyr::adply(args, .margins=1, .fun=function(row) {
    row <- as.list(row)
    row$timeframe <- timeframe
    if(is.null(cache)) {
      row['.cache'] <- list(NULL)
    } else {
      row$.cache <- cache
    }
    row$.quiet <- quiet
    res <- try(do.call(getdataraw, row), silent=TRUE)
    if(class(res) == "try-error") {
      message("Download failed for args ", paste(names(row), unlist(row, use.names = F), sep="=", collapse="/"),
              ": ", res)
      NULL
    } else if(format=="wide") {
      res
    } else if(format=="long") {
      tolong(df)
    }
  }, .progress=progress)
}

tolong <- function(df) {
  cols <- names(df)
  quals <- c("stationID", "year", "month", "Date/Time","Year","Month","Day", "Time", "Data Quality", "Weather")
  quals <- quals[quals %in% cols]
  flags <- cols[grepl("Flag", cols)]
  vals <- cols[!(cols %in% c(flags, quals))]
  if(length(flags) != length(vals)) {
    browser()
    stop("Length of flags not equal to length of values")
  }
  melt.parallel(df, id.vars = quals, variable.name = 'param', value=vals, flags=flags)
}

#' Get parsed CSV data from Environment Canada
#'
#' @param stationID A stationID (you could find this using \link{getsites})
#' @param timeframe One of "montly" "daily" or "hourly"
#' @param year The year for which to fetch the data
#' @param month The month for which to fetch the data
#' @param day The day for which to fetch the data
#'
#' @return A data.frame of results
#' @export
#'
getdataraw <- function(stationID, timeframe=c("monthly", "daily", "hourly"),
                    year=NA, month=NA, ...) {
  timeframe <- match.arg(timeframe)
  if(timeframe == "daily" && is.na(year)) stop("Year required for daily requests")
  if(timeframe == "hourly" && (is.na(year) || is.na(month) ))
    stop("Year and month required for hourly requests")

  if(timeframe == "monthly" && (!is.na(year) || !is.na(month)))
    stop("Specification of year/month not necessary for montly data")
  if(timeframe == "daily" && (!is.na(month)))
    stop("Specification of month/day not necessary for daily data")

  x <- restquery("http://climate.weather.gc.ca/climate_data/bulk_data_e.html", .encoding="UTF-8",
                           format="csv", stationID=stationID, submit="Download Data",
                           timeframe=which(timeframe == c("hourly", "daily", "monthly")),
                           Year=year, Month=month, ...)
  if(is.null(x)) stop("Download failed")
  # find how many lines are in the header
  xlines <- readLines(textConnection(x))
  empty <- which(nchar(xlines) == 0)
  empty <- empty[empty != length(xlines)]
  read.csv(textConnection(x), skip=empty[length(empty)], stringsAsFactors = F, check.names = F)
}
