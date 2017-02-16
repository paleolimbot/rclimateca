
#' Get Parsed and aggregated Environment Canada data
#'
#' Use this function to get Environment Canada climate data in bulk over multiple
#' years and/or stations.
#'
#' @param stationID The station ID, possibly found by \link{getClimateSites}
#' @param timeframe One of "monthly", "hourly", or "daily"
#' @param year A vector of years for which to fetch data
#' @param month A vector of months for which to fetch data
#' @param day A vector of day numbers for which to fetch data
#' @param cache A folder in which to cache downloaded files
#' @param quiet Suppress update messages
#' @param progress A plyr progress bar, one of "text", "tk", or "none"
#' @param format One of "wide" or "long". Use long for easy integration with ggplot.
#' @param rm.na Pass rm.na=TRUE to remove rows with no values. This may help compress large
#'   datasets
#' @param parsedates Flag to parse date/time information (useful for plotting).
#' @param checkdates If checkdates is TRUE, the loop will not attempt to download if a year is
#'   marked as missing in \link{climateLocs2016}. Note that this information may be out of date,
#'   but this flag is useful to minimize the amount of downloading that needs to occur. This will
#'   also subset the resulting data frame to only contain the years/months requested.
#' @param nicenames Use lower-case, unit-free names for columns.
#' @param ply The plyr-like function that executes the loop and returns the result. Pass your
#'   own function accepting named arguments .data, .margins=1, .fun=function(row),
#'   .progress. This may be useful if all you need to do is extract information out of a large
#'   amount of climate data without the need to store it to disk.
#'
#' @return A data.frame (or the results of \code{ply} if passed).
#'
#' @references
#' \url{http://climate.weather.gc.ca/historical_data/search_historic_data_e.html}
#' \url{ftp://client_climate@ftp.tor.ec.gc.ca/Pub/Get_More_Data_Plus_de_donnees/Readme.txt}
#'
#' @export
#'
#' @examples
#' # don't test because fetching of file slows down testing
#' \donttest{
#' wv <- getClimateSites("Kentville, NS", year=2016, nicenames=TRUE)
#' stationID <- wv$stationid[1]
#' df <- getClimateData(stationID, timeframe="daily", year=2014:2016)
#'
#' # easy plotting
#' library(ggplot2)
#' df <- getClimateData(stationID, timeframe="daily", year=2014:2016, format="long")
#' ggplot(df, aes(parsedDate, value)) + geom_line() + facet_wrap(~param, scales="free_y")
#'
#' # nicenames are FALSE by default
#' df <- getClimateData(stationID, timeframe="daily", year=2014:2016, format="long", nicenames=TRUE)
#' ggplot(df, aes(parseddate, value)) + geom_line() + facet_wrap(~param, scales="free_y")
#'
#' # use MUData option to use the MUData format
#' md <- getClimateMUData(c(27141, 6354), year=1999, month=7:8, timeframe="daily")
#' plot(md)
#' }
getClimateData <- function(stationID, timeframe=c("monthly", "daily", "hourly"),
                           year=NULL, month=NULL, day=NULL, cache="ec.cache", quiet=TRUE,
                           progress=c("text", "none", "tk"), format=c("wide", "long"),
                           rm.na=FALSE, parsedates=TRUE, checkdates=TRUE,
                           nicenames=FALSE, ply=plyr::adply) {
  timeframe <- match.arg(timeframe)
  progress <- match.arg(progress)
  format <- match.arg(format)
  stationID <- unique(stationID)

  if(timeframe=="monthly") {
    if(!is.null(day)) stop("Cannot get montly data with a day constraint")
    args <- data.frame(stationID=stationID, Year=NA, Month=NA,
                       stringsAsFactors = FALSE)
  } else if(timeframe=="daily") {
    if(is.null(year)) stop("Year required for daily requests")
    args <- expand.grid(stationID=stationID, Year=year, stringsAsFactors = FALSE)
    args$Month <- NA
  } else if(timeframe=="hourly") {
    if(is.null(year)) stop("Year required for hourly requests")
    if(is.null(month)) {
      month <- 1:12
    }
    args <- expand.grid(stationID=stationID, Year=year, Month=month, stringsAsFactors = FALSE)
  } else {
    stop("Unrecognized timeframe: ", timeframe)
  }

  result <- ply(.data=args, .margins=1, .fun=function(row) {
    if(checkdates) {
      # check that the stationID/year combination exists
      yrs <- getyears(row$stationID, timeframe)
      if(!is.na(row$Year) && !(row$Year %in% yrs)) {
        return(data.frame())
      }
    }

    row <- as.list(row)
    row$timeframe <- timeframe
    if(is.null(cache)) {
      row['.cache'] <- list(NULL)
    } else {
      row$.cache <- cache
    }
    row$.quiet <- quiet
    res <- try(do.call(getClimateDataRaw, row), silent=TRUE)
    if(class(res) == "try-error") {
      stop("Download failed for args ", paste(names(row), unlist(row, use.names = F),
                                              sep="=", collapse="/"), ": ", res)
    }
    if(parsedates) {
      res <- parsedates(res, timeframe)
    }
    if(checkdates) {
      filter <- rep(TRUE, nrow(res))
      if(!is.null(year)) {
        filter <- filter & res$Year %in% year
      }
      if(!is.null(month)) {
        filter <- filter & res$Month %in% month
      }
      if(!is.null(day)) {
        filter <- filter & res$Day %in% day
      }
      res <- res[filter,]
    }
    if(format=="long") {
      res <- climatelong(res, rm.na=rm.na)
    }
    return(res)
  }, .progress=progress)

  if(nicenames) {
    if(format=="long") {
      result$param <- nice.names(result$param)
    }
    names(result) <- nice.names(names(result))
  }
  return(result)
}

#' @rdname getClimateData
#' @export
getClimateMUData <- function(stationID, timeframe=c("monthly", "daily", "hourly"),
                             year=NULL, month=NULL, day=NULL, cache="ec.cache", quiet=TRUE,
                             progress=c("text", "none", "tk"), rm.na=FALSE) {
  if(!requireNamespace("mudata", quietly = TRUE))
    stop("Package 'mudata' required for call to 'getClimateMUData()")
  # get data
  stationID <- unique(stationID)
  longdata <- getClimateData(stationID, timeframe=timeframe, year=year, month=month,
                             day=day, cache=cache, quiet=quiet, progress=progress,
                             rm.na=rm.na, format="long",
                             nicenames = FALSE, checkdates = TRUE, parsedates = TRUE)
  names(longdata) <- nice.names(names(longdata))

  # get locations
  locs <- climateLocs2016[match(stationID, climateLocs2016$`Station ID`),]
  names(locs) <- nice.names(names(locs))
  # prevent duplicate location names (some exist)
  locs$location <- ifelse(duplicated(locs$name), paste(locs$name, 1:nrow(locs)), locs$name)
  locs$dataset <- 'ecclimate'

  # make params table and convert to nice names
  allparams <- unique(longdata$param)
  params <- data.frame(dataset='ecclimate', param=nice.names(allparams),
                       label=allparams, stringsAsFactors = FALSE)
  longdata$param <- nice.names(longdata$param)

  # subset data columns
  tags <- c('dataquality', 'flags')
  tags <- tags[tags %in% names(longdata)]
  longdata$dataset <- 'ecclimate'
  # make locations as names, not IDs
  longdata$location <- locs$name[match(longdata$stationid, locs$stationid)]
  longdata$x <- longdata$parseddate
  longdata <- longdata[c('dataset', 'location', 'param', 'x', 'value', tags)]

  mudata::mudata(longdata, locations=locs, params=params)
}

#' Transform EC data to long format
#'
#' @param df A wide data frame (obtained from \link{getClimateData} or \link{getClimateDataRaw})
#' @param rm.na Flag to remove rows with an empty value. This may help compress large datasets.
#'
#' @return A melted \code{data.frame} (see reshape2::melt)
#' @export
#'
#' @examples
#' # don't test because fetching of file slows down testing
#' \donttest{
#' df <- getClimateData(27141, timeframe="daily", year=2014:2016)
#' climatelong(df)
#' # also works with nicenames=TRUE
#' df <- getClimateData(27141, timeframe="daily", year=2014:2016, nicenames=TRUE)
#' climatelong(df)
#' }
#'
climatelong <- function(df, rm.na=FALSE) {
  cols <- names(df)
  quals <- c("parsedDate", "stationID", "Date/Time","Year","Month","Day",
             "Time", "Data Quality", "Weather")
  quals <- c(quals, nice.names(quals))
  quals <- quals[quals %in% cols]
  flags <- cols[grepl("flag$", cols, ignore.case = TRUE)]
  vals <- cols[!(cols %in% c(flags, quals))]
  if(length(flags) != length(vals)) {
    stop("Length of flags not equal to length of values")
  }
  df <- melt.parallel(df, id.vars = quals, variable.name = 'param', value=vals, flags=flags)
  df$value <- suppressWarnings(as.numeric(df$value))
  if(rm.na) {
    df <- df[!is.na(df$value),]
  }
  df
}

#' Get parsed CSV data from Environment Canada
#'
#' This function just downloads a .csv file from the bulk data service from
#' Environment Canda. It follows as closely as possible the EC specifications,
#' and does not modify the result except to remove the header information. To
#' apply this function over multiple months/stations/years/months, use \link{getClimateData}.
#'
#' @param stationID A stationID (you could find this using \link{getClimateSites})
#' @param timeframe One of "montly" "daily" or "hourly"
#' @param Year The year for which to fetch the data
#' @param Month The month for which to fetch the data
#' @param endpoint The url from which to fetch data (in case this changes in the future)
#' @param flag.info Pass TRUE to get a \code{list} with elements \code{$data} and \code{$flags}
#' @param ... further arguments passed on to the downloading function
#'
#' @return A data.frame of results, or a list if flag.info=TRUE
#' @export
#'
#' @references
#' \url{http://climate.weather.gc.ca/historical_data/search_historic_data_e.html}
#' \url{ftp://client_climate@ftp.tor.ec.gc.ca/Pub/Get_More_Data_Plus_de_donnees/Readme.txt}
#'
#' @examples
#' # don't test because fetching of file slows down testing
#' \donttest{
#' getClimateDataRaw(27141, timeframe="monthly")
#' }
getClimateDataRaw <- function(stationID, timeframe=c("monthly", "daily", "hourly"),
                              Year=NA, Month=NA,
                              endpoint="http://climate.weather.gc.ca/climate_data/bulk_data_e.html",
                              flag.info=FALSE, ...) {
  timeframe <- match.arg(timeframe)
  if(timeframe == "daily" && is.na(Year)) stop("Year required for daily requests")
  if(timeframe == "hourly" && (is.na(Year) || is.na(Month) ))
    stop("Year and month required for hourly requests")

  if(timeframe == "monthly" && (!is.na(Year) || !is.na(Month)))
    stop("Specification of year/month not necessary for montly data")
  if(timeframe == "daily" && (!is.na(Month)))
    stop("Specification of month/day not necessary for daily data")

  x <- restquery(endpoint, .encoding="UTF-8",
                 format="csv", stationID=stationID, submit="Download Data",
                 timeframe=which(timeframe == c("hourly", "daily", "monthly")),
                 Year=Year, Month=Month, ...)
  if(is.null(x)) stop("Download failed")
  # find how many lines are in the header
  xlines <- readLines(textConnection(x))
  empty <- which(nchar(xlines) == 0)
  empty <- empty[empty != length(xlines)]
  cdata <- utils::read.csv(textConnection(x), skip=empty[length(empty)],
                           stringsAsFactors = F, check.names = F)
  if(flag.info) {
    if(length(empty) == 2) {
      # legend is between the two blank lines
      nrows <- empty[2]-empty[1]-2
      if(nrows > 0) {
        flags <- try(utils::read.csv(textConnection(x), skip=empty[1]+1,
                                 stringsAsFactors = F, check.names = F, header = FALSE,
                                 nrows = nrows), silent = TRUE)
        if(class(flags) != "try-error") {
          names(flags) <- c("flag", "description")
        }
      } else {
        flags <- data.frame(flag=NA, description=NA)[FALSE,]
      }
    } else {
      flags <- data.frame(flag=NA, description=NA)[FALSE,]
    }
    return(list(data=cdata, flags=flags))
  } else {
    return(cdata)
  }
}


parsedates <- function(df, timeframe) {
  if(timeframe == "monthly") {
    df$parsedDate <- lubridate::ymd(paste(df[["Date/Time"]], "-1", sep="-"))
  } else if(timeframe == "daily") {
    df$parsedDate <- lubridate::ymd(df[["Date/Time"]])
  } else if(timeframe == "hourly") {
    df$parsedDate <- lubridate::ymd_hm(df[["Date/Time"]])
  }
  return(df)
}

getyears <- function(stationID, timeframe) {
  loc <- climateLocs2016[climateLocs2016$`Station ID`==stationID,]
  if(nrow(loc) != 1) {
    stop("Location ", stationID, " not found")
  } else if(timeframe == "monthly") {
    fy <- loc[["MLY First Year"]]
    ly <- loc[["MLY Last Year"]]
  } else if(timeframe == "daily") {
    fy <- loc[["DLY First Year"]]
    ly <- loc[["DLY Last Year"]]
  } else if(timeframe == "hourly") {
    fy <- loc[["DLY First Year"]]
    ly <- loc[["DLY Last Year"]]
  }
  if(is.na(fy)) return(NULL)
  if(is.na(ly) || ly == 2016) {
    ly <- lubridate::year(Sys.Date())
  }
  return(fy:ly)
}
