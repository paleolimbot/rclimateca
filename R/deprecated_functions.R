
#' DEPRECATED: Get Parsed and aggregated Environment Canada data
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
#'   marked as missing in \link{ecclimatelocs}. Note that this information may be out of date,
#'   but this flag is useful to minimize the amount of downloading that needs to occur. This will
#'   also subset the resulting data frame to only contain the years/months requested.
#' @param nicenames Use lower-case, unit-free names for columns.
#' @param ply The plyr-like function that executes the loop and returns the result. Pass your
#'   own function accepting named arguments .data, .margins=1, .fun=function(row),
#'   .progress. This may be useful if all you need to do is extract information out of a large
#'   amount of climate data without the need to store it to disk.
#' @param dataset.id The dataset identifier to use for mudata creation.
#'
#' @return A data.frame (or the results of \code{ply} if passed).
#'
#' @references
#' \url{http://climate.weather.gc.ca/historical_data/search_historic_data_e.html}
#' \url{ftp://client_climate@ftp.tor.ec.gc.ca/Pub/Get_More_Data_Plus_de_donnees/Readme.txt}
#'
#' @export
#'
getClimateData <- function(stationID, timeframe=c("monthly", "daily", "hourly"),
                           year=NULL, month=NULL, day=NULL, cache="ec.cache", quiet=TRUE,
                           progress=c("text", "none", "tk"), format=c("wide", "long"),
                           rm.na=FALSE, parsedates=TRUE, checkdates=TRUE,
                           nicenames=FALSE, ply=plyr::adply) {
  # deprecation warning
  message("getClimateData() is deprecated and will be removed in future versions: ",
          "use ec_climate_data() instead")

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
                             progress=c("text", "none", "tk"), rm.na=FALSE,
                             dataset.id="ecclimate") {
  # deprecation warning
  message("getClimateMUData() is deprecated and will be removed in future versions: ",
          "use ec_climate_mudata() instead")

  if(!requireNamespace("mudata2", quietly = TRUE))
    stop("Package 'mudata2' required for call to 'getClimateMUData()")
  # get data
  stationID <- unique(stationID)
  longdata <- getClimateData(stationID, timeframe=timeframe, year=year, month=month,
                             day=day, cache=cache, quiet=quiet, progress=progress,
                             rm.na=rm.na, format="long",
                             nicenames = FALSE, checkdates = TRUE, parsedates = TRUE)
  if(nrow(longdata) == 0) {
    stop("No data available (zero rows) for call: ", deparse(match.call()))
  }
  names(longdata) <- nice.names(names(longdata))

  # get locations
  locs <- ecclimatelocs[match(stationID, ecclimatelocs$`Station ID`),]
  names(locs) <- nice.names(names(locs))
  locs <- locs[!duplicated(names(locs))] # removes duplicate lat/lon columns
  # prevent duplicate location names (some exist)
  locs$location <- ifelse(duplicated(locs$name), paste(locs$name, 1:nrow(locs)), locs$name)
  locs$dataset <- dataset.id

  # make params table and convert to nice names
  allparams <- unique(as.character(longdata$param))
  params <- data.frame(dataset=dataset.id, param=nice.names(allparams),
                       label=allparams, stringsAsFactors = FALSE)
  longdata$param <- nice.names(longdata$param)

  # subset data columns
  tags <- c('dataquality', 'flags')
  tags <- tags[tags %in% names(longdata)]
  longdata$dataset <- 'ecclimate'
  # make locations as names, not IDs
  longdata$location <- locs$name[match(longdata$stationid, locs$stationid)]
  longdata$date <- longdata$parseddate
  longdata <- longdata[c('dataset', 'location', 'param', 'date', 'value', tags)]

  mudata2::mudata(longdata, locations=locs, params=params, x_columns = "date")
}

#' DEPRECATED: Transform EC data to long format
#'
#' @param df A wide data frame (obtained from \link{getClimateData} or \link{getClimateDataRaw})
#' @param rm.na Flag to remove rows with an empty value. This may help compress large datasets.
#'
#' @return A melted \code{data.frame} (see reshape2::melt)
#' @export
#'
climatelong <- function(df, rm.na=FALSE) {
  # deprecation warning
  message("climatelong() is deprecated and will be removed in future versions.")

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

  # make sure data types are consistent ("" should be NA in long form for data quality & flags)
  dqname <- ifelse("Data Quality" %in% quals, "Data Quality", "dataquality")
  flagsname <- ifelse("Flags" %in% quals, "Flags", "flags")
  if(dqname %in% names(df)) {
    df[[dqname]][!is.na(df[[dqname]]) & df[[dqname]]==""] <- NA
  }
  if(flagsname %in% names(df)) {
    df[[flagsname]][!is.na(df[[flagsname]]) & df[[flagsname]]==""] <- NA
  }
  df$value <- suppressWarnings(as.numeric(df$value))
  if(rm.na) {
    df <- df[!is.na(df$value),]
  }
  df
}

#' DEPRECATED: Get parsed CSV data from Environment Canada
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
getClimateDataRaw <- function(stationID, timeframe=c("monthly", "daily", "hourly"),
                              Year=NA, Month=NA,
                              endpoint="http://climate.weather.gc.ca/climate_data/bulk_data_e.html",
                              flag.info=FALSE, ...) {
  # deprecation warning
  message("getClimateDataRaw() is deprecated and will be removed in future versions: ",
          "use ec_climate_data_base() instead")

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

#' DEPRECATED: Deprecated climate locations (February 2017)
#'
#' Climate locations for Environment Canada, as of February 2017. This object is available
#' for historical reasons, and will be removed in future versions.
#' Instead, use \link{ec_climate_locations_all}.
#'
#' @format A data frame with 8735 rows and  19 variables. There are many columns,
#'   only several of which are used within this package.
#' \describe{
#'   \item{Name}{the name of the location (in all caps)}
#'   \item{Province}{the province containing the location (in all caps)}
#'   \item{Climate ID}{IDs that may be used outside of EC}
#'   \item{Station ID}{the ID to be used in \link{getClimateData} and \link{getClimateDataRaw}}
#'   \item{WMO ID}{IDs that may be used outside of EC}
#'   \item{TC ID}{IDs that may be used outside of EC}
#'   \item{Latitude (Decimal Degrees)}{the latitude of the site}
#'   \item{Longitude (Decimal Degrees)}{the longitude of the site}
#'   \item{Latitude}{integer representation of the latitude}
#'   \item{Longitude}{integer representation of the longitude}
#'   \item{Elevation (m)}{The elevation of the site (in metres)}
#'   \item{First Year}{The first year where data exists for this location (for MLY, DLY, or HLY resolution)}
#'   \item{Last Year}{The first year where data exists for this location (for MLY, DLY, or HLY resolution)}
#'   \item{MLY First Year}{The first year where data exists for this location (for MLY, DLY, or HLY resolution)}
#'   \item{MLY Last Year}{The first year where data exists for this location (for MLY, DLY, or HLY resolution)}
#'   \item{DLY First Year}{The first year where data exists for this location (for MLY, DLY, or HLY resolution)}
#'   \item{DLY Last Year}{The first year where data exists for this location (for MLY, DLY, or HLY resolution)}
#'   \item{HLY First Year}{The first year where data exists for this location (for MLY, DLY, or HLY resolution)}
#'   \item{HLY Last Year}{The first year where data exists for this location (for MLY, DLY, or HLY resolution)}
#' }
#'
#' @source \url{ftp://client_climate@ftp.tor.ec.gc.ca/Pub/Get_More_Data_Plus_de_donnees/}
"ecclimatelocs"

# load within package so the data can be used in getClimateSites()
data("ecclimatelocs", envir=environment())

#' Get Environment Canada locations.
#'
#' @param location A human-readable location that will be geocoded
#' @param year A vector of years that the location must have data for
#' @param n The number of rows to return
#' @param locs The data.frame of locations. Use NULL to for \link{ecclimatelocs}.
#' @param nicenames Sanitize names to type-able form
#' @param cols The columns to return (use NULL for all columns)
#'
#' @return A subset of \link{ecclimatelocs}
#' @export
#'
#' @references
#' \url{ftp://client_climate@ftp.tor.ec.gc.ca/Pub/Get_More_Data_Plus_de_donnees/Readme.txt}
#' \url{ftp://client_climate@ftp.tor.ec.gc.ca/Pub/Get_More_Data_Plus_de_donnees/}
#'
getClimateSites <- function(location, year=NULL, n=5, locs=NULL, nicenames=FALSE,
                            cols=c("Name", "Province", "Station ID", "distance", "Latitude (Decimal Degrees)",
                                   "Longitude (Decimal Degrees)", "First Year", "Last Year")) {
  # deprecation warning
  message("getClimateSites() is deprecated and will be removed in future versions: ",
          "use ec_climate_find_locations() instead")

  if(is.null(locs)) {
    locs <- ecclimatelocs
  }
  if(is.null(cols)) {
    cols <- names(locs)
  }
  locinfo <- suppressMessages(prettymapr::geocode(location))
  lat <- locinfo$lat
  lon <- locinfo$lon
  if(is.na(lat) || is.na(lon)) {
    stop("Location ", location, " could not be geocoded")
  }

  if(!is.null(year)) {
    locs <- locs[sapply(1:nrow(locs), function(i) {
      all(year %in% (locs[["First Year"]][i]:locs[["Last Year"]][i]))
    }),]
  }
  locs$distance <- geodist(lon, lat,
                           locs[["Longitude (Decimal Degrees)"]],
                           locs[["Latitude (Decimal Degrees)"]]) / 1000.0
  if(nicenames) {
    names(locs) <- nice.names(names(locs))
    locs <- locs[!duplicated(names(locs))] # removes duplicate lat/lon columns
    cols <- nice.names(cols)
  }
  locs <- locs[order(locs$distance), cols][1:n,]
  return(locs)
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
  loc <- ecclimatelocs[ecclimatelocs$`Station ID`==stationID,]
  if(nrow(loc) != 1) {
    stop("Location ", stationID, " not found")
  } else if(timeframe == "monthly") {
    fy <- loc[["MLY First Year"]]
    ly <- loc[["MLY Last Year"]]
  } else if(timeframe == "daily") {
    fy <- loc[["DLY First Year"]]
    ly <- loc[["DLY Last Year"]]
  } else if(timeframe == "hourly") {
    fy <- loc[["HLY First Year"]]
    ly <- loc[["HLY Last Year"]]
  }
  if(is.na(fy)) return(NULL)
  if(is.na(ly) || ly == 2017) {
    ly <- lubridate::year(Sys.Date())
  }
  return(fy:ly)
}

# Melt multiple sets of columns in parallel
#
# Essentially this is a wrapper around \code{reshape2::melt.data.frame} that
# is able to \code{cbind} several melt operations.
#
# @param x A data.frame
# @param id.vars vector of ID variable names
# @param variable.name Column name to use to store variables
# @param ... Named arguments specifying the \code{measure.vars} to be stored to the
#   column name specified.
# @param factorsAsStrings Control whether factors are converted to character when melted as
#   measure variables.
#
# @return A \code{qtag.long} object
# @export
#
# @examples
# data(pocmajpb210)
# melt.parallel(pb210,
#               id.vars=c("core", "depth"),
#               values=c("Pb210", "age", "sar"),
#               err=c("Pb210_sd", "age_sd", "sar_err"))
#
melt.parallel <- function(x, id.vars, variable.name="column", ..., factorsAsStrings=TRUE) {
  combos <- list(...)
  combonames <- names(combos)
  if(length(combonames) != length(combos)) stop("All arguments must be named")
  lengths <- unique(sapply(combos, length))
  if(length(lengths) > 1) stop("All melted columns must have the same number of source columns")
  melted <- lapply(combonames, function(varname) {
    reshape2::melt(x, id.vars=id.vars, measure.vars=combos[[varname]], value.name=varname,
                   variable.name=variable.name, factorsAsStrings=factorsAsStrings)
  })
  iddata <- melted[[1]][c(id.vars, variable.name)]
  melted <- lapply(melted, function(df) df[names(df) %in% names(combos)])
  do.call(cbind, c(list(iddata), melted))
}

nice.names <- function(x) {
  # rename columns for easier access (strip units and whitespace and lowercase)
  tolower(gsub("\\s|/", "", gsub("\\s*?\\(.*?\\)\\s*", "", x)))
}
