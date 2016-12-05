
#' Get Environment canada data
#'
#' @param location A human-readable location that will be geocoded
#' @param years A vector of years that the location must have data for
#' @param n The number of rows to return
#'
#' @return A subset of \link{climateLocs2016}
#' @export
#'
#' @examples
#' getsites("Wolfville, NS")
#'
getsites <- function(location, years=NULL, n=5, locs=climateLocs2016,
                     cols=c("Name", "Province", "Station ID", "Latitude (Decimal Degrees)",
                            "Longitude (Decimal Degrees)")) {
  locinfo <- prettymapr::geocode(location)
  lat <- locinfo$lat
  lon <- locinfo$lon
  if(is.na(lat) || is.na(lon)) {
    stop("Location ", location, " could not be geocoded")
  }

  if(!is.null(years)) {
    locs <- locs[sapply(1:nrow(locs), function(i) {
      all(years %in% (locs[["First Year"]][i]:locs[["Last Year"]][i]))
    }),]
  }
  locs$distance <- rfuncs::geodist(lon, lat,
                                   locs[["Longitude (Decimal Degrees)"]],
                                   locs[["Latitude (Decimal Degrees)"]]) / 1000.0
  locs[order(locs$distance), cols][1:n,]
}
