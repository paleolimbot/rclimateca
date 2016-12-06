
#' Get Environment canada data
#'
#' @param location A human-readable location that will be geocoded
#' @param year A vector of years that the location must have data for
#' @param n The number of rows to return
#' @param locs The data.frame of locations. Use NULL to for \link{climateLocs2016}.
#'
#' @return A subset of \link{climateLocs2016}
#' @export
#'
#' @examples
#' getsites("Wolfville, NS", year=2016)
#'
getsites <- function(location, year=NULL, n=5, locs=NULL,
                     cols=c("Name", "Province", "Station ID", "Latitude (Decimal Degrees)",
                            "Longitude (Decimal Degrees)")) {
  if(is.null(locs)) {
    locs <- climateLocs2016
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
  locs$distance <- rfuncs::geodist(lon, lat,
                                   locs[["Longitude (Decimal Degrees)"]],
                                   locs[["Latitude (Decimal Degrees)"]]) / 1000.0
  locs[order(locs$distance), cols][1:n,]
}
