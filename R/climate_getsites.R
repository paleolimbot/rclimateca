
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
#' @examples
#' # don't test because fetching of file slows down testing
#' \donttest{
#' getClimateSites("Wolfville, NS", year=2016)
#' }
#'
getClimateSites <- function(location, year=NULL, n=5, locs=NULL, nicenames=FALSE,
                     cols=c("Name", "Province", "Station ID", "distance", "Latitude (Decimal Degrees)",
                            "Longitude (Decimal Degrees)", "First Year", "Last Year")) {
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
