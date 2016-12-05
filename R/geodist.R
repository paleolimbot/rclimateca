# Calculate geographic distances
#
# Calculates distances from latitudes and longitudes. Math is done such that
# passing vectors for each parameter is allowed provided that vectors are the same
# length for corresponding arguments. Returns a vector of distances if location1
# or location2 is of length 1, or a distance matrix otherwise. If only one
# location is specified, a distance matrix between locations is calculated.
# Distance is based on the haversine formula.
#
# @param long1 longitude of location 1
# @param lat1  latitude of location 1
# @param long2 longitude of location 2 (optional)
# @param lat2 latitude of location 2 (optional)
# @param labels labels assigned to location 1 (for dist matrix)
# @param labels2 labels assigned to location 2 (for dist matrix; optional)
# @param R radius of the earth (using the same as \code{geosphere} package)
#
# @return A vector of distances if location1 or location2 is of length 1, or a
#   distance matrix otherwise.
# @export
#
# @examples
# #compute distance from one point to another
# geodist(-64.35984, 45.09176, -63.57532, 44.64886)
#
# #compute a distance matrix
# locs <- c("wolfville, ns", "halifax, ns", "yarmouth, ns", "sydney, ns", "amherst, ns")
# lons <- c(-64.35984, -63.57532, -66.11738, -60.19422, -64.21672)
# lats <- c(45.09176, 44.64886, 43.83746, 46.13679, 45.81667)
#
# #compute distances from one point to a vector of distances
# geodist(-64.45665, 44.73701, lons, lats)
#
geodist <- function(long1, lat1, long2=NULL, lat2=NULL, labels=NULL, labels2=NULL, R=6378137) {
  if(length(long1) != length(lat1)) {
    stop("Lat/lon arguments for location 1 must be of same length")
  }
  if(length(long2) != length(lat2)) {
    stop("Lat/lon arguments for location 2 must be of same length")
  }
  if(is.null(lat2) && is.null(long2)) {
    #assign values of long1 and lat1 so distance matrix is calculated
    long2 <- long1
    lat2 <- lat1
    labels2 <- labels
  }

  if((length(lat1) == 1) || (length(lat2) == 1)) {
    #return single vector of distances
    long1 <- long1*pi/180.0
    lat1 <- lat1*pi/180.0
    long2 <- long2*pi/180.0
    lat2 <- lat2*pi/180.0

    delta.long <- (long2 - long1)
    delta.lat <- (lat2 - lat1)
    a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
    c <- 2 * asin(pmin(1,sqrt(a)))
    return(R * c)
  } else {
    stop("Cannot produce matrix from data")
  }
}
