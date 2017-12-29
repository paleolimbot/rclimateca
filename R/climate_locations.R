
#' Environment Canada Historical Climate Locations
#'
#' @param x A vector to convert to EC historical climate locations
#'
#' @return An object of type ec_climate_location
#' @export
#'
as_ec_climate_location <- function(x) {
  UseMethod("as_ec_climate_location")
}

#' @rdname as_ec_climate_location
#' @export
as_ec_climate_location.ec_climate_location <- function(x) {
  x
}

#' @rdname as_ec_climate_location
#' @export
as_ec_climate_location.double <- function(x) {
  ecloc <- new_ec_climate_location(as.integer(x))
  validate_ec_climate_location(ecloc)
  ecloc
}

#' @rdname as_ec_climate_location
#' @export
as_ec_climate_location.integer <- function(x) {
  ecloc <- new_ec_climate_location(x)
  validate_ec_climate_location(ecloc)
  ecloc
}

#' @rdname as_ec_climate_location
#' @export
as_ec_climate_location.character <- function(x) {
  matches <- pmatch(tolower(x), tolower(ec_climate_locations_all$location))
  new_ec_climate_location(ec_climate_locations_all$station_id[matches])
}

#' @rdname as_ec_climate_location
#' @export
is_ec_climate_location <- function(x) {
  inherits(x, "ec_climate_location")
}

#' @rdname as_ec_climate_location
#' @export
new_ec_climate_location <- function(x) {
  if(!is.integer(x)) stop("ec_climate_location objects must be of type integer")
  structure(x, class = "ec_climate_location")
}

#' @rdname as_ec_climate_location
#' @export
validate_ec_climate_location <- function(x) {
  if(!is.integer(x)) stop("Object is not an integer vector")
  bad_locations <- !(x %in% ec_climate_locations_all$station_id)
  if(any(bad_locations)) {
    stop("The following locations are not valid Environment Canada historical climate station IDs: ",
         paste(x[bad_locations], collapse = ", "))
  }
  invisible(x)
}
