
#' Environment Canada Historical Climate Locations
#'
#' @param x A vector to convert to EC historical climate locations
#' @param ... Not used in these functions
#'
#' @return An object of type ec_climate_location
#' @export
#'
as_ec_climate_location <- function(x, ...) {
  UseMethod("as_ec_climate_location")
}

#' @rdname as_ec_climate_location
#' @export
as_ec_climate_location.ec_climate_location <- function(x, ...) {
  x
}

#' @rdname as_ec_climate_location
#' @export
as_ec_climate_location.double <- function(x, ...) {
  ecloc <- new_ec_climate_location(as.integer(x))
  validate_ec_climate_location(ecloc)
  ecloc
}

#' @rdname as_ec_climate_location
#' @export
as_ec_climate_location.integer <- function(x, ...) {
  ecloc <- new_ec_climate_location(x)
  validate_ec_climate_location(ecloc)
  ecloc
}

#' @rdname as_ec_climate_location
#' @export
as_ec_climate_location.character <- function(x, ...) {
  matches <- charmatch(tolower(x), tolower(ec_climate_locations_all$location))
  ambig_partial_matches <- x[!is.na(matches) & matches == 0]
  no_matches <- x[is.na(matches)]
  if(length(ambig_partial_matches) > 0 && length(no_matches) > 0) {
    stop("The following items had more than one possible match: ", paste0('"', ambig_partial_matches, '"', collapse = ", "),
         ". Additionally, there were items with no possible match: ", paste0('"', no_matches, '"', collapse = ", "))
  } else if(length(ambig_partial_matches) > 0 && length(no_matches) == 0) {
    stop("The following items had more than one possible match: ", paste0('"', ambig_partial_matches, '"', collapse = ", "))
  } else if(length(no_matches) > 0 && length(ambig_partial_matches) == 0) {
    stop("The following items had no possible match: ", paste0('"', no_matches, '"', collapse = ", "))
  }

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

#' @importFrom tibble as_tibble
#' @export
#' @rdname as_ec_climate_location
as_tibble.ec_climate_location <- function(x, ...) {
  location_rows <- match(x, ec_climate_locations_all$station_id)
  ec_climate_locations_all[location_rows, ]
}

#' @rdname as_ec_climate_location
#' @export
as.data.frame.ec_climate_location <- function(x, ...) {
  as.data.frame(as_tibble(x, ...))
}

#' @rdname as_ec_climate_location
#' @export
print.ec_climate_location <- function(x, ...) {
  cat("<ec_climate_location>\n")
  print(as.character(x), quote = FALSE, ...)
  invisible(x)
}

#' @rdname as_ec_climate_location
#' @export
as.character.ec_climate_location <- function(x, ...) {
  location_rows <- match(x, ec_climate_locations_all$station_id)
  ec_climate_locations_all$location[location_rows]
}
