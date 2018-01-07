
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

#' @rdname as_ec_climate_location
#' @export
as.integer.ec_climate_location <- function(x, ...) {
  unclass(x)
}

#' @rdname as_ec_climate_location
#' @export
as.numeric.ec_climate_location <- function(x, ...) {
  as.numeric(unclass(x))
}

#' Search climate locations
#'
#' @param query A query in several forms
#' @param timeframe The target timeframe for the query
#' @param year An optional year when the location must have data
#' @param limit The maximum number of locations to return (or NULL for no limit).
#'   Lon/lat queries are automatically capped at 30 locations.
#' @param ... Additional arguments are used to \link[dplyr]{filter}
#'   \link{ec_climate_locations_all}
#'
#' @export
#'
ec_climate_search_locations <- function(query = NULL, ...,
                                        timeframe = c("NA", "monthly", "daily", "hourly"),
                                        year = NULL, limit = NULL) {

  # extra arguments are used to filter the ec_climate_locations_all table
  final_filter_params <- rlang::quos(...)

  # match the timeframe param
  timeframe <- match.arg(timeframe)

  # get the query as a string
  query_str <- deparse(substitute(query))

  # setup the tbl that will be modified a few times
  tbl <- ec_climate_locations_all

  if(is.integer(query)) {
    # integer queries return any location with that station ID
    tbl <- dplyr::filter(tbl, .data$station_id %in% query)
  } else if(is.numeric(query)) {

    if(all((query %% 1) == 0)) {
      # if everything is literally an integer, use integer search
      tbl <- dplyr::filter(tbl, .data$station_id %in% query)

    } else if(length(query) == 2) {
      # length two queries are in the form c(lon, lat)
      tbl$distance <- geodist(query[1], query[2], tbl$longitude, tbl$latitude)
      tbl <- dplyr::arrange(tbl, .data$distance)

      if(is.null(limit)) {
        # set a default limit of 25 for geo queries
        limit <- 25
      }

    } else {
      stop("Length of a numeric query must be in the form c(lon, lat)")
    }

  } else if(inherits(query, "pattern")) {
    # can pass a regex in directly if non-fixed behaviour is desired
    # treated with OR logic if length > 1
    tbl <- purrr::map_dfr(
      query,
      function(q) {
        attributes(q) <- attributes(query)
        dplyr::filter(tbl, stringr::str_detect(.data$location, q))
      }
    )

  } else if(is.character(query)) {
    # character queries are a fixed case-insensitive contains
    # treated with OR logic if length > 1
    tbl <- purrr::map_dfr(
      query,
      function(q) {
        dplyr::filter(
          tbl,
          stringr::str_detect(.data$location, stringr::fixed(q, ignore_case = TRUE))
        )
      }
    )

  } else if(is.null(query)) {
    # do nothing...no restrictions
  } else {
    stop("ec_climate_search_locations() doesn't know how to deal with object of class ",
         paste(class(query), collapse = "/"))
  }

  # filter by year, if present
  if(length(year) != 0) {
    min_year <- min(year)
    max_year <- max(year)

    if(timeframe == "NA") {
      min_col <- "first_year"
      max_col <- "last_year"
    } else if(timeframe == "monthly") {
      min_col <- "mly_first_year"
      max_col <- "mly_last_year"
    } else if(timeframe == "daily") {
      min_col <- "dly_first_year"
      max_col <- "dly_last_year"
    } else if(timeframe == "hourly") {
      min_col <- "hly_first_year"
      max_col <- "hly_last_year"
    }

    tbl <- dplyr::filter(tbl, min_year >= .data[[min_col]], max_year <= .data[[max_col]])
  }

  # apply extra filtering from ...
  tbl <- dplyr::filter(tbl, rlang::UQS(final_filter_params))

  # apply the limit parameter
  if(!is.null(limit)) {
    tbl <- utils::head(tbl, limit)
  }

  # return tbl as an ec_climate_location vector with a custom subclass
  structure(
    tbl$station_id,
    class = c("ec_climate_location_search", "ec_climate_location"),
    query = query,
    query_str = query_str
  )
}
