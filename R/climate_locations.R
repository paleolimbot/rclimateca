
#' Environment Canada Historical Climate Locations
#'
#' @param x A vector to convert to EC historical climate locations
#' @param i Used to subset an EC historical climate location vector
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
  rlang::set_attrs(x, NULL)
}

#' @rdname as_ec_climate_location
#' @export
as.numeric.ec_climate_location <- function(x, ...) {
  as.numeric(rlang::set_attrs(x, NULL))
}

#' @rdname as_ec_climate_location
#' @export
`[.ec_climate_location` <- function(x, i, ...) {
  new_ec_climate_location(rlang::set_attrs(x, NULL)[i, ...])
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
#' @examples
#'
#' # character searches match the location column of ec_climate_locations_all
#' # (case-insensitive)
#' ec_climate_search_locations("ottawa")
#'
#' # multiple values use OR logic
#' ec_climate_search_locations(c("ottawa on", "halifax"))
#'
#' # you can use a year and a timeframe to find locations that are known to have some
#' # data for that year/timeframe
#' ec_climate_search_locations("ottawa", year = 2016)
#' ec_climate_search_locations("ottawa", timeframe = "daily", year = 2016)
#'
#' # you can also use a vector of years
#' ec_climate_search_locations("ottawa", timeframe = "daily", year = 2000:2016)
#'
#' # if you need to search geographically, you can pass a numeric vector in the form
#' # c(lon, lat)
#' ec_climate_search_locations(c(-75.69031, 45.42111))
#'
#' \donttest{
#' # to use a human readable geocoded location, use ec_climate_geosearch_locations()
#' ec_climate_geosearch_locations("ottawa on")
#' }
#'
ec_climate_search_locations <- function(query = NULL, ...,
                                        timeframe = c("NA", "monthly", "daily", "hourly"),
                                        year = NULL, limit = NULL) {

  # extra arguments are used to filter the ec_climate_locations_all table
  final_filter_params <- rlang::quos(...)

  # match the timeframe param
  timeframe <- match.arg(timeframe)

  # get the query as a string
  query_str <- ec_climate_search_locations_format_call(match.call())

  # setup the tbl that will be modified a few times
  tbl <- ec_climate_locations_all
  tbl$context <- ""

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
      tbl$context <- paste0(tbl$context, sprintf(" / %0.1f km", tbl$distance / 1000))

      tbl <- dplyr::arrange(tbl, .data$distance)

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

    if(timeframe == "monthly") {
      min_col <- "mly_first_year"
      max_col <- "mly_last_year"
    } else if(timeframe == "daily") {
      min_col <- "dly_first_year"
      max_col <- "dly_last_year"
    } else if(timeframe == "hourly") {
      min_col <- "hly_first_year"
      max_col <- "hly_last_year"
    } else {
      min_col <- "first_year"
      max_col <- "last_year"
    }

    # generate context information for printing of search results after
    if(timeframe != "NA") {
      tbl$context <- paste0(
        tbl$context,
        sprintf(" (%s %s-%s)", timeframe,  tbl[[min_col]], tbl[[max_col]])
      )
    } else {
      tbl$context <- paste0(
        tbl$context,
        sprintf(" (%s-%s)", tbl[[min_col]], tbl[[max_col]])
      )
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
  # plus information about the original search
  structure(
    tbl$station_id,
    class = c("ec_climate_location_search", "ec_climate_location"),
    query_str = query_str,
    context = tbl$context
  )
}

#' @rdname ec_climate_search_locations
#' @export
ec_climate_geosearch_locations <- function(query = NULL, ...,
                                           timeframe = c("NA", "monthly", "daily", "hourly"),
                                           year = NULL, limit = NULL) {
  if(length(query) != 1) stop("query must be of length 1 for geocode searches")

  # resolve timeframe arg
  timeframe <- match.arg(timeframe)

  # get the query as a string
  query_str <- ec_climate_search_locations_format_call(match.call())

  # geocode location using prettymapr
  locinfo <- suppressMessages(prettymapr::geocode(query))
  lat <- locinfo$lat
  lon <- locinfo$lon
  label <- locinfo$address

  if(is.na(lat) || is.na(lon)) {
    stop("Location '", query, "' could not be geocoded")
  }

  # return result of search_locations()
  result <- ec_climate_search_locations(c(lon, lat), ..., timeframe = timeframe,
                                        year = year, limit = limit)

  # replace the query_str of the result with the call to _geosearch
  attr(result, "query_str") <- query_str

  result
}

#' Print climate location search results
#'
#' @param x A climate location search result from \link{ec_climate_search_locations} or
#'   \link{ec_climate_geosearch_locations}.
#' @param limit The number of results to show (use NULL for no limit)
#' @param ... Not used.
#'
#' @return The input, invisibly.
#' @export
#'
print.ec_climate_location_search <- function(x, limit = 20, ...) {
  cat("Search results for", attr(x, "query_str"), "\n")
  if(!is.null(limit)) {
    chr <- paste0(
      utils::head(as.character(x), limit),
      utils::head(attr(x, "context"), limit)
    )
  } else {
    chr <- paste0(as.character(x), attr(x, "context"))
  }

  if(length(x) > 0) {
    print(chr, quote = FALSE)
  } else {
    cat("<zero results>\n")
  }

  if(!is.null(limit) && (length(x) > limit)) {
    cat("...plus", length(x) - limit, "more\n")
  }

  invisible(x)
}

ec_climate_search_locations_format_call <- function(call) {
  if(length(call) == 1) {
    # no arguments, just use func()
    deparse(call)
  } else {
    # use one arg per line
    call_names <- names(call)
    call_values <- vapply(call, deparse, character(1))

    sprintf(
      "%s(\n%s\n)",
      call_values[1],
      paste0(
        "  ",
        call_names[-1],
        " = ",
        call_values[-1],
        collapse = "\n"
      )
    )
  }
}

