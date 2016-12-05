
#' Clear cached results
#'
#' Clears the local cache of downloaded files
#' (located at \code{.api_result} in the \code{.GlobalEnv}).
#'
#' @export
#'
#' @examples
#' clear_cache()
#'
clear_cache <- function(cache='.api_result') {
  if(class(cache) == "character") {
    assign(cache, list(), envir = .GlobalEnv)
  } else {
    warning("Unrecognized class for cache (list and character accepted)")
  }
}

put_cached <- function(cache, url, data) {
  url_hash <- digest::digest(url)
  if(class(cache) == "character") {
    if(!exists(cache, envir=.GlobalEnv)) {
      db <- list()
    } else {
      db <- get(cache, envir=.GlobalEnv)
    }
    db[[url_hash]] <- data
    assign(cache, db, envir=.GlobalEnv)
  } else {
    warning("Unrecognized class for cache (list and character accepted)")
  }
}

get_cached <- function(cache, url) {
  url_hash <- digest::digest(url)
  if(class(cache) == "character") {
    if(!exists(cache, envir=.GlobalEnv)) {
      return(NULL)
    } else {
      return(get(cache, envir=.GlobalEnv)[[url_hash]])
    }
  } else {
    warning("Unrecognized class for cache (list and character accepted)")
    return(NULL)
  }
}
