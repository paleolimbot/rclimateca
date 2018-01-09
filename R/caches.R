
#' Cache options
#'
#' Clears the local cache of downloaded files (by default in the
#' ec.cache folder in the working directory).
#'
#' @param cache A folder name where cached files should be stored.
#'
#' @export
#'
#' @examples
#' clear_cache()
#'
clear_cache <- function(cache = "ec.cache") {
  if(dir.exists(cache)) {
    unlink(cache, recursive = TRUE)
  }
}

# create environment for session cache options
cache_options <- new.env(parent = emptyenv())
cache_options$default_cache <- "ec.cache"

#' @rdname clear_cache
#' @export
set_default_cache <- function(cache = "ec.cache") {
  old_cache <- get_default_cache()
  cache_options$default_cache <- cache
  invisible(old_cache)
}

#' @rdname clear_cache
#' @export
get_default_cache <- function() {
  cache_options$default_cache
}

put_cached <- function(cache, url, data) {
  url_hash <- digest::digest(url)
  if(!dir.exists(cache) && !is.null(data)) {
    dir.create(cache)
  }
  fname <- file.path(cache, paste0(url_hash, ".csv"))
  if(is.null(data) && file.exists(fname)) {
    unlink(fname)
  } else if(!is.null(data)) {
    write(data, fname)
  }
}

get_cached <- function(cache, url) {
  url_hash <- digest::digest(url)
  fname <- file.path(cache, paste0(url_hash, ".csv"))
  if(file.exists(fname)) {
    paste0(readLines(fname), collapse = "\n")
  } else {
    NULL
  }
}
