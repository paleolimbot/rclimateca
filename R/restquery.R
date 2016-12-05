
# Query an endpoint and cache the data
#
# @param ... Key/value pairs to send to the query (will be passed through URLEncode)
# @param .endpoint The URL endpoint to send the query
# @param .cache A variable name in .GlobalEnv to use as a cache (NULL to disable)
# @param .parser A function that the (character) results will be passed through
# @param .quiet Use .quiet=TRUE to supress messaging
#
# @return The query result
# @export
#
# @examples
# restquery(.endpoint="https://www.goodreads.com/book/title",
#           key="HSkIMuOGlxFIOmfBCGFVA", title='catch-22')
#
restquery <- function(.endpoint, ..., .cache='.api_result', .parser=NULL, .quiet=FALSE,
                      .encoding=NULL) {
  # make URL
  searchparams <- sapply(list(...), function(x) {
    if(is.null(x)) {
      return(NA)
    } else if(is.na(x)) {
      return(NA)
    } else {
      return(as.character(x))
    }
  }, simplify = FALSE)
  # verify search params are all named
  if(any(nchar(names(searchparams)) == 0)) stop("restquery only takes named parameters")
  # sorting ensures consistent url_hash with identical parameters
  params <- sapply(sort(names(searchparams)), function(item) {
                    v <- searchparams[[item]]
                    if(is.na(v)) return(NA)
                    paste(utils::URLencode(item), utils::URLencode(v), sep="=")
                   })
  params <- params[!is.na(params)]
  url_string <- sprintf("%s?%s", .endpoint, paste(params, collapse="&"))

  lines <- NULL
  # check for cached result
  if(!is.null(.cache)) {
    lines <- get_cached(.cache, url_string)
    if(!is.null(lines)) {
      if(!.quiet) message("Using cached information for ", url_string)
    }
  }
  # if there is no cached result, query the URL and parse
  if(is.null(lines) || is.null(.cache)) {
    if(!.quiet) message("Retreiving information from ", url_string)
    connect <- try(httr::GET(url_string), silent=TRUE)
    # check for fail
    if(class(connect) != "try-error") {
      # try to get content
      if(!.quiet) httr::warn_for_status(connect)
      lines <- httr::content(connect, as="text", encoding=.encoding)
      # store response information
      if(!is.null(.cache)) {
        put_cached(.cache, url_string, lines)
      }
    } else {
      if(!.quiet) message("Unable to connect to ", url_string, ": ", as.character(connect))
    }
  }

  if(is.null(.parser)) {
    return(lines)
  } else {
    return(.parser(lines))
  }

}
