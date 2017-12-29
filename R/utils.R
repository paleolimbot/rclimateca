
#' Convert object names to programmer-friendly names
#'
#' @param x A vector of names (nice_names()) or an object with names (set_nice_names())
#'
#' @return A new vector of names or an object with the names attribute modified
#' @export
#'
#' @examples
#' nice_names(c("Name 1", "name2", "name_3", "NAME     4 "))
#'
nice_names <- function(x) {
  lower <- tolower(x)
  no_alpha_numeric <- gsub("[^a-z0-9_ ]+", " ", lower)
  no_lead_trail_whitespace <- stringr::str_trim(no_alpha_numeric)
  no_spaces <- gsub("\\s+", "_", no_lead_trail_whitespace)
  tibble::tidy_names(no_spaces)
}

#' @rdname nice_names
#' @export
set_nice_names <- function(x) {
  x_names <- names(x)
  if(!is.null(x_names)) {
    names(x) <- nice_names(x_names)
  }

  x
}
