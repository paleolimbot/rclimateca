
nice.names <- function(x) {
  # rename columns for easier access (strip units and whitespace and lowercase)
  tolower(gsub("\\s", "", gsub("\\s*?\\(.*?\\)\\s*", "", x)))
}
