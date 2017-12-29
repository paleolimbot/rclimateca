
library(tidyverse)

# functions to rename column names (same ones as in utils.R)
nice_names <- function(x) {
  lower <- tolower(x)
  no_alpha_numeric <- gsub("[^a-z0-9_ ]+", " ", lower)
  no_lead_trail_whitespace <- stringr::str_trim(no_alpha_numeric)
  no_spaces <- gsub("\\s+", "_", no_lead_trail_whitespace)
  tibble::tidy_names(no_spaces)
}

set_nice_names <- function(x) {
  x_names <- names(x)
  if(!is.null(x_names)) {
    names(x) <- nice_names(x_names)
  }

  x
}

provinces <- c(
  "ALBERTA" = "AB",  "BRITISH COLUMBIA" = "BC",
  "MANITOBA" = "MB",  "NEW BRUNSWICK" = "NB",  "NEWFOUNDLAND" = "NL",
  "NORTHWEST TERRITORIES" = "NT",  "NOVA SCOTIA" = "NS",  "NUNAVUT" = "NU",
  "ONTARIO" = "ON",  "PRINCE EDWARD ISLAND" = "PE",  "QUEBEC" = "QC",
  "SASKATCHEWAN" = "SK",  "YUKON TERRITORY" = "YT"
)

# download the current list of climate locations
if(file.exists("data-raw/eclocs.csv")) unlink("data-raw/eclocs.csv")
download.file("ftp://client_climate@ftp.tor.ec.gc.ca/Pub/Get_More_Data_Plus_de_donnees/Station%20Inventory%20EN.csv",
              "data-raw/eclocs.csv")

# read in the list of climate locations
ec_climate_locations_all <- read_csv(
  "data-raw/eclocs.csv",
  skip = 3,
  col_types = cols(
      Name = col_character(),
      Province = col_character(),
      `Climate ID` = col_character(),
      `Station ID` = col_integer(),
      `WMO ID` = col_integer(),
      `TC ID` = col_character(),
      `Latitude (Decimal Degrees)` = col_double(),
      `Longitude (Decimal Degrees)` = col_double(),
      Latitude = col_integer(),
      Longitude = col_integer(),
      `Elevation (m)` = col_double(),
      `First Year` = col_integer(),
      `Last Year` = col_integer(),
      `HLY First Year` = col_integer(),
      `HLY Last Year` = col_integer(),
      `DLY First Year` = col_integer(),
      `DLY Last Year` = col_integer(),
      `MLY First Year` = col_integer(),
      `MLY Last Year` = col_integer()
    )
  ) %>%
  set_nice_names() %>%
  # add in the dataset name and location name ([name] [province abbr] [station_id])
  mutate(dataset = "ec_climate", location = paste(name, provinces[province], station_id, sep = " ")) %>%
  # move columns to the front
  select(dataset, location, station_id, everything())

# rewrite the data in the package
devtools::use_data(ec_climate_locations_all, overwrite = TRUE)

# remove the downloaded file
unlink("data-raw/eclocs.csv")
