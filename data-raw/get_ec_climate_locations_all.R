
library(tidyverse)
library(sf)

# possibly useful later:
# http://climate.weather.gc.ca/glossary_e.html

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

# setup timezone shapefile
if(!file.exists("data-raw/tz_info/dist/combined_shapefile.shp")) {
  curl::curl_download(
    "https://github.com/evansiroky/timezone-boundary-builder/releases/download/2017c/timezones.shapefile.zip",
    "data-raw/tz_info.zip",
    quiet = FALSE
  )
  unzip("data-raw/tz_info.zip", exdir = "data-raw/tz_info")
}
tz_info <- read_sf("data-raw/tz_info/dist/combined_shapefile.shp")
tz_offsets <- read_delim("data-raw/timeZones.txt", delim = "\t",
                         col_types = cols(
                           CountryCode = col_character(),
                           TimeZoneId = col_character(),
                           `GMT offset 1. Jan 2017` = col_double(),
                           `DST offset 1. Jul 2017` = col_double(),
                           `rawOffset (independant of DST)` = col_double()
                         )
) %>%
  select(timezone_id = TimeZoneId, lst_utc_offset = starts_with("rawOffset"))

getTZ <- function(lat, long) {
  pts <- tibble(long, lat) %>%
    # make missing combinations the point 0,0, which has no timezone
    mutate(long = if_else(is.na(long) | is.na(lat), 0, long),
           lat = if_else(is.na(long) | is.na(lat), 0, lat)) %>%
    transpose() %>%
    map(~st_point(c(.x$long, .x$lat))) %>%
    do.call(st_sfc, .) %>%
    st_set_crs(4326)

  matches <- suppressMessages(st_contains(tz_info$geometry, pts, sparse = FALSE))
  tz_matches <- plyr::alply(matches, 2, function(x) tz_info$tzid[x])

  map_chr(tz_matches, function(x) {
    if(length(x) == 0) {
      NA_character_
    } else {
      x
    }
  })
}

# getTZ(0, 52) # Europe/London

# province abbreviations, used to create rclimateca's version of a human-readable location identifier
provinces <- c(
  "ALBERTA" = "AB",  "BRITISH COLUMBIA" = "BC",
  "MANITOBA" = "MB",  "NEW BRUNSWICK" = "NB",  "NEWFOUNDLAND" = "NL",
  "NORTHWEST TERRITORIES" = "NT",  "NOVA SCOTIA" = "NS",  "NUNAVUT" = "NU",
  "ONTARIO" = "ON",  "PRINCE EDWARD ISLAND" = "PE",  "QUEBEC" = "QC",
  "SASKATCHEWAN" = "SK",  "YUKON TERRITORY" = "YT"
)

# default timezones, offsets
default_tz = c(
  "NOVA SCOTIA" = "America/Halifax",
  "QUEBEC" = "America/Toronto",
  "SASKATCHEWAN" = "America/Regina",
  "PRINCE EDWARD ISLAND" = "America/Halifax",
  "YUKON TERRITORY" = "America/Whitehorse",
  "ALBERTA" = "America/Calgary",
  "ONTARIO" = "America/Toronto",
  "NORTHWEST TERRITORIES" = "America/Yellowknife",
  "NEWFOUNDLAND" = "America/St_Johns",
  "NEW BRUNSWICK" = "America/Moncton",
  "MANITOBA" = "America/Winnepeg",
  "BRITISH COLUMBIA" = "America/Vancouver",
  "NUNAVUT" = "America/Iqaluit"
)

# download the current list of climate locations
if(!file.exists("data-raw/eclocs.csv")) {
  download.file(
    "ftp://client_climate@ftp.tor.ec.gc.ca/Pub/Get_More_Data_Plus_de_donnees/Station%20Inventory%20EN.csv",
    "data-raw/eclocs.csv"
  )
}

# read in the list of climate locations
ec_climate_locations_first <- read_csv(
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
  # add in the location name ([name] [province abbr] [station_id])
  mutate(location = paste(name, provinces[province], station_id, sep = " ")) %>%
  # rename the verbose lat/long columns, remove conusing lat/lon as integer columns
  select(-latitude, -longitude) %>%
  rename(longitude = longitude_decimal_degrees, latitude = latitude_decimal_degrees) %>%
  # there is one site where there is a positive longitude that should be negative, and
  # several where the lat/lon is 0,0 (which should be missing)
  mutate(longitude = if_else(longitude > 0, -longitude, longitude)) %>%
  mutate(longitude = na_if(longitude, 0), latitude = na_if(latitude, 0))

# add timezone information
ec_climate_tzinfo <- ec_climate_locations_first %>%
  # get timezone information for each location
  select(location, station_id, province, longitude, latitude) %>%
  mutate(default_tz = default_tz[province]) %>%
  mutate(computed_tz = getTZ(long = longitude, lat = latitude)) %>%
  mutate(timezone_id = coalesce(computed_tz, default_tz)) %>%
  left_join(tz_offsets, by = "timezone_id")

ec_climate_locations_all <- ec_climate_locations_first %>%
  left_join(ec_climate_tzinfo %>% select(station_id, timezone_id, lst_utc_offset), by = "station_id") %>%
  select(location, longitude, latitude, timezone_id, lst_utc_offset, station_id, everything())


# rewrite the data in the package
devtools::use_data(ec_climate_locations_all, overwrite = TRUE)

# remove the downloaded files
unlink("data-raw/eclocs.csv")
unlink("data-raw/tz_info", recursive = TRUE)
unlink("data-raw/tz_info.zip")
