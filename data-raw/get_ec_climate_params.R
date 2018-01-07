
library(tidyverse)

# functions to rename column names (same ones as in utils.R)
nice_names <- function(x) {
  lower <- tolower(x)
  no_alpha_numeric <- gsub("[^a-z0-9_ ]+", " ", lower)
  no_lead_trail_whitespace <- stringr::str_trim(no_alpha_numeric)
  no_spaces <- gsub("\\s+", "_", no_lead_trail_whitespace)
  no_spaces
}

url <- "http://climate.weather.gc.ca/climate_data/bulk_data_e.html?format=csv&stationID=1706&Year=2008&Month=5&Day=14&timeframe=%d&submit=Download+Data"

curl::curl_download(sprintf(url, 3), "data-raw/test_monthly.csv")
curl::curl_download(sprintf(url, 2), "data-raw/test_daily.csv")
curl::curl_download(sprintf(url, 1), "data-raw/test_hourly.csv")

monthly <- read_csv("data-raw/test_monthly.csv", skip = 18, col_types = cols(.default = col_character()))
daily <- read_csv("data-raw/test_daily.csv", skip = 25, col_types = cols(.default = col_character()))
hourly <- read_csv("data-raw/test_hourly.csv", skip = 16, col_types = cols(.default = col_character()))

all_names <- map_df(lst(monthly, daily, hourly), ~tibble(col_name = colnames(.x)),
                    .id = "timeframe") %>%
  mutate(nice_name = nice_names(col_name)) %>%
  mutate(param = nice_names(stringr::str_replace(col_name, "\\s\\(.*?\\)|\\sFlag", ""))) %>%
  mutate(col_type = case_when(
    endsWith(col_name, "Flag") ~ "flag_label",
    paste0(param, "_flag") %in% nice_name ~ "param_label",
    param == "weather" ~ "param_label",
    TRUE ~ "ID"
  )) %>%
  select(-nice_name) %>%
  filter(col_type != "ID") %>%
  spread(key = col_type, value = col_name) %>%
  group_by(timeframe) %>%
  mutate(col_order = match(param_label, colnames(get(unique(timeframe), envir = .GlobalEnv)))) %>%
  ungroup() %>%
  arrange(timeframe, col_order) %>%
  extract(param_label, into = "unit", regex = "\\((.*?)\\)$", remove = FALSE)

ec_climate_params_all <- all_names %>%
  mutate(dataset = paste0("ec_climate_", timeframe)) %>%
  mutate(nice_label = nice_names(param_label)) %>%
  select(dataset, param, nice_label, label = param_label, flag_label, unit)

devtools::use_data(ec_climate_params_all, overwrite = TRUE)

# cleanup files
unlink("data-raw/test_monthly.csv")
unlink("data-raw/test_daily.csv")
unlink("data-raw/test_hourly.csv")
