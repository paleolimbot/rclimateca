
#' Climate locations (January 2018)
#'
#' Climate locations for Environment Canada, as of January 2018, with column names
#' as nice_names and a unique string identifier added.
#'
#' @format A data frame with 8735 rows and  19 variables.
#' \describe{
#'   \item{location}{A unique string identifier for each location, consisting of the name, province, and station_id}
#'   \item{latitude}{latitude of the site (decimal degrees)}
#'   \item{longitude}{longitude of the site (decimal degrees)}
#'   \item{timezone_id}{The timezone ID of the site}
#'   \item{lst_utc_offset}{The offset of local standard time to UTC (estimated)}
#'   \item{station_id}{The primary identifier of the site used by Environment Canada}
#'   \item{name}{the name of the location (in all caps)}
#'   \item{province}{the province containing the location (in all caps)}
#'   \item{climate_id}{IDs that may be used outside of EC}
#'   \item{wmo_id}{IDs that may be used outside of EC}
#'   \item{tc_id}{IDs that may be used outside of EC}
#'   \item{elevation_m}{The elevation of the site (in metres)}
#'   \item{first_year}{The first year where data exists for this location (for any resolution)}
#'   \item{last_year}{The last year where data exists for this location (for any resolution)}
#'   \item{mly_first_year}{The first year where data exists for this location (for MLY, DLY, or HLY resolution)}
#'   \item{mly_last_year}{The last year where data exists for this location (for MLY, DLY, or HLY resolution)}
#'   \item{dly_first_year}{The first year where data exists for this location (for MLY, DLY, or HLY resolution)}
#'   \item{dly_last_year}{The last year where data exists for this location (for MLY, DLY, or HLY resolution)}
#'   \item{hly_first_year}{The first year where data exists for this location (for MLY, DLY, or HLY resolution)}
#'   \item{hly_last_year}{The last year where data exists for this location (for MLY, DLY, or HLY resolution)}
#' }
#'
#' @source <ftp://client_climate@ftp.tor.ec.gc.ca/Pub/Get_More_Data_Plus_de_donnees/>
"ec_climate_locations_all"

# load within package so the data can be used in ec_climate_* functions
data("ec_climate_locations_all", envir=environment())

#' Climate parameters (January 2018)
#'
#' Climate parameters for Environment Canada datasets, as of January 2018.
#'
#' @format A data frame with 32 rows and  6 variables.
#' \describe{
#'   \item{dataset}{The dataset name (ec_climate)}
#'   \item{param}{The parameter identifier}
#'   \item{nice_label}{The column name from the output of [ec_climate_data]}
#'   \item{label}{The label for the parameter (with unit)}
#'   \item{flag_label}{The raw column name used for the Flag parameter}
#'   \item{unit}{The unit of measurement for the parameter}
#' }
#'
"ec_climate_params_all"

# load within package so the data can be used in ec_climate_* functions
data("ec_climate_params_all", envir=environment())
