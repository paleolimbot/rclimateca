
#' Climate locations (January 2018)
#'
#' Climate locations for Environment Canada, as of January 2018, with column names
#' rewritten by \link{nice_names} and a unique string identifier added.
#'
#' @format A data frame with 8735 rows and  19 variables.
#' \describe{
#'   \item{dataset}{The dataset name (ec_climate)}
#'   \item{location}{A unique string identifier for each location, consisting of the name, province, and station_id}
#'   \item{name}{the name of the location (in all caps)}
#'   \item{province}{the province containing the location (in all caps)}
#'   \item{climate_id}{IDs that may be used outside of EC}
#'   \item{station_id}{the ID to be used in \link{getClimateData} and \link{getClimateDataRaw}}
#'   \item{wmo_id}{IDs that may be used outside of EC}
#'   \item{tc_id}{IDs that may be used outside of EC}
#'   \item{latitude_decimal_degrees}{the latitude of the site}
#'   \item{longitude_decimal_degrees}{the longitude of the site}
#'   \item{latitude}{integer representation of the latitude}
#'   \item{longitude}{integer representation of the longitude}
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
#' @source \url{ftp://client_climate@ftp.tor.ec.gc.ca/Pub/Get_More_Data_Plus_de_donnees/}
"ec_climate_locations_all"

# load within package so the data can be used in ec_climate_locations()
data("ec_climate_locations_all", envir=environment())
