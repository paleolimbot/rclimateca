
<!-- README.md is generated from README.Rmd. Please edit that file -->
rclimateca
==========

[![](http://cranlogs.r-pkg.org/badges/rclimateca)](https://cran.r-project.org/package=rclimateca) [![Travis-CI Build Status](https://travis-ci.org/paleolimbot/rclimateca.svg?branch=master)](https://travis-ci.org/paleolimbot/rclimateca) [![Coverage Status](https://img.shields.io/codecov/c/github/paleolimbot/rclimateca/master.svg)](https://codecov.io/github/paleolimbot/rclimateca?branch=master)

This package is designed to be an R interface to various Environment Canada datasets, including the [historical climate data archive](http://climate.weather.gc.ca/). In the future, the package may also provide access to [hydrometric data](https://wateroffice.ec.gc.ca/) using the same interface (currently in the form of the [hydatr package](https://github.com/paleolimbot/hydatr).

Installation
------------

You can install rclimateca from CRAN with:

``` r
install.packages("rclimateca")
```

Or the development version from github with:

``` r
# install.packages("devtools")
devtools::install_github("paleolimbot/rclimateca")
```

If you can load the package, everything worked!

``` r
library(rclimateca)
```

Example
-------

The workflow for which this **rclimateca** is designed is as follows:

1.  Find climate stations using `ec_climate_search_locations()` or `ec_climate_geosearch_locations()`.
2.  Download the data using `ec_climate_data()` or `ec_climate_mudata()`.

For example, to find recent daily data for a location close to Wolfville, Nova Scotia, one might do the following::

``` r
ec_climate_geosearch_locations(
  "Wolfville NS", timeframe = "daily",
  year = 2017, limit = 5
)
#> Search results for ec_climate_geosearch_locations(
#>   query = "Wolfville NS"
#>   timeframe = "daily"
#>   year = 2017
#>   limit = 5
#> ) 
#> [1] WHITE ROCK NS 6507 / 4.7 km (daily 1977-2017)           
#> [2] KENTVILLE CDA CS NS 27141 / 9.4 km (daily 1996-2018)    
#> [3] WATERVILLE CAMBRIDGE NS 6497 / 22.9 km (daily 1980-2017)
#> [4] PARRSBORO NS 43183 / 35.5 km (daily 2004-2018)          
#> [5] GREENWOOD A NS 6354 / 45.4 km (daily 1942-2018)
```

``` r
ec_climate_data(
  "WHITE ROCK NS 6507", timeframe = "daily",
  start = "2017-01-01", end = "2017-12-31"
)
#> # A tibble: 365 x 29
#>             dataset           location  year month   day       date
#>               <chr>              <chr> <int> <int> <int>     <date>
#>  1 ec_climate_daily WHITE ROCK NS 6507  2017     1     1 2017-01-01
#>  2 ec_climate_daily WHITE ROCK NS 6507  2017     1     2 2017-01-02
#>  3 ec_climate_daily WHITE ROCK NS 6507  2017     1     3 2017-01-03
#>  4 ec_climate_daily WHITE ROCK NS 6507  2017     1     4 2017-01-04
#>  5 ec_climate_daily WHITE ROCK NS 6507  2017     1     5 2017-01-05
#>  6 ec_climate_daily WHITE ROCK NS 6507  2017     1     6 2017-01-06
#>  7 ec_climate_daily WHITE ROCK NS 6507  2017     1     7 2017-01-07
#>  8 ec_climate_daily WHITE ROCK NS 6507  2017     1     8 2017-01-08
#>  9 ec_climate_daily WHITE ROCK NS 6507  2017     1     9 2017-01-09
#> 10 ec_climate_daily WHITE ROCK NS 6507  2017     1    10 2017-01-10
#> # ... with 355 more rows, and 23 more variables: data_quality <chr>,
#> #   max_temp_c <dbl>, max_temp_flag <chr>, min_temp_c <dbl>,
#> #   min_temp_flag <chr>, mean_temp_c <dbl>, mean_temp_flag <chr>,
#> #   heat_deg_days_c <dbl>, heat_deg_days_flag <chr>,
#> #   cool_deg_days_c <dbl>, cool_deg_days_flag <chr>, total_rain_mm <dbl>,
#> #   total_rain_flag <chr>, total_snow_cm <dbl>, total_snow_flag <chr>,
#> #   total_precip_mm <dbl>, total_precip_flag <chr>, snow_on_grnd_cm <dbl>,
#> #   snow_on_grnd_flag <chr>, dir_of_max_gust_10s_deg <dbl>,
#> #   dir_of_max_gust_flag <chr>, spd_of_max_gust_km_h <dbl>,
#> #   spd_of_max_gust_flag <chr>
```

More information
----------------

See `vignette("ec_climate", package = "rclimateca")` and `?ec_climate_data` for more information about using **rclimateca** with the Environment Canada historical climate archive.
