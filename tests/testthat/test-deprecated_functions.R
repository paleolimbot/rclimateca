

test_that("all timeframes and output types of data work for a random location", {
  # define testing function
  test_random_locations <- function(n=1, error_action=stop) {

    # subset (don't test all locations at once!)
    testlocs <- ecclimatelocs[sample(1:nrow(ecclimatelocs), size = n, replace = FALSE),]
    names(testlocs) <- rclimateca:::nice.names(names(testlocs))

    for(i in 1:nrow(testlocs)) {
      site <- testlocs[i,]
      message(sprintf("Testing site %s (%s)", site$name, site$stationid))

      if(!is.na(site$mlyfirstyear)) {
        message("Testing monthly...")
        tryCatch({
          # test monthly for long and wide and mudata
          dfwide <- getClimateData(site$stationid, timeframe="monthly")
          dflong <- getClimateData(site$stationid, timeframe="monthly", format = "long")
          md <- getClimateMUData(site$stationid, timeframe="monthly")
        }, error=function(err) {
          msg <- sprintf("Monthly data for site %s (%s) failed: %s",
                               site$name, site$stationid, err)
          message(msg)
          error_action(msg)
        })
      }

      if(!is.na(site$dlyfirstyear)) {
        yearstart <- site$dlyfirstyear
        yearend <- site$dlylastyear
        message(sprintf("Testing daily (%s-%s)...", yearstart, yearend))
        tryCatch({
          # test daily for long and wide and mudata
          dfwide <- getClimateData(site$stationid, timeframe="daily", year=yearstart:yearend)
          dflong <- getClimateData(site$stationid, timeframe="daily", year=yearstart:yearend,
                                   format = "long")
          # md <- getClimateMUData(site$stationid, timeframe="daily", year=yearstart:yearend)
        }, error=function(err) {
          msg <- sprintf("Daily data for site %s (%s) failed (years %s-%s): %s",
                               site$name, site$stationid, yearstart, yearend, err)
          message(msg)
          error_action(msg)
        })
      }

      if(!is.na(site$hlyfirstyear)) {
        yearstart <- site$hlyfirstyear
        yearend <- site$hlylastyear
        theyear <- sample(yearstart:yearend, size=1)
        message(sprintf("Testing monthly (%s)...", theyear))
        tryCatch({
          # test hourly for long and wide and mudata
          dfwide <- getClimateData(site$stationid, timeframe="hourly", year=theyear)
          dflong <- getClimateData(site$stationid, timeframe="hourly", format = "long", year=theyear)
          md <- getClimateMUData(site$stationid, timeframe="hourly", year=theyear)
        }, error=function(err) {
          msg <- sprintf("Hourly data for site %s (%s) failed (year %s): %s",
                         site$name, site$stationid, theyear, err)
          message(msg)
          error_action(msg)
        })
      }
    }

    return(TRUE)
  }

  expect_true(test_random_locations(n=1, error_action = stop))
})

test_that("deprecated functions all have a warning", {
  "is deprecated and will be removed in future versions"

  expect_message(
    getClimateData(27141, timeframe="daily", year=2014:2016),
    "is deprecated and will be removed in future versions"
  )

  expect_message(
    getClimateData(27141, timeframe="daily", year=2014:2016) %>% climatelong(),
    "is deprecated and will be removed in future versions"
  )

  expect_message(
    getClimateMUData(c(27141, 6354), year=1999, month=7:8, timeframe="daily"),
    "is deprecated and will be removed in future versions"
  )

  expect_message(
    getClimateSites("Wolfville NS"),
    "is deprecated and will be removed in future versions"
  )
})
