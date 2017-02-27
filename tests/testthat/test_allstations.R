

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
          error_action(sprintf("Monthly data for site %s (%s) failed: %s",
                               site$name, site$stationid, err))
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
          md <- getClimateMUData(site$stationid, timeframe="daily", year=yearstart:yearend)
        }, error=function(err) {
          error_action(sprintf("Daily data for site %s (%s) failed (years %s-%s): %s",
                               site$name, site$stationid, yearstart, yearend, err))
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
          error_action(sprintf("Hourly data for site %s (%s) failed (year %s): %s",
                               site$name, site$stationid, theyear, err))
        })
      }
    }

    return(TRUE)
  }

  expect_true(test_random_locations(n=1, error_action = stop))
})
