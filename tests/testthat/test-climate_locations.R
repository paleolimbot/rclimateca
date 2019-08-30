context("climate_locations")

test_that("is_ec_climate_location works", {
  base_obj <- new_ec_climate_location(14L)
  expect_is(base_obj, "ec_climate_location")
  expect_true(is_ec_climate_location(base_obj))
})

test_that("print() ing of ec_climate_location vectors outputs location names", {
  expect_output(
    print(new_ec_climate_location(14L)),
    "<ec_climate_location>.*?ACTIVE PASS BC 14"
  )
})

test_that("as_ec_climate_location works with integers, doubles, and strings", {
  base_obj <- new_ec_climate_location(14L)

  expect_identical(as_ec_climate_location(14), base_obj)
  expect_identical(as_ec_climate_location(14L), base_obj)
  expect_identical(as_ec_climate_location("ACTIVE PASS BC 14"), base_obj)
})

test_that("as_ec_climate_location does not allow invalid station IDs", {
  expect_error(as_ec_climate_location(1L), "The following locations are not valid")
  expect_error(as_ec_climate_location(1), "The following locations are not valid")
  expect_error(as_ec_climate_location(NA_integer_), "The following locations are not valid")
  expect_error(as_ec_climate_location("not a valid station name"), "The following items had no possible match")
})

test_that("partial matching in as_ec_climate_location.character() is not case-sensitive", {
  expect_identical(as_ec_climate_location("ACTIVE PASS BC 14"),
                   as_ec_climate_location("active pass bc 14"))
})

test_that("partial matching in as_ec_climate_location.character() fails when there are multiple partial matches", {
  expect_error(as_ec_climate_location("ottawa"), "The following items had more than one possible match")
  expect_error(as_ec_climate_location("ottttaaawaaaa"), "The following items had no possible match")
  expect_error(
    as_ec_climate_location(c("ottawa", "otttaaawwaaaa")),
    "The following items had more than one possible match.*?Additionally, there were items with no possible match"
  )
})

test_that("as_tibble and as.data.frame functions produce tibbles and data frames, respectively", {
  loc <- new_ec_climate_location(14L)
  expect_is(tibble::as_tibble(loc), "tbl")
  expect_is(as.data.frame(loc), "data.frame")
})

test_that("character representation of locations are correct", {
  loc <- new_ec_climate_location(14L)
  expect_equal(as.character(loc), "ACTIVE PASS BC 14")
})

test_that("integer representation of locations is correct", {
  loc <- new_ec_climate_location(14L)
  expect_identical(as.integer(loc), 14L)
  expect_identical(as.numeric(loc), 14)
})

test_that("as. functions strip attributes from location vectors", {
  loc <- new_ec_climate_location(14L)
  attr(loc, "thing") <- "stuff"
  expect_identical(as.integer(loc), 14L)
  expect_identical(as.numeric(loc), 14)
})

test_that("subsetted location vectors are still location vectors", {
  loc <- new_ec_climate_location(14L)
  expect_is(loc[1], "ec_climate_location")
  expect_is(head(loc), "ec_climate_location")
})

test_that("search locations function works as intended", {

  # check output type and attributes
  expect_is(ec_climate_search_locations(), "ec_climate_location")
  expect_is(ec_climate_search_locations(), "ec_climate_location_search")
  expect_is(attr(ec_climate_search_locations(), "query_str"), "character")

  # null query returns everything
  expect_equal(
    length(ec_climate_search_locations(query = NULL, limit = NULL)),
    nrow(ec_climate_locations_all)
  )

  # integer and numeric queries return sites with that station_id
  expect_equal(
    ec_climate_search_locations(c(6375L, 27141L)) %>% strip_attrs(),
    c(6375L, 27141L)
  )
  expect_equal(
    ec_climate_search_locations(c(6375, 27141)) %>% strip_attrs(),
    c(6375L, 27141L)
  )

  # character query return sites that contain that text (text insensitive)
  expect_equal(
    ec_climate_search_locations("kentville") %>% strip_attrs(),
    c(6375L, 27141L)
  )

  # regex queries return sites that contain that regex
  expect_equal(
    ec_climate_search_locations(stringr::regex("^KENTVILLE CDA")) %>% strip_attrs(),
    c(6375L, 27141L)
  )

  # lon/lat queries
  expect_equal(
    ec_climate_search_locations(c(-64.48, 45.07), limit = 2) %>% strip_attrs(),
    c(6375L, 27141L)
  )
})

test_that("location search date filtering works properly", {

  expect_equal(
    ec_climate_search_locations("ottawa", timeframe = "NA", year = 2000) %>%
      strip_attrs(),
    ec_climate_search_locations("ottawa", first_year <= 2000, last_year >= 2000) %>%
      strip_attrs()
  )

  expect_equal(
    ec_climate_search_locations("ottawa", timeframe = "monthly", year = 2000) %>%
      strip_attrs(),
    ec_climate_search_locations("ottawa", mly_first_year <= 2000, mly_last_year >= 2000) %>%
      strip_attrs()
  )

  expect_equal(
    ec_climate_search_locations("ottawa", timeframe = "daily", year = 2000) %>%
      strip_attrs(),
    ec_climate_search_locations("ottawa", dly_first_year <= 2000, dly_last_year >= 2000) %>%
      strip_attrs()
  )

  expect_equal(
    ec_climate_search_locations("ottawa", timeframe = "hourly", year = 2000) %>%
      strip_attrs(),
    ec_climate_search_locations("ottawa", hly_first_year <= 2000, hly_last_year >= 2000) %>%
      strip_attrs()
  )

})

test_that("multiple length character queries are treated with OR logic", {
  expect_equal(
    ec_climate_search_locations(c("ottawa", "halifax")) %>%
      strip_attrs(),
    c(
      ec_climate_search_locations("ottawa"),
      ec_climate_search_locations("halifax")
    ) %>% strip_attrs()
  )
})

test_that("multiple length regex queries are treated with OR logic", {
  expect_equal(
    ec_climate_search_locations(stringr::regex(c("^ottawa", "^halifax"), ignore_case = TRUE)) %>%
      strip_attrs(),
    c(
      ec_climate_search_locations(stringr::regex("^ottawa", ignore_case = TRUE)),
      ec_climate_search_locations(stringr::regex("^halifax", ignore_case = TRUE))
    ) %>% strip_attrs()
  )
})

test_that("invalid queries throw errors", {
  expect_error(
    ec_climate_search_locations(TRUE),
    "doesn't know how to deal with object of class logical"
  )

  expect_error(
    ec_climate_search_locations(c(1.2, 2.3, 3.4)),
    "Length of a numeric query must be in the form"
  )
})

test_that("the limit search parameter is respected", {
  expect_length(
    ec_climate_search_locations(c(-64.48, 45.07), limit = 2),
    2
  )
  expect_length(
    ec_climate_search_locations(c(-64.48, 45.07), limit = 25),
    25
  )
  expect_length(
    ec_climate_search_locations(c(-64.48, 45.07), limit = NULL),
    nrow(ec_climate_locations_all)
  )
})

test_that("geocode searching works", {
  skip_if_offline()

  expect_equal(
    ec_climate_geosearch_locations("wolfville ns") %>% strip_attrs() %>% sort(),
    ec_climate_search_locations(c(-64.36449, 45.09123)) %>% strip_attrs() %>% sort()
  )

  expect_error(
    ec_climate_geosearch_locations("not a location that is searchable ever"),
    "Location 'not a location that is searchable ever' could not be geocoded"
  )
})

test_that("printing of search results gives the correct information", {
  skip_if_offline()

  expect_output(print(ec_climate_search_locations()), "Search results for")
  expect_output(print(ec_climate_geosearch_locations("wolfville ns")), "Search results for")
  expect_output(print(ec_climate_geosearch_locations("wolfville ns")), "/[0-9. ]+?km")
  expect_output(print(ec_climate_search_locations(c(-64.36449, 45.09123))), "/[0-9. ]+?km")
  expect_output(
    print(ec_climate_search_locations(year = 2017, timeframe = "NA")),
    "[0-9]{4}-[0-9]{4}"
  )
  expect_output(
    print(ec_climate_search_locations(year = 2017, timeframe = "monthly")),
    "monthly [0-9]{4}-[0-9]{4}"
  )
  expect_output(
    print(ec_climate_search_locations(year = 2017, timeframe = "daily")),
    "daily [0-9]{4}-[0-9]{4}"
  )
  expect_output(
    print(ec_climate_search_locations(year = 2017, timeframe = "hourly")),
    "hourly [0-9]{4}-[0-9]{4}"
  )

  # check empty results
  expect_output(
    print(ec_climate_search_locations("no stations have this text")),
    "<zero results>"
  )

  # check limited results
  expect_output(
    print(ec_climate_search_locations("ottawa"), limit = NULL),
    "OTTAWA GATINEAU A QC 53001"
  )
  expect_output(
    print(ec_climate_search_locations("ottawa"), limit = 100),
    "OTTAWA GATINEAU A QC 53001"
  )
  expect_output(
    print(ec_climate_search_locations("ottawa"), limit = 27),
    "OTTAWA GATINEAU A QC 53001"
  )
  expect_output(
    print(ec_climate_search_locations("ottawa"), limit = 26),
    "\\.\\.\\.plus 1 more"
  )
})
