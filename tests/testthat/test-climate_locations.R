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
