context("utils")

test_that("nice names functions work as expected", {
  expect_equal(
    nice_names(c("Name 1", "name_2", "name_3", "NAME     4 ")),
    c("name_1", "name_2", "name_3", "name_4")
  )

  tbl <- tibble::tibble(1, 2, 3, 4)
  names(tbl) <- c("Name 1", "name_2", "name_3", "NAME     4 ")
  expect_equal(
    names(set_nice_names(tbl)),
    c("name_1", "name_2", "name_3", "name_4")
  )
})

test_that("geodist function works as expected", {
  # compute distance from one point to another
  expect_true(dplyr::near(geodist(-64.35984, 45.09176, -63.57532, 44.64886), 79129.35, tol = 1))

  # compute a distance matrix
  locs <- c("wolfville, ns", "halifax, ns", "yarmouth, ns", "sydney, ns", "amherst, ns")
  lons <- c(-64.35984, -63.57532, -66.11738, -60.19422, -64.21672)
  lats <- c(45.09176, 44.64886, 43.83746, 46.13679, 45.81667)

  # distance matricies don't currently work...could probably fix this
  # but this feature isn't used in this package
  expect_error(geodist(lons, lats), "Cannot produce matrix from data")

  # compute distances from one point to a vector of distances
  expect_true(
    all(
      dplyr::near(
        geodist(-64.45665, 44.73701, lons, lats),
        c(40221.2, 70431.0, 165950.0, 367541.3, 121647.6),
        tol = 1
      )
    )
  )
})
