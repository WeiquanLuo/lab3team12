context("test-fun-team_5")

test_that("input file is correct", {
  library(assertthat)
  file_good <- "./data/gadm36_AUS_1.shp"
  file_bad <- "/data/gadm36_FRA_2.dbf"

  # has correct file
  expect_that(has_extension(file_good, "shp"), is_true())
  expect_that(has_extension(file_bad, "shp"), is_false())

  # has geometry column
  shp_big <- sf::read_sf(file_good)
  shp_sp <- maptools::thinnedSpatialPoly(sf::as(shp_big, "Spatial"),
                                           tolerance = 0.1, minarea = 0.001,
                                           topologyPreserve = TRUE)
  shp_thin <- sf::st_as_sf(shp_sp)

})

test_that("sub-function Mat2Df makes the correct data frame", {
  test <- oz$geometry[[1]][[1]][[1]] ## need to create properly!!

  test_output <- Mat2Df(test)
  df_len <- length(test_output$order)

  expect_that(is.matrix(test), is_true())
  expect_that(test_output$order, is_equivalent_to(1:df_len))
  expect_that(is.data.frame(test_output), is_true())
})

test_that("new geometry function has correct output", {
  test2 <- oz$geometry[[1]]
  sub_list <- test2[[1]][[1]]
  sub_len <- length(sub_list[,1])
  t2_output <- newgeo(test2)

  # check that group is correct
  expect_that(t2_output$group[1], matches("1"))
  expect_that(t2_output$group[sub_len], matches("1"))
  expect_that(t2_output$group[1+sub_len], matches("2"))

  # check format of output
  expect_that(names(t2_output), matches(c("long", "lat", "group", "order")))
  expect_that(is.data.frame(t2_output), is_true())
})
