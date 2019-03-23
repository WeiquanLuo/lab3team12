context("test-fun-team_5")

test_that("input file is correct", {
  library(assertthat)
  file_good <- "data/AUS_1/gadm36_AUS_1.shp"
  file_bad <- "data/FRA_1/gadm36_FRA_2.dbf"

  # has correct file
  expect_that(has_extension(file_good, "shp"), is_true())
  expect_that(has_extension(file_bad, "shp"), is_false())
  expect_that(team_5(file_bad, tolerance = 0.1), throws_error())

  # has geometry column
  shp_good <- lab3team12:::aus_shp
  shp_thin_bad <- dplyr::select(as.data.frame(shp_good), -which(names(shp_good) == "geometry"))
  expect_that(has_name(shp_good, "geometry"), is_true())
  expect_that(has_name(shp_thin_bad, "geometry"), is_false())

})

test_that("sub-function Mat2Df makes the correct data frame", {
  aus_shp <- lab3team12:::aus_shp
  test <- aus_shp$geometry[[1]][[1]][[1]]

  test_output <- Mat2Df(test)
  df_len <- length(test_output$order)

  expect_that(is.matrix(test), is_true())
  expect_that(test_output$order, is_equivalent_to(1:df_len))
  expect_that(is.data.frame(test_output), is_true())
})

test_that("new geometry function has correct output", {
  aus_shp <- lab3team12:::aus_shp
  test2 <- aus_shp$geometry[[1]]
  sub_list <- test2[[1]][[1]]
  sub_len <- length(sub_list[,1])
  t2_output <- newgeo(test2)

  # check that group is correct
  char_sg <- as.character(t2_output$subgroup)
  expect_that(char_sg[1], is_identical_to("1"))
  expect_that(char_sg[sub_len], is_identical_to("1"))
  expect_that(char_sg[1+sub_len], is_identical_to("2"))

  # check format of output
  expect_that(names(t2_output), is_equivalent_to(c("long", "lat", "subgroup", "order")))
  expect_that(is.data.frame(t2_output), is_true())
})

#test_that("final team_5 function has new geometry and other columns", {
#  file_oz <- "data/AUS_1/gadm36_AUS_1.shp"
#
#  f_output <- team_5(file_oz, tolerance = 0.1)
#  all_d <- make_shp(file_oz, tolerance = 0.1)
#  other <- dplyr::select(as.data.frame(all_d), -which(names(all_d) == "geometry"))
#
#  expect_that(is.data.frame(f_output), is_true())
#  expect_that(names(f_output)[1:5], matches(c("long", "lat", "group", "subgroup", "order")))
#  expect_that(length(names(f_output)), equals(length(names(other))+5))
#})
# Note: I cannot figure out how to store a .shp in the package to test this part of the function :(
