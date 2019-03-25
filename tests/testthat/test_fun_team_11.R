context("test-fun-team_11")

test_that("input file is correct", {

  # test data
  shp <- lab3team12:::aus_shp
  expect_equivalent(class(shp),c("sf", "data.frame"))

  # test problematic input
  expect_error(team_11(x = array(1:24, 2:4)))
  expect_error(team_11(x = na ))
  expect_error(team_11(x = seq(1:10) ))
  expect_error(team_11(x = data.frame(1:2)))

  # test output
  expect_equivalent(class(team_11(shp)), c("data.frame"))
  expect_equivalent(names((team_11(shp))), c("group", "long", "lat", "order"))
  expect_equivalent(ncol((team_11(shp))), c(4))
})

