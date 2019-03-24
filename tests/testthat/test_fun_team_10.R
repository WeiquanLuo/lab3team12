context("test_fun_team_10")

test_that("Successfully detect the input type", {
#  right_file="data/gadm36_AUS_shp/gadm36_AUS_1.shp"
#  # tolerance checks
#  expect_error(team_10(file = right_file,0))
#  expect_error(team_10(file = right_file,"a"))
#  expect_error(team_10(file = right_file,1))
#

  # sf file checks
  expect_error(team_10(file = 1))
  expect_error(team_10(file = "a"))
  expect_error(team_10(file = data.frame(1:2)))
  expect_error(team_10(file = "data/gadm36_AUS_shp/gadm36_AUS_1.cpg"))
  expect_error(team_10(file = "data/gadm36_AUS_shp/gadm36_AUS_1.dbf"))
  expect_error(team_10(file = "data/gadm36_AUS_shp/gadm36_AUS_1.shx"))
  expect_error(team_10(file = "data/gadm36_AUS_shp/gadm36_AUS_1.prj"))

  })

# cant open the file, I cant figure it out.
#test_that("correct output type", {
#  right_file="tests/testdata/gadm36_AUS_1.shp"
#
#  #test for class of the output
#  expect_equal(class(team_10(right_file)), "data.frame")
#
#  #test for expected variables
#  expect_equal(names(team_10(right_file)), c("listno", names(puerto_rico)[1:2],
#                                             "group", "order", "long", "lat"))
#})