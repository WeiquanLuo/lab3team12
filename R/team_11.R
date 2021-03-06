#'@name team_11
#'@title team_11's function for 2-level list extraction
#'@description This function extracts data from a shape data with 2 levels in file$geometry
#'@usage team_11(x, tolerance)
#'@param x the result of read_sf(".shp")
#'@param tolerance Tolerance level for thinning shape file. A percentage between 0 and 1.
#'@details Converts the geometry section of a shape file to latitude-longitude format
#' \itemize{
#' \item name = subregion name depicted by the data
#' \item region = coded subregion
#' \item group = indicates which polygon a set of points corresponds to
#' \item long = longitude of the point
#' \item lat = latitude of the point
#' }
#'@return a small shape file in latitude-longitude format
#'@author Lab 2 team 11 from STAT 585 Spring 2019
#'@seealso team_10, team_5
#'@examples
#'dsn=system.file("extdata", "gadm36_AUS_1.shp", package = "lab3team12")
#'dsh=sf::read_sf(dsn)
#'tmp=team_11(x = dsh)
#'@export
#'@importFrom sf read_sf st_as_sf
#'@importFrom maptools thinnedSpatialPoly
#'@importFrom purrr map_depth flatten map_dfr

team_11 <- function(x, tolerance = 0.1) {

  checkmate::expect_numeric(tolerance)
  checkmate::assert_true(na.ok = T, tolerance > 0 & tolerance < 1)

  shp_st <- thinnedSpatialPoly(
    as(x, "Spatial"), tolerance = tolerance,
    minarea = 0.001, topologyPreserve = TRUE)
  checkmate::check_class(shp_st,"SpatialPolygonsDataFrame")

  shp <- st_as_sf(shp_st)
  checkmate::check_class(shp, c("sf", "data.frame"))

  map_depth(.x = shp$geometry, 2, .f = c(1)) %>% flatten %>%
    map_dfr(data.frame, .id = "group") -> df
  checkmate::check_class(shp, "data.frame")

  colnames(df) <- c("group", "long", "lat")
  df$order <- seq(from = 1, to = nrow(df), by = 1)
  return(df)
}
