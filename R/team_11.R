#'@name team_11
#'@title team_11's function for 2-level list extraction
#'@description This function extracts data from a shapefile with 2 levels in file$geometry
#'@usage team_11(tile, tolerance)
#'@param file path of a geometry file, extension should be .shp.
#'@param tolerance Tolerance level for thinning shape file. A percentage between 0 and 1.
#'@details Converts the geometry section of a shape file to latitude-longitude format
#'@return a small shape file
#'@author Lab 2 team 11 from STAT 585 Spring 2019
#'@seealso team_11, team_5
#'@examples
#'dsn="data/gadm36_AUS_shp/gadm36_AUS_1.shp"
#'tmp=team_11(file = dsn)
#'@export
#'@importFrom sf read_sf st_as_sf
#'@importFrom maptools thinnedSpatialPoly
#'@importFrom purrr map_depth flatten map_dfr

team_11 <- function(file, tolerance = 0.1) {

  shpbig <- read_sf(file)
  shp_st <- thinnedSpatialPoly(
    as(shpbig, "Spatial"), tolerance = tolerance,
    minarea = 0.001, topologyPreserve = TRUE)
  shp <- st_as_sf(shp_st)

  map_depth(.x = shp$geometry, 2, .f = c(1)) %>% flatten %>%
    map_dfr(data.frame, .id = "group") -> df
  colnames(df) <- c("group", "long", "lat")
  df$order <- seq(from = 1, to = nrow(df), by = 1)
  return(df)
}
