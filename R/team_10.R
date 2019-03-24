#' Converts the geometry section of a shape file to latitude-longitude format
#' @name team_10
#' @author Hao
#' @export
#'
#' @param file File path of a geometry file, extension should be .shp.
#' @param tolerance Tolerance level for thinning shape file. A percentage between 0 and 1.
#'
#' @return A data frame object
#'
#' @details The variables included in the dataframe that is returned from \code{team_10}
#' are as follows.
#' \itemize{
#' \item name = subregion name depicted by the data
#' \item region = coded subregion
#' \item group = indicates which polygon a set of points corresponds to
#' \item long = longitude of the point
#' \item lat = latitude of the point
#' }
#'
#' @import tidyverse
#' @import dplyr
#' @import purrr
#' @importFrom sf read_sf st_as_sf st_geometry
#' @importFrom tidyr unnest
#' @examples
#' gdat="data/gadm36_AUS_shp/gadm36_AUS_1.shp"
#' tmp=team_10(gdat,0.1)
#' library(ggplot2)
#' library(dplyr)
#' tmp %>% ggplot(aes(x=long,y=lat,group=group))+geom_polygon()
#'


team_10=function(file, tolerance=0.1){
  shpbig <- sf::read_sf(file)
  shp_st <- maptools::thinnedSpatialPoly(
    as(shpbig, "Spatial"), tolerance = tolerance,
    minarea = 0.001, topologyPreserve = TRUE)
  shp <- sf::st_as_sf(shp_st)
  shpSmall <- shp %>% select(NAME_1, geometry) %>%
    group_by() %>%
    mutate(coord = geometry %>% map(.f = function(m) flatten(.x=m)),
           region = row_number()) %>% unnest()
  sf::st_geometry(shpSmall) <- NULL
  shpSmall <- shpSmall %>%
    mutate(coord = coord %>% map(.f = function(m) as_tibble(m)),
           group = row_number()) %>% unnest %>%
    setNames(c("name", "region","group", "long", "lat"))
  return(shpSmall)

}


