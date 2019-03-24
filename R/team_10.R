#' Converts the geometry section of a shape file to latitude-longitude format
#' @name team_10
#' @author Stat585 Spring2019
#' @export
#' @param file path of a geometry file, extension should be .shp.
#' @param tolerance Tolerance level for thinning shape file. A percentage between 0 and 1.
#' @return a data frame object
#'
#' @import tidyverse
#' @import dplyr
#' @import purrr
#' @import sf
#' @example
#' gdat="data/gadm36_AUS_shp/gadm36_AUS_1.shp"
#' tmp=team_10(gdat,0.1)


team_10=function(file, tolerance=0.1){
  shpbig <- read_sf(file)
  shp_st <- maptools::thinnedSpatialPoly(
    as(shpbig, "Spatial"), tolerance = tolerance,
    minarea = 0.001, topologyPreserve = TRUE)
  shp <- st_as_sf(shp_st)
  shpSmall <- shp %>% select(NAME_1, geometry) %>%
    group_by() %>%
    mutate(coord = geometry %>% map(.f = function(m) flatten(.x=m)),
           region = row_number()) %>%
    unnest
  st_geometry(shpSmall) <- NULL
  shpSmall <- shpSmall %>%
    mutate(coord = coord %>% map(.f = function(m) as_tibble(m)),
           group = row_number()) %>%
    unnest %>%
    setNames(c("name", "region","group", "long", "lat"))
  return(shpSmall)

}


