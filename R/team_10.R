team_10=function(file, tolerance=0.1){
  library(tidyverse)
  library(purrr)
  library(sf)
  library(dplyr)
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

#library(tidyverse)
#library(purrr)
#library(sf)
#library(dplyr)
#ozbig <- read_sf("./data/gadm36_AUS_shp/gadm36_AUS_1.shp")
#oz_st <- maptools::thinnedSpatialPoly(as(ozbig, "Spatial"), tolerance = 0.1, minarea = 0.001, topologyPreserve = TRUE)
#oz <- st_as_sf(oz_st)


#dsn <- "data/gadm36_AUS_shp/gadm36_AUS_1.shp"

#test=team_10(dsn,0.1)
#test %>% ggplot(aes(x = long, y = lat, group = group)) +
#  geom_polygon(color = "black")

#shpSmall <- shpBigToSmall(dsn)
#plotMap <- shpSmall %>% ggplot(aes(x = long, y = lat, group = group)) +
#  geom_polygon(color = "black")
#plotMap
