library(dplyr)
library(purrr)
library(sf)
ozbig <- sf::read_sf("../Lab2/Data/gadm36_AUS_shp/gadm36_AUS_1.shp")
oz_st <- maptools::thinnedSpatialPoly(as(ozbig, "Spatial"), tolerance = 0.1, minarea = 0.001, topologyPreserve = TRUE)
oz <- st_as_sf(oz_st)

Mat2Df <- function(Mat){
  long <- Mat[,1]
  lat <- Mat[,2]
  order <- 1:nrow(Mat)
  # group <- rep(rnorm(1),nrow(Mat))
  df <- data.frame(long=long,lat=lat,order=order)
  return(df)
}

ozplusplus <- oz %>% mutate(model = purrr::map(geometry, myfun))

test <- oz$geometry[[1]][[1]][[1]]
is.matrix(test)
Mat2Df(test)

test2 <- oz$geometry[[1]]
Mat2Df(test2)

newgeo <- function(multpoly){
  lst <- flatten(multpoly) %>% purrr::map(.f = Mat2Df) %>%
    bind_rows(.id = "group")
  newdf <- lst %>% select(-group, -order, everything())
  return(newdf)
}

newgeo(test2)

#' Team 5
#'
#' Converts the geometry section of a shape file to latitude-longitude format
#' @export team_5
#' @export Mat2Df
#' @param file Shape file path, extension .shp. Must contain a geometry column.
#' @param tolerance Tolerance level for thinning shape file. A percentage between 0 and 1.
#' @return The structure of the original file with the geometry column converted to long-lat format.
#'
#' @import dplyr
#' @import sf
#' @import purrr
#'
#' @examples
#' team_5(ozbig, 0.1)
team_5 <- function(file, tolerance){
  sh_orig <- sf::read_sf(as.character(file))
  shp_thin <- maptools::thinnedSpatialPoly(as(file, "Spatial"),
                               tolerance = tolerance, minarea = 0.001,
                               topologyPreserve = TRUE)
  sf_thin <- sf::st_as_sf(shp_thin)

}

newck <- oz[1:2,] %>% transmute(n_geometry = geometry %>% purrr::map(.f = newgeo))


newgeo(oz$geometry[[1]]) %>% nest()-> ck
