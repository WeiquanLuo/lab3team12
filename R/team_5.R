# helper function
# takes matrix and converts to dataframe with order of rows preserved in order column
Mat2Df <- function(Mat){
  long <- Mat[,1]
  lat <- Mat[,2]
  order <- 1:nrow(Mat)
  # group <- rep(rnorm(1),nrow(Mat))
  df <- data.frame(long=long,lat=lat,order=order)
  return(df)
}

# helper function
# takes multipoly geometry list and returns data frame
newgeo <- function(multpoly){
  lst <- purrr::flatten(multpoly) %>% purrr::map(.f = Mat2Df) %>%
    dplyr::bind_rows(.id = "subgroup")
  newdf <- lst %>% dplyr::select(-subgroup, -order, dplyr::everything())
  newdf$subgroup <- as.factor(newdf$subgroup)
  return(newdf)
}

#' Team 5
#'
#' Converts the geometry section of a shape file to latitude-longitude format
#' @name team_5
#' @author Kellie McClernon
#' @export team_5
#' @param file Shape file path, extension .shp. Must contain a geometry column.
#' @param tolerance Tolerance level for thinning shape file. A percentage between 0 and 1.
#' @return a data frame with latitude-longitude and additional geography information
#'
#' @import assertthat
#' @import dplyr
#' @import purrr
#' @importFrom maptools thinnedSpatialPoly
#' @importFrom sf read_sf st_as_sf

team_5 <- function(file, tolerance = 0.05){
  assertthat::assert_that(assertthat::has_extension(file, "shp"))
  sh_orig <- sf::read_sf(as.character(file))
  shp_thin <- maptools::thinnedSpatialPoly(as(sh_orig, "Spatial"),
                               tolerance = tolerance, minarea = 0.001,
                               topologyPreserve = TRUE)
  sf_thin <- sf::st_as_sf(shp_thin)

  # returns a list of data frames with the geometry information
  assertthat::assert_that(assertthat::has_name(sf_thin, "geometry"))
  new_df <- sf_thin %>% dplyr::transmute(data = geometry %>%
                                    purrr::map(.f = function(x){newgeo(x)}))

  # converting to data frame with geographic information
  geom_data <- new_sf$data %>% dplyr::bind_rows(.id = "group") %>%
    dplyr::select(-group, -subgroup, -order, dplyr::everything())
  geom_data$group <- as.factor(geom_data$group)
  rest <- dplyr::select(as.data.frame(new_df), -which(names(new_df) == "geometry"))
  rest <- dplyr::add_rownames(rest, var = "group")
  rest$group <- as.factor(rest$group)
  df.final <- dplyr::right_join(geom_data, rest, by = "group")
  return(df.final)
}
