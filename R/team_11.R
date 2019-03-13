#'@name team_11
#'@title team_11's function for 2-level list extraction
#'@author Lab 2 team 11 from STAT 585 Spring 2019
#'@import ggplot2, sf, ggspatial, maps, maptools, rgeos, purrr, tidyverse
#'@seealso purr::flatten
#'@return a small shape file
#'@description This function extracts data from a shapefile with 2 levels in spf$geometry
#' @export
#' @importFrom purrr flatten

team_11 <- function(spf) {
  map_depth(.x = spf$geometry, 2, .f = c(1)) %>% flatten %>%
    map_dfr(data.frame, .id = "group") -> df
  colnames(df) <- c("group", "long", "lat")
  df$order <- seq(from = 1, to = nrow(df), by = 1)
  return(df)
}
