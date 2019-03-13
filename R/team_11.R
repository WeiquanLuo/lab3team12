# This function extracts data from a shapefile with 2 levels in spf$geometry
extract_data_L2 <- function(spf) {
  # Reads in the shapefile and extracts data from nested lists, finally
  # recording them into one data frame with columns: long, lat, group and
  # order.
  #
  # Args:
  #   spf:    shapefile of a country
  #
  # Output:
  #   df:     data frame
  map_depth(.x = spf$geometry, 2, .f = c(1)) %>% flatten %>%
    map_dfr(data.frame, .id = "group") -> df
  # rename columns
  colnames(df) <- c("group", "long", "lat")
  # add new variable = "order"
  df$order <- seq(from = 1, to = nrow(df), by = 1)
  return(df)
}
