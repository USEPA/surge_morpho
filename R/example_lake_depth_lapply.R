library(elevatr)
library(lakemorpho)
library(mapview)
library(dplyr)

lakes <- st_read(here("data/surge/all_lakes.gpkg"), "all_lakes") |>
  st_transform(5072)

morph_it <- function(lake){
  lake_elev <- get_elev_raster(lake, z = 12, expand = 1000)
  lake_lm <- lakeSurroundTopo(lake, lake_elev)
  lake_maxdepth <- lakeMaxDepth(lake_lm, correctFactor = 0.4)
  data.frame(lake_id = lake$lake_id, lake_maxdepth)
}

lake_list <- split(lakes[1:10,], 1:10)

#lake_depth <- lapply(lake_list, morph_it)
#
plan(multisession, workers = 2)
lake_depth <- future_lapply(lake_list, morph_it)
plan(sequential)

lake_depth_df <- bind_rows(lake_depth)
lake_depth_df
