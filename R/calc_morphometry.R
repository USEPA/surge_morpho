source(here::here("R/packages.R"))

#fs::dir_create(here("data/surge"))
site <- get_sharepoint_site('SuRGE: Survey of Reservoir Greenhouse gas Emissions')
surge_sp <- site$get_drive()
surge_sp$download_file("data/spatial/2023_Flooded_Lands_Reservoir_Inventory_Final_121923.gpkg", 
                       dest = here("data/flooded_lands_inventory.gpkg"))

# Read in data - below is just an example to get started.
lakes <- st_read(here("data/flooded_lands_inventory.gpkg")) |>
  st_transform(5072)
lakes_sub <- st_make_valid(lakes[1000:1010,])

morph_it <- function(lake) {
  elev <- elevatr::get_elev_raster(lake, 12)
  lake_lm <- lakemorpho::lakeSurroundTopo(lake, elev)
  perim <- lakeShorelineLength(lake_lm)
  num_pts <- round(perim/50)
  if(num_pts > 1000) {num_pts <- 1000}
  metrics <- calcLakeMetrics(lake_lm, 0, num_pts)
  comid <- select(lake, comid, globalid, objectid)
  bind_cols(comid, round(data.frame(metrics), 2))
}

#xdf <- data.frame()
#for(i in seq_along(lakes$COMID)){
#  xdf <- rbind(xdf, morph_it(lakes[i,]))  
#}
#xdf

plan(multisession, workers = 4)
morpho_metrics <- future_lapply(split(lakes_sub, 1:nrow(lakes_sub)), morph_it, 
                                future.seed=TRUE)

bind_rows(morpho_metrics)
       
