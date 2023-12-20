source(here::here("R/packages.R"))

# Read in data
flooded_lands <- arrow::open_dataset(here("data/flooded"), partitioning = "stusps") |> 
  sfarrow::read_sf_dataset() |>
  st_transform(5072)

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

plan(multisession, workers = 6)
morpho_metrics <- future_lapply(split(lakes_sub, 1:nrow(lakes_sub)), morph_it, 
                                future.seed=TRUE)

bind_rows(morpho_metrics)
       
