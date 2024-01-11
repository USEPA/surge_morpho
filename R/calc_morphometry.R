source(here::here("R/packages.R"))

# Read in data
flooded_lands <- arrow::open_dataset(here("data/flooded"), partitioning = "stusps") |> 
  sfarrow::read_sf_dataset() |>
  st_transform(5072) 

small <- split(flooded_lands[1:5,], 1:nrow(flooded_lands[1:5,]))

flooded_lands_l <- split(flooded_lands, 1:nrow(flooded_lands))

morph_it <- function(lake) {
  tryCatch({
  lake <- st_make_valid(lake)
  elev <- suppressMessages(elevatr::get_elev_raster(lake, 12, override_size_check = TRUE))
  lake_lm <- lakemorpho::lakeSurroundTopo(lake, elev)
  perim <- lakeShorelineLength(lake_lm)
  num_pts <- round(perim/50)
  if(num_pts > 1000) {num_pts <- 1000}
  metrics <- calcLakeMetrics(lake_lm, 0, num_pts)
  comid <- select(lake, comid, globalid, objectid)
  sf <- bind_cols(comid, round(data.frame(metrics), 2))
  df <- sf
  st_geometry(df) <- NULL
  id <- c(df$comid, df$objectid, df$globalid)
  id <- id[!is.na(id)]
  },
  
  error = function(e){
    df <- list(NA)
    id <- c(df$comid, df$objectid, df$globalid)
    sf <- NA
  })
  data.table::fwrite(df,here("data/metrics/metrics.csv"), append = TRUE)
  sf
}

plan(multisession)
morpho_metrics <- future_lapply(flooded_lands_l, morph_it, 
                                future.seed=TRUE)

flooded_lands_morpho <-bind_rows(morpho_metrics)
st_write(flooded_lands_morpho, "flooded_lands_morpho.gpkg")
       
