source(here::here("R/packages.R"))


# Read in data
flooded_lands <- arrow::open_dataset(here("data/flooded"), partitioning = "stusps") |> 
  sfarrow::read_sf_dataset() |>
  st_transform(5072)

lakes <- flooded_lands[1:100,]

morph_it <- function(lake) {
 
  elev <- elevatr::get_elev_raster(lake, 12)
  lake_lm <- lakemorpho::lakeSurroundTopo(lake, elev)
  perim <- lakeShorelineLength(lake_lm)
  num_pts <- perim/50
  if(num_pts > 1000) {num_pts <- 1000}
  metrics <- calcLakeMetrics(lake_lm, 0, num_pts)
  comid <- lake$COMID
  bind_cols(data.frame(comid), round(data.frame(metrics), 2))
}

#xdf <- data.frame()
#for(i in seq_along(lakes$COMID)){
#  xdf <- rbind(xdf, morph_it(lakes[i,]))  
#}
#xdf

plan(multisession, workers = 6)
morpho_metrics <- future_lapply(split(lakes, 1:nrow(lakes)), morph_it, 
                                future.seed=TRUE)

bind_rows(morpho_metrics)
       
