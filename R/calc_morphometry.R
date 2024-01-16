
remotes::install_github("jhollist/lakemorpho")
source(here::here("R/packages.R"))

#fs::dir_create(here("data/surge"))
#site <- get_sharepoint_site('SuRGE: Survey of Reservoir Greenhouse gas Emissions')
#surge_sp <- site$get_drive()
#surge_sp$download_file("data/spatial/2023_Flooded_Lands_Reservoir_Inventory_Final_121923.gpkg", 
#                       dest = here("data/flooded_lands_inventory.gpkg"))

# Read in data
flooded_lands <- arrow::open_dataset(here("data/flooded"), partitioning = "stusps") |> 
  sfarrow::read_sf_dataset() |>
  st_transform(5072)

# Read in data - below is just an example to get started.
#flooded_lands <- st_read(here("data/flooded_lands_inventory.gpkg")) |>
#  st_transform(5072)

set.seed(23)
lakes_sub <- st_make_valid(flooded_lands[sample(seq_along(flooded_lands$nid_id), 1000),])
lakes_sub2 <- lakes_sub[1:50,]

morph_it <- function(lake, p = function(...) message(...)) {
  suppressWarnings({
  result <- tryCatch({
    p()
    # Removes slivers from lake
    lake <- st_buffer(lake, 1)
    lake <- st_buffer(lake, -1)
    lake <- st_remove_holes(lake, max_area = 100)
    
    elev <- suppressMessages(elevatr::get_elev_raster(lake, 12))
    lake_lm <- lakemorpho::lakeSurroundTopo(lake, elev)
    perim <- lakeShorelineLength(lake_lm)
    num_pts <- round(perim/50)
    if(num_pts > 1000) {num_pts <- 1000}
    metrics <- calcLakeMetrics(lake_lm, 0, num_pts)
    comid <- select(lake, comid, globalid, objectid)
    bind_cols(comid, round(data.frame(metrics), 2))
  }, error = function(e){
    select(lake, comid, globalid, objectid)
  })
  if(!dir.exists(here("data/calcout"))){
    dir.create(here("data/calcout"))
  }
  filename <- paste0("data/calcout/", 
                    paste0(c(result$comid, result$globalid, result$objectid), 
                           collapse = ""), ".gpkg")
  st_write(obj = result, dsn = filename , append = FALSE, driver = "GPKG", 
           quiet = TRUE)
  result
  })
}

handlers("progress")

tictoc::tic()
plan(multisession, workers = 7)
with_progress({
  p <- progressor(length(lakes_sub2$nid_id))
  morpho_metrics <- future_lapply(split(lakes_sub2, 1:nrow(lakes_sub2)), morph_it, 
                                  p = p, 
                                  future.seed=TRUE)
})
plan(sequential)
tictoc::toc()

ncols <- lapply(morpho_metrics, ncol)
morpho_metrics_errors <- morpho_metrics[ncols < 17]
morpho_metrics_errors <- bind_rows(morpho_metrics_errors)
morpho_metrics <- morpho_metrics[ncols==17]
morpho_metrics <- bind_rows(morpho_metrics)
mapview(morpho_metrics_errors)
