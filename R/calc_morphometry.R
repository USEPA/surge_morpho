
#remotes::install_github("jhollist/lakemorpho")
source(here::here("R/packages.R"))

# Read in data

surge_reservoirs <- sf::st_read(here::here("data/surge/all_lakes.gpkg"), 
                                layer = "all_lakes") |>
  st_transform(5072) 

set.seed(23)

morph_it <- function(lake, p = function(...) message(...)) {
  #browser()
  suppressWarnings({
    result <- tryCatch({
      p()
      # Removes slivers from lake
      lake <- st_buffer(lake, 1)
      lake <- st_buffer(lake, -1)
      lake <- st_remove_holes(lake, max_area = 100)
      
      elev <- suppressMessages(elevatr::get_elev_raster(lake, 11)) 
      elev_resamp <- raster::raster(crs = raster::crs(elev), 
                                    ext = raster::extent(elev), 
                                    resolution = c(30,30)) 
      elev <- raster::resample(elev, elev_resamp)
      lake_lm <- lakemorpho::lakeSurroundTopo(lake, elev)
      perim <- lakeShorelineLength(lake_lm)
      num_pts <- round(perim/50)
      if(num_pts > 1000) {num_pts <- 1000}
      #Correction Factors - NLA07 HUC 01 and 02: 0.553, 0.462 respectively - avg = 0.5075
      # Not using correction factor initially.  Can apply after the fact.
      metrics <- list(surfaceArea = lakeSurfaceArea(lake_lm),
        shorelineLength = lakeShorelineLength(lake_lm),
        shorelineDevelopment = lakeShorelineDevelopment(lake_lm),
        maxDepth = lakeMaxDepth(lake_lm), 
        volume = lakeVolume(lake_lm),
        meanDepth = lakeMeanDepth(lake_lm),
        maxLength = lakeMaxLength(lake_lm, num_pts), 
        maxWidth = lakeMaxWidth(lake_lm, num_pts), 
        meanWidth = lakeMeanWidth(lake_lm),
        fetch = lakeFetch(lake_lm, 0))
      comid <- select(lake, lake_id, lake_name)
      bind_cols(comid, round(data.frame(metrics), 2))
    }, error = function(e){
      select(lake, lake_id, lake_name)
    })
  })
}

handlers("progress")

tictoc::tic()
plan(multisession, workers = 7)
with_progress({

  p <- progressor(nrow(surge_reservoirs))
  morpho_metrics <- future_lapply(split(surge_reservoirs, 1:nrow(surge_reservoirs)), morph_it, 
                                   p = p, 
                                   future.seed=TRUE)
})
plan(sequential)
tictoc::toc()

ncols <- lapply(morpho_metrics, ncol)
morpho_metrics_errors <- morpho_metrics[ncols < 13]
morpho_metrics_errors <- bind_rows(morpho_metrics_errors)

morpho_metrics_2 <- morpho_metrics[ncols==13]
morpho_metrics_2 <- bind_rows(morpho_metrics_2)
sf::st_geometry(morpho_metrics_2) <- NULL 
sf::st_geometry(morpho_metrics_errors) <- NULL

miss_names <- setdiff(names(morpho_metrics_2), names(morpho_metrics_errors))
morpho_metrics_errors[miss_names] <- NA
morpho_metrics_all <- bind_rows(morpho_metrics_2, morpho_metrics_errors)

readr::write_csv(morpho_metrics_all, here::here("data/all_lakes_lakemorpho.csv"))
#surge_sp$upload_file(here::here("data/all_lakes_lakemorpho.csv"), 
#                     dest = "data/siteDescriptors/all_lakes_lakemorpho.csv")

