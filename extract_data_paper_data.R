library(readr)
library(dplyr)
library(sf)
library(here)
library(terra)
library(purrr)

################################################
# Get Bathy files to fill in missing site depths
################################################
bathy_files <- list.files(here("data/bathymetry_raster"), full.names = TRUE)
surge_poly <- st_read(here("data/surge/all_lakes.gpkg"), layer = "all_lakes") |>
  st_transform(5072)
surge_pts <- st_read(here("data/surge/all_lakes.gpkg"), layer = "points") |>
  st_transform(5072)

get_and_crop <- function(bathy_file, res, res_pts, overwrite = TRUE){
  if(overwrite){
    bathy <- rast(bathy_file) |>
      project(terra::crs(res))
    bathy_ext <- ext(bathy)
    bathy_ext_vect <- as.polygons(bathy_ext, crs = crs(bathy))
    bathy_ext_polygons <- st_as_sf(bathy_ext_vect)
    reservoir <- st_intersection(bathy_ext_polygons, res) |>
      st_cast("MULTIPOLYGON")
    points <- st_intersection(bathy_ext_polygons, res_pts)
    bathy_crop <- crop(bathy, reservoir) * -1
    if(reservoir$lake_id == "1006"){
      # It appears the bathymetry for  Buckhorn Reservoir in Kentuky is in Feet
      # Converting to meters.
      bathy_crop <- bathy_crop * 0.3048
    }
    writeRaster(bathy_crop, paste0(here("data/bathymetry_clipped"), "/",
                                   reservoir$lake_id, "_",
                                   basename(bathy_file)),
                overwrite = TRUE)
    write_sf(reservoir, here("data/bathymetry_reservoirs.gpkg"),
             layer = "reservoirs", append = TRUE)
    write_sf(points, here("data/bathymetry_reservoirs.gpkg"),
             layer = "points", append = TRUE)
  }
}

lapply(bathy_files, get_and_crop, res = surge_poly, res_pts = surge_pts,
       overwrite = FALSE)

################################################
# Fill in missing site depths from bathy
################################################
surge_res_morpho <- read_csv(here("data/all_lakes_lakemorpho.csv"))
surge_res_points <- st_read(here("data/surge/all_lakes.gpkg"), "points") |>
  st_transform(5072)
surge_res_points_missing <- surge_res_points |>
  filter(.by = lake_id, all(is.na(site_depth)))
missing_ids <- surge_res_points_missing$lake_id |>
  unique()
bathy_files <- list.files(here("data/bathymetry_clipped"), full.names = TRUE)

fill_in_missing_depth <- function(i){
  surge_res_points_missing_i <- surge_res_points_missing |>
    filter(lake_id == i)
  bathy_file <- bathy_files[grep(i, bathy_files)]
  bathy_missing <- terra::rast(bathy_file) |>
    project(terra::crs(surge_res_points_missing_i))
  # site_id
  bathy_pts <- terra::extract(bathy_missing, surge_res_points_missing_i,
                              bind = TRUE) |>
    as.data.frame() |>
    mutate(lake_id = i)
  names(bathy_pts)[length(names(bathy_pts))] <- "bathy_depth"
  bathy_pts <- mutate(bathy_pts, site_depth = bathy_depth) |>
    select(-bathy_depth)
  surge_res_points_missing_i |>
    select(-site_depth) |>
    left_join(bathy_pts)
}

missing <- lapply(missing_ids, fill_in_missing_depth)
surge_res_points_filled <- bind_rows(surge_res_points, missing) |>
  filter(!is.na(site_depth))

surge_res_points_filled |>



#


surge_morpho <- surge_res_morpho_all |>
  filter(source %in% c("surge_morpho", "surge sites")) |>
  filter(variables %in% c("surfacearea","shorelinelength",
                          "shorelinedevelopment", "volume", "maxwidth",
                          "meanwidth", "maxlength", "max_depth", "mean_depth")) |>
  filter(!(source == "surge_morpho" & variables == "volume")) |>
  select(lake_id, lake_name, source, variables, values)


