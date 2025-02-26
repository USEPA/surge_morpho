source(here::here("R/packages.R"))
source(here("R/functions.R"))

#Add source field

surge_poly <- st_read(here("data/surge/all_lakes.gpkg"))  |>
  st_transform(5072)
surge_pts <- st_read(here("data/surge/all_lakes.gpkg"), layer = "points")  |>
  st_transform(5072) |>
  select(lake_id, depth = site_depth)
st_geometry(surge_pts) <- "geometry"
surge_pts <- st_intersection(surge_pts, surge_poly)  |>
  select(lake_id, lake_name, depth) |>
  mutate(source = "surge sites")
surge_phab_pts <- nla_phab_pts_to_sf(here("data/nla/nla2012_wide_phab_08232016_0.csv"),
                               here("data/nla/nla_2017_phab-data.csv"),
                               here("data/nla/nla22_phab_wide.csv"))  |>
  st_transform(5072)
surge_phab_pts <- st_intersection(surge_phab_pts, surge_poly) |>
  select(lake_id, lake_name, source, depth = depth_at_station) |>
  mutate(lake_id = as.numeric(lake_id))

#NLA Index Site Depth
#NEED TO DO THIS

surge_nla07_index_pts <- read_csv(here("data/nla/nla2007_sampledlakeinformation_20091113.csv")) |>
  rename_all(tolower) |>
  select(site_id, fld_lon_dd, fld_lat_dd, fld_src, depth = depth_x) |>
  filter(fld_src == "Index_site") |>
  st_as_sf(coords = c("fld_lon_dd", "fld_lat_dd"), crs = 4326) |>
  st_transform(st_crs(surge_poly)) |>
  st_intersection(surge_poly) |>
  mutate(source = "nla07 index") |>
  select(lake_id, lake_name, source, depth) |>
  mutate(lake_id = as.numeric(lake_id))

surge_nla12_index_pts <- read_csv(here("data/nla/nla12_keyvariables_data.csv")) |>
  rename_all(tolower) |>
  select(site_id, index_lon_dd, index_lat_dd, depth = index_site_depth) |>
  mutate(source = "nla12 index") |>
  st_as_sf(coords = c("index_lon_dd", "index_lat_dd"), crs = 4326) |>
  st_transform(st_crs(surge_poly)) |>
  st_intersection(surge_poly) |>
  select(lake_id, lake_name, source, depth) |>
  mutate(lake_id = as.numeric(lake_id))

surge_nla17_index_pts <- read_csv(here("data/nla/nla_2017_profile-data.csv")) |>
  rename_all(tolower) |>
  filter(!is.na(index_lon_dd)) |>
  filter(!is.na(index_lat_dd)) |>
  fix_lon_lat("index_lon_dd", "index_lat_dd", dropna = FALSE) |>
  select(site_id, index_lon_dd, index_lat_dd, depth = index_site_depth) |>
  mutate(source = "nla17 index") |>
  st_as_sf(coords = c("index_lon_dd", "index_lat_dd"), crs = 4326) |>
  st_transform(st_crs(surge_poly)) |>
  st_intersection(surge_poly) |>
  select(lake_id, lake_name, source, depth) |>
  mutate(lake_id = as.numeric(lake_id))

surge_nla22_index_pts <- read_csv(here("data/nla/nla22_siteinfo.csv")) |>
  rename_all(tolower) |>
  filter(!is.na(index_lon_dd)) |>
  filter(!is.na(index_lat_dd)) |>
  select(site_id, index_lon_dd, index_lat_dd, depth = index_site_depth) |>
  mutate(source = "nla22 index") |>
  st_as_sf(coords = c("index_lon_dd", "index_lat_dd"), crs = 4326) |>
  st_transform(st_crs(surge_poly)) |>
  st_intersection(surge_poly) |>
  select(lake_id, lake_name, source, depth) |>
  mutate(lake_id = as.numeric(lake_id))

# Add TIN depths
bath_tif <- fs::dir_ls(here("data/surge/bathymetry_raster/"), type = "file",
                       glob = "*.tif")

extract_depth_surge_pts <- function(tif_file){
  tif <- terra::rast(tif_file)
  pt_dep <- terra::extract(tif, surge_pts)
  pt_dep <- drop_na(pt_dep)
  names(pt_dep) <- c("ID", "depth")
  pt_dep
}

pre_surge_pt_depths <- map_df(bath_tif, extract_depth_surge_pts)
surge_pts <- mutate(surge_pts, ID = as.numeric(1:nrow(surge_pts)))
pre_surge_pts <- left_join(pre_surge_pt_depths, select(surge_pts, -depth),"ID")
pre_surge_pts <- mutate(pre_surge_pts,
                        source = "pre-SuRGE resevoirs sampling loaction - bathymetry",
                        depth = depth * -1)
pre_surge_pts <- select(pre_surge_pts, lake_id, lake_name, source, depth, geometry)

# mean and max Depth from pre_surge bathymetry
bathy_mean_max <- function(tif_file){
  #browser()
  tif <- terra::rast(tif_file)
  tif_pts <- st_as_sf(as.points(tif))
  names(tif_pts)[1] <- "depth"
  max_depth <- min(tif_pts$depth) # min because of negative depth
  #mean_depth <- mean(tif_pts$depth)
  max_depth_pts <- dplyr::filter(tif_pts, depth == max_depth)
  max_depth_pts
}

max_depth_pts <- map_df(bath_tif, bathy_mean_max)
pre_surge_bathy_max_depth <- sf::st_join(max_depth_pts, surge_poly)
pre_surge_bathy_max_depth <-mutate(pre_surge_bathy_max_depth,
                                   depth = depth * -1,
                                   source = "pre-SuRGE reservoir bathymetry max depth",
                                   lake_id = as.numeric(lake_id)) |>
  select(lake_id, lake_name, source, depth)


# Combine all pts

surge_measured_depths <- bind_rows(surge_pts, surge_phab_pts,
                                   surge_nla07_index_pts, surge_nla17_index_pts,
                                   surge_nla12_index_pts, surge_nla22_index_pts,
                                   pre_surge_pts, pre_surge_bathy_max_depth)

st_write(surge_measured_depths, here("data/surge_morpho_point_depth.gpkg"),
         append = FALSE)


tin_gdbs <- fs::dir_ls(here("data/surge/tin"), recurse = T,
                          type = "directory", regexp = "\\.gdb$")

create_tif <- function(x){
  rast_layer <- gsub("RASTER_DATASET=", "",
                     gdal_metadata(x))
  rst <- terra::rast(x, rast_layer)
  tif_file <- here(paste0("data/surge/bathymetry_raster/",rast_layer,".tif"))
  terra::writeRaster(rst, tif_file, overwrite=TRUE)
}

lapply(tin_gdbs, create_tif)

surge_measured_depths <- bind_rows(surge_pts, surge_phab_pts,
                                   surge_nla07_index_pts, surge_nla17_index_pts,
                                   surge_nla12_index_pts, surge_nla22_index_pts)

st_write(surge_measured_depths, here("data/surge_morpho_point_depth.gpkg"))

