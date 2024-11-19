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


