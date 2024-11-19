source(here::here("R/packages.R"))
source(here("R/functions.R"))

surge_morpho_all <- read_csv(here("data/surge_res_morpho_all.csv"))

surge_res_multi_comid <- surge_morpho_all |>
  filter(grepl("comid", .data$join_id_name)) |>
  select(lake_id, join_id) |>
  unique() |>
  group_by(lake_id) |>
  summarize(num_comid = n()) |>
  ungroup() |>
  filter(num_comid > 1)

# Making sure we don't have a single nhd plus poly with multiple surge polys...  We don't!
surge_morpho_all |>
  filter(grepl("comid", .data$join_id_name)) |>
  select(lake_id, join_id) |>
  unique() |>
  group_by(join_id) |>
  summarize(num_comid = n()) |>
  ungroup() |>
  filter(num_comid > 1)

# Look at some individual examples
surge_poly <- st_read("data/surge/all_lakes.gpkg")
surge_pts <- st_read("data/surge/all_lakes.gpkg", layer = "points")
lmorpho_poly <- st_read("data/lakemorpho/national_lake_morphometry.gpkg")
nhdplus_poly <- st_read("data/nhd/nhd_plus_waterbodies.gpkg")
lagos_poly <- st_read("data/lagos/gis_locus_v1.0_gpkg/gis_locus_v1.0.gpkg")

surge_single <- filter(surge_poly, lake_id == 070)
surge_single_pts <- filter(surge_pts, lake_id == 070)
lmorpho_single <-  filter(lmorpho_poly, COMID == 11546104)
nhdplus_single <- filter(nhdplus_poly, COMID == 120053557)
nhdplus_single_2 <- filter(nhdplus_poly, COMID ==167245953)
lagos_single <- filter(lagos_poly, lagoslakeid == 345199)

mapview(surge_single) + lmorpho_single + nhdplus_single + nhdplus_single_2 + lagos_single
