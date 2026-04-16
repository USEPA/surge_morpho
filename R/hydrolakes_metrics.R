source(here::here("R/packages.R"))
source(here("R/functions.R"))

surge_hylak_ids <- read_csv(here("data/surge_master_crosswalk_long.csv")) |>
  filter(join_id_name == "hylak_id") |>
  select(lake_id, hylak_id = join_id) |>
  mutate(hylak_id = as.integer(hylak_id))

hydrolakes <- st_read(here("data/hydrolakes/HydroLAKES_polys_v10.gdb/"))

surge_hydrolakes <- filter(hydrolakes, hydrolakes$Hylak_id %in% surge_hylak_ids$hylak_id) |>
  left_join(surge_hylak_ids, by = c("Hylak_id" = "hylak_id"))

st_geometry(surge_hydrolakes) <- NULL

surge_hydrolakes <- select(surge_hydrolakes, lake_id, hylak_id = Hylak_id, hydrolakes_lake_area = Lake_area, 
                           hydrolakes_shoreline_length = Shore_len, hydrolakes_volume = Vol_total,
                           hydrolakes_mean_depth = Depth_avg)

write_csv(surge_hydrolakes, here("data/surge_hydrolakes_metrics.csv"))


