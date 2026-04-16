source(here::here("R/packages.R"))
source(here("R/functions.R"))

surge_lagos_ids <- read_csv(here("data/surge_master_crosswalk_long.csv")) |>
  filter(join_id_name == "lagoslakeid") |>
  select(lake_id, lagoslakeid = join_id) |>
  mutate(lake_id = as.integer(lake_id), lagoslakeid = as.integer(lagoslakeid))

surge_lagos <- read_csv(here("data/lagos/lagos_depth.csv")) |>
  filter(lagoslakeid %in% surge_lagos_ids$lagoslakeid) |>
  left_join(surge_lagos_ids) |>
  select(lake_id, lagoslakeid, lagos_max_depth = lake_maxdepth_m, lagos_mean_depth = lake_meandepth_m, lagos_lake_area = lake_waterarea_ha)

write_csv(surge_lagos, here("data/surge_lagos_metrics.csv"))