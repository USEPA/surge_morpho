source(here::here("R/packages.R"))
source(here("R/functions.R"))

surge_hylak_ids <- read_csv(here("data/surge_master_crosswalk_long.csv")) |>
  filter(join_id_name == "hylak_id") |>
  select(lake_id, hylak_id = join_id)

globathy_depths <- map(surge_hylak_ids$hylak_id, function(x){
  x <- as.integer(x)
  bathy <- rast(build_path(x))
  zmax <- max(values(bathy), na.rm = TRUE)
  zmean <- mean(values(bathy), na.rm = TRUE)
  tibble(hylak_id = as.character(x), globathy_max_depth = zmax, globathy_mean_depth = zmean)
}) |>
  bind_rows()

surge_globathy_depths <- left_join(surge_hylak_ids, globathy_depths)
write_csv(surge_globathy_depths, here("data/surge_globathy_depths.csv"))

