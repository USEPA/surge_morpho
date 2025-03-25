source(here::here("R/packages.R"))
source(here("R/functions.R"))
surge_pts_all <- st_read(here("data/surge_morpho_point_depth.gpkg"))
st_geometry(surge_pts_all) <- NULL

surge_pts_max_mean <- surge_pts_all |>
  filter(source %in% c("surge sites",
                       "pre-SuRGE resevoirs sampling loaction - bathymetry"),
         lake_id != 1001) |>
  summarize(
    .by = lake_id,
    max_depth = max(depth, na.rm = TRUE),
    mean_depth = mean(depth, na.rm = TRUE)) |>
  bind_rows(tibble(lake_id = 1001, max_depth = NA_real_, mean_depth = NA_real_))


surge_area <- read_csv(here("data/surge_res_morpho_all.csv")) |>
  filter(variables == "surfacearea",
         source == "surge_morpho") |>
  select(lake_id, values) |>
  mutate(lake_id = as.numeric(lake_id))

surge_pts_max_mean <- left_join(surge_pts_max_mean, surge_area) |>
  mutate(volume = values * mean_depth) |>
  select(lake_id, max_depth, mean_depth, volume)
