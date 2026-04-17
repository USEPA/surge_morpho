source(here::here("R/packages.R"))
source(here("R/functions.R"))

surge_nla <- st_read(here("data/surge_morpho_point_depth.gpkg")) |>
  filter(grepl("^nla", source))

st_geometry(surge_nla) <- NULL

surge_nla_mean <- surge_nla |>
  group_by(lake_id) |>
  summarize(mean_depth = mean(depth, na.rm = TRUE),
            n()) |>
  mutate(source = "nla index and nla phab",
         units = "m",
        variable = "mean_depth") |>
  select(lake_id, source, units, variable, value = mean_depth)

surge_nla_max <- surge_nla |>
  filter(.by = lake_id, depth == max(depth, na.rm = TRUE)) |>
  mutate(units = "m", variable = "max_depth") |>
  select(lake_id, source, units, variable, value = depth)

surge_nla <- bind_rows(surge_nla_mean, surge_nla_max) |>
  arrange(lake_id)

write_csv(surge_nla, here("data/surge_nla_metrics.csv"))
