source(here::here("R/packages.R"))
source(here("R/functions.R"))

#globathy
surge_globathy <- read_csv(here("data/surge_globathy_depths.csv")) |>
  mutate(source = "globathy", units = "m") |> # https://www.nature.com/articles/s41597-022-01132-9/figures/3
  select(lake_id, source, units, max_depth = globathy_max_depth, mean_depth = globathy_mean_depth) |>
  pivot_longer(max_depth:mean_depth, names_to = "variable", values_to = "value")


#hydrolakes
surge_hydrolakes <- read_csv(here("data/surge_hydrolakes_metrics.csv")) |>
  select(lake_id, surface_area = hydrolakes_lake_area, shoreline_length = hydrolakes_shoreline_length,
         volume = hydrolakes_volume, mean_depth = hydrolakes_mean_depth) |>
  pivot_longer(surface_area:mean_depth, names_to = "variable", values_to = "value") |>
  mutate(source = "hydrolakes",
         units = case_when(variable == "volume" ~ "mcm", # https://data.hydrosheds.org/file/technical-documentation/HydroLAKES_TechDoc_v10.pdf page 13
                           variable == "surface_area" ~ "km2", # https://data.hydrosheds.org/file/technical-documentation/HydroLAKES_TechDoc_v10.pdf page 12
                           TRUE ~ "m")) |>
  select(lake_id, source, units, variable, value)

#nhd
surge_nhd <- read_csv(here("data/surge_nhd_metrics.csv")) |>
  select(lake_id, surface_area = nhdplus_lake_area_sqkm, max_depth = nhdplus_maxdepth, 
         mean_depth = nhdplus_meandepth, volume = nhdplus_volume) |>
  pivot_longer(surface_area:volume, names_to = "variable", values_to = "value") |>
  mutate(source = "nhdplus",
         units = case_when(variable == "surface_area" ~ "km2", # https://www.epa.gov/system/files/documents/2023-04/NHDPlusV2_User_Guide.pdf page 161
                           variable == "volume" ~ "m3", # https://www.epa.gov/system/files/documents/2023-04/NHDPlusV2_User_Guide.pdf Page 50
                           TRUE ~ "m")) |>
  select(lake_id, source, units, variable, value)

#lagos
surge_lagos <- read_csv(here("data/surge_lagos_metrics.csv")) |>
  select(lake_id, max_depth = lagos_max_depth, mean_depth = lagos_mean_depth, surface_area = lagos_lake_area) |>
  pivot_longer(max_depth:surface_area, names_to = "variable", values_to = "value") |>
  mutate(source = "lagos",
         units = case_when(variable == "surface_area" ~ "ha",
                           TRUE ~ "m")) |> # https://lagoslakes.org/lagos-research-platforms/
  select(lake_id, source, units, variable, value)

#nla
surge_nla <- read_csv(here("data/surge_nla_metrics.csv"))

surge_stats_national_morpho <- bind_rows(surge_globathy, surge_hydrolakes, surge_nhd, surge_lagos, surge_nla) |>
  arrange(lake_id) |>
  filter(!is.na(value))

write_csv(surge_stats_national_morpho, here("data/surge_stats_national_morpho_metrics.csv"))
