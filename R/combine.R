source(here::here("R/packages.R"))
source(here("R/functions.R"))

# crosswalk
crosswalk_long <- read_csv(here("data/surge_master_crosswalk_long.csv"),
                           col_types = cols(lake_id = "c"))

# surge reservoir morpho
# Also adds in max lake length from point density runs up to 6000
surge_res_morpho <- read_csv(here("data/all_lakes_lakemorpho_old_maxLength.csv"))
mll <- read_csv(here("data/actual_max_lake_length.csv")) |>
  select(lake_id, maxLakeLengthBest = max_lake_length) |>
  mutate(lake_id = case_when(lake_id == 69 ~
                               paste0("0", as.character(lake_id)),
                             lake_id == 70 ~
                               paste0("0", as.character(lake_id)),
                             TRUE ~ as.character(lake_id)))
surge_res_morpho <- surge_res_morpho |>
  left_join(mll) |>
  select(-maxLength) |>
  rename(maxLength = maxLakeLengthBest)

write_csv(surge_res_morpho, here("data/all_lakes_lakemorpho.csv"))


# NLA depths
nla_index_depths <- read_csv(here("data/nla_depths.csv")) |>
  filter(variable == "depth_x" |
           variable == "index_site_depth") |>
  select(-src, -variable) |>
  filter(!is.na(value))  |>
  filter(!is.na(site_id))

# lakemorpho
original_morpho <- read_csv(here("data/original_morpho.csv"),
                            col_types = cols(lmorpho_comid = "c"))

# nhd plus waterbodies
nhdplus_morpho <- read_csv(here("data/nhdplus_morpho.csv"),
                           col_types = cols(nhdplus_comid = "c"))

# lagos depth
lagos_depth_area <- read_csv(here("data/lagos_depth_area.csv"),
                             col_types = cols(lagoslakeid = "c"))

# globathy max depth

globathy_max_depth <- read_csv(here("data/globath_dmax.csv"),
                               col_types = cols(globathy_hylak_id = "c"))

# nid
#nid <- st_read(here("data/nid/nation.gpkg"))

# combine
surge_nla <- merge_nlas(nla_index_depths, surge_res_morpho,
                                   crosswalk_long)

surge_orig <- merge_orig(original_morpho, surge_res_morpho,
                                    crosswalk_long)

surge_nhd <- merge_nhdplus(nhdplus_morpho, surge_res_morpho,
                                      crosswalk_long)

surge_lagos <- merge_lagos(lagos_depth_area, surge_res_morpho,
                                      crosswalk_long)

surge_measure <- sf::st_read(here("data/surge_morpho_point_depth.gpkg")) |>
  mutate(lake_id = case_when(lake_id == 69 ~ paste0("0", as.character(lake_id)),
                             lake_id == 70 ~ paste0("0", as.character(lake_id)),
                             TRUE ~ as.character(lake_id)))
st_geometry(surge_measure) <- NULL
surge_measure <- surge_measure |>
  group_by(lake_id) |>
  mutate(max_measured_depth = max(depth, na.rm = TRUE)) |>
  ungroup() |>
  filter(depth == max_measured_depth) |>
  mutate(join_id = NA_character_, join_id_name = NA_character_,
         variables = "maximum measured depth") |>
  select(lake_id, lake_name, join_id, join_id_name, source, variables,
         value = max_measured_depth) |>
  unique()

#sugre_nid <- merge_nid(nid, surge_res_morpho, crosswalk_long)

surge_res_morpho_long <- surge_res_morpho |>
  mutate(source = "surge_morpho", join_id = lake_id) |>
  relocate(lake_id, lake_name, join_id, source) |>
  filter(!is.na(lake_id)) |>
  filter(!is.na(join_id)) |>
  unique() |>
  pivot_longer(cols = surfaceArea:maxLength, names_to = "variables",
               values_to = "values") |>
  mutate(variables = tolower(variables)) |>
  mutate(join_id_name = "lake_id")

surge_globathy_max <- merge_globathy_max(globathy_max_depth, surge_res_morpho, crosswalk_long)

surge_res_morpho_all <- bind_rows(surge_orig, surge_lagos, surge_nhd, surge_nla,
                                  surge_res_morpho_long, surge_globathy_max,
                                  surge_measure) |>
  relocate(lake_id, lake_name, join_id, join_id_name) |>
  arrange(as.numeric(lake_id))

write_csv(surge_res_morpho_all, "data/surge_res_morpho_all.csv")



