source(here::here("R/packages.R"))
source(here("R/functions.R"))

# crosswalk
crosswalk_long <- read_csv(here("data/surge_master_crosswalk_long.csv"))

# surge reservoir morpho
surge_res_morpho <- read_csv(here("data/all_lakes_lakemorpho.csv"))

# NLA depths
nla_index_depths <- read_csv(here("data/nla_depths.csv")) |>
  filter(variable == "depth_x" |
           variable == "index_site_depth") |>
  select(-src, -variable) |>
  filter(!is.na(value))  |>
  filter(!is.na(site_id))


# lakemorpho
original_morpho <- read_csv(here("data/original_morpho.csv"))

# nhd plus waterbodies
nhdplus_morpho <- read_csv(here("data/nhdplus_morpho.csv"))

# lagos depth
lagos_depth_area <- read_csv(here("data/lagos_depth_area.csv"))


# Missing depths... Why?  For example 070 - francis case has depths from 12 and 17 but not in the output
surge_res_morpho_nla <- merge_nlas(nla_index_depths, surge_res_morpho, crosswalk_long)
