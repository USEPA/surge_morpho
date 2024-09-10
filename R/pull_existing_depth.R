source(here::here("R/packages.R"))
source(here("R/functions.R"))

# NLA
nla22_site <- read_in_nla_depth(here("data/nla/nla22_siteinfo.csv"),
                           c("SITE_ID", "INDEX_LON_DD", "INDEX_LAT_DD",
                             "INDEX_SITE_DEPTH"), "nla2022", "site") |>
  pivot_longer(cols = -c("site_id", "nla", "src"), names_to = "variable",
               values_to = "value")
nla22_phab <- read_in_nla_depth(here("data/nla/nla22_phab_wide.csv"),
                            c("SITE_ID", "LONGITUDE", "LATITUDE",
                              "DEPTH_AT_STATION"), "nla2022", "phab") |>
  pivot_longer(cols = -c("site_id", "nla", "src"), names_to = "variable",
               values_to = "value")
nla17_prof <- read_in_nla_depth(here("data/nla/nla_2017_profile-data.csv"),
                            c("SITE_ID", "INDEX_LON_DD", "INDEX_LAT_DD",
                              "INDEX_SITE_DEPTH"), "nla2017", "profile") |>
  fix_lon_lat("index_lon_dd", "index_lat_dd") |>
  pivot_longer(cols = -c("site_id", "nla", "src"), names_to = "variable",
               values_to = "value")
nla17_phab <- read_in_nla_depth(here("data/nla/nla_2017_phab-data.csv"),
                            c("SITE_ID", "LONGITUDE", "LATITUDE",
                              "DEPTH_AT_STATION"), "nla2017", "phab") |>
  pivot_longer(cols = -c("site_id", "nla", "src"), names_to = "variable",
               values_to = "value")
nla12_prof <- read_in_nla_depth(here("data/nla/nla2012_wide_profile_08232016.csv"),
                            c("SITE_ID", "INDEX_LON_DD", "INDEX_LAT_DD",
                              "INDEX_SITE_DEPTH"), "nla2012", "profile") |>
  pivot_longer(cols = -c("site_id", "nla", "src"), names_to = "variable",
               values_to = "value")
nla12_phab <- read_in_nla_depth(here("data/nla/nla2012_wide_phab_08232016_0.csv"),
                            c("SITE_ID", "LONGITUDE", "LATITUDE",
                              "DEPTH_AT_STATION"), "nla2012", "phab") |>
  pivot_longer(cols = -c("site_id", "nla", "src"), names_to = "variable",
               values_to = "value")
nla07_site <- read_in_nla_depth(here("data/nla/nla2007_sampledlakeinformation_20091113.csv"),
                            c("SITE_ID", "FLD_LON_DD", "FLD_LAT_DD", "FLD_SRC",
                              "DEPTH_X", "DEPTHMAX"), "nla2007", "site") |>
  filter(fld_src == "Index_site") |>
  select( -fld_src) |>
  pivot_longer(cols = -c("site_id", "nla", "src"), names_to = "variable",
               values_to = "value")

nla_depths <- bind_rows(nla07_site, nla12_phab, nla12_prof, nla17_phab,
                       nla17_prof, nla22_phab, nla22_site)

write_csv(nla_depths, "data/nla_depths.csv")

# NHD Plus Lake Morpho
nhdplus_morpho <- read_sf(here("data/nhd/nhd_plus_waterbodies.gpkg")) |>
  rename_all(tolower) |>
  select(nhdplus_comid = comid, nhdplus_meandepth, nhdplus_lakevolume,
         nhdplus_maxdepth, nhdplus_meandused, nhdplus_meandcode,
         nhdplus_lakearea)

write_csv(nhdplus_morpho, "data/nhdplus_morpho.csv")

# globathy dmax
globathy_dmax <- read_csv(here("data/globathy/globathy_max/GLOBathy_basic_parameters/GLOBathy_basic_parameters(ALL_LAKES).csv")) |>
  rename_all(tolower) |>
  select(globathy_hylak_id = hylak_id, globathy_dmax_use = dmax_use_m)

write_csv(globathy_dmax, "data/globath_dmax.csv")

