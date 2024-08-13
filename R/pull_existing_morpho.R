source(here::here("R/packages.R"))
source(here("R/functions.R"))

# NLA
nla22_site <- read_in_nla_depth(here("data/nla/nla22_siteinfo.csv"), 
                           c("SITE_ID", "INDEX_LON_DD", "INDEX_LAT_DD", 
                             "INDEX_SITE_DEPTH"), "nla2022", "site") |>
  pivot_longer(cols = -c("site_id", "nla", "src"), names_to = "variable", 
               values_to = "value") |>
  filter(!is.na(value)) |>
  filter(!is.na(site_id))
nla22_phab <- read_in_nla_depth(here("data/nla/nla22_phab_wide.csv"),
                            c("SITE_ID", "LONGITUDE", "LATITUDE", 
                              "DEPTH_AT_STATION"), "nla2022", "phab") |>
  pivot_longer(cols = -c("site_id", "nla", "src"), names_to = "variable", 
               values_to = "value") |>
  filter(!is.na(value)) |>
  filter(!is.na(site_id))
nla17_prof <- read_in_nla_depth(here("data/nla/nla_2017_profile-data.csv"),
                            c("SITE_ID", "INDEX_LON_DD", "INDEX_LAT_DD", 
                              "INDEX_SITE_DEPTH"), "nla2017", "profile") |>
  fix_lon_lat("index_lon_dd", "index_lat_dd") |>
  pivot_longer(cols = -c("site_id", "nla", "src"), names_to = "variable", 
               values_to = "value") |>
  filter(!is.na(value)) |>
  filter(!is.na(site_id))
nla17_phab <- read_in_nla_depth(here("data/nla/nla_2017_phab-data.csv"),
                            c("SITE_ID", "LONGITUDE", "LATITUDE", 
                              "DEPTH_AT_STATION"), "nla2017", "phab") |>
  pivot_longer(cols = -c("site_id", "nla", "src"), names_to = "variable", 
               values_to = "value") |>
  filter(!is.na(value)) |>
  filter(!is.na(site_id))
nla12_prof <- read_in_nla_depth(here("data/nla/nla2012_wide_profile_08232016.csv"),
                            c("SITE_ID", "INDEX_LON_DD", "INDEX_LAT_DD", 
                              "INDEX_SITE_DEPTH"), "nla2012", "profile") |>
  pivot_longer(cols = -c("site_id", "nla", "src"), names_to = "variable", 
               values_to = "value") |>
  filter(!is.na(value)) |>
  filter(!is.na(site_id))
nla12_phab <- read_in_nla_depth(here("data/nla/nla2012_wide_phab_08232016_0.csv"),
                            c("SITE_ID", "LONGITUDE", "LATITUDE", 
                              "DEPTH_AT_STATION"), "nla2012", "phab") |>
  pivot_longer(cols = -c("site_id", "nla", "src"), names_to = "variable", 
               values_to = "value") |>
  filter(!is.na(value)) |>
  filter(!is.na(site_id))
nla07_site <- read_in_nla_depth(here("data/nla/nla2007_sampledlakeinformation_20091113.csv"),
                            c("SITE_ID", "FLD_LON_DD", "FLD_LAT_DD", "FLD_SRC", 
                              "DEPTH_X", "DEPTHMAX"), "nla2007", "site") |>
  filter(fld_src == "Index_site") |>
  select( -fld_src) |>
  pivot_longer(cols = -c("site_id", "nla", "src"), names_to = "variable", 
               values_to = "value") |>
  filter(!is.na(value)) |>
  filter(!is.na(site_id))


nla_depths <- bind_rows(nla07_site, nla12_phab, nla12_prof, nla17_phab, 
                       nla17_prof, nla22_phab, nla22_site) |>
  filter(!is.na(value))  |>
  filter(!is.na(site_id))

write_csv(nla_depths, "data/nla_depths.csv")

# lagos
lagos_depth_clean <- read_csv(here("data/lagos/lagos_depth.csv")) |>
  select(lagoslakeid, lagos_maxdepth = lake_maxdepth_m, 
         lagos_meandepth = lake_meandepth_m, lagos_lakearea = lake_waterarea_ha)

write_csv(lagos_depth_clean, here("data/lagos_depth_area.csv"))

# NHD Plus Lake Morpho
nhdplus_morpho <- read_sf(here("data/nhd/nhd_plus_waterbodies.gpkg")) |>
  rename_all(tolower) |>
  select(nhdplus_comid = comid, nhdplus_meandepth = meandepth, 
         nhdplus_lakevolume = lakevolume, nhdplus_maxdepth = maxdepth, 
         nhdplus_meandused = meandused, nhdplus_meandcode = meandcode, 
         nhdplus_lakearea = lakearea)
st_geometry(nhdplus_morpho) <- NULL

write_csv(nhdplus_morpho, "data/nhdplus_morpho.csv")
  
# Original Lake Morpho
orig_morpho <- read_sf(here("data/lakemorpho/national_lake_morphometry.gpkg")) |>
  rename_all(tolower) |>
  select(lmorpho_comid = comid, origmorpho_surfacearea = surfacearea, 
         origmorpho_shorelinelength = shorelinelength, 
         origmorpho_shorelinedev = shorelinedev, origmorpho_maxlength = maxlength,
         origmorpho_maxwidth = maxwidth, origmorpho_meanwidth = meanwidth, 
         origmorpho_maxdepth_c = maxdepthcorrect, 
         origmorpho_meandepth_c = meandepthcorrect, 
         origmorpho_volume_c = volumecorrect)
st_geometry(orig_morpho) <- NULL
write_csv(orig_morpho , here("data/original_morpho.csv"))
