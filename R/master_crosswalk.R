source(here::here("R/packages.R"))

surge_reservoirs <- st_read(here("data/surge/all_lakes.gpkg"), "all_lakes") |>
  st_transform(4326)

# NLA
nla17 <- read_csv(here("data/nla/nla_2017_profile-data.csv")) |>
  select(nla17_site_id = SITE_ID, lon = INDEX_LON_DD, 
         lat = INDEX_LAT_DD) |>
  mutate(lon = as.numeric(lon),
         lat = as.numeric(lat)) |>
  filter(!is.na(lon)) |> #Some issues with a few locations no entered in correctly. These are thrown out.
  filter(!is.na(lat))|>
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
surge_nla17 <- st_join(surge_reservoirs, nla17)
st_geometry(surge_nla17) <- NULL
surge_nla17 <- unique(surge_nla17) |>
  select(-lake_name)

nla12 <- read_csv(here("data/nla/nla2012_wide_profile_08232016.csv")) |>
  select(nla12_site_id = SITE_ID, lon = INDEX_LON_DD, 
         lat = INDEX_LAT_DD) |>
  filter(!is.na(lon)) |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
surge_nla12 <- st_join(surge_reservoirs, nla12)
st_geometry(surge_nla12) <- NULL 
surge_nla12 <- unique(surge_nla12)  |>
  select(-lake_name)

nla07 <- read_csv(here("data/nla/nla2007_sampledlakeinformation_20091113.csv")) |>
  select(nl07_site_id = SITE_ID, lon = LON_DD, lat = LAT_DD) |>
  filter(!is.na(lon)) |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
surge_nla07 <- st_join(surge_reservoirs, nla07)
st_geometry(surge_nla07) <- NULL
surge_nla07 <- unique(surge_nla07)  |>
  select(-lake_name)

#surge_nla <- full_join(surge_nla17, surge_nla12) |>
#  full_join(surge_nla07)

#lakemorpho from NHDPlus V2
lmorpho <- st_read(here("data/lakemorpho/national_lake_morphometry.gpkg")) |>
  select(lmorpho_comid = COMID, lmorpho_nla07 = nlaSITE_ID) |>
  st_transform(st_crs(surge_reservoirs)) |>
  st_make_valid()
surge_lmorpho <- st_join(surge_reservoirs, lmorpho) |>
  select(-lake_name)
st_geometry(surge_lmorpho) <- NULL

#nhdplus v21 - check on lakemorpho
nhdplus <- st_read(here("data/nhd/nhd_plus_waterbodies.gpkg")) |>
  select(nhdplus_comid = COMID, gnis_id = GNIS_ID) |>
  st_transform(st_crs(surge_reservoirs))
sf_use_s2(FALSE)
nhdplus <- st_make_valid(nhdplus)
surge_nhdplus <- st_join(surge_reservoirs, nhdplus) |>
  select(-lake_name)
sf_use_s2(TRUE)
st_geometry(surge_nhdplus) <- NULL

#lagos depth
lagos_depth <- read_csv(here("data/lagos/lagos_depth.csv")) |>
  select(lon = lake_lon_decdeg, lat = lake_lat_decdeg, lagoslakeid) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326)
surge_lagos <- st_join(surge_reservoirs, lagos_depth) |>
  select(-lake_name)
st_geometry(surge_lagos) <- NULL

# Existing Crosswalks: hylak
surge_hylak <- readr::read_csv(here::here("data/surge/SuRGE_design_hylakID.csv")) |>
  select(lake_id = siteID, hylak_comid = COMID, hylak_id) |>
  mutate(lake_id = str_replace(lake_id, "CH4-", "")) |>
  mutate(lake_id = str_replace(lake_id, "^0+", ""))
  
# Existing Crosswalks: nid
surge_nid <- readr::read_csv(here::here("data/surge/NID_data_for_Surge_and_hand_sites.csv")) |>
  select(lake_id = siteID, nid_id = NID.ID) |>
  mutate(lake_id = str_replace(lake_id, "CH4-", "")) |>
  mutate(lake_id = str_replace(lake_id, "^0+", ""))

# Jake indicated some issues...
#link <- readr::read_csv(here::here("data/surge/lake_link.csv"))

# Existing Crosswalks: nhdhr
surge_nhdhr <- readr::read_csv(here::here("data/surge/Surge_nhdhr.csv"))



# Combine in Master
surge_master_crosswalk <- surge_reservoirs
st_geometry(surge_master_crosswalk) <- NULL

surge_master_crosswalk <- left_join(surge_master_crosswalk, surge_nla17, 
                                    by = "lake_id",  
                                    relationship = "many-to-many") |>
  left_join(surge_nla12, by = "lake_id", relationship = "many-to-many") |>
  left_join(surge_nla07, by = "lake_id", relationship = "many-to-many") |>
  left_join(surge_lmorpho, by = "lake_id", relationship = "many-to-many") |>
  left_join(surge_nhdplus, by = "lake_id", relationship = "many-to-many") |>
  left_join(surge_lagos, by = "lake_id", relationship = "many-to-many") |>
  left_join(surge_hylak, by = "lake_id", relationship = "many-to-many") |>
  left_join(surge_nid, by = "lake_id", relationship = "many-to-many") |>
  mutate(across(c(lmorpho_comid, nhdplus_comid, lagoslakeid, hylak_comid, 
                hylak_id), as.character))

surge_master_crosswalk_long <- pivot_longer(surge_master_crosswalk, 3:13, 
                                            names_to = "join_id_name",
                                            values_to = "join_id") |>
  filter(!is.na(join_id)) |>
  filter(join_id != " ") |>
  filter(join_id != "NA") |>
  unique()

write_csv(surge_master_crosswalk_long, "data/surge_master_crosswalk_long.csv")
