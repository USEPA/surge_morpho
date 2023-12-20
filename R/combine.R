source(here::here("R/packages.R"))

################################################################################
# SuRGE
################################################################################
read_surge <- function(file){
  surge_xl <- read_excel(file,"data", skip = 1, na = c(" ", "NA")) |>
    select(lake.id, site.id, long, lat, site.depth, sample.depth.d) |>
    mutate(lake.id = as.character(lake.id), site.id = as.character(site.id))
}

surge_files <- list.files(here("data/surge"), pattern = ".xlsx",
                          full.names = TRUE)

surge_depths <- map_df(surge_files, read_surge) |>
  filter(!is.na(long)) |>
  mutate(long = case_when(long > 0 ~ long * -1,
                          TRUE ~ long))

# Guess on CRS and assuming longitude entered incorrectly...
surge_depths_sf <- st_as_sf(surge_depths, coords = c("long", "lat"), crs = 4326) |>
  st_transform(5072) |>
  rename_all(tolower) |>
  rename(surge_site_id = site.id)
#mapview(surge_depths_sf)

surge_dsn <- read_excel(here("data/surge/design/SuRGE_design_20191206_eval_status.xlsx"), 
                        "SuRGE_design_20191206")
surge_dsn <- select(surge_dsn, siteID, COMID, SITE_ID, UNIQUE_ID, MAXDEPTH, INDX_DEPTH) |>
  rename_all(tolower) |>
  rename(surge_site_id = siteid, nla_site_id = site_id, nla_unique_id = unique_id,
         nla_maxdepth = maxdepth, nla_indxdepth = indx_depth)
################################################################################
# lake morpho
################################################################################
lm <- read_sf(here("data/lakemorpho/national_lake_morphometry.gpkg")) |>
  st_transform(5072) |>
  rename_all(tolower)
lm_surge <- left_join(lm, surge_dsn, by = "comid") |>
  filter(!is.na(surge_site_id))
################################################################################
# nhd plus waterbodies
################################################################################
nhdplus <- read_sf(here("data/nhd/nhd_plus_waterbodies.gpkg")) |>
  st_transform(5072) |>
  rename_all(tolower)
nhd_surge <- left_join(nhdplus, surge_dsn, by = "comid") |>
  filter(!is.na(surge_site_id))
nhdplus_nogeo <- nhdplus
st_geometry(nhdplus_nogeo) <- NULL
lm_nhd_surge <- left_join(lm_surge, nhdplus_nogeo, by = "comid")
st_geometry(lm_nhd_surge) <- NULL
# Need to pull out surge id 
all <- left_join(surge_depths_sf, lm_nhd_surge, by = c("comid"))



################################################################################
# nla - Need to double check crs for each lat lon - currently assume 4326
################################################################################
nla_07 <- read_csv(here("data/nla/nla2007_sampledlakeinformation_20091113.csv")) |>
  rename_all(tolower) |>
  filter(!is.na(fld_lon_dd)) |>
  st_as_sf(coords = c("fld_lon_dd", "fld_lat_dd"), crs = 4326) |>
  st_transform(5072) |>
  select(site_id, date_col, depth = depthmax) |>
  mutate(uid = NA)
  
#nla_12_secchi <- read_csv(here("data/nla/nla2012_secchi_08232016.csv"))
nla_12_profile <- read_csv(here("data/nla/nla2012_wide_profile_08232016.csv"))  |>
  rename_all(tolower) |>
  filter(!is.na(index_lon_dd)) |>
  st_as_sf(coords = c("index_lon_dd", "index_lat_dd"), crs = 4326) |>
  st_transform(5072) |>
  select(uid, site_id, date_col, depth = index_site_depth)
#nla_17_secchi <- read_csv(here("data/nla/nla_2017_secchi-data.csv"))
nla_17_profile <- read_csv(here("data/nla/nla_2017_profile-data.csv"))   |>
  rename_all(tolower) |>
  # This will blow away messed up entries.  Might want to fix if this code becomes real
  mutate(index_lon_dd = as.numeric(index_lon_dd), index_lat_dd = as.numeric(index_lat_dd)) |>
  filter(!is.na(index_lon_dd)) |>
  filter(!is.na(index_lat_dd)) |>
  st_as_sf(coords = c("index_lon_dd", "index_lat_dd"), crs = 4326) |>
  st_transform(5072) |>
  select(uid, site_id, date_col, depth = index_site_depth)

nla <- bind_rows(nla_07, nla_12_profile, nla_17_profile)

#morpho <- st_join(surge_depths_sf, lm) |>
#  select(lake.id, site.id, comid, nlasite_id:meandepthcorrect)
##st_geometry(morpho) <- NULL
##nla07 <- st_join(nla_07, nhdplus) |>
#  select(site_id, comid, date_col, depth = depthmax) |>
#  mutate(uid = NA_real_, date_col = mdy(date_col))
#nla12 <- st_join(nla_12_profile, nhdplus) |>
#  select(uid, site_id, comid, date_col, depth = index_site_depth) |>
#  mutate(date_col = dmy(date_col))
#nla17 <- st_join(nla_17_profile, nhdplus) |>
#  select(uid, site_id, comid, date_col, depth = index_site_depth) |>
#  mutate(date_col = dmy(date_col))
#nla <- bind_rows(nla07, nla12, nla17)

nhd_surge_idx <- st_intersects(nhdplus, surge_depths_sf, sparse = FALSE)[1,]
nhd_nla_idx <- st_intersects(nhdplus, nla, sparse = FALSE)
lm_surge_idx <- st_intersects(lm, surge_depths_sf, sparse = FALSE)
lm_nla_idx <- st_intersects(lm, nla, sparse = FALSE)
 

#nla <- nla |>
#  mutate(year = year(date_col)) |>
#  group_by(site_id, comid, year) |>
#  summarize(depth = mean(depth, na.rm = TRUE)) |>
#  ungroup()
#nla <- filter(nla, !is.na(comid))
st_geometry(nla) <- NULL
# This still looks wonky...  Fix it
surge_all <- left_join(surge,morpho) |>
  left_join(nla, by = "comid" , 
            relationship = "many-to-many") 


