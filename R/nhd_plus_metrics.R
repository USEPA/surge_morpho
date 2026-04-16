source(here::here("R/packages.R"))
source(here("R/functions.R"))

surge_nhd_ids <- read_csv(here("data/surge_master_crosswalk_long.csv")) |>
  filter(join_id_name == "nhdplus_comid") |>
  select(lake_id, nhdplus_comid = join_id) |>
  mutate(lake_id = as.integer(lake_id), nhdplus_comid = as.integer(nhdplus_comid))

surge_nhd <- st_read(here("data/nhd/nhd_plus_waterbodies.gpkg")) |>
  filter(COMID %in% surge_nhd_ids$nhdplus_comid) |>
  st_transform(crs = 5072) |>
  left_join(surge_nhd_ids, by = c("COMID" = "nhdplus_comid")) |>
  select(lake_id, nhdplus_comid = COMID, nhdplus_lake_area_sqkm = AREASQKM, nhdplus_maxdepth = MaxDepth,
         nhdplus_meandepth = MeanDUsed, nhdplus_volume = LakeVolume)

st_geometry(surge_nhd) <- NULL

surge_nhd <- surge_nhd |>
  mutate(nhdplus_lake_area_sqkm = case_when(nhdplus_lake_area_sqkm == -9998 ~ NA_real_ ,
                                            TRUE ~ nhdplus_lake_area_sqkm),
         nhdplus_maxdepth = case_when(nhdplus_maxdepth == -9998 ~ NA_real_ ,
                                            TRUE ~ nhdplus_maxdepth),
         nhdplus_meandepth = case_when(nhdplus_meandepth == -9998 ~ NA_real_ ,
                                            TRUE ~ nhdplus_meandepth),
         nhdplus_volume = case_when(nhdplus_volume == -9998 ~ NA_real_ ,
                                            TRUE ~ nhdplus_volume)) |>
  group_by(lake_id) |>
  summarize(nhdplus_lake_area_sqkm = sum(nhdplus_lake_area_sqkm, na.rm = TRUE), 
            nhdplus_maxdepth = max(nhdplus_maxdepth, na.rm = TRUE),
            nhdplus_meandepth = mean(nhdplus_meandepth, na.rm = TRUE), 
            nhdplus_volume = sum(nhdplus_volume, na.rm = TRUE)) |>
  mutate(nhdplus_lake_area_sqkm = case_when(!is.finite(nhdplus_lake_area_sqkm) ~ NA_real_,
                                            TRUE ~ nhdplus_lake_area_sqkm),
         nhdplus_maxdepth = case_when(!is.finite(nhdplus_maxdepth) ~ NA_real_,
                                            TRUE ~ nhdplus_maxdepth),
         nhdplus_meandepth = case_when(!is.finite(nhdplus_meandepth) ~ NA_real_,
                                            TRUE ~ nhdplus_meandepth),
         nhdplus_volume = case_when(!is.finite(nhdplus_volume) ~ NA_real_,
                                    nhdplus_volume == 0.00 ~ NA_real_,
                                            TRUE ~ nhdplus_volume))

write_csv(surge_nhd, here("data/surge_nhd_metrics.csv"))
