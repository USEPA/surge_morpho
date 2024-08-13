source(here::here("R/packages.R"))

# Get SuRGE Lakes
if(!dir.exists(here::here("data/surge"))){
  get_lab_sites <- function(lab){
    path <- paste0("data/", lab)
    paths <- surge_sp$list_files(path)$name
    idx <- grepl("CH4_", paths)
    paths <- paths[idx]
    paths <- paste0("data/", lab, "/", paths, "/dataSheets/")
    surge_id <- str_extract(paths, "_[0-9]+_")
    surge_id <- str_extract(surge_id, "[0-9]+")
    paste0(paths, "surgeData", surge_id, ".xlsx")
  }
  download_it <- function(path){
    files <- surge_sp$list_files(dirname(path))
    if(any(grepl(basename(path), files$name))){
      surge_sp$download_file(path, 
                             dest = here::here(paste0("data/surge/", 
                                                      basename(path))), 
                             overwrite = TRUE)
    }
  }
  fs::dir_create(here::here("data/surge"))
  site <- get_sharepoint_site('SuRGE: Survey of Reservoir Greenhouse gas Emissions')
  surge_sp <- site$get_drive()
  labs <- list("ADA", "NAR", "CIN","DOE", "R10", "RTP", "USGS")
  data_paths <- purrr::map(labs, function(x) get_lab_sites(x))
  data_paths <- unlist(data_paths)
  not_falls_lake <- !grepl("Falls_Lake", data_paths)
  data_paths <- data_paths[not_falls_lake]
  purrr::map(data_paths, download_it)
  
  surge_sp$download_file("surgeDsn/SuRGE_design_20191206_eval_status.xlsx", 
                         dest = "data/surge/SuRGE_design_20191206_eval_status.xlsx", 
                         overwrite = TRUE)
  surge_sp$download_file("lakeDsn/all_lakes.gpkg", 
                         dest = "data/surge/all_lakes.gpkg", 
                         overwrite = TRUE)
}

# Get existing crosswalks
surge_sp$download_file("data/siteDescriptors/SuRGE_design_hylakID.csv", 
                       dest = "data/surge/SuRGE_design_hylakID.csv", 
                       overwrite = TRUE)
surge_sp$download_file("data/siteDescriptors/NID_data_for_Surge_and_hand_sites.csv", 
                       dest = "data/surge/NID_data_for_Surge_and_hand_sites.csv", 
                       overwrite = TRUE)
surge_sp$download_file("data/siteDescriptors/lake_link.csv", 
                       dest = "data/surge/lake_link.csv", 
                       overwrite = TRUE)
surge_sp$download_file("data/siteDescriptors/Surge_nhdhr.csv", 
                       dest = "data/surge/Surge_nhdhr.csv", 
                       overwrite = TRUE)

# Get Lake morphometry
fs::dir_create(here::here("data/lakemorpho"))
if(!file.exists(here::here("data/lakemorpho/lakemorphometry.zip"))){
  GET("https://edg.epa.gov/data/PUBLIC/ORD/NHEERL/LakeMorphometry.zip", # This still exists as of 2023-10-25
      write_disk(here::here("data/lakemorpho/lakemorphometry.zip")))
}
if(!dir.exists(here::here("data/lakemorpho/LakeMorphGdb.gdb"))){
  unzip(here::here("data/lakemorpho/lakemorphometry.zip"),
        exdir = here::here("data/lakemorpho"))
}
if(!file.exists(here::here("data/lakemorpho/national_lake_morphometry.gpkg"))){
  lyrs <- st_layers(here::here("data/lakemorpho/LakeMorphGdb.gdb"))$name
  all_morpho <- assign(lyrs[1],st_read(here::here("data/lakemorpho/LakeMorphGdb.gdb"),lyrs[1],
                                       stringsAsFactors = FALSE)) %>%
    mutate(huc_region = lyrs[1])
  for(i in lyrs[-1]){
    assign(i, st_read(here::here("data/lakemorpho/LakeMorphGdb.gdb"),i,stringsAsFactors = FALSE))
    all_morpho <- get(i) %>%
      mutate(huc_region = i) %>%
      rbind(all_morpho)
  }
  st_write(all_morpho, 
           here::here("data/lakemorpho/national_lake_morphometry.gpkg"))
}

# Get NLA max depth 
fs::dir_create(here::here("data/nla"))

if(!file.exists(here::here("data/nla/nla2007_sampledlakeinformation_20091113.csv"))){
  nla2007_site <- httr::GET("https://www.epa.gov/sites/default/files/2014-01/nla2007_sampledlakeinformation_20091113.csv", 
                            httr::write_disk(here::here("data/nla/nla2007_sampledlakeinformation_20091113.csv"),
                                             overwrite = TRUE))
}

## NLA 12
if(!file.exists(here::here("data/nla/nla2012_wide_phab_08232016_0.csv"))){
  nla2012_key <- httr::GET("https://www.epa.gov/sites/default/files/2016-12/nla2012_wide_phab_08232016_0.csv", 
                           httr::write_disk(here::here("data/nla/nla2012_wide_phab_08232016_0.csv"),
                                            overwrite = TRUE))
}
if(!file.exists(here::here("data/nla/nla12_keyvariables_data.csv"))){
  nla2012_key <- httr::GET("https://www.epa.gov/sites/default/files/2020-12/nla12_keyvariables_data.csv", 
                           httr::write_disk(here::here("data/nla/nla12_keyvariables_data.csv"),
                                            overwrite = TRUE))
}
if(!file.exists(here::here("data/nla/nla2012_wide_profile_08232016.csv"))){
  nla2012_profile <- httr::GET("https://www.epa.gov/sites/default/files/2016-12/nla2012_wide_profile_08232016.csv", 
                               httr::write_disk(here::here("data/nla/nla2012_wide_profile_08232016.csv"),
                                                overwrite = TRUE))
}


## NLA 17
if(!file.exists(here::here("data/nla/nla_2017_profile-data.csv"))){
  nla2017_profile <- httr::GET("https://www.epa.gov/sites/default/files/2021-04/nla_2017_profile-data.csv", 
                               httr::write_disk(here::here("data/nla/nla_2017_profile-data.csv"),
                                                overwrite = TRUE))
}
if(!file.exists(here::here("data/nla/nla_2017_phab-data.csv"))){
  nla2017_profile <- httr::GET("https://www.epa.gov/sites/default/files/2021-04/nla_2017_phab-data.csv", 
                               httr::write_disk(here::here("data/nla/nla_2017_phab-data.csv"),
                                                overwrite = TRUE))
}
if(!file.exists(here::here("data/nla/nla_2017_secchi-data.csv"))){
  nla2017_secchi <- httr::GET("https://www.epa.gov/sites/default/files/2021-04/nla_2017_secchi-data.csv", 
                              httr::write_disk(here::here("data/nla/nla_2017_secchi-data.csv"),
                                               overwrite = TRUE))
}
## NLA 22
if(!file.exists(here::here("data/nla/nla22_siteinfo.csv"))){
  nla2007_site <- httr::GET("https://www.epa.gov/system/files/other-files/2024-04/nla22_siteinfo.csv", 
                            httr::write_disk(here::here("data/nla/nla22_siteinfo.csv"),
                                             overwrite = TRUE))
}
if(!file.exists(here::here("data/nla/nla22_phab_wide.csv"))){
  nla2007_site <- httr::GET("https://www.epa.gov/system/files/other-files/2024-04/nla22_phab_wide.csv", 
                            httr::write_disk(here::here("data/nla/nla22_phab_wide.csv"),
                                             overwrite = TRUE))
}


# Get all NHD+
httr::GET("https://dmap-data-commons-ow.s3.amazonaws.com/NHDPlusV21/Data/NationalData/NHDPlusV21_NationalData_Seamless_Geodatabase_Lower48_07.7z",
          httr::write_disk(here::here("data/nhd/NHDPlusV21_NationalData_Seamless_Geodatabase_Lower48_07.7z")))

# Get LAGOS US
# https://portal.edirepository.org/nis/dataviewer?packageid=edi.1043.1&entityid=f6ffb2325156008edffbea5b6c1ea24c
lagos_depth <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1043.1&entityid=f6ffb2325156008edffbea5b6c1ea24c")
write_csv(lagos_depth, here("data/lagos/lagos_depth.csv"))
download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.854.1&entityid=5488e333ce818597fa3dbfc9b4e0c131",
              here("data/lagos/lake_link.csv"))
download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.854.1&entityid=fd70ea9852cda4caa7eb44c0c7348630",
              here("data/lagos/gis_locus_v1.0_gpkg.zip"))




# Download GLOBathy datasets from figshare
# The raster is big... 15GB +

if(!dir.exists(here::here("data/globathy"))){
  fs::dir_create("data/globathy")
  globathy_max_resp <- httr::GET("https://api.figshare.com/v2/file/download/28919991", 
                                 httr::write_disk(here::here("data/globathy/globathy_max.zip"),
                                                  overwrite = FALSE))
  unzip(here::here("data/globathy/globathy_max.zip"),
        exdir = here::here("data/globathy/globathy_max"))
  globathy_hav_resp <- httr::GET("https://api.figshare.com/v2/file/download/28917783", 
                                 httr::write_disk(here::here("data/globathy/globathy_hav.nc"),
                                                  overwrite = TRUE))
  globathy_rast_resp <- httr::GET("https://api.figshare.com/v2/file/download/28919850", 
                                 httr::write_disk(here::here("data/globathy/globathy_rast.zip"),
                                                  overwrite = FALSE))
  unzip(here::here("data/globathy/globathy_rast.zip"),
         exdir = here::here("data/globathy/globathy_rast"))
}


