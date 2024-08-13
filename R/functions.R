#' Function to correct wonkiness in longitude and latitude readings
#' @param df data frame with lon and lat in it
#' @param lon the column name for longitude
#' @param lat the column name for latitude

fix_lon_lat <- function(df, lon, lat, site = "site_id") {
 
  df <- drop_na(df)
  fixed_df <- df |>
    mutate({{lon}} := case_when(str_detect(.data[[lon]], "^w") ~
                             str_replace(.data[[lon]], "^w", "-"),
                           str_detect(.data[[lon]], "$?��") ~
                             str_replace(.data[[lon]], "$?��", ""),
                           str_detect(.data[[lon]], " '") ~
                             str_replace(.data[[lon]], " '", ""),
                           str_detect(.data[[lon]], "'") ~
                             str_replace(.data[[lon]], "'", ""),
                           str_detect(.data[[lon]], "/3$") ~
                             str_replace(.data[[lon]], "/3$", ""),
                           str_detect(.data[[lon]], "\\?\\?$") ~
                             str_replace(.data[[lon]], "\\?\\?$", ""),
                           TRUE ~ .data[[lon]])) |>
    mutate({{lat}} := case_when(str_detect(.data[[lat]], "^w") ~
                             str_replace(.data[[lat]], "^w", "-"),
                           str_detect(.data[[lat]], "$?��") ~
                             str_replace(.data[[lat]], "$?��", ""),
                           str_detect(.data[[lat]], " '") ~
                             str_replace(.data[[lat]], " '", ""),
                           str_detect(.data[[lat]], "'") ~
                             str_replace(.data[[lat]], "'", ""),
                           str_detect(.data[[lat]], "/3$") ~
                             str_replace(.data[[lat]], "/3$", ""),
                           str_detect(.data[[lat]], "\\?\\?$") ~
                             str_replace(.data[[lat]], "\\?\\?$", ""),
                           TRUE ~ .data[[lat]])) |>
    mutate({{lon}} := case_when(str_detect(.data[[lon]], "\\?$") ~
                             str_replace(.data[[lon]], "\\?$", ""),
                           str_detect(.data[[lon]], "\\?\\?") ~ str_replace(paste(
                             str_split(.data[[lon]], "\\?\\?", simplify = TRUE)[,1],
                             round(as.numeric(str_split(.data[[lon]], "\\?\\?", 
                                                        simplify = TRUE)[,2])/60, 5), 
                             sep = "."), ".0.","."),
                           str_detect(.data[[lon]], "(\\..*?)\\.") ~
                             str_replace(.data[[lon]], "(\\..*?)\\.", "."),
                           TRUE ~ .data[[lon]])) |>
    mutate({{lon}} := case_when(!str_detect(.data[[lon]], "^-") ~
                             paste0("-", .data[[lon]]),
                           TRUE ~ .data[[lon]])) |>
    
    mutate({{lat}} := case_when(str_detect(.data[[lat]], "\\?$") ~
                             str_replace(.data[[lat]], "\\?$", ""),
                           str_detect(.data[[lat]], "\\?\\?") ~ str_replace(paste(
                             str_split(.data[[lat]], "\\?\\?", simplify = TRUE)[,1],
                             round(as.numeric(str_split(.data[[lat]], "\\?\\?", 
                                                        simplify = TRUE)[,2])/60, 5), 
                             sep = "."), ".0.","."),
                           .data[[lat]] == "." ~
                             NA_character_,
                           str_detect(.data[[lat]], " ") ~
                             str_replace(.data[[lat]], " ",""),
                           TRUE ~ .data[[lat]])) |>
    filter(!is.na(.data[[lon]])) |> 
    filter(!is.na(.data[[lat]])) |>
    filter(!is.na(.data[[site]])) |>
    mutate({{lon}} := str_replace(str_remove(str_replace(.data[[lon]],"\\.", ";"), "\\."),
                             ";", "."),
           {{lat}} := str_replace(str_remove(str_replace(.data[[lat]],"\\.", ";"), "\\."), 
                             ";", ".")) |>
    mutate({{lon}} := as.numeric(.data[[lon]]),
           {{lat}} := as.numeric(.data[[lat]]))
  fixed_df
}

#' Function to read in data and prep for pulling depth data
#' @param path for dataset
#' @param col_names columns to read in

read_in_nla_depth <- function(path, col_names, 
                              nla = c("nla2007", "nla2012", "nla2017", "nla2022"),
                              src = c("phab", "profile", "site")){

  nla <- match.arg(nla)
  src <- match.arg(src)
  df <- readr::read_csv(here(path), 
                  guess_max = 35000) |>
    select(all_of(col_names)) |>
    rename_all(tolower) |>
    mutate(nla = nla, src = src)
  df 
}

merge_nlas <- function(nla_depth, surge_morpho, crosswalk){
  
  cwl_nla22 <- filter(crosswalk, grepl("nla22_", join_id_name)) |>
    select(lake_id, site_id = join_id) |>
    mutate(lake_id = as.character(lake_id)) |>
    unique()
  cwl_nla17 <- filter(crosswalk, grepl("nla17_", join_id_name)) |>
    select(lake_id, site_id = join_id) |>
    mutate(lake_id = as.character(lake_id)) |>
    unique()
  cwl_nla12 <- filter(crosswalk, grepl("nla12_", join_id_name)) |>
    select(lake_id, site_id = join_id) |>
    mutate(lake_id = as.character(lake_id)) |>
    unique()
  cwl_nla07 <- filter(crosswalk, grepl("nla07_", join_id_name)) |>
    select(lake_id, site_id = join_id) |>
    mutate(lake_id = as.character(lake_id)) |>
    unique()
  
  nla22_depths <- nla_depth |>
    filter(nla == "nla2022") |>
    select(site_id, nla2022_index_depth = value)
  nla17_depths <- nla_depth |>
    filter(nla == "nla2017") |>
    select(site_id, nla2017_index_depth = value)
  nla12_depths <- nla_depth |>
    filter(nla == "nla2012") |>
    select(site_id, nla2012_index_depth = value)
  nla07_depths <- nla_depth |>
    filter(nla == "nla2007") |>
    select(site_id, nla2007_index_depth = value)
  
  nla22_surge <- left_join(surge_res_morpho, cwl_nla22, by = "lake_id") |>
    left_join(nla22_depths, by = "site_id", relationship = "many-to-many") |>
    select(-site_id) |>
    unique()
  nla17_surge <- left_join(surge_res_morpho, cwl_nla17, by = "lake_id") |>
    left_join(nla17_depths, by = "site_id", relationship = "many-to-many") |>
    select(-site_id) |>
    unique()
  nla12_surge <- left_join(surge_res_morpho, cwl_nla12, by = "lake_id") |>
    left_join(nla12_depths, by = "site_id", relationship = "many-to-many") |>
    select(-site_id) |>
    unique()
  nla07_surge <- left_join(surge_res_morpho, cwl_nla07, by = "lake_id") |>
    left_join(nla07_depths, by = "site_id", relationship = "many-to-many") |>
    select(-site_id) |>
    unique() 
  
  nla_surge <- full_join(nla07_surge, nla12_surge, relationship = "many-to-many") |>
    full_join(nla17_surge, relationship = "many-to-many") |>
    full_join(nla22_surge, relationship = "many-to-many")|>
    pivot_longer(surfaceArea:nla2022_index_depth, names_to = "variable", 
                 values_to = "value") |>
    unique() |>
    pivot_wider(id_cols = lake_id:lake_name, names_from = "variable", 
                values_from = "value", values_fn = max)
  nla_surge
}

