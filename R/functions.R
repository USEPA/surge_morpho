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

#' Function to merge NLA data
#' @param nla_depth
#' @param surge_morpho
#' @param crosswalk

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

  nla22_surge <- left_join(nla22_depths, cwl_nla22, by = "site_id") |>
    right_join(surge_morpho, by = "lake_id") |>
    mutate(source = "NLA 2022") |>
    relocate(lake_id, lake_name, join_id = site_id, source, index_depth = nla2022_index_depth) |>
    select(lake_id:index_depth) |>
    filter(!is.na(lake_id)) |>
    filter(!is.na(join_id)) |>
    unique()
  nla17_surge <- left_join(nla17_depths, cwl_nla17, by = "site_id") |>
    right_join(surge_morpho, by = "lake_id") |>
    mutate(source = "NLA 2017") |>
    relocate(lake_id, lake_name, join_id = site_id, source, index_depth = nla2017_index_depth) |>
    select(lake_id:index_depth) |>
    filter(!is.na(lake_id)) |>
    filter(!is.na(join_id)) |>
    unique()
  nla12_surge <- left_join(nla12_depths, cwl_nla12, by = "site_id") |>
    right_join(surge_morpho, by = "lake_id") |>
    mutate(source = "NLA 2012") |>
    relocate(lake_id, lake_name, join_id = site_id, source, index_depth = nla2012_index_depth) |>
    select(lake_id:index_depth) |>
    filter(!is.na(lake_id)) |>
    filter(!is.na(join_id)) |>
    unique()
  nla07_surge <- left_join(nla07_depths, cwl_nla07, by = "site_id") |>
    right_join(surge_morpho, by = "lake_id") |>
    mutate(source = "NLA 2007") |>
    relocate(lake_id, lake_name, join_id = site_id, source, index_depth = nla2007_index_depth) |>
    select(lake_id:index_depth) |>
    filter(!is.na(lake_id)) |>
    filter(!is.na(join_id)) |>
    unique()

  nla_surge <- bind_rows(nla07_surge, nla12_surge, nla17_surge, nla22_surge) |>
    pivot_longer(cols = index_depth, names_to = "variables", values_to = "values") |>
    filter(!is.na(values)) |>
    mutate(join_id_name = "site_id")



  nla_surge
}

#' Function to merge in the original lake morpho runs
#'
#' @param orig
#' @param surge
#' @param crosswalk
#'
merge_orig <- function(orig, surge, crosswalk){

  cw_lm <- filter(crosswalk, join_id_name == "lmorpho_comid") |>
    select(lmorpho_comid = join_id, lake_id)
  orig_join <- left_join(orig, cw_lm, by = "lmorpho_comid")
  surge_orig <- left_join(surge, orig_join, by = "lake_id") |>
    mutate(source = "nlmd") |>
    relocate(lake_id, lake_name, join_id = lmorpho_comid, source) |>
    filter(!is.na(lake_id)) |>
    filter(!is.na(join_id)) |>
    unique() |>
    pivot_longer(cols = surfaceArea:origmorpho_volume_c, names_to = "variables",
                 values_to = "values") |>
    filter(grepl("origmorpho", variables)) |>
    mutate(variables = str_replace(variables, "origmorpho_", "")) |>
    filter(!is.na(values)) |>
    mutate(join_id_name = "lmorpho_comid")


  surge_orig
}

#' Function to merge in the original lake morpho runs
#'
#' @param nhdplus
#' @param surge
#' @param crosswalk
#'
merge_nhdplus <- function(nhdplus, surge, crosswalk){

  cw_nhd <- filter(crosswalk, join_id_name == "nhdplus_comid") |>
    select(nhdplus_comid = join_id, lake_id)
  nhdplus_join <- left_join(nhdplus, cw_nhd, by = "nhdplus_comid")
  surge_nhdplus <- left_join(surge, nhdplus_join, by = "lake_id")  |>
    mutate(source = "nhdplus") |>
    relocate(lake_id, lake_name, join_id = nhdplus_comid, source) |>
    filter(!is.na(lake_id)) |>
    filter(!is.na(join_id)) |>
    unique() |>
    pivot_longer(cols = surfaceArea:nhdplus_lakearea, names_to = "variables",
                 values_to = "values") |>
    filter(grepl("nhdplus_", variables)) |>
    mutate(variables = str_replace(variables, "nhdplus_", "")) |>
    filter(!is.na(values)) |>
    mutate(join_id_name = "nhdplus_comid")
  surge_nhdplus
}

#' Function to merge in the original lake morpho runs
#'
#' @param lagos
#' @param surge
#' @param crosswalk
#'
merge_lagos <- function(lagos, surge, crosswalk){

  cw_lagos <- filter(crosswalk, join_id_name == "lagoslakeid") |>
    select(lagoslakeid = join_id, lake_id)
  lagos_join <- left_join(lagos, cw_lagos, by = "lagoslakeid")
  surge_lagos <- left_join(surge, lagos_join, by = "lake_id") |>
    mutate(source = "lagos") |>
    relocate(lake_id, lake_name, join_id = lagoslakeid, source) |>
    filter(!is.na(lake_id)) |>
    filter(!is.na(join_id)) |>
    unique() |>
    pivot_longer(cols = surfaceArea:lagos_lakearea, names_to = "variables",
                 values_to = "values") |>
    filter(grepl("lagos_", variables)) |>
    mutate(variables = str_replace(variables, "lagos_", "")) |>
    filter(!is.na(values)) |>
    mutate(join_id_name = "lagoslakeid")
  surge_lagos

}

#' Function to merge in the original lake morpho runs
#'
#' @param globathy
#' @param surge
#' @param crosswalk
#'
merge_globathy_max <- function(globathy, surge, crosswalk){

  cw_globathy <- filter(crosswalk, join_id_name == "hylak_id") |>
    select(globathy_hylak_id = join_id, lake_id)
  globathy_join <- left_join(globathy, cw_globathy, by = "globathy_hylak_id")
  surge_globathy <- left_join(surge, globathy_join, by = "lake_id") |>
    mutate(source = "globathy") |>
    relocate(lake_id, lake_name, join_id = globathy_hylak_id, source) |>
    filter(!is.na(lake_id)) |>
    filter(!is.na(join_id)) |>
    unique() |>
    pivot_longer(cols = surfaceArea:globathy_dmax_use, names_to = "variables",
                 values_to = "values") |>
    filter(grepl("globathy_", variables)) |>
    mutate(variables = str_replace(variables, "globathy_", "")) |>
    filter(!is.na(values)) |>
    mutate(join_id_name = "hylak_id")
  surge_globathy

}

#' Function to capture point density and max lake length
#'
#' @param lake
#' @param sequence
#' @param ncore
#'
max_lake_length_var <- function(lake, sequence){#, ncore = 7){
  #lake_lm <- lakeSurroundTopo(lake, reso = 300)
  get_max_length <- function(lake, dens){
    filter_length <- surge_lake_length(lake, 100, 50)
    if(is.na(filter_length)) {filter_length <- 30}
    max_length <- surge_lake_length(lake, dens, filter_length)
    gc()
    data.frame(dens, max_length)
  }
  #plan(multisession, workers = ncore)
  max_lengths <- lapply(sequence, function(x) get_max_length(lake, x))
  #plan(sequential)
  #max_lengths <- lapply(sequence, function(x) get_max_length(lake_lm, x))
  bind_rows(max_lengths)
}

max_lake_length_var_old <- function(lake, sequence){#, ncore = 7){
  #lake_lm <- lakeSurroundTopo(lake, reso = 300)
  get_max_length <- function(lake, dens){
    filter_length <- surge_lake_length(lake, 100, 0)
    max_length <- surge_lake_length(lake, dens)#, filter_length)
    gc()
    data.frame(dens, max_length)
  }
  #plan(multisession, workers = ncore)
  max_lengths <- lapply(sequence, function(x) get_max_length(lake, x))
  #plan(sequential)
  #max_lengths <- lapply(sequence, function(x) get_max_length(lake_lm, x))
  bind_rows(max_lengths)
}

surge_lake_length <- function(lake, pointDens, addLine = TRUE, dist_filt = 100) {
  #if (!inherits(inLakeMorpho, "lakeMorpho")) {
  #  stop("Input data is not of class 'lakeMorpho'.  Run lakeSurround Topo or lakeMorphoClass first.")
  #}
  result <- NA
  myLines <- build_my_lines(lake, pointDens, dist_filt = dist_filt)
  myInd <- sf::st_contains(lake, myLines, sparse = FALSE)[1,]
  if (sum(myInd) == 0) {
    return(NA)
  }
  if(capabilities("long.double")){
    myLine <- myLines[myInd][sf::st_length(myLines[myInd]) == max(sf::st_length(myLines[myInd]))]
  } else {
    myLine <- myLines[myInd][round(sf::st_length(myLines[myInd]),8) == round(max(sf::st_length(myLines[myInd])),8)]
  }

  result <- as.numeric(sf::st_length(myLine))

  #if (addLine) {
  #  myName <- deparse(substitute(inLakeMorpho))
  #  inLakeMorpho$maxLengthLine <- NULL
  #  inLakeMorpho$maxLengthLine <- myLine
  #  class(inLakeMorpho) <- "lakeMorpho"
  #  assign(myName, inLakeMorpho, envir = parent.frame())
  #}
  return(round(result,4))
}

#' Function to build myLines for max lake length
build_my_lines <- function(lake, pointDens=100, dist_filt = 30){
  lakeShorePoints <- sf::st_sample(st_cast(lake,"MULTILINESTRING"), pointDens,
                                   type = "regular")
  lakeShorePoints <- st_cast(lakeShorePoints, "POINT")
  lakeShorePoints <- st_coordinates(lakeShorePoints)
  dm <- dist(lakeShorePoints)
  md <- nrow(lakeShorePoints)
  x0 <- lakeShorePoints[which(lower.tri(matrix(1, md, md)) == 1, arr.ind = TRUE)[, 1], ][, 1][order(dm, decreasing = TRUE)]  #[30:md]
  y0 <- lakeShorePoints[which(lower.tri(matrix(1, md, md)) == 1, arr.ind = TRUE)[, 1], ][, 2][order(dm, decreasing = TRUE)]  #[30:md]
  x1 <- lakeShorePoints[which(lower.tri(matrix(1, md, md)) == 1, arr.ind = TRUE)[, 2], ][, 1][order(dm, decreasing = TRUE)]  #[30:md]
  y1 <- lakeShorePoints[which(lower.tri(matrix(1, md, md)) == 1, arr.ind = TRUE)[, 2], ][, 2][order(dm, decreasing = TRUE)]  #[30:md]
  xydf <- matrix(c(x0, x1, y0, y1), ncol = 4)
  dist_idx <-dm[order(dm, decreasing = TRUE)] > dist_filt
  xydf <- xydf[dist_idx,]
  xylist <- split(xydf, 1:nrow(xydf))
  myLines <- st_sfc(lapply(xylist,
                           function(x) st_linestring(matrix(as.numeric(x),2,2))),
                    crs = sf::st_crs(lake))
  myLines
}
