source(here::here("R/packages.R"))
source(here("R/functions.R"))
surge_pts_all <- st_read(here("data/surge_morpho_point_depth.gpkg"))
st_geometry(surge_pts_all) <- NULL

surge_pts_max_mean <- surge_pts_all |>
  filter(source %in% c("surge sites",
                       "pre-SuRGE resevoirs sampling loaction - bathymetry"),
         lake_id != 1001) |>
  summarize(
    .by = lake_id,
    max_depth = max(depth, na.rm = TRUE),
    mean_depth = mean(depth, na.rm = TRUE)) |>
  bind_rows(tibble(lake_id = 1001, max_depth = NA_real_, mean_depth = NA_real_))


surge_area <- read_csv(here("data/surge_res_morpho_all.csv")) |>
  filter(variables == "surfacearea",
         source == "surge_morpho") |>
  select(lake_id, values) |>
  mutate(lake_id = as.numeric(lake_id))

# Voronoi Volumes
surge_bathy_res <- st_read(here("data/surge/all_lakes.gpkg"),
                           layer = "all_lakes") |>
  st_transform(crs = 5072)

surge_sampled_pts <- st_read(here("data/surge_morpho_point_depth.gpkg")) |>
  st_transform(crs = 5072) |>
  filter(source == "surge sites")
# Removing pts that are outside of the reservoir area that doesn't have bathymetry
# For example Lake Loramie, bathy tif stops short of actual reservori poly but
# some samples occured in the area outside fo the bathy
surge_sampled_pts <- st_intersection(surge_sampled_pts, surge_bathy_res)
get_volumes <- function(id, reservoir, pts){
  #if(id == "1019"){browser()}
  reservoir <- filter(reservoir, lake_id == id)
  pts <- filter(pts, lake_id == id)
  # Should we use the spsurvey weights? I am gonna say no, because some
  # reservoirs will have other points as well that wouldn't have weights.
  mean_depth = mean(pts$depth, na.rm = TRUE)
  area = as.numeric(st_area(reservoir))
  vol_formula <- mean_depth * area

  # Voronoi Poloygons
  # From https://gis.stackexchange.com/questions/362134/i-want-to-create-a-voronoi-diagram-while-retaining-the-data-in-the-data-frame
  st_voronoi_point <- function(points){
    ## points must be POINT geometry
    # check for point geometry and execute if true
    if(!all(st_geometry_type(points) == "POINT")){
      stop("Input not  POINT geometries")
    }
    g = st_combine(st_geometry(points)) # make multipoint
    v = st_voronoi(g)
    v = st_collection_extract(v)
    return(v[unlist(st_intersects(points, v))])
  }

  vor_poly <- st_voronoi_point(pts)
  vor_poly <- st_intersection(vor_poly, st_as_sfc(reservoir))
  #vor_poly <- st_cast(vor_poly, "POLYGON") #Not working Multi-polygons messing things up.
  vor_poly <- st_set_geometry(pts, vor_poly)
  vol_vor <- sum(as.numeric(st_area(vor_poly)*vor_poly$depth), na.rm = TRUE)

  data.frame(lake_id  = as.character(id), volume_formula = vol_formula,
             volume_voronoi = vol_vor)

}

surge_sites_volumes <- lapply(surge_bathy_res$lake_id, get_volumes,
                              reservoir = surge_bathy_res,  pts = surge_sampled_pts) |>
  bind_rows() |>
  mutate(lake_id = as.numeric(lake_id)) #|>
  #pivot_longer(volume_formula:volume_voronoi, names_to = "variables",
  #             values_to = "values") |>
  #mutate(source = "surge sites")



surge_pts_max_mean <- left_join(surge_pts_max_mean, surge_sites_volumes) |>
  select(lake_id, max_depth, mean_depth, volume_formula, volume_voronoi)

View(surge_pts_max_mean)
