source(here::here("R/packages.R"))
library(USAboundaries)
ri <- USAboundaries::us_states(resolution = "high") |>
  filter(state_abbr == "RI") |>
  st_transform(5072)

flooded_lands <- st_read(here("data/flooded_lands_inventory.gpkg")) |>
  st_transform(5072)

nhd_lakes <- st_read(here("data/nhd/nhd_plus_waterbodies.gpkg")) |>
  st_transform(5072) |>
  filter(FTYPE == "LakePond")
  

ri_flooded_lands <- filter(flooded_lands, stusps == "RI")
ri_nhd_lakes <- st_intersection(nhd_lakes, ri)
mapview(ri_flooded_lands) + mapview(ri_nhd_lakes, col.regions= "red")

example_flooded <- filter(ri_flooded_lands, objectid == 1425826)
example_nhd <- filter(ri_nhd_lakes, COMID == 6139676)
mapview(example_flooded) + mapview(example_nhd, col.regions = "red")

# One way
# But what about legit islands?

perim_holes <- example_flooded |>
  st_cast("MULTILINESTRING") |>
  st_length()
perim_no_holes <- example_flooded |>
  st_remove_holes() |>
  st_cast("MULTILINESTRING") |>
  st_length()
perim_holes
perim_no_holes
(perim_holes - perim_no_holes)/perim_no_holes


area_holes <- example_flooded |>
  st_area()
area_no_holes <- example_flooded |>
  st_remove_holes() |>
  st_area()
area_holes
area_no_holes

(area_no_holes - area_holes)/area_holes

