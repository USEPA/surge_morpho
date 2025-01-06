library(here)
library(sf)
library(hrbrthemes)
library(gpplot2)
library(dplyr)
library(extrafont)

#' Create SuRGE National Map
#'
surge_national_map <- function(surge_polys, us_states){

  loadfonts(device = "win", quiet = TRUE)
  windowsFonts("Roboto Condensed"=windowsFont("Roboto Condensed"))
  surge_pts <- st_centroid(surge_polys)

  x <- ggplot() +
    geom_sf(data = us_states) +
    geom_sf(data = surge_pts, aes(col = study, shape = study), size = 5) +
    scale_color_manual(values = c("darkred", "grey10", "darkblue")) +
    theme_ipsum_rc() +
    theme(axis.text.x = element_text(size = 32),
          axis.text.y = element_text(size = 32),
          legend.title = element_blank(),
          legend.position = "inside",
          legend.position.inside = c(0.9,0.5),
          legend.text = element_text(size = 28),
          legend.spacing.y = unit(5.5, 'in'))
  x
}


surge_poly <- st_read(here("data/surge/all_lakes.gpkg"), layer = "all_lakes")  |>
  st_transform(5072)
surge_poly <- mutate(surge_poly,
                     study = case_when(as.numeric(lake_id) >= 1000 ~
                                         "2016 Study",
                                       as.numeric(lake_id) %in%
                                         c(239,253,263,302,308,323,331,999) ~
                                         "2018 Study",
                                       TRUE ~ "SuRGE"))
surge_poly <- filter(surge_poly, as.numeric(lake_id) != 1033)
us <- USAboundaries::us_states() |>
  filter(!state_abbr %in% c("AK", "HI")) |>
  st_transform(5072)
nat_map <- surge_national_map(surge_poly, us)
ggsave(here("products/hollister_et_al_agu_present/figures/nat_map.jpg"), nat_map, width = 24, height = 18, units = "in", dpi = 600)
