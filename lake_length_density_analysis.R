source(here::here("R/packages.R"))
source(here("R/functions.R"))

#files <- list.files(here("data"), "tll_", full.names = TRUE)
#tll <- map_df(files, function(x) read_csv(x, col_types =  "ddd"))
#write_csv(tll, "data/total_lake_length_density.csv")

tll <- read_csv(here("data/total_lake_length_density.csv"))
tll |>
  group_by(lake_id) |>
  summarize(max_lake_length = max(max_length, na.rm = TRUE)) |>
  write_csv("data/actual_max_lake_length.csv")
