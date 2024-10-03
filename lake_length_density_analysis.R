source(here::here("R/packages.R"))

files <- list.files(here("data"), "tll_", full.names = TRUE)

tll <- map_df(files, function(x) read_csv(x, col_types =  "ddd"))
