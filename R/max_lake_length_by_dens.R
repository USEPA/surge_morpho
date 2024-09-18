source("R/packages.R")
source("R/functions.R")

surge_reservoirs <- sf::st_read(here::here("data/surge/all_lakes.gpkg"),
                                layer = "all_lakes") |>
  st_transform(5072)

surge_res_morpho_all <- read_csv("data/surge_res_morpho_all.csv")

#test_lake <- filter(surge_reservoirs, lake_id == "069")
#max <- round(lakeShorelineLength(lakeSurroundTopo(test_lake))/30,0)
#sequence <- c(seq(10,90, by = 10), seq(100, 900, by = 100), seq(1000, 10000, by=1000))
#sequence <- c(10, 50, 100, 200, 300, 400, 500, 750, 1000)#seq(10, 90, by = 10), seq(100,500, by = 100), seq(1000, round(max,-3), 1000))
#tic(); test_lake_lengths <- max_lake_length_var(test_lake, sequence[1:6], ncore = 3); toc()


plan(multisession, workers = 3)
tll2 <-future_lapply(sample(nrow(surge_reservoirs), 10, replace = FALSE),
                     function(x) {
                       test_lake <- surge_reservoirs[x,]
                       sequence <- c(10,20,40,80,100,250,500,750,1000,1500,2000,
                                     2500,3000,3500,4000,4500,5000,5500,6000)
                       tll <- max_lake_length_var(test_lake, sequence[1:9], ncore = 1)
                       mutate(tll, lake_id = test_lake$lake_id)
                       },
                     future.seed = TRUE)
plan(sequential)
test_lake_lengths <- bind_rows(tll2)
ggplot(test_lake_lengths, aes(x = dens, y = max_length)) +
  geom_point() +
  facet_wrap(lake_id ~ ., scales = "free")

surge_res_shore_dev <- surge_res_morpho_all |>
  filter(variables == "shorelinedevelopment") |>
  select(lake_id, shorelinedev = values) |>
  unique()

test_lake_lengths <- left_join(test_lake_lengths, surge_res_shore_dev) |>
  mutate(lake_id = fct_reorder(lake_id, shorelinedev))

write_csv(test_lake_lengths, "data/test_lake_lengths.csv")

