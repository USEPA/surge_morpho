source("R/packages.R")
source("R/functions.R")

surge_reservoirs <- sf::st_read(here::here("data/surge/all_lakes.gpkg"),
                                layer = "all_lakes") |>
  st_transform(5072)

#max <- round(lakeShorelineLength(lakeSurroundTopo(test_lake))/30,0)
#sequence <- c(seq(10,90, by = 10), seq(100, 900, by = 100), seq(1000, 10000, by=1000))
#sequence <- c(10, 50, 100, 200, 300, 400, 500, 750, 1000)#seq(10, 90, by = 10), seq(100,500, by = 100), seq(1000, round(max,-3), 1000))
#tic(); test_lake_lengths <- max_lake_length_var(test_lake, sequence, ncore = 4); toc()


plan(multisession, workers = 3)
tll2 <-future_lapply(sample(nrow(surge_reservoirs), 30, replace = FALSE),
                     function(x) {
                       test_lake <- surge_reservoirs[x,]
                       sequence <- c(10,20,40,80,100,250,500,750,1000,1500,2000, 2500)
                       tll <- max_lake_length_var(test_lake, sequence, ncore = 1)
                       mutate(tll, lake_id = test_lake$lake_id)
                       },
                     future.seed = TRUE)
plan(sequential)
test_lake_lengths <- bind_rows(tll2)
ggplot(test_lake_lengths, aes(x = dens, y = max_length)) +
  geom_point() +
  facet_wrap(lake_id ~ ., scales = "free")
