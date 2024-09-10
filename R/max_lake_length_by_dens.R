source("R/packages.R")
source("R/functions.R")

surge_reservoirs <- sf::st_read(here::here("data/surge/all_lakes.gpkg"),
                                layer = "all_lakes") |>
  st_transform(5072)

#max <- round(lakeShorelineLength(lakeSurroundTopo(test_lake))/30,0)
#sequence <- c(seq(10,90, by = 10), seq(100, 900, by = 100), seq(1000, 10000, by=1000))
sequence <- c(10, 50, 100, 200, 300, 400, 500, 750, 1000)#seq(10, 90, by = 10), seq(100,500, by = 100), seq(1000, round(max,-3), 1000))
tic(); test_lake_lengths <- max_lake_length_var(test_lake, sequence, ncore = 4); toc()

test_lake_lengths <- data.frame()
for(i in 1:10){
test_lake <- surge_reservoirs[sample(nrow(surge_reservoirs), 1),]
#num_pts <- lakeShorelineLength(lakeSurroundTopo(test_lake, inElev =
#                                       raster::raster(setValues(rast(vect(test_lake),
#                                                                     res = 500),1))))/50
#sequence <- unique(round(seq(0, num_pts, by = num_pts/10), -1)[-1])
sequence <- c(10,20,40,80,160,320,640,1280,2560)
if(max(sequence) < 100){sequence <- seq(10, 100, by = 10)}
tll <- max_lake_length_var(test_lake, sequence, ncore = 7)
tll2 <- mutate(tll, lake_id = test_lake$lake_id)
test_lake_lengths <- rbind(test_lake_lengths, tll2)
}

ggplot(test_lake_lengths, aes(x = dens, y = max_length)) +
  geom_point() +
  facet_wrap(lake_id ~ ., scales = "free")
