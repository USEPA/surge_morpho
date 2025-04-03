library(terra)


acton_path <- normalizePath("C:\\Users\\JHollist\\Environmental Protection Agency (EPA)\\SuRGE Survey of Reservoir Greenhouse gas Emissions - Documents (1)\\lakeDsn\\2016_survey\\acton\\acton_tin_ras")

acton_bathy <- terra::rast(acton_path)
writeRaster(acton_bathy, "data/bathymetry_raster/acton_ras.tif", overwrite = TRUE)
