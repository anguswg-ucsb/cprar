library(tidyverse)
library(sf)
library(terra)
library(raster)

# read in grid
hsi_grid <- readRDS("data/MP2023_S00_G000_C000_U00_V00_SLA_I_00_00_V_grid480.rds") %>%
  dplyr::rename(GridID = CELLID)


# Template raster to rasterize data onto
r <- terra::rast(
  crs  = terra::crs('+init=EPSG:26915'),
  ext  = terra::ext(c(404709.999904666, 909669.999904667, 3199480, 3374680)),
  res  = c(480, 480),
  vals = 1
)

# Template raster to rasterize data onto
# r <- raster::raster(
#   crs = CRS('+init=EPSG:26915'),
#   ext = extent(c(404709.999904666, 909669.999904667, 3199480, 3374680)),
#   res = c(480, 480),
#   vals = 1
# )

# save out raster
terra::writeRaster(r, "data/hsi_grid.tif", overwrite = T)
