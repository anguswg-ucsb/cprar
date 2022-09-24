library(sf)
library(doParallel)
library(parallel)
library(foreach)
library(dplyr)

library(tidyverse)
library(sf)
library(terra)
library(raster)
library(fasterize)

source("R/parse_directory.R")
source("R/get_fetch.R")

# hsi grid for resampling
# hsi_grid <- terra::rast("data/hsi_grid.tif")
base_folder = "D:/cpra"

# model directories
model_dirs     <- parse_directory(base_folder = base_folder)

# landtype files
file_paths <- parse_files(dir = model_dirs$path[1])

# landtype files
landtype_files <-
  model_dirs %>%
  dplyr::filter(type == "lndtyp")
r_path = landtype_files$full_path[1]
# old fetch file
# tmpf  <- raster::raster("C:/Users/angus/OneDrive/Desktop/lynker/CPRA/data/fetch/raster/fetch_mean_dist_480m_resampled.tif")
#
# oldfetch <- raster::raster("C:/Users/angus/OneDrive/Desktop/github/cpra_orz/data/fetch/fetch_raster_S07_03_03.tif")
# plot(rast)
# plot(fetch_crop)


system.time(
  fetchr <- get_fetch(
    r_path   = landtype_files$full_path[1],
    max_dist = 20000,
    verbose  = TRUE
  )
)

lw     <- get_landwater(r_path = r_path)
water  <- get_water(lw)

plot(lw)
plot(water)
plot(fetchr)
fetch_crop <-
  fetchr %>%
  terra::mask(water, inverse = F)
plot(fetch_crop)

fetch_raw <- raster::raster(fetchr)
fetch_raster <- raster::raster(fetch_crop)
mapview::mapview(fetch_raster)  + fetch_raw


# Reclassify fetch values
fetch_mat_shallow <- matrix(
  c(0, 1000, 1,
    1000, 5000, .5,
    5000, 20001, .2),
  ncol=3,
  byrow = T
)


# Reclassify fetch values for deep water
fetch_mat_deep <- matrix(
  c(0,     5000,   1,          # deep water fetch bins
    5000,  10000, .5,
    10000, 20001, .2),
  ncol=3, byrow = T
)

terra::classify(fetchr, fetch_mat_shallow)
# calculate CV SI
# Fetch shallow/deep SI
fetch_shallow_cv            <- terra::classify(fetchr, fetch_mat_shallow)
fetch_deep_cv               <- terra::classify(fetchr, fetch_mat_deep)
plot(fetch_shallow_cv)
plot(fetch_deep_cv)


# *******************************************************************************************
# *******************************************************************************************
