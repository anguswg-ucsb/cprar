library(sf)
library(doParallel)
library(parallel)
# library(foreach)
library(dplyr)

library(tidyverse)
library(sf)
library(terra)
library(raster)

source("R/parse_directory.R")
source("R/get_fetch.R")
source("R/utils.R")
source("R/si_functions.R")
source("R/roads.R")
source("R/get_ov.R")
source("R/get_cv.R")
source("R/get_aoc.R")
source("R/extract_rasters.R")

# hsi grid for resampling
# hsi_grid <- terra::rast("data/hsi_grid.tif")
base_folder = "D:/cpra"

# model directories
model_dirs <-
  base_folder %>%
  parse_directory()
# dplyr::left_join(
#   model_years(),
#   by = c("year" = "model_year")
# ) %>%
# dplyr::group_by(model_dir, scenario) %>%
# dplyr::arrange(num_year, .by_group = T) %>%
# dplyr::ungroup()
unique(model_dirs$type)
land_path <-
  model_dirs %>%
  dplyr::filter(type == "lndtyp") %>%
  dplyr::slice(1) %>%
  .$full_path

# hsi grid template
hsi_grid <- readRDS("data/grid_template.rds")

road_buffer_path = "D:/cpra_fixed/road_buffer.gpkg"

final_stk <- make_rasters(
  data_paths       = model_dirs,
  grid             = hsi_grid,
  road_buffer_path = road_buffer_path
)

final_stk$S08_G511_without_diversions_12_12$S08_G511_without_diversions_12_12_cv_deep
# CRS
crs <- CRS('+init=EPSG:26915')

# Template raster to rasterize data onto
r <- raster(
  crs = crs,
  ext = extent(hsi_grid),
  res = c(480, 480)
)


# ********************************************************************************************************
# ********************************************************************************************************

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
