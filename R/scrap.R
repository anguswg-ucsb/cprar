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
source("R/save_out.R")
source("R/extract_rasters.R")
source("R/make_plot.R")

# hsi grid for resampling
# hsi_grid <- terra::rast("data/hsi_grid.tif")
base_folder = "D:/cpra"

# model directories
model_dirs <-
  base_folder %>%
  parse_directory()
# model
# dplyr::left_join(
#   model_years(),
#   by = c("year" = "model_year")
# ) %>%
# dplyr::group_by(model_dir, scenario) %>%
# dplyr::arrange(num_year, .by_group = T) %>%
# dplyr::ungroup()
# unique(model_dirs$type)

# hsi grid template
hsi_grid <- readRDS("data/grid_template.rds")

road_buffer_path = "D:/cpra_fixed/road_buffer.gpkg"

final_stk <- make_rasters(
  data_paths       = model_dirs,
  grid             = hsi_grid,
  road_buffer_path = road_buffer_path
)


# save out final layers
save_out(
  rasters   = final_stk,
  file_path = "D:/cpra_model_runs",
  verbose   = TRUE
)

# make plots for all layers and save out
make_plot(
  stk       = final_stk,
  save_path = "D:/cpra_plots",
  verbose   = TRUE
)

# create metadata files
finalize_metadata(
  srd         = final_stk,
  file_path   = "D:/cpra_metadata",
  base_folder = "D:/cpra",
  template    = "data/metadata_template2.xml",
  verbose     = TRUE
)

# create metadata files
# create_metadata(
#   srd       = final_stk,
#   file_path = "D:/cpra_metadata",
#   verbose   = TRUE
# )

