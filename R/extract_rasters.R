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

# hsi grid template
hsi_grid <- readRDS("data/grid_template.rds")

# CRS
crs <- CRS('+init=EPSG:26915')

# Template raster to rasterize data onto
r <- raster(
  crs = crs,
  ext = extent(hsi_grid),
  res = c(480, 480)
)
terra::ext(make_grid())

make_grid()



path_df <-
  model_dirs %>%
  dplyr::filter(decade == "decade_1")

  # dplyr::filter(type == "OYSTE", scenario == "07", decade == "decade_1")


hsi_grid <- readRDS("data/grid_template.rds")
paths$full_path
grid <- hsi_grid

#' Rasterize data from MP CSV flat file
#' @param csv_path character path to Masterplan CSV. File ending in OYSTE.csv
#' @param grid SF object representing the MP 480x480 grid. Contains GridID column to match with Masterplan CSV
#' @param verbose logical, whether messages should print or not. Default is TRUE, prints messages.
#'
#' @return
#' @export
rasterize_csv <- function(
    csv_path,
    grid,
    verbose = TRUE
) {

  # extract info from path
  path_details <- path_info(csv_path)

  # Read MP CSV and join year w/ MP grid polygon
  mp <-
    csv_path %>%
    readr::read_csv(show_col_types = FALSE) %>%
    dplyr::left_join(
      grid,
      .,
      by = "GridID"
    )
    # terra::vect()

  # # Template raster to rasterize data onto (terra)
  # r <- terra::rast(
  #   crs = terra::crs(hsi_grid),
  #   ext = terra::ext(hsi_grid),
  #   res = c(480, 480)
  # )

  # Template raster to rasterize data onto (use raster for fasterize::fasterize())
  r <- raster::raster(
    crs = raster::crs(hsi_grid),
    ext = raster::extent(hsi_grid),
    res = c(480, 480)
  )

  # columns to lapply over and rasterize
  layer_names <- grep(
    "GridID|geometry",
    x      = names(mp),
    value  = T,
    invert = T
  )


  # print message
  if(verbose == TRUE) {
    message(paste0("Rasterizing MP CSV...\nPath: ", csv_path))
  }

  # Create names for each raster layer
  rast_names <- lapply(1:length(layer_names), function(x) {
    paste0(tolower(layer_names[[x]]), "_S", path_details$scenario[1],
                    "_", path_details$year[1], "_", path_details$year[1])
  })

  # rasterize each column and make raster stack
  stk <- lapply(1:length(layer_names), function(y) {

    # print message
    if(verbose == TRUE) {
      message(paste0(y, "/", length(layer_names), " - ", layer_names[[y]]))
    }

    fasterize::fasterize(
        sf      = mp,
        raster  = r,
        field   = layer_names[[y]]
        ) %>%
      terra::rast() %>%
      stats::setNames(c(rast_names[[y]]))

      }) %>%
    stats::setNames(c(rast_names)) %>%
    terra::rast()

  return(stk)

}

#' Extract year numbers from standard MP file names
#' @param txt character string following MP_xxxx_xx_yr_yr_xxxx.ext file name conventions
#' @return numeric vector of the year number
#' @export
extract_year <- function(txt) {

  # retrieve model year number from general MP string format
  str_year <- as.numeric(substr(gsub(".*(\\d{2}_\\d{2}).*", "\\1", txt), 1, 2))

  return(str_year)
}


#' Calculates average sediment deposition over a period
#' @description Iterates over multiple years of MP output rasters and calculates the mean of all years given
#' @param stack_lst List of SpatRaster stacks containing multiple years of MP Rasters, must include "sedim" layers
#' @param verbose logical, whether to print messages. Default is TRUE, prints messages
#' @return SpatRaster
#' @export
extract_sedim <- function(
    stack_lst,
    verbose = TRUE
) {

  if(verbose == TRUE) {

    message(paste0("Calculating mean sediment deposition..."))

  }

  # Extract sedimentation rasters for each model run/scenario/year
  sedim_stk <- lapply(1:length(stack_lst), function(i) {

      if(verbose == TRUE) {
        message(paste0("Model version: ", names(stack_lst)[i], " - ", i, "/", length(stack_lst)))
      }

      # stack of model run years
      stk   <- stack_lst[[i]]

      # loop through each year in model run and get sedim layer
      sedim <- lapply(1:length(stk), function(z) {

              if(verbose == TRUE) {
                message(paste0("Year: ", z, "/", length(stk)))
              }

              # Find layer containing "sedim" and extract
              sedim <- stk[[z]][[grep('sedim', names(stk[[z]]), value = T)]]

              }) %>%
        terra::rast()

        # max year of layers
        max_year <- ifelse(
          max(extract_year(names(sedim))) < 10,
          paste0("0", max(extract_year(names(sedim)))),
          paste0(max(extract_year(names(sedim))))
        )

        # max year of layers
        min_year <- ifelse(
          min(extract_year(names(sedim))) < 10,
          paste0("0", min(extract_year(names(sedim)))),
          paste0(min(extract_year(names(sedim))))
        )

        # make range text for naming
        year_range <- paste0(min_year, "_", max_year)

        # compute mean sediment deposition and name layer
        sedim <-
          sedim %>%
          terra::mean() %>%
          stats::setNames(c(paste0(names(stack_lst)[i], "_", year_range, "_sediment_deposition")))

        # convert centimeters to millimeters
        sedim <- sedim*10

      })
  # stats::setNames(c(names(stack_lst))) %>%
  # terra::sds()

  # get layers names
  stk_names <- lapply(1:length(sedim_stk), function(k){
    names(sedim_stk[[k]])
    }) %>%
    unlist()

  # name stack by layer names and make sds
  sedim_stk <-
    sedim_stk %>%
    stats::setNames(c(stk_names)) %>%
    terra::sds()

  return(sedim_stk)

}

#' Depth relationship Function
#' @param x numeric value representing depth
#' @param ...
#' @return Numeric representing a depth bin
#' @export
depth_func <- function(x, ...) {
  ifelse(x > 0 & x < 2, 1,
         ifelse(x >= 2 & x <= 5, 2,
                ifelse(x > 5, 3, NA))
  )
}

extract_depth <- function(
    data_paths,
    verbose = TRUE
) {

  # data_paths <- path_df

  # # data names
  # lst_names <- unique(paste0("S", data_paths$scenario, "_", data_paths$model_dir))
  #
  # # Inundation file paths
  # inun_paths <-
  #   data_paths %>%
  #   dplyr::filter(type == "inun") %>%
  #   dplyr::group_by(model_dir, scenario) %>%
  #   dplyr::arrange(year, .by_group = T) %>%
  #   dplyr::mutate(
  #     file_name = dplyr::case_when(
  #       year < 10 ~ paste0("S", scenario, "_", model_dir, "_0", year, "_0", year),
  #       TRUE      ~ paste0("S", scenario, "_", model_dir, "_", year, "_", year)
  #     )
  #   ) %>%
  #   dplyr::group_split()

  # grid to resample rasters to
  resample_grid <- make_grid()

  if(verbose == TRUE) {
    message(paste0("Processing depth inundation..."))
  }

  depth_stk <- lapply(1:length(data_paths), function(y) {

    if(verbose == TRUE) {
      message(paste0(data_paths[[y]]$file_name, " - ", y, "/", length(data_paths)))
    }

    # path to Inun TIF
    depth_path <- data_paths[[y]]$full_path

    # read in depth (inun) raster and set CRS to match grid
    depth <-
      depth_path  %>%
      terra::rast() %>%
      terra::set.crs(terra::crs(resample_grid))

    # convert meters to feet
    depth <- depth*3.281

    # resample 30x30 depth inundation raster to 480x480 grid and make depth bins
    depth <-
      depth %>%
      terra::resample(resample_grid) %>%
      terra::app(fun = depth_func) %>%
      stats::setNames(c(paste0(data_paths[[y]]$file_name, "_water_depth")))


  })

  # get layers names
  stk_names <- lapply(1:length(depth_stk), function(k){
    names(depth_stk[[k]])
  }) %>%
    unlist()

  # name stack by layer names and make sds
  depth_stk <-
    depth_stk %>%
    stats::setNames(c(stk_names)) %>%
    terra::sds()

  return(depth_stk)
}

extract_fetch <- function(
    data_paths,
    lst_names,
    verbose = TRUE
) {

  # # data names
  # lst_names <- unique(paste0("S", data_paths$scenario, "_", data_paths$model_dir))
  #
  # # Inundation file paths
  # landtype_paths <-
  #   data_paths %>%
  #   dplyr::filter(type == "lndtyp") %>%
  #   dplyr::group_by(model_dir, scenario) %>%
  #   dplyr::arrange(year, .by_group = T) %>%
  #   dplyr::mutate(
  #     file_name = dplyr::case_when(
  #       year < 10 ~ paste0("S", scenario, "_", model_dir, "_0", year, "_0", year),
  #       TRUE      ~ paste0("S", scenario, "_", model_dir, "_", year, "_", year)
  #     )
  #   ) %>%
  #   dplyr::group_split()

  if(verbose == TRUE) {
    message(paste0("Processing fetch..."))
  }

  # calculate fetch
  fetch_stk <-  lapply(1:length(data_paths), function(y) {

    if(verbose == TRUE) {
      message(paste0(data_paths[[y]]$file_name, " - ", y, "/", length(data_paths)))
    }

    # path to Inun TIF
    fetch_path <- data_paths[[y]]$full_path

    # calculate fetch from landtype raster
    fetch <- get_fetch(
      r_path    = fetch_path,
      max_dist  = 20000,
      verbose   = TRUE
    ) %>%
      stats::setNames(c(paste0(data_paths[[y]]$file_name, "_fetch")))

  })

  # get layers names
  stk_names <- lapply(1:length(fetch_stk), function(k){
    names(fetch_stk[[k]])
  }) %>%
    unlist()

  # name stack by layer names and make sds
  fetch_stk <-
    fetch_stk %>%
    stats::setNames(c(stk_names)) %>%
    terra::sds()

  return(fetch_stk)
}

#' Calculate Oyster viability SI (cool)
#'
#' @param x numeric value indicating min temp cold months (smin_c) to calculate SI values
#' @param ...
#' @return numeric SI values
#' @export
#' @examples
si_cool_func <- function(x, ...) {
  ifelse(x <= 1, 0,
         ifelse(x > 1 & x < 8, (0.1429*x) - 0.1429,
                ifelse(x >= 8 & x < 10, 1,
                       ifelse(x >= 10 & x < 15, ((-0.16)*x) + 2.6,
                              ifelse(x >= 15 & x < 20, ((-0.0398)*x) + 0.797,
                                     ifelse(x >= 20, 0.001, 0)
                              ))))
  )
}

#' Calculate Oyster viability SI (warm)
#'
#' @param x numeric value indicating min temp warm months (smin_w) to calculate SI values
#' @param ...
#' @return numeric SI values
#' @export
si_warm_func <- function(x, ...) {
  ifelse(x <= 2, 0,
         ifelse(x > 2 & x < 8, (0.1667*x) - 0.3333,
                ifelse(x >= 8 & x < 10, 1,
                       ifelse(x >= 10 & x < 15, ((-0.16)*x) + 2.6,
                              ifelse(x >= 15 & x < 20, ((-0.0398)*x) + 0.797,
                                     ifelse(x >= 20, 0.001, 0)
                              ))))
  )
}

#' Calculate Oyster viability SI (average)
#' @param x numeric value indicating mean temp (s_mean) to calculate SI values
#' @param ...
#' @return numeric SI values
#' @export
si_av_func <- function(x, ...) {
  ifelse(x < 5, 0,
         ifelse(x >= 5 & x < 10, (0.2*x) - 1.0,
                ifelse(x >= 10 & x <= 25, 1,
                       ifelse(x > 25 & x <= 30, ((-0.16)*x) + 5,
                              ifelse(x > 30 & x <= 36, ((-0.0332)*x) + 1.195,
                                     ifelse(x > 36, 0.001, 0)
                              ))))
  )
}

#' Calculate Oyster Viability SI from multiple model runs/scenarios over a period
#' @description Iterates over multiple years of MP output rasters and calculates Oyster viability SI values and returns SpatRasters
#' @param stack_lst List of SpatRaster stacks containing multiple years of MP Rasters, must include "smin_c", "smin_w", and "s_mean" layers
#' @param verbose logical, whether to print messages. Default is TRUE, prints messages
#' @return SpatRaster of OV SI values
#' @export
extract_ov <- function(
    stack_lst,
    verbose = TRUE
    ) {

  # iterate over all models and calculate Oyster viability SI
  ov_stk <- lapply(1:length(stack_lst), function(i) {

    if(verbose == TRUE) {
      message(paste0("Processing ", names(stack_lst)[i], " - ", i, "/", length(stack_lst)))

    }
    # model to iterate over
    st <- stack_lst[[i]]

    # iterate over model run years
    ov <- lapply(1:length(st), function(z) {

      if(verbose == TRUE) {
        message(paste0("Year: ", z, "/", length(st)))
      }

      # process Oyster viability SI
      ov_si <- get_ov(
        coolr         = st[z][grep('smin_c', names(st[z]), value = T)],
        warmr         = st[z][grep('smin_w', names(st[z]), value = T)],
        avgr          = st[z][grep('s_mean', names(st[z]), value = T)],
        model_version = names(st)[z]
      )


    }) %>%
      terra::sds() %>%
      stats::setNames(c(paste0(names(st), "_ov")))

  })

  return(ov_stk)

}

# make oyster viability layers from MP raster layers
get_ov <- function(
    coolr,
    warmr,
    avgr,
    model_version
    ) {

  #   coolr <- st$S07_G510_new_FWOA_03_03$smin_c_S07_03_03
  #   warmr <- st$S07_G510_new_FWOA_03_03$smin_w_S07_03_03
  #   avgr  <- st$S07_G510_new_FWOA_03_03$s_mean_S07_03_03

  # Apply SI equations

  # SI cool
  si_cool <-
    coolr %>%
    terra::app(fun = si_cool_func) %>%
    stats::setNames(c(paste0(model_version, "_si_cool")))

  # SI warm
  si_warm <-
    warmr %>%
    terra::app(fun = si_warm_func) %>%
    stats::setNames(c(paste0(model_version, "_si_warm")))

  # SI average
  si_avg  <-
    avgr %>%
    terra::app(fun  = si_av_func) %>%
    stats::setNames(c(paste0(model_version, "_si_avg")))

  # SI MS
  si_ms     <- (si_cool * si_warm)**(0.5)
  names(si_ms) <-  c(paste0(model_version, "_si_ms"))

  # SI OV
  si_ov     <- (si_ms  * si_avg)**(0.5)
  names(si_ov) <-  c(paste0(model_version, "_si_ov"))

  # return raster stack
  ov <- c(si_cool, si_warm, si_avg, si_ms, si_ov)

  return(ov)

  }
# make oyster viability layers from MP raster layers
# get_ov <- function(mp_data, land = NULL, mask = FALSE) {
#
#   # resample to raster
#   resamp_r <- raster(
#     nrows = 452,
#     ncols = 1051,
#     crs = CRS('+init=EPSG:26915'),
#     ext = extent(405220, 909700, 3199570, 3416530)
#   )
#
#   # resample data to ensure they are on the same grid
#   mp_data   <- resample(mp_data, resamp_r)
#   # water     <- resample(water, resamp_r)
#
#   # Apply SI equation
#   si_cool <- raster::calc(mp_data$smin_c,  si_cool_func)
#   si_warm <- raster::calc(mp_data$smin_w,  si_warm_func)
#   si_avg  <- raster::calc(mp_data$s_mean,  si_av_func)
#
#   # SI MS
#   si_ms     <- (si_cool * si_warm)**(0.5)
#   # si_ms     <- (si_sal_stk$si_sal_cool * si_sal_stk$si_sal_warm)**(0.5)
#
#   # SI OV
#   si_ov    <- (si_ms  * si_avg)**(0.5)
#   # si_ov    <- (si_ms  * si_sal_stk$si_sal_avg)**(0.5)
#
#   if(mask == TRUE) {
#     # SI sal stack w/ layernames + mask
#     si_sal_stk <- stack(si_cool, si_warm, si_avg, si_ms, si_ov) %>%
#       setNames(c("si_sal_cool", "si_sal_warm", "si_sal_avg", "si_ms", "si_ov")) %>%
#       mask(land, inverse = T)
#   } else{
#
#     # SI sal stack w/ layernames
#     si_sal_stk <- stack(si_cool, si_warm, si_avg, si_ms, si_ov) %>%
#       setNames(c("si_sal_cool", "si_sal_warm", "si_sal_avg", "si_ms", "si_ov"))
#
#   }
#
#   return(si_sal_stk)
#   rm(si_ms, si_cool, si_avg, si_warm, si_warm_func, si_cool_func, si_av_func, resamp_r)
# }

#' Rasterize all Masterplan Oyster CSVs in a directory.
#' @param data_paths dataframe containing paths to Masterplan Oyster Flatfiles/CSVs. Dataframe can be created from parse_files() function
#' @param grid SF object representing the MP 480x480 grid. Contains GridID column to match with Masterplan CSV
#' @return
#' @export
make_rasters <- function(
    data_paths,
    grid
    ) {

  # data_paths <- model_dirs
  # grid <- hsi_grid
  # data names
  # lst_names <- unique(paste0("S", data_paths$scenario, "_", data_paths$model_dir))
  #
  # # data names
  # lst_names <- unique(paste0("S", data_paths$scenario, "_", data_paths$model_dir))
  #
  # # Inundation file paths
  # landtype_paths <-
  #   data_paths %>%
  #   dplyr::filter(type == "lndtyp") %>%
  #   dplyr::group_by(model_dir, scenario) %>%
  #   dplyr::arrange(year, .by_group = T) %>%
  #   dplyr::mutate(
  #     file_name = dplyr::case_when(
  #       year < 10 ~ paste0("S", scenario, "_", model_dir, "_0", year, "_0", year),
  #       TRUE      ~ paste0("S", scenario, "_", model_dir, "_", year, "_", year)
  #     )
  #   ) %>%
  #   dplyr::group_split()

  # data names
  lst_names <- unique(data_paths$model_range)

  # data names
  # lst_names <- unique(paste0("S", data_paths$scenario, "_", data_paths$model_dir))

  # seperate paths by data type
  path_type <-
    data_paths %>%
    dplyr::group_by(type) %>%
    dplyr::arrange(year, .by_group = T) %>%
    dplyr::mutate(
      file_name = dplyr::case_when(
        year < 10 ~ paste0("S", scenario, "_", model_dir, "_0", year, "_0", year),
        TRUE      ~ paste0("S", scenario, "_", model_dir, "_", year, "_", year)
      )
    )

  # split paths into list by type
  type_lst <-
    path_type %>%
    dplyr::group_split() %>%
    stats::setNames(c(dplyr::group_keys(path_type)$type))

  # Inundation file paths
  # path_df <-
  #   data_paths %>%
  #   dplyr::group_by(model_dir, scenario, decade, type) %>%
  #   dplyr::arrange(year, .by_group = T) %>%
  #   dplyr::mutate(
  #     file_name = dplyr::case_when(
  #       year < 10 ~ paste0("S", scenario, "_", model_dir, "_0", year, "_0", year),
  #       TRUE      ~ paste0("S", scenario, "_", model_dir, "_", year, "_", year)
  #     )
  #   )
  #
  # path_lst <-
  #   path_df %>%
  #   dplyr::group_split() %>%
  #   stats::setNames(c(dplyr::mutate(
  #                       dplyr::group_keys(path_df),
  #                       file_name = paste0("S", scenario, "_", model_dir)
  #                       )$file_name)
  #                   )
  #
  #
  # # lst_names <-
  # #   data_paths %>%
  # #   dplyr::group_by(model_dir, scenario) %>%
  # #   dplyr::slice(1)
  #
  # # Inundation file paths
  # inun_paths <-
  #   data_paths %>%
  #   dplyr::filter(type == "inun") %>%
  #   dplyr::group_by(model_dir, scenario) %>%
  #   dplyr::arrange(year, .by_group = T) %>%
  #   dplyr::mutate(
  #     file_name = dplyr::case_when(
  #       year < 10 ~ paste0("S", scenario, "_", model_dir, "_0", year, "_0", year),
  #       TRUE      ~ paste0("S", scenario, "_", model_dir, "_", year, "_", year)
  #       )
  #     ) %>%
  #   dplyr::group_split()
  #
  # # OYSTE file paths
  # oyst_paths <-
  #   data_paths %>%
  #   dplyr::filter(type == "OYSTE") %>%
  #   dplyr::group_by(model_dir, scenario) %>%
  #   dplyr::arrange(year, .by_group = T) %>%
  #   dplyr::mutate(
  #     file_name = dplyr::case_when(
  #       year < 10 ~ paste0("S", scenario, "_", model_dir, "_0", year, "_0", year),
  #       TRUE      ~ paste0("S", scenario, "_", model_dir, "_", year, "_", year)
  #       )
  #   ) %>%
  #   dplyr::slice(1:2) %>%
  #   dplyr::group_split()

  # Masterplan CSV files
  mp_paths <-
    type_lst[["OYSTE"]] %>%
    dplyr::group_by(model_dir, scenario, range) %>%
    dplyr::arrange(year, .by_group = T) %>%
    dplyr::slice(1:2) %>%
    dplyr::group_split()

  # Depth inundation files
  depth_paths <-
    type_lst[["inun"]] %>%
    dplyr::group_by(model_dir, scenario, range) %>%
    dplyr::arrange(year, .by_group = T) %>%
    dplyr::group_split()

  # Land Type TIF files
  land_paths <-
    type_lst[["lndtyp"]] %>%
    dplyr::group_by(model_dir, scenario, range) %>%
    dplyr::arrange(year, .by_group = T) %>%
    dplyr::group_split()

  # paths <- data_paths$full_path[1:2]
  # i <- 1
  # z <- 1
  # system.time(
  # loop through paths and rasterize each column variable in each MP CSV
  model_stk <- lapply(1:length(mp_paths), function(i) {

    paths <- mp_paths[[i]]$full_path
    message(paste0("Model: ", i, "/", length(mp_paths)))

    # go through each unique model run and extract raster layers for all variables and all columns of CSV
    mp_rast <- lapply(1:length(paths), function(z) {

      rasterize_csv(
        csv_path = paths[z],
        grid     = grid,
        verbose  = TRUE
        )
      }) %>%
      stats::setNames(c(mp_paths[[i]]$file_name)) %>%
      terra::sds()

    }) %>%
    stats::setNames(c(lst_names))

# )

  message(paste0("Extracting sedimentation layers..."))

  # Calculate Mean sediment deposition
  # calculate mean sediment deposition per model version/decade/scenario
  sedim_stk <- extract_sedim(
                      stack_lst = model_stk,
                      verbose   = TRUE
                      )

  # Calculate Oyster Viability
  # iterate over all models and calculate Oyster viability SI
  ov_stk <- extract_ov(
    stack_lst = model_stk,
    verbose   = TRUE
  )
 # ov_stk[[1]]$S07_G510_new_FWOA_03_03_ov %>% plot()

  # extract depth rasters and process into bins
  depth_stk <- extract_depth(
    data_paths = depth_paths,
    verbose    = TRUE
  )

  # extract depth rasters and process into bins
  fetch_stk <- extract_fetch(
    data_paths = land_paths,
    verbose    = TRUE
  )
  # plot(fetch_stk$S07_G511_without_diversions_32_32_fetch)
  # Calculate Oyster Viability


  # r1 <- rast(matrix(sample(1:4), nrow = 2, ncol = 2), crs = terra::crs(sedim_stk$S07_G510_new_FWOA$sedim_S07_03_03))
  # r2 <- rast(matrix(sample(1:4), nrow = 2, ncol = 2), crs = terra::crs(sedim_stk$S07_G510_new_FWOA$sedim_S07_03_03))
  # r3 <- rast(matrix(sample(50:100, size =4), nrow = 2, ncol = 2), crs = terra::crs(sedim_stk$S07_G510_new_FWOA$sedim_S07_03_03))
  # plot(r1)
  # plot(r2)
  # plot(r3)
  # mean_tmp <- mean(r1, r2, r3)
  # sttt <- c(r1, r2, r3, mean_tmp)
  # sttt
  # plot(sttt)
  # mean_tmp <- mean(r1, r2, r3)
  # m <- matrix(1:25, nrow=5, ncol=5)
  # rm <- rast(m)
  # plot(mean_tmp)
  # sedim_stk[[1]][[1]]$sedim_S07_03_03
  # tmp  <- c(model_stk)
  # names(tmp)
  # object.size(tmp)

  mp_rast[[grep('sedim', names(mp_rast), value = T)]]

  names(mp_rast)
  lapply(1:length(mp_rast), function(i) {


    # sedim <-


  })
tmp <- mp_rast[[1]]

object.size(  (mp_rast))
  mp_rast[[3]][[c("sedim_S07_05_05")]]
  # file_name <- gsub(".csv", ".rds", mp_csv_files[i])

  logger::log_info("Reading MP CSV - mp_csv_files[i]")

  # Read MP CSV
  mp <- readr::read_csv(
    file      = paths$full_path[1]
  )

  mp_grid     <- dplyr::left_join(                               # Join year w/ MP grid polygon
    hsi_grid,
    mp,
    by = "GridID"
  ) %>%
    terra::vect()

  # Template raster to rasterize data onto
  r <- terra::rast(
    crs = terra::crs(hsi_grid),
    ext = terra::ext(hsi_grid),
    res = c(480, 480)
  )

  # columns to lapply over and rasterize
  layer_names <- grep(
    "GridID",
    x      = names(mp_grid),
    value  = T,
    invert = T
    )


  message(paste0("Rasterizing MP CSV... "))

  # rasterize each column
  mp_rast <- lapply(1:length(layer_names), function(y) {

    message(paste0(y, "/", length(layer_names), " - ", layer_names[[y]]))

    terra::rasterize(
      x = mp_grid,
      y = r,
      layer_names[[y]]
    )

  })

  mp_rast
  mp_rast <- terra::rasterize(
    x = mp_grid,
    y = r,
    "pct_land"
    )
  plot(mp_rast)

}


# SI 2 function
si2_func <- function(x, ...) {
  ifelse(x < 5, 0,
         ifelse(x >= 5 & x < 10, (0.06*x) - 0.3,
                ifelse(x >= 10 & x < 15, (0.07*x) - 0.4,
                       ifelse(x >= 15 & x < 18, ((0.1167)*x) -1.1,
                              ifelse(x >= 18 & x < 22, 1.0,
                                     ifelse(x >= 22 & x < 30, ((-0.0875)*x) + 2.925,
                                            ifelse(x >= 30 & x < 35, ((-0.04)*x) + 1.5,
                                                   ifelse(x >= 35 & x < 40, ((-0.02)*x) + 0.8, 0)
                                            ))))))
  )

}

# SI 3 cool function
si3_cool_func <- function(x, ...) {
  ifelse(x <= 1, 0,
         ifelse(x > 1 & x < 8, (0.1429*x) - 0.1429,
                ifelse(x >= 8 & x < 10, 1,
                       ifelse(x >= 10 & x < 15, ((-0.16)*x) + 2.6,
                              ifelse(x >= 15 & x < 20, ((-0.0398)*x) + 0.797,
                                     ifelse(x >= 20, 0.001, 0)
                              ))))
  )
}

# SI 3 warm function
si3_warm_func <- function(x, ...) {
  ifelse(x <= 2, 0,
         ifelse(x > 2 & x < 8, (0.1667*x) - 0.3333,
                ifelse(x >= 8 & x < 10, 1,
                       ifelse(x >= 10 & x < 15, ((-0.16)*x) + 2.6,
                              ifelse(x >= 15 & x < 20, ((-0.0398)*x) + 0.797,
                                     ifelse(x >= 20, 0.001, 0)
                              ))))
  )
}

# SI 4 average function
si4_avg_func <- function(x, ...) {
  ifelse(x < 5, 0,
         ifelse(x >= 5 & x < 10, (0.2*x) - 1.0,
                ifelse(x >= 10 & x < 15, 1,
                       ifelse(x >= 15 & x < 20, ((-0.16)*x) + 3.4,
                              ifelse(x >= 20 & x < 25, ((-0.04)*x) + 0.9996,
                                     ifelse(x >= 25, 0.001, 0)
                              ))))
  )
}

# SI 5 function % of cell covered by land
#' Calculate percent of cell covered by land SI values (SI 5)
#' @param x numeric value representing percent of land (pct_land)
#' @param ...
#' @return numeric SI 5 values
#' @export
si5_func <- function(x, ...) {
  -x + 1.0
}

# SI 6 Sediment Deposition function
si6_sed_dep_func <- function(x, ...) {
  ifelse(x < 35, 1,
         ifelse(x >= 35 & x < 40, (-0.2*x) + 8.0,
                ifelse(x >= 40, 0, 0)
         )
  )
}

#' Depth relationship Function
#' @param x numeric value representing depth
#' @param ...
#' @return Numeric representing a depth bin
#' @export
depth_func <- function(x, ...) {
  ifelse(x > 0 & x < 2, 1,
         ifelse(x >= 2 & x <= 5, 2,
                ifelse(x > 5, 3, NA))
  )
}

extract_depth <- function(
    data_paths,
    verbose = TRUE
    ) {

  # data_paths <- path_df

  # # data names
  # lst_names <- unique(paste0("S", data_paths$scenario, "_", data_paths$model_dir))
  #
  # # Inundation file paths
  # inun_paths <-
  #   data_paths %>%
  #   dplyr::filter(type == "inun") %>%
  #   dplyr::group_by(model_dir, scenario) %>%
  #   dplyr::arrange(year, .by_group = T) %>%
  #   dplyr::mutate(
  #     file_name = dplyr::case_when(
  #       year < 10 ~ paste0("S", scenario, "_", model_dir, "_0", year, "_0", year),
  #       TRUE      ~ paste0("S", scenario, "_", model_dir, "_", year, "_", year)
  #     )
  #   ) %>%
  #   dplyr::group_split()

  # grid to resample rasters to
  resample_grid <- make_grid()

  if(verbose == TRUE) {
    message(paste0("Processing depth inundation..."))
  }

  depth_stk <- lapply(1:length(data_paths), function(y) {

    if(verbose == TRUE) {
      message(paste0(data_paths[[y]]$file_name, " - ", y, "/", length(data_paths)))
    }

    # path to Inun TIF
    depth_path <- data_paths[[y]]$full_path

    # read in depth (inun) raster and set CRS to match grid
    depth <-
      depth_path  %>%
      terra::rast() %>%
      terra::set.crs(terra::crs(resample_grid)) %>%
      terra::resample(resample_grid) %>%                    # resample 30x30 depth inundation raster to match grid
      terra::app(fun = depth_func) %>%
      stats::setNames(c(paste0(data_paths[[y]]$file_name, "_water_depth")))

  })

  # get layers names
  stk_names <- lapply(1:length(depth_stk), function(k){
    names(depth_stk[[k]])
  }) %>%
    unlist()

  # name stack by layer names and make sds
  depth_stk <-
    depth_stk %>%
    stats::setNames(c(stk_names)) %>%
    terra::sds()

  return(depth_stk)
}

extract_fetch <- function(
    data_paths,
    # lst_names,
    verbose = TRUE
    ) {

  # # data names
  # lst_names <- unique(paste0("S", data_paths$scenario, "_", data_paths$model_dir))
  #
  # # Inundation file paths
  # landtype_paths <-
  #   data_paths %>%
  #   dplyr::filter(type == "lndtyp") %>%
  #   dplyr::group_by(model_dir, scenario) %>%
  #   dplyr::arrange(year, .by_group = T) %>%
  #   dplyr::mutate(
  #     file_name = dplyr::case_when(
  #       year < 10 ~ paste0("S", scenario, "_", model_dir, "_0", year, "_0", year),
  #       TRUE      ~ paste0("S", scenario, "_", model_dir, "_", year, "_", year)
  #     )
  #   ) %>%
  #   dplyr::group_split()

  if(verbose == TRUE) {
    message(paste0("Processing fetch..."))
  }

  # calculate fetch
  fetch_stk <-  lapply(1:length(data_paths), function(y) {

    if(verbose == TRUE) {
      message(paste0(data_paths[[y]]$file_name, " - ", y, "/", length(data_paths)))
    }

    # path to Inun TIF
    fetch_path <- data_paths[[y]]$full_path

    # calculate fetch from landtype raster
    fetch <- get_fetch(
      r_path    = fetch_path,
      max_dist  = 20000,
      ncores    = 12,
      verbose   = TRUE
      ) %>%
      stats::setNames(c(paste0(data_paths[[y]]$file_name, "_fetch")))

  })

  # get layers names
  stk_names <- lapply(1:length(fetch_stk), function(k){
    names(fetch_stk[[k]])
  }) %>%
    unlist()

  # name stack by layer names and make sds
  fetch_stk <-
    fetch_stk %>%
    stats::setNames(c(stk_names)) %>%
    terra::sds()

  return(fetch_stk)
}
# Calculate tradition cultch model HSI
# inputs:
# 1. Masterplan raster data w/ naming conventions
# 2. Sediment deposition 10 year mean
get_trad_cultch <- function(mp
                            # sed_dep
) {

  # ---- SI 2 ----
  # Mean salinity during the spawning season, April through November
  si2      <- raster::calc(mp$s_spwn, si2_func)

  # ---- SI 3 ----
  #  Minimum monthly mean salinity cool/warm months
  si3_cool <- raster::calc(mp$smin_c, si3_cool_func)
  si3_warm <- raster::calc(mp$smin_w, si3_warm_func)

  # SI3 = (SI3 cool x SI3 warm)^1/2
  si3      <- (si3_cool * si3_warm)**(0.5)

  # ---- SI 4 ----
  # Mean annual salinity

  si4      <- raster::calc(mp$s_mean, si4_avg_func)

  # ---- SI 5 ----
  # % cell covered by land

  si5      <- raster::calc(mp$pct_land, si5_func)

  # ---- SI 6 ----
  # Cumulative Sediment Deposition
  # sed_dep <- mp$sedim*10
  si6      <- raster::calc(mp$sedim*10, si6_sed_dep_func)

  # si6      <- raster::calc(sed_dep, si6_sed_dep_func)

  # ---- HSI ----
  # HSI = (SI2 x SI3 x SI4 x SI5 x SI6)^1/5

  hsi    <- (si2 * si3 * si4 * si5 * si6)**(1/5)

  hsi_st <- raster::stack(si2, si3, si4, si5, si6, hsi) %>%
    setNames(c("si2", "si3", "si4", "si5", "si6", "hsi")) %>%
    terra::rast()

  return(hsi_st)
}

model_dirs$path[1]
library(stringr)
str_match(model_dirs$path[1], "\\w{36}\\d" )
str_extract(model_dirs$path[1], "([^_]+$)")
gsub(".*(\\d{2}_\\d{2}).*", "\\1", model_dirs$path[1])

# landtype files
file_paths <- parse_files(dir = model_dirs$path[1])

# extract_m
