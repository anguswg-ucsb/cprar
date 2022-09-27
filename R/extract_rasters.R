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
    crs = raster::crs(grid),
    ext = raster::extent(grid),
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
          stats::setNames(c(paste0(names(stack_lst)[i], "_sediment_deposition")))
          # stats::setNames(c(paste0(names(stack_lst)[i], "_", year_range, "_sediment_deposition")))

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
    verbose = TRUE
) {

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

#' Package up all layers into single SpatRasterDataset
#' @param depth SpatRasterDataset Water depth
#' @param cv SpatRasterDataset Commercial Viability
#' @param ov SpatRasterDataset Oyster Viability
#' @param aoc SpatRasterDataset AOC
#' @param aoc_mean SpatRasterDataset AOC mean
#' @param aoc_sd SpatRasterDataset AOC Standard Deviation
#' @param verbose logical, whether messages should print or not. Default is TRUE, print messages.
#' @return SpatRasterDataset with all SpatRasters for each model year
#' @export
finalize_stacks <- function(
    depth,
    cv,
    ov,
    aoc,
    aoc_mean,
    aoc_sd,
    verbose = TRUE
    ) {

  if(verbose == TRUE) {
    message(paste0("Preparing final dataset..."))
  }

  final_stk <- lapply(1:length(aoc), function(i) {

    if(verbose == TRUE) {
      message(paste0(i, "/", length(aoc)))
    }

    finalr <- c(depth[[i]], cv[[i]], ov[[i]], aoc[[i]], aoc_mean[[i]], aoc_sd[[i]])

  }) %>%
    terra::sds() %>%
    stats::setNames(c(gsub("_cv", "", names(cv))))

  return(final_stk)

}
#' Rasterize all Masterplan Oyster CSVs in a directory.
#' @param data_paths dataframe containing paths to Masterplan Oyster Flatfiles/CSVs. Dataframe can be created from parse_files() function
#' @param grid SF object representing the MP 480x480 grid. Contains GridID column to match with Masterplan CSV
#' @return
#' @export
make_rasters <- function(
    data_paths,
    grid,
    road_buffer_path
    ) {

  # road_buffer_path <- "D:/cpra_fixed/road_buffer.gpkg"
  # data_paths <- model_dirs
  # grid <- hsi_grid

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
  #     )) %>%
  #   dplyr::group_split()

  # data names
  lst_names <- unique(data_paths$model_range)
  # lst_names <- unique(data_paths$model_range)[1]
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
  #       TRUE      ~ paste0("S", scenario, "_", model_dir, "_", year, "_", year)))
  # path_lst <-
  #   path_df %>%
  #   dplyr::group_split() %>%
  #   stats::setNames(c(dplyr::mutate(
  #                       dplyr::group_keys(path_df),
  #                       file_name = paste0("S", scenario, "_", model_dir)
  #                       )$file_name))

  # Masterplan CSV files
  mp_paths <-
    type_lst[["OYSTE"]] %>%
    dplyr::group_by(model_dir, scenario, range) %>%
    dplyr::arrange(year, .by_group = T) %>%
    # dplyr::ungroup() %>%
    # dplyr::slice(1) %>%
    dplyr::group_split()

  # Depth inundation files
  depth_paths <-
    type_lst[["inun"]] %>%
    dplyr::group_by(model_dir, scenario, range) %>%
    dplyr::arrange(year, .by_group = T) %>%
    # dplyr::ungroup() %>%
    # dplyr::slice(1) %>%
    dplyr::group_split()

  # Land Type TIF files
  land_paths <-
    type_lst[["lndtyp"]] %>%
    dplyr::group_by(model_dir, scenario, range) %>%
    dplyr::arrange(year, .by_group = T) %>%
    # dplyr::ungroup() %>%
    # dplyr::slice(1) %>%
    dplyr::group_split()

  # extract depth rasters and process into bins
  fetch_stk <- extract_fetch(
    data_paths = land_paths,
    verbose    = TRUE
  )

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

  # Calculate Oyster Viability SI - iterate over all models
  ov_stk <- extract_ov(
    stack_lst = model_stk,
    verbose   = TRUE
  )

  # extract depth rasters and process into bins
  depth_stk <- extract_depth(
    data_paths = depth_paths,
    verbose    = TRUE
  )

  # calculate fetch SI values
  fetch_si <- calc_fetch_si(
    stack_lst = fetch_stk,
    verbose   = TRUE
  )

  message(paste0("Extracting sedimentation layers..."))

  # calculate mean sediment deposition per model version/decade/scenario
  sedim_stk <- extract_sedim(
    stack_lst = model_stk,
    verbose   = TRUE
  )

  # calculate sediment deposition SI values
  sedim_si <- calc_sedim_si(
    stack_lst = sedim_stk,
    verbose   = TRUE
  )

  # rasterize road buffer polygons and calculate SI values
  road_si <- make_road_raster(
    shp_path = road_buffer_path,
    verbose  = TRUE
    ) %>%
    terra::app(fun = si_roads) %>%
    stats::setNames("layer")

  # Generate Commercial viability SpatRasterDataset
  cv_stk <- get_cv(
    roads   = road_si,
    fetch   = fetch_si,
    sedim   = sedim_si,
    depth   = depth_stk,
    verbose = TRUE
  )

  # Generate Alternative Oyster Cultch SpatRasterDataset from CV and OV
  aoc_stk <- get_aoc(
    cv      = cv_stk,
    ov      = ov_stk,
    verbose = TRUE
  )

  # AOC means per period
  aoc_mean_stk <- get_aoc_mean(
    aoc     = aoc_stk,
    verbose = TRUE
  )

  # AOC Standard Deviation per period
  aoc_sd_stk <- get_aoc_sd(
    aoc     = aoc_stk,
    verbose = TRUE
  )

  # Combine data into single dataset
  final_stk <- finalize_stacks(
    depth    = depth_stk,
    cv       = cv_stk,
    ov       = ov_stk,
    aoc      = aoc_stk,
    aoc_mean = aoc_mean_stk,
    aoc_sd   = aoc_sd_stk,
    verbose  = TRUE
  )

  return(final_stk)
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
