
#' Parse model directory
#' @param dir highest level directory housing all the model folders
#' @return data.frame
#' @export
#' @importFrom stringr str_extract
#' @importFrom utils tail
#' @importFrom dplyr group_by mutate ungroup relocate
# get_fetch = function(dir){

  # file_df <- data.frame(
  #   model_dir = basename(dir),
  #   full_path = list.files(dir, full.names = T),
  #   path      = list.files(dir)
  # ) %>%
  #   dplyr::group_by(path) %>%
  #   dplyr::mutate(
  #     scenario = stringr::str_extract(path, "(?<=S)[0-9]+"),
  #     ext      = gsub("\\.", "", stringr::str_extract(path, "\\.[^_]+$")),
  #     type     = gsub(stringr::str_extract(path, "\\.[^_]+$"), "", tail(unlist(strsplit(path, '_')), 1))
  #   ) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::relocate(scenario, type, ext, model_dir, path, full_path)
  #
  # return(file_df)

# }

#' Create HSI grid template
#' @description returns a template SpatRaster of HSI grid specs. All cell values are 1 so the template raster is not actually "empty"
#' @return SpatRaster of the HSI grid
#' @export
#' @importFrom terra rast crs ext
make_grid = function(){

  # Template raster of HSI grid
  r_grid <- terra::rast(
    crs  = terra::crs('+init=EPSG:26915'),
    ext  = terra::ext(c(404709.999904666, 909669.999904667, 3199480, 3374680)),
    res  = c(480, 480),
    vals = 1
  )

  return(r_grid)

}

#' Make a landwater raster from CPRA Master Plan Land Type Raster
#' @description returns a resampled SpatRaster with land cells set to 1 and water cells set to 0. Fits the HSI raster specifications.
#' @param r_path character path to MP landtype TIF
#' @param water_value value to assign water in output raster. Default is 1.
#' @param land_value value to assign land in output raster. Default is 0.
#' @return SpatRaster
#' @export
#' @importFrom terra rast set.crs crs ext resample clamp setValues values
get_landwater = function(
    r_path,
    water_value = 1,
    land_value  = 0
    ){

  # read in landtype, set CRS if necessary
  landtype <-
    r_path %>%
    terra::rast() %>%
    terra::set.crs(terra::crs('+init=EPSG:26915'))

  # resample to HSI grid specs
  landtype <-
    landtype %>%
    terra::resample(
      y      = make_grid(),
      method = "near"
      ) %>%
    terra::clamp(
      lower  = 0,
      values = FALSE
    )

  # make land values = 0, water = 1
  landtype <- terra::setValues(
    landtype, ifelse(terra::values(landtype) == 2, water_value, land_value)
  )

  return(landtype)

  # # view map
  # landtype %>%
  #   raster::raster()%>%
  #   mapview::mapview()

}

#' Make a land raster from binary land type raster
#' @description returns a SpatRaster of only land cells. Land cells have a value of 2 by default, all other cells are NA. Fits the HSI raster specifications.
#' @param r SpatRast of binary land type raster. Land and water values only.
#' @param land_value numeric indicating what value land is represented by in raster. Default is 0 to conform to get_landwater() output
#' @param out_value numeric indicating what value land should be represented by in output raster. Default is 2
#' @param as_polygon logical, should polygon be returned instead of a raster. Default is FALSE, returns a terra rast object
#' @param as_sf logical, should polygon be returned as an SF object. Default is FALSE, returns a terra vect object
#' @return SpatRaster with only land cells having a value, all other cells are NA
#' @export
#' @importFrom terra setValues values
get_land = function(
    r,
    land_value = 0,
    out_value  = 2,
    as_polygon = FALSE,
    as_sf      = FALSE
    ) {

  # if cell value is 1 (land) make it a value of 2, otherwise make cell NA
  land <- terra::setValues(
    r, ifelse(terra::values(r) == land_value, out_value, NA)
  )

  # if a polygon should be returned instead of a raster
  if(as_polygon == TRUE) {

    # if polygon should be returned as an SF object
    if(as_sf == TRUE) {

      message("Generating land polygon...")
      message("Type: SF")

      # SF land polygon
      land <-
        land %>%
        terra::as.polygons() %>%
        sf::st_as_sf()  %>%
        sf::st_make_valid() %>%
        sf::st_cast() %>%
        stats::setNames(c("land", "geometry")) %>%
        sf::st_transform(26915)

    # return as terra vect object
    } else {

      message("Generating land polygon...")
      message("Type: SpatVector")

      # spatvector land polygon
      land <-
        land %>%
        terra::as.polygons() %>%
        stats::setNames(c("land"))

    }


  } else {

    message("Generating land raster...")
    message("Type: SpatRaster")

  }

  return(land)

  # view map
  # land %>%
  #   raster::raster()%>%
  #   mapview::mapview()

}

#' Make a water raster from binary land type raster
#' @description returns a SpatRaster of only water cells. Water cells have a value of 1 by default, all other cells are NA. Fits the HSI raster specifications.
#' @param r SpatRast of binary land type raster. Land and water values only.
#' @param water_value numeric indicating what value water is represented by in raster. Default is 1 to conform to get_landwater() output
#' @param out_value numeric indicating what value land should be represented by in output raster. Default is 1
#' @param as_polygon logical, should polygon be returned instead of a raster. Default is FALSE, returns a terra rast object
#' @param as_sf logical, should polygon be returned as an SF object. Default is FALSE, returns a terra vect object
#' @return SpatRaster with only land cells having a value, all other cells are NA
#' @export
#' @importFrom terra setValues values
get_water = function(
    r,
    water_value = 1,
    out_value   = 1,
    as_polygon  = FALSE,
    as_sf       = FALSE
) {

  # if cell value is 1 (land) make it a value of 2, otherwise make cell NA
  water <- terra::setValues(
    r, ifelse(terra::values(r) == water_value, out_value, NA)
  )

  # if a polygon should be returned instead of a raster
  if(as_polygon == TRUE) {

    # if polygon should be returned as an SF object
    if(as_sf == TRUE) {

      # make polygon from water raster
      water <-
        water %>%
        terra::as.polygons() %>%
        sf::st_as_sf()  %>%
        sf::st_make_valid() %>%
        sf::st_cast() %>%
        stats::setNames(c("water", "geometry")) %>%
        sf::st_transform(26915)

    # return as terra vect object
    } else {

      water <-
        water %>%
        terra::as.polygons() %>%
        stats::setNames(c("water"))

    }
  }

  return(water)

  # view map
  # water %>%
  #   raster::raster()%>%
  #   mapview::mapview()

}

#' Make water points from binary land type raster
#' @description returns a SpatVector of water points of only water cells. Water cells have a value of 1 by default, all other cells are NA. Fits the HSI raster specifications.
#' @param r SpatRast of binary land type raster. Land and water values only.
#' @param water_value numeric indicating what value water is represented by in raster. Default is 1 to conform to get_landwater() output
#' @param out_value numeric indicating what value land should be represented by in output raster. Default is 1
#' @param as_polygon logical, should polygon be returned instead of a raster. Default is FALSE, returns a terra rast object
#' @param as_sf logical, should polygon be returned as an SF object. Default is FALSE, returns a terra vect object
#' @return SpatVector representing water points
#' @export
#' @importFrom terra setValues values
get_water_pts = function(
    r,
    water_value = 1,
    out_value   = 1,
    as_sf       = FALSE
) {

  # if cell value is 1 (land) make it a value of 2, otherwise make cell NA
  water <- terra::setValues(
    r, ifelse(terra::values(r) == water_value, out_value, NA)
  )

  # convert raster cells to water points
  if(as_sf == TRUE) {

    message("Creating water points...")
    message("Type: SF")

    # sf point object
    water_pts <-
      water %>%
      terra::as.points() %>%
      sf::st_as_sf() %>%
      stats::setNames(c("water", "geometry"))


  } else {

    message("Creating water points...")
    message("Type: SpatVector")

    # terra vect points
    water_pts <-
      water %>%
      terra::as.points() %>%
      stats::setNames(c("water"))

  }

  return(water_pts)

}
#' Retrieve indices of diagonal cells from a specific matrix cell in a direction
#' @description Given a matrix and the row and column number for the cell of interest, return the index values for the diagonal cells in any direction.
#' @param m matrix of interest
#' @param row_index numeric row number of matrix cell to get indices of diagonals for
#' @param col_index numeric column number of matrix cell to get indices of diagonals for
#' @param direction character indicating which direction get diagonal indices for. Either "top_left", "top_right", "bottom_left", or "bottom_right". Default is "top_left".
#' @param id_col logical. Whether a column stating the direction of the diagonal should be included. Default is FALSE, no direction column is added.
#' @param verbose logical. Whether messages should output to console. Default is FALSE, no messages output.
#' @return dataframe with columns for the row/column indexes of the diagonol cells. Each row of the dataframe indicates a diagonal cell
#' @export
index_diag <- function(
    m,
    row_index = NULL,
    col_index = NULL,
    direction = "top_left",
    id_col    = FALSE,
    verbose   = FALSE
) {

  # m <- rmat
  # row_index <- row_nest$data[[1]]$row
  # col_index <- row_nest$data[[1]]$col


  # top left/north west diagonal
  if(direction == "top_left") {

    if(verbose == TRUE) {
      message(paste0("Retrieving top left diaganol indices..."))
      }
    # check if row 1
    if(row_index == 1) {

      row_start = 1

    } else {

      row_start = row_index - 1

    }

    # check if col 1
    if(col_index == 1) {

      col_start = 1

    } else {

      col_start = col_index - 1

    }

    # sequence of northeast columns
    col_seq <- seq(1, col_start, 1)
    # col_seq <- seq(1, col_index - 1, 1)

    # sequence of northeast rows
    row_seq <- seq(row_start,  1,  -1)
    # row_seq <- seq(row_index - 1,  1,  -1)

    # minimum sequence
    min_seq <- min(length(col_seq), length(row_seq))

    # indices of diaganols
    diag_df <- data.frame(
      diag_row = row_seq[1:min_seq],
      diag_col = rev(col_seq)[1:min_seq]
    )

  }
  # top right/north east diagonal
  if(direction == "top_right") {

    if(verbose == TRUE) {
      message(paste0("Retrieving top right diaganol indices..."))
    }

    # check if row 1
    if(row_index == 1) {

      row_start = 1

    } else {

      row_start = row_index - 1

    }

    # check if col 1
    if(col_index == ncol(m)) {

      col_start = col_index

    } else {

      col_start = col_index + 1

    }

    # sequence of northeast columns
    col_seq <- seq(col_start, ncol(m), 1)
    # col_seq <- seq(col_index + 1, ncol(m), 1)

    # sequence of northeast rows
    row_seq <- seq(row_start,  1,  -1)

    # minimum sequence
    min_seq <- min(length(col_seq), length(row_seq))

    # indices of diaganols
    diag_df <- data.frame(
      diag_row = row_seq[1:min_seq],
      diag_col = col_seq[1:min_seq]
    )

  }

  # bottom left/south west diagonal
  if(direction == "bottom_left") {

    if(verbose == TRUE) {
      message(paste0("Retrieving bottom left diaganol indices..."))
    }

    # check if row 1
    if(row_index == nrow(m)) {

      row_start = row_index

    } else {

      row_start = row_index + 1

    }

    # check if col 1
    if(col_index == 1) {

      col_start = col_index

    } else {

      col_start = col_index - 1

    }

    # sequence of northeast columns
    col_seq <- seq(1, col_start, 1)
    # col_seq <- seq(1, col_index - 1, 1)

    # sequence of northeast rows
    row_seq <- seq(row_start, nrow(m), 1)
    # row_seq <- seq(row_index + 1, nrow(m), 1)

    # minimum sequence
    min_seq <- min(length(col_seq), length(row_seq))

    # indices of diaganols
    diag_df <- data.frame(
      diag_row = row_seq[1:min_seq],
      diag_col = rev(col_seq)[1:min_seq]
    )

  }

  # bottom right/south east diagonal
  if(direction == "bottom_right") {

    if(verbose == TRUE) {
      message(paste0("Retrieving bottom right diaganol indices..."))
    }

    # check if row 1
    if(row_index == nrow(m)) {

      row_start = row_index

    } else {

      row_start = row_index + 1

    }

    # check if col 1
    if(col_index == ncol(m)) {

      col_start = col_index

    } else {

      col_start = col_index + 1

    }

    # sequence of northeast columns
    col_seq <- seq(col_start, ncol(m), 1)
    # col_seq <- seq(col_index + 1, ncol(m), 1)

    # sequence of northeast rows
    row_seq <- seq(row_start, nrow(m), 1)
    # row_seq <- seq(row_index + 1, nrow(m), 1)

    # minimum sequence
    min_seq <- min(length(col_seq), length(row_seq))

    # indices of diaganols
    diag_df <- data.frame(
      diag_row = row_seq[1:min_seq],
      diag_col = col_seq[1:min_seq]
    )

  }

  # if an ID column stating the direction should be added
  if(id_col == TRUE) {

    diag_df <-
      diag_df %>%
      dplyr::mutate(direction = direction)

  }

  return(diag_df)
}

#' Count diagonal cells of dataframe by direction
#' @param m matrix of interest
#' @param diag_indices dataframe with the indices of matrix diagnols from a reference cell and a "direction" column indicating in which direction does the diagonal extend out
#' @param verbose logical, whether to print loop messages or not. Default is FALSE.
#' @return a dataframe with the diagonal cell count from a specified matrix cell, for each specified direction
#' @export
count_diag_dir <- function(
    m,
    diag_indices,
    verbose = FALSE
    ) {

  # split diaganols into groups
  diag_lst <-
    diag_indices %>%
    dplyr::group_by(direction) %>%
    dplyr::group_split()

  # lapply over each diaganol direction
  diag_lst <- lapply(1:length(diag_lst), function(x) {

    # count on the diagonals
    count_diag(
      m            = m,
      diag_indices = diag_lst[[x]],
      as_df        = TRUE,
      verbose      = verbose
    ) %>%
      dplyr::mutate(
        direction = diag_lst[[x]]$direction[1]
      )
    }) %>%
    dplyr::bind_rows()

  return(diag_lst)

  }

#' Count the diagnol cells from a matrix index to end of matrix or a stopping value.
#'
#' @param m matrix to count diagnols
#' @param diag_indices dataframe with the indices of matrix diagnols from a reference cell
#' @param stop_value numeric value to stop counting cells if encountered. Default is 0
#' @param as_df logical, whether return a dataframe or a single count vector. Default is TRUE, returns a dataframe
#' @param verbose logical, whether to print loop messages or not. Default is TRUE
#'
#' @return a dataframe or numeric vector indicating the number of diagonal cells before specific value comes up
#' @export
#'
#' @examples
count_diag <- function(
    m,
    diag_indices,
    stop_value = 0,
    as_df      = TRUE,
    verbose    = TRUE
    ) {
  # m <- lw_mat
  # diag_indices <- row_nest$indices[[1]]
  # row_nest$data[[1]]
  # rm(counter, diag_df, cell_val, count_lst, n, k)

  counter <- 1

  # loop through diagnols and add to cell counter until a 0 is reached
  for (k in 1:nrow(diag_indices)) {

    if(verbose == TRUE) {

      message(paste0("K: ", k, " (r", diag_indices$diag_row[k], ", c", diag_indices$diag_col[k], ")"))

    }

    # value of kth southwest cell
    cell_val <- m[diag_indices$diag_row[k], diag_indices$diag_col[k]]

    # if cell value is 0, stop loop
    if (cell_val == stop_value | is.na(cell_val)) {

      if(verbose == TRUE) {
        message(paste0(stop_value, " detected, stopping counter"))
      }

      break
    }

    counter <- counter + 1

  }

  # Whether to return a dataframe or a numberic vector
  if(as_df == FALSE) {

    return(counter)

  } else {

    # simple dataframe with count value
    counter_df <- data.frame(
                      cell_count = counter
                    )

    return(counter_df)

    }

}

#' Check run length encoding output for cell run of zeros
#' @param rle_vect rle object
#' @param verbose logical, whether messages should be printed. Default is FALSE, no messages print.
#' @return Count of consecutive non-zero cells from a matrix cell, extending out in a given direction
#' @export
check_zeros <- function(rle_vect, verbose = FALSE) {
  # rle_vect <- right_rle
  # right_rle$values
  # check if any zeros occur after calculating RLE on a vector
  if(0 %in% unique(rle_vect$values)) {

    if(verbose == TRUE) {
      message(paste0("Calculating consecutive non-zero cell count..."))
    }
    # rle_vect$lengths[(which(is.na(rle_vect$values))[1]) - 1]
    # count of up/down cells before a 0
    count   <- rle_vect$lengths[(which(rle_vect$values == 0)[1]) - 1]

  } else {

    if(verbose == TRUE) {
      message(paste0("Calculating consecutive non-zero cell count..."))
      message(paste0("No zeros found, all cells counted"))
    }

    # count of up/down cells when no zeros
    count   <- rle_vect$lengths

  }

  return(count)

}

#' Count number of consequetive cells around a matrix cell to the up/down/left/right of a matrix cell
#' @param m matrix of interest
#' @param row_index numeric row number of matrix cell to calculate cell count going up/down/left/right of the specified cell
#' @param col_index numeric column number of matrix cell to calculate cell count going up/down/left/right of the specified cell
#' @param wide logical, whether dataframe should be returned in wide or long format. Default is FALSE, returns long dataframe column number of matrix cell to calculate cell count to left/right
#'
#' @return dataframe with consequetive cell counts (non-zero cells) up/down/left/right of the specified cell in a matrix
#' @export
side_count <- function(
    m,
    row_index = NULL,
    col_index = NULL,
    wide      = FALSE
) {
  # m         = m
  # row_index = row_nest$data[[4]]$row
  # col_index = row_nest$data[[4]]$col
  # wide      = FALSE

  # ---- col (up/down) ----
  # vector of current matrix col
  vcol <- m[, col_index]

  # cells to the up/down of occurrence of water cell
  up_cells   <- 1:row_index
  down_cells <- row_index:nrow(m)

  # run length encoding of up/down cells
  up_rle    <- rle(rev(vcol[up_cells]))
  down_rle  <- rle(vcol[down_cells])

  # count of up/down cells before a 0, if no zeros occur, returns length of consecutive 1s
  up_count   <- check_zeros(rle_vect = up_rle)
  down_count <- check_zeros(rle_vect = down_rle)

  # ---- row (left/right) ----
  # vector of current matrix row
  vrow <- m[row_index, ]

  # cells to the left/right of occurrence of water cell
  left_cells  <- 1:col_index
  right_cells <- col_index:ncol(m)

  # run length encoding of left/right cells
  left_rle   <- rle(rev(vrow[left_cells]))
  right_rle  <- rle(vrow[right_cells])

  # count of left/right  cells before a 0, if no zeros occur, returns length of consecutive 1s
  left_count   <- check_zeros(rle_vect = left_rle)
  right_count  <- check_zeros(rle_vect = right_rle)

  # count of cells on right and left side of cell before a 0 occurs
  side_df <-
    data.frame(
      row   = row_index,
      col   = col_index,
      up    = up_count,
      down  = down_count,
      left  = left_count,
      right = right_count
    )



  # if wide data requested
  if(wide == TRUE) {

    return(side_df)

  } else {

    # pivot to long format data
    side_df <-
      side_df %>%
      tidyr::pivot_longer(
        cols      = c(up, down, left, right),
        names_to  = "direction",
        values_to = "cell_count"
      ) %>%
      dplyr::select(cell_count, direction)

    return(side_df)

  }

}

dist_summary <- function(m,
                         indices_df) {
  # m = rmat
  # indices_df = idx[[i]]  # diaganol direction to map over

  diag_dirs <- c("top_left", "top_right", "bottom_left", "bottom_right")
# row_nest$data[[4]]$row
# row_nest$data[[4]]$col
# side_count(
#   m         = m,
#   row_index = row_nest$data[[4]]$row,
#   col_index = row_nest$data[[4]]$col,
#   wide      = FALSE
# )
  # nest and map over each row and get the indices of the diagonals
  row_nest <-
    indices_df %>%
    dplyr::mutate(row_id = 1:dplyr::n()) %>%
    dplyr::group_by(row_id) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      sides   = purrr::map(
        .x = data,
        .f = ~side_count(
          m         = m,
          row_index = .x$row,
          col_index = .x$col,
          wide      = FALSE
        )
      ),
      indices = purrr::map(
        .x = data,
        .f = ~lapply(1:length(diag_dirs), function(y) {
          index_diag(
            m         = m,
            row_index = .x$row,
            col_index = .x$col,
            direction = diag_dirs[y],
            id_col    = TRUE,
            verbose   = FALSE
          )
        }) %>%
          dplyr::bind_rows()
      ),
      diag_counts = purrr::map(
        .x = indices,
        .f = ~count_diag_dir(
          m            = m,
          diag_indices = .x,
          verbose      = FALSE
        )
      )
    )

  # row distances
  row_dist <-
    row_nest %>%
    dplyr::select(-sides, -indices) %>%
    tidyr::unnest(c(data, diag_counts)) %>%
    dplyr::bind_rows(
      tidyr::unnest(
        dplyr::select(row_nest, -diag_counts, -indices),
        c(data, sides)
      )
    ) %>%
    dplyr::mutate(
      # convert_factor = 480,
      convert_factor = dplyr::case_when(
        grepl("_", direction) ~ 678.8225,
        TRUE                  ~ 480
      ),
      distance = cell_count*convert_factor
    ) %>%
    dplyr::group_by(row_id, row, col) %>%
    dplyr::summarise(
      cell_count = mean(cell_count, na.rm = T),
      distance   = mean(distance, na.rm = T)
    ) %>%
    dplyr::ungroup()

  return(row_dist)

}
library(sf)
library(doParallel)
library(parallel)
library(foreach)
library(dplyr)

library(tidyverse)
library(sf)
library(terra)
library(raster)

source("R/parse_directory.R")
# hsi grid for resampling
# hsi_grid <- terra::rast("data/hsi_grid.tif")
base_folder = "D:/cpra"

# model directories
model_dirs     <- parse_directory(base_folder = base_folder)

# landtype files
landtype_files <- parse_files(dir = model_dirs$path[1]) %>%
  dplyr::filter(type == "lndtyp")
# r <- landtype_files$full_path[1]

# tmpf  <- raster::raster("C:/Users/angus/OneDrive/Desktop/lynker/CPRA/data/fetch/raster/fetch_mean_dist_480m_resampled.tif")
r_path = landtype_files$full_path[1]

fetch_r <- get_fetch(r_path = r_path, ncores = 12)
landtype <- get_landwater(r_path = landtype_files$full_path[1])

pt_buffer <- data.frame(
  lng = 730390,
  lat = 3240040
) %>%
  sf::st_as_sf(coords = c("lng", "lat"), crs = 26915) %>%
  sf::st_buffer(2000) %>%
  sf::st_bbox() %>%
  sf::st_as_sfc() %>%
  terra::vect()

lw_crop <-
  landtype %>%
  terra::crop(pt_buffer) %>%
  terra::mask(pt_buffer)

plot(lw_crop)

# *****************************************************************************
# *****************************************************************************

#' Make fetch summary dstances
#' @description returns a dataframe with fetch lengths for a coastal area
#' @param r_path Path to binary land type raster TIF. Land and water values only.
#' @param r Path to binary land type raster TIF. Land and water values only.
#' @param ncores numeric indicating how many cores to use during parallel processing
#' @return SpatVector representing mean wind fetch distance
#' @importFrom dplyr arrange group_by group_split mutate select bind_rows n summarise case_when ungroup
#' @importFrom purrr map
#' @importFrom tidyr nest unnest
#' @importFrom foreach foreach
#' @importFrom parallel makeCluster stopCluster
#' @importFrom doParallel registerDoParallel stopImplicitCluster
#' @importFrom terra as.matrix rast crs ext
#' @export
get_fetch = function(
    r_path = NULL,
    r      = NULL,
    ncores = 10
) {
  # error if no path or raster are given
  if(is.null(r_path) & is.null(r)) {

    stop(paste0("Please enter a path to a binary landwater raster or a SpatRaster object"))

  }
  # r_path = landtype_files$full_path[1]
  # if no path to raster is given, read it in
  if(!is.null(r_path)) {

    r <- get_landwater(r_path = r_path)

  }

  # replace NA raster values w/ 0 for calcs
  r <- terra::classify(r, cbind(NA, 0))
  # r %>%
  #   raster::raster() %>%
  #   mapview::mapview()

  # pt_buffer <- data.frame(
  #   lng = 730390,
  #   lat = 3240040
  # ) %>%
  #   sf::st_as_sf(coords = c("lng", "lat"), crs = 26915) %>%
  #   sf::st_buffer(25000) %>%
  #   sf::st_bbox() %>%
  #   sf::st_as_sfc() %>%
  #   terra::vect()
  #
  # lw_crop <-
  #   r %>%
  #   terra::crop(pt_buffer) %>%
  #   terra::mask(pt_buffer)
  # plot(lw_crop)

  # make landwater raster a wide matrix
  rmat <- terra::as.matrix(r, wide = TRUE)
  # rmat <- terra::as.matrix(lw_crop, wide = TRUE)

  # get indices of water points
  idx <-
    which(rmat == 1, arr.ind = T) %>%
    as.data.frame() %>%
    dplyr::arrange(row) %>%
    dplyr::group_by(row) %>%
    dplyr::group_split()
# i <- 1

  message(paste0("Calculating fetch distances..."))
  # ncores = 12
  cl    <- parallel::makeCluster(ncores) #not to overload your computer

  # register parallel backend
  doParallel::registerDoParallel(cl)

  # run a loop using a parallel backend. ~2x faster
  fetch_loop <- foreach::foreach(i = 1:length(idx),
                                 .combine  = "rbind",
                                 .export   = c('side_count', 'index_diag', 'count_diag_dir', 'count_diag' ,'dist_summary', 'check_zeros'),
                                 .packages = c("dplyr", "tidyr", "purrr")) %dopar% {

                                  row_dist <- dist_summary(m = rmat, indices_df = idx[[i]])
                                  row_dist

                                 }

  # stop extra workers
  doParallel::stopImplicitCluster()
  parallel::stopCluster(cl)

  row_lst <- list()
  for (i in 1:length(idx)) {

    message(paste0("Index: ", i, "/", length(indx)))
    row_dist <- dist_summary(m = rmat, indices_df = idx[[i]])

    row_lst[[i]] <- row_dist

  }
  rmat2 <- rmat
  # loop over each row in dataframe and replace old water matrix values with mean fetch distances
  for (j in 1:nrow(fetch_loop)) {

    message(paste0("Row: ", fetch_loop$row[j], " col: ", fetch_loop$col[j]))
    rmat2[fetch_loop$row[j], fetch_loop$col[j]]  <- fetch_loop$distance[j]

  }

  # replace 0 values (land) with NA
  rmat2[which(rmat2 == 0)] <- NA

  # create fetch raster from matrix
  fetch_r <-
    terra::rast(
      rmat2,
      crs    = terra::crs(r),
      extent = terra::ext(r)
    )

  plot(fetch_r)
  return(fetch_r)


}
# system.time(
#   lst_dist <- lapply(1:length(idx), function(x) {
#     # idx[[1]]$row
#     message(paste0(x))
#     sides <- side_count(
#       m         = rmat,
#       row_index = idx[[x]]$row,
#       col_index = idx[[x]]$col,
#       wide      = FALSE
#     )
#
#   })
# )
# system.time(
#   diag_lst <- lapply(1:length(idx), function(x) {
#     # idx[[1]]$row
#     message(paste0(x))
#
#     lapply(1:length(diag_dirs), function(y) {
#       index_diag(
#         m         = rmat,
#         row_index = idx[[x]]$row,
#         col_index = idx[[x]]$col,
#         direction = diag_dirs[y],
#         id_col    = TRUE,
#         verbose   = FALSE
#       )
#     }) %>%
#       dplyr::bind_rows()
#   })
# )
# diag_lst[[1]]
# system.time(
#   diag_counts <- lapply(1:length(diag_lst), function(y) {
#     message(paste0(y, "/", length(diag_lst)))
#     count_diag_dir(
#       m         = rmat,
#       diag_indices = diag_lst[[y]],
#       verbose      = FALSE
#     )
#   })
# )
# system.time(
#   diag_counts  <-  purrr::map(
#     .x = indices,
#     .f = ~count_diag_dir(
#       m            = rmat,
#       diag_indices = .x,
#       verbose      = FALSE
#     )
#   )
# )
# *****************************************************************************

# z <-  2

# sequence along columns
# for (z in 1:nrow(row_df)) {

# message(paste0("r", row_df$row[z], " c", row_df$col[z]))

# row of the matrix
# row_vect <- rmat[row_df$row[z],]
# row_vect

# # cells to the west of occurance of water cell
# west_cells <- 1:row_df$col[z]
#
# # cells to the east of occurance of water cell
# east_cells <- row_df$col[z]:ncol(rmat)

# # east count of cells
# east_count <- which(row_vect[east_cells] == 0)[1]
#
# # west count of cells
# west_count <- which(rev(row_vect)[west_cells] == 0)[1]

# # run length encoding of eastern cells
# east_rle <- rle(row_vect[east_cells])
#
# # index of first 0 occurance
# east_idx   <- which(east_rle$values == 0)[1]
#
# # count of eastern cells before a 0
# east_count <- east_rle$lengths[east_idx - 1]
#
# # run length encoding of western cells
# west_rle <- rle(rev(row_vect[west_cells]))
#
# # index of first 0 occurance
# west_idx   <- which(west_rle$values == 0)[1]
#
# # count of western cells before a 0
# west_count <- west_rle$lengths[west_idx - 1]

# *****************************************************************************
# *****************************************************************************
#' Make fetch summary dstances
#' @description returns a dataframe with fetch lengths for a coastal area
#' @param r Path to binary land type raster TIF. Land and water values only.
#' @param land_value numeric indicating what value land is represented by in raster. Default is 0 to conform to get_landwater() output
#' @param land_out umeric indicating what value land should be represented by in output raster. Default is 2
#' @return SpatVector representing water points
#' @export
#' @importFrom terra setValues values
# get_fetch = function(
#     r,
#     land_value  = 0,
#     land_out    = 2
# ) {
#   # r = landtype_files$full_path[1]
#
#   # make binary landtype raster
#   landwater <-
#     r %>%
#     get_landwater()
#
#   # make binary landtype raster
#   land <-
#     landwater %>%
#     get_land(
#       as_polygon = T,
#       as_sf      = T
#     )
#
#   # library(sp)
#
#   # spatial polygon
#   land_sp <-
#     land %>%
#     as("Spatial") %>%
#     as("SpatialPolygonsDataFrame") %>%
#     sp::spTransform(sp::CRS('+init=EPSG:26915'))
#
#   # get waterpoints from landwater raster
#    water_pts <-
#      landwater %>%
#      get_water_pts(
#        as_sf = TRUE
#        )
#
#    # add lat/long columns
#    water_pts <-
#      water_pts %>%
#      # sf::st_transform(26915) %>%
#      dplyr::mutate(
#        lng    =  sf::st_coordinates(.)[,1],
#        lat    =  sf::st_coordinates(.)[,2]
#      ) %>%
#      stats::setNames(c("value", "geometry", "lng", "lat")) %>%
#      dplyr::relocate(value, lng, lat, geometry)
#
#    # filter water points for those NOT intersecting polygon
#    water_pts <- water_pts[sapply(sf::st_intersects(water_pts, land), function(x){length(x) == 0}), ]
#
#    # transform points to SP object
#    water_pts_sp <-
#      water_pts %>%
#      # slice(1:1000) %>%
#      as("Spatial") %>%
#      as("SpatialPoints") %>%
#      sp::spTransform(sp::CRS('+init=EPSG:26915'))
#
#
#    # Parallel backend
#    ncores = 14
#
#    cl    <- parallel::makeCluster(ncores) #not to overload your computer
#
#    doParallel::registerDoParallel(cl)
#    # i <- 1
#    system.time(
#    fetch_loop <- foreach::foreach(i = 1:length(water_pts_sp),
#                                   .combine = "rbind",
#                                   .packages = c("dplyr", "tibble", "waver", "sf", "sp")
#                                   ) %dopar% {
#
#       # SP point  object
#      fetch_pts <- water_pts_sp[i]
#
#      # latitude of point
#      pt_lat    <- water_pts[i,]$lat
#
#      # longitude of point
#      pt_lng    <- water_pts[i,]$lng
#
#
#      fetch_df <- waver::fetch_len(
#        p           = fetch_pts,
#        bearings    = c(0, 45, 90, 135, 180, 225, 270, 315),
#        shoreline   = land_sp,
#        dmax        = 20000,
#        spread      = c(0, 0, 0),
#        projected   = TRUE
#      ) %>%
#        data.frame() %>%
#        tibble::rownames_to_column() %>%
#        stats::setNames(c("bearing", "fetch")) %>%
#        dplyr::mutate(
#          id  = i,
#          lng = pt_lng,
#          lat = pt_lat
#        ) %>%
#        dplyr::group_by(id, lat, lng) %>%
#        dplyr::summarize(fetch = mean(fetch, na.rm = TRUE)) %>%    # summarize mean of 3 max distances
#        dplyr::ungroup()
#        # sf::st_as_sf(coords = c("lng", "lat"), crs = 26915) %>%
#        # st_transform(26915) %>%
#        # st_as_sf(coords = c("lng", "lat"), crs = 5070) %>%
#        # st_transform(26915) %>%
#        # mutate(
#        #   lng = st_coordinates(.)[,1],
#        #   lat = st_coordinates(.)[,2]
#        # )  %>%
#        # sf::st_drop_geometry() %>%
#        # dplyr::group_by(id, lat, lng) %>%
#        # dplyr::summarize(fetch = mean(fetch, na.rm = TRUE)) %>%    # summarize mean of 3 max distances
#        # dplyr::ungroup()
#      fetch_df
#    }
#
# )
#    # stop extra workers
#    doParallel::stopImplicitCluster()
#    stopCluster(cl)
#
# }


# **************************************************************************************
# **************************************************************************************
