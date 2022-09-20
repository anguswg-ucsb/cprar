
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

  # r_path = landtype_paths$full_path[1]

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
      values = F
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
#' @return dataframe with columns for the row/column indexes of the diagonol cells. Each row of the dataframe indicates a diagonal cell
#' @export
index_diag <- function(
    m,
    row_index = NULL,
    col_index = NULL,
    direction = "top_left",
    id_col    = FALSE
) {

  # m <- lw_mat
  # row_index <- row_df$row[z]
  # col_index <- row_df$col[z]

  # top left/north west diagonal
  if(direction == "top_left") {

    message(paste0("Retrieving top left diaganol indices..."))

    m
    # sequence of northeast columns
    col_seq <- seq(1, col_index - 1, 1)

    # sequence of northeast rows
    row_seq <- seq(row_index - 1,  1,  -1)

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

    message(paste0("Retrieving top right diaganol indices..."))

    # sequence of northeast columns
    col_seq <- seq(col_index + 1, ncol(m), 1)

    # sequence of northeast rows
    row_seq <- seq(row_index - 1,  1,  -1)

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

    message(paste0("Retrieving bottom left diaganol indices..."))

    # sequence of northeast columns
    col_seq <- seq(1, col_index - 1, 1)

    # sequence of northeast rows
    row_seq <- seq(row_index + 1, nrow(m), 1)

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

    message(paste0("Retrieving bottom right diaganol indices..."))

    # sequence of northeast columns
    col_seq <- seq(col_index + 1, ncol(m), 1)

    # sequence of northeast rows
    row_seq <- seq(row_index + 1, nrow(m), 1)

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
    if (cell_val == stop_value) {

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

#' Count the diagnol cells from a matrix index to end of matrix or a stopping value.
#'
#' @param m matrix to count diagnols
#' @param diag_indices dataframe with the indices of matrix diagnols from a reference cell
#' @param stop_value numeric value to stop counting cells if encountered. Default is 0
#' @param verbose logical, whether to print loop messages or not. Default is TRUE
#'
#' @return value indicating the number of diagonal cells before specific value comes up
#' @export
#'
#' @examples
count_diag2 <- function(m, diag_indices, stop_value = 0, verbose = TRUE) {
  # m <- lw_mat
  # row_index <- row_nest$data[[1]]$diag_row
  # col_index <- row_nest$data[[1]]$diag_col
  # diag_indices <- row_nest$indices[[1]]
  # row_nest$data[[1]]
  # rm(counter, diag_df, cell_val, count_lst, n, k)
  # counter <- 1
  diag_lst <-
    diag_indices %>%
    dplyr::group_by(direction) %>%
    dplyr::group_split()

  # empty list to add to
  count_lst <- list()

  # loop through diagonals and add to cell counter until a 0 is reached
  for (k in 1:length(diag_lst)) {
    # for (k in 1:nrow(diag_indices)) {
    # k <- 3

    diag_df <- diag_lst[[k]]

    counter <- 1

    for (n in 1:nrow(diag_df)) {
      #  diag_df$diag_row[n]
      # n = 1
      # n = 1
      # diag_df$diag_col
      if(verbose == TRUE) {

        message(paste0("iter: ", n, " (r",   diag_df$diag_row[n], ", c",   diag_df$diag_col[n], ")"))

      }

      # value of kth southwest cell
      cell_val <- m[diag_df$diag_row[n], diag_df$diag_col[n]]

      # if cell value is 0, stop loop
      if (cell_val == stop_value) {

        if(verbose == TRUE) {
          message(paste0(stop_value, " detected, stopping counter"))
        }

        data.frame(
          diag_count = counter,
          direction  = diag_df$direction[1]
        )

        break
      }

      counter <- counter + 1

    }

    # add diagnol count to datarame
    count_lst[[k]] <- data.frame(
      diag_count = counter,
      direction  = diag_df$direction[1]
    )

  }

  return(counter)

}

#' Count number of cells to the left/right of a matrix cell
#' @param m matrix of interest
#' @param row_index numeric row number of matrix cell to calculate cell count to left/right
#' @param col_index numeric column number of matrix cell to calculate cell count to left/right
#' @param wide logical, whether dataframe should be returned in wide or long format. Default is FALSE, returns long dataframe column number of matrix cell to calculate cell count to left/right
#'
#' @return dataframe with cell counts to the left and right of a specified cell in a matrix
#' @export
side_lengths <- function(
    m,
    row_index = NULL,
    col_index = NULL,
    wide      = FALSE
) {
  # m <- lw_mat
  # row_index <- row_nest$data[[2]]$row
  # col_index <- row_nest$data[[2]]$col

  # vector of current matrix row
  vrow <- m[row_index, ]

  # cells to the left/right of occurrence of water cell
  left_cells  <- 1:col_index
  right_cells <- col_index:ncol(m)

  # run length encoding of left/right cells
  left_rle   <- rle(rev(vrow[left_cells]))
  right_rle  <- rle(vrow[right_cells])

  # count of left/right cells before a 0
  left_count  <- left_rle$lengths[(which(left_rle$values == 0)[1]) - 1]
  right_count <- right_rle$lengths[(which(right_rle$values == 0)[1]) - 1]

  # count of cells on right and left side of cell before a 0 occurs
  side_df <-
    data.frame(
      row   = row_index,
      col   = col_index,
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
        cols      = c(left, right),
        names_to  = "direction",
        values_to = "cell_count"
      ) %>%
      dplyr::select(cell_count, direction)

    return(side_df)

  }

  # index of first 0 occurance
  # left_idx   <- which(left_rle$values == 0)[1]
  # right_idx   <- which(right_rle$values == 0)[1]

  # count of left/right cells before a 0
  # left_count <- left_rle$lengths[left_idx - 1]
  # right_count <- right_rle$lengths[right_idx - 1]
}

library(sf)
library(doParallel)
library(parallel)
library(foreach)
library(dplyr)

library(tidyverse)
library(foreach)
library(doParallel)
library(parallel)
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
#' @param land_value numeric indicating what value land is represented by in raster. Default is 0 to conform to get_landwater() output
#' @param land_out umeric indicating what value land should be represented by in output raster. Default is 2
#' @return SpatVector representing water points
#' @export
#' @importFrom terra setValues values
get_fetch = function(
    r_path = NULL,
    r      = NULL
) {

  # error if no path or raster are given
  if(is.null(r_path) & is.null(r)) {

    stop(paste0("Please enter a path to a binary landwater raster or a SpatRaster object"))

  }

  # if no path to raster is given, read it in
  if(!is.null(r_path)) {

    r <- get_landwater(r_path = r_path)

  }

  # make landwater raster a wide matrix
  rmat <- terra::as.matrix(r, wide = TRUE)

  # get indices of water points
  idx <-
    which(rmat == 1, arr.ind = T) %>%
    as.data.frame() %>%
    dplyr::arrange(row) %>%
    dplyr::group_by(row) %>%
    dplyr::group_split()

  # i = 3

  # sequence along rows
  for (i in seq_along(idx)) {

    print(i)

    # data frame with row indices
    row_df <- idx[[i]]

    # diaganol direction to map over
    diag_dirs <- c("top_left", "top_right", "bottom_left", "bottom_right")

    # nest and map over each row and get the indices of the diagonals
    row_nest <-
      row_df %>%
      dplyr::mutate(row_id = 1:n()) %>%
      dplyr::group_by(row_id) %>%
      tidyr::nest() %>%
      dplyr::mutate(
        sides   = map(
          .x = data,
          .f = ~side_lengths(
            m         = rmat,
            row_index = .x$row,
            col_index = .x$col,
            wide      = FALSE
          )
        ),
        indices = map(
          .x = data,
          .f = ~lapply(1:length(diag_dirs), function(y) {
            index_diag(
              m         = rmat,
              row_index = .x$row,
              col_index = .x$col,
              direction = diag_dirs[y],
              id_col    = TRUE
              )
            }) %>%
            dplyr::bind_rows()
          ))

    # diagonals
    row_diags <-
      row_nest %>%
      dplyr::select(row_id, data, indices) %>%
      dplyr::mutate(
        diag_counts = map(
          .x = indices,
          .f = ~count_diag_dir(
            m            = rmat,
            diag_indices = .x,
            verbose      = FALSE
            )
          )
        ) %>%
      dplyr::select(row_id, data, diag_counts) %>%
      tidyr::unnest(c(data, diag_counts)) %>%
      dplyr::ungroup()

    # sides
    row_sides <-
      row_nest %>%
      dplyr::select(row_id, data, sides) %>%
      tidyr::unnest(c(data, sides)) %>%
      dplyr::ungroup()

    row_dist <-
      row_diags %>%
      dplyr::bind_rows(row_sides) %>%
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
      )
    }

return(row_dist)

}

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



f = distanceFromPoints(raster::raster(land), sf::st_as_sf(water_pts[seq(82000, 82100, 1),]))
plot(f)

# **************************************************************************************
# **************************************************************************************

r = landtype_files$full_path[1]

mapview::mapview(f) + tmpf

# as.data.frame(r, xy=TRUE, na.rm=TRUE)
# writeRaster(landtype, "landwater_binary.tif")
# tmpf  <- raster::raster("C:/Users/angus/OneDrive/Desktop/lynker/CPRA/data/fetch/raster/fetch_mean_dist_480m_resampled.tif")
# writeRaster(tmpf, "correct_distance_raster.tif")
 library(mapview)

  landtype <-
    r %>%
    get_landwater()

  land_r <-
    landtype %>%
    get_land() %>%
    raster::raster()

  terra::split()
  land_t <-
    landtype %>%
    get_land() %>%
    raster::raster()

  land <-
    landtype %>%
    get_land(as_polygon = T)

  plot(land)

  water_pts <-
    r %>%
    get_landwater() %>%
    get_water_pts()

  water_pts2 <-
    water_pts %>%
    sf::st_as_sf() %>%
    dplyr::slice(seq(82000, 82100, 1)) %>%
    terra::vect()


  # water_pts2[82,] %>%
    water_pts2[72,] %>%
    sf::st_as_sf() %>%
    sf::st_coordinates()

  pt_buffer <- data.frame(
    lng = 730390,
    lat = 3240040
    ) %>%
    sf::st_as_sf(coords = c("lng", "lat"), crs = 26915) %>%
    sf::st_buffer(50000) %>%
    sf::st_bbox() %>%
    sf::st_as_sfc() %>%
    terra::vect()

  lw_crop <-
    landtype %>%
    terra::crop(pt_buffer) %>%
    terra::mask(pt_buffer)
  plot(lw_crop)

  terra::as.matrix()

  # *****************************************************************************
  lw_mat <- terra::as.matrix(landtype, wide = TRUE)

  lw_points <-
    # lw_crop %>%
    landtype %>%
    as.data.frame(xy = T) %>%
    sf::st_as_sf(coords = c("x", "y"), crs = 26915) %>%
    stats::setNames(c('vals', 'geometry'))

  l_pts <-
    lw_points %>%
    dplyr::filter(vals == 0)

  w_pts <-
    lw_points %>%
    dplyr::filter(vals == 1)

  dist_df <- as.data.frame(dist_matrix)
  i <- 1
  for (i in 1:length(w_pts)) {
    message(paste0("Point: ", i))

    w_buff <-
      w_pts[i, ] %>%
      sf::st_buffer(20000)

    new_l_pts <- sf::st_intersection(w_buff, l_pts)

    dist_df <- sf::st_distance(w_pts[i, ], new_l_pts)

    mean_dist <- mean(dist_df)

mapview::mapview(new_l_pts) + w_pts[i, ]
  }
  system.time(
  dist_matrix  <- st_distance(w_pts, l_pts)
  )
  diag(dist_matrix) <- NA
  w_pts$distance <- matrixStats::rowMins(dist_matrix)
  w_pts$distance <- matrixStats::rowMeans2(dist_matrix)

  mapview::mapview(w_pts, color = "red") + l_pts
  # convert matrix to data frame and set column and row names
  dist_matrix <- data.frame(dist_matrix)
  names(dist_matrix) <- pts$cell
  rownames(dist_matrix) <- pts$cell
  lw_mat == 1
  windex <- lw_mat[1,] == 1
  tmp <- which(lw_mat ==1, arr.ind = TRUE) %>%
    data.frame() %>%
    dplyr::mutate(num = 1:n())

  ggplot() +
    geom_point(data = tmp, aes(x = row, y = col))
  condition <- function(x) {
    x >= 1
  }

  lw_mat[1, 6]
  rle(lw_mat[1,])
  r <- rle(condition(lw_mat[1,]))

  r$lengths[r$values == TRUE]
  max(r$lengths[r$values])
  rle(lw_mat[3,])
  which(lw_mat[3, ])
  lw_mat[1,]
  which(lw_mat[1,] == 0)
  lw_mat[1,2]
  trialtype <- c(1,1,0,1,1,0,0,1,0,1)
  RT <- c(100,200,300,400,500,600,700,800,900,950)
  fakedata <- cbind(trialtype,RT)
  zis <- which(fakedata[,'trialtype']==0)
  data.frame(x=seq_along(zis),numones=diff(c(0L,zis))-1L);
  CHR <- c(1,1,1,1,2,2,2,3,3,3,3)
  POS <- c(10,10000,12000,15000,25,75,50000,50,100,40000,45000)
  CONDITION <- c(F,T,T,F,T,F,F,T,F,T,F)
  df <- data.frame(CHR,POS,CONDITION)

  CHR_r <- c(1,1,2,2,3,3)
  from <- c(10,10000,25,50000,50,40000)
  to <- c(10,15000,75,50000,100,45000)
  count <- c(1,3,2,1,2,2)
  result <- data.frame(CHR_r,from,to,count)

  count_of=c(0,length(from))
  for  (i in c(1:length(from))){
    ind=which(POS>from[i] & POS<to[i])
    count_of[i]=length(ind)
  }
  which(lw_mat, arr.ind = TRUE)
  apply(lw_mat, 1, dist)
dist(lw_mat)
  apply(lw_mat, 2, function (x) length(x)-1)
    df <- data.frame(x = c(11:20), y= c(12:21))
    dst <- dist(df)
  m1 <- as.matrix(dst)
  which(m1==1, arr.ind=TRUE)

  writeRaster(lw_crop, "landwater_subset.tif")
  lw_crop
  plot(lw_crop)
  mapview(pt_buffer)
  pt_dist <-
    land %>%
    terra::distance(water_pts2, symmetrical=T)
  pt_dist
  water_pts2 %>%
    sf::st_as_sf() %>%
  mapview() + land_r
  # terra:::.oldGridDistance()

  # # plot(landtype)
  # land <-
  #   r %>%
  #   get_landwater() %>%
  #   get_land(as_polygon = T)
  #
  # lw_bound <-
  #   landtype %>%
  #   terra::boundaries()
  # plot(lw_bound)
  # lw_dist <-
  #   landtype %>%
  #   terra::distance()
  # plot(lw_dist)

  # Goal is to get the distance from a water cell to a land cell in each direction, then take the mean of those directions

  library(terra)
  library(raster)
  library(mapview)

  #landwater raster with water = 1, land = 0.
  landtype     <-  terra::rast("path/to/landwater_binary.tif")

  # correct distances I am aiming for
  correct_dist <-  raster::raster("path/to/correct_distance_raster.tif")


  # calculate grid distances
  landtype_gdist <-
    landtype %>%
    terra::gridDistance()

  # view to compare values.
  landtype_gdist %>%
    raster::raster() %>%
    mapview::mapview() + correct_dist

  # calculate grid distances
  landtype_gdist <-
    landtype %>%
    terra::distance()

  plot(lw_gdist)
  dist_mask <- terra::mask(lw_dist, landtype,  inverse = T)
  plot(dist_mask)

  tmpf  <- raster::raster("C:/Users/angus/OneDrive/Desktop/lynker/CPRA/data/fetch/raster/fetch_mean_dist_480m_resampled.tif")
  dist_mask <-
    lw_dist %>%
    # terra::crop(terra::rast(tmpf)) %>%
    terra::mask(land,  inverse = T)
  plot(dist_mask)

  lw_gdist %>%
    raster::raster() %>%
    mapview::mapview() + tmpf

  water_pts <-
    r %>%
    get_landwater() %>%
    get_water_pts()

  land <-
    r %>%
    get_landwater() %>%
    get_land()
  water <-
    r %>%
    get_landwater() %>%
    get_water(as_polygon = T, as_sf = T)


dist(mat, method = "manhattan")
  # fetch raster
  fetch <-
    landtype %>%
    # terra::distance(water)
    terra::distance(grid = TRUE)

  flip1 = terra::flip(landtype)
plot(flip1)
fetch1  = terra::gridDistance(landtype, target = 0)
  fetch2  = terra::gridDistance(flip1, target = 0)

  fetch1 = raster::raster(fetch) %>%
    raster::mask(water, inverse = F)
  plot(water$geometry)
fetch2 = raster::raster(fetch2)
mapview::mapview(fetch1)  + tmpf


land_r   <- get_land(landtype, as_polygon = TRUE)

plot(land_r)
water_r   <- get_water(landtype)

# view map
landtype %>%
  raster::raster()%>%
  mapview::mapview()

plot(land_type$MP2023_S07_G510_C000_U00_V00_SLA_O_12_12_W_lndtyp)
unique(values(land_type$MP2023_S07_G510_C000_U00_V00_SLA_O_12_12_W_lndtyp))
