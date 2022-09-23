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
#' Retrieve length of any diagonal direction for a specific matrix cell
#' @description Given a matrix and the row and column number for the cell of interest, return the number of consecutive non-zero values
#' @param m matrix of interest
#' @param row_index numeric row number of matrix cell to get indices of diagonals for
#' @param col_index numeric column number of matrix cell to get indices of diagonals for
#' @param vdirect character indicating vertical direction for diagonal. Either "up" or "down". Default is "down".
#' @param hdirect character indicating horizontal direction for diagonal. Either "left" or "right". Default is "right".
#' @param max_count Maximum number of cells within max_dist
#' @param id_col logical. Whether a column stating the direction of the diagonal should be included. Default is FALSE, no direction column is added.
#' @param as_matrix logical. Whether a matrix with row/column/cell_count/direction is returned or a numeric vector. Default is FALSE, returns a numeric vector of cell counts
#' @param verbose logical. Whether messages should output to console. Default is FALSE, no messages output.
#' @return integer of diagonal cell counts or a matrix with diagonal cell counts
#' @export
diag_dist <- function(
    m,
    row_index  = NULL,
    col_index  = NULL,
    vdirect    = "down",
    hdirect    = "right",
    max_count,
    as_matrix  = FALSE,
    id_col     = FALSE,
    verbose    = FALSE
) {

  # m <- rmat
  # hdirect = "right"
  # vdirect = "down"

  # concatenate direction string
  direction <- paste0(vdirect, "_", hdirect)

  # check if left direction
  if(grepl("left", hdirect) == TRUE) {

    if(verbose == TRUE) {
      message(paste0("Sequencing ", hdirect, " diagonals", " - (", direction, ")"))
    }

    # check if col 1
    if(col_index == 1) {

      col_start = col_index

    } else {

      col_start = col_index - 1

    }

    # sequence of above columns
    col_seq <- seq(1, col_start, 1)


  } else {    # check if right direction

  # if(grepl("right", hdirect) == TRUE) {
    if(verbose == TRUE) {
      message(paste0("Sequencing ", hdirect, " diagonals", " - (", direction, ")"))
    }

    # check if col 1
    if(col_index == ncol(m)) {

      col_start = col_index

    } else {

      col_start = col_index + 1

    }

    # sequence of right columns
    col_seq <- seq(col_start, ncol(m), 1)

  }

  # check if above direction
  if(grepl("up", vdirect) == TRUE) {

    if(verbose == TRUE) {
      message(paste0("Sequencing ", vdirect, " diagonals", " - (", direction, ")"))
    }

    # check if row 1
    if(row_index == 1) {

      row_start = 1

    } else {

      row_start = row_index - 1

    }

    # sequence of above rows
    row_seq <- seq(row_start,  1,  -1)

  } else {  # check if bottom direction

  # if(grepl("bottom", vdirect) == TRUE) {
    if(verbose == TRUE) {
      message(paste0("Sequencing ", vdirect, " diagonals", " - (", direction, ")"))
    }

    # check if row 1
    if(row_index == nrow(m)) {

      row_start = row_index

    } else {

      row_start = row_index + 1

    }

    # sequence of below rows
    row_seq <- seq(row_start, nrow(m), 1)

  }

  # minimum sequence
  min_seq <- min(length(col_seq), length(row_seq))

  # top left/north west diagonal
  if(vdirect == "down" & hdirect == "right") {

      # vector diagonal indices
      diag_row = row_seq[1:min_seq]
      diag_col = col_seq[1:min_seq]

  } else if(vdirect == "down" & hdirect == "left"){

      # vector diagonal indices
      diag_row = row_seq[1:min_seq]
      diag_col = rev(col_seq)[1:min_seq]

      # # diagonal matrix
      # diag_mat <-
      #   matrix( cbind(row_index, col_index, check_length(m[cbind(diag_row, diag_col)]), direction),ncol = 4,
      #     dimnames = list(c("val"), c("row_idx", "col_idx", "cell_count", "direction")) )

      # diagonal dataframe
      # diag_df <-data.frame(cell_count = check_length(m[cbind(diag_row, diag_col)]))

  } else if(vdirect == "up" & hdirect == "right"){

      # vector diagonal indices
      diag_row = row_seq[1:min_seq]
      diag_col = col_seq[1:min_seq]

  } else if(vdirect == "up" & hdirect == "left"){

      # vector diagonal indices
      diag_row = row_seq[1:min_seq]
      diag_col = rev(col_seq)[1:min_seq]

  }

  # if a matrix is requested, with or without ID columns (direction)
  if(as_matrix == TRUE) {

    # add an ID column stating the direction of the diagonal
    if(id_col == TRUE) {

        # diagonal matrix
        diag_mat <-
          matrix(
            cbind(
              row_index,
              col_index,
              check_length(
                vect      = m[cbind(diag_row, diag_col)],
                max_count = max_count
                ),
              direction
            ),
            ncol     = 4,
            dimnames = list(c("r"), c("row", "col", "cell_count", "direction"))
          )
      } else {

        # diagonal matrix w/o row & column data
        diag_mat <-
          matrix(
            cbind(
              row_index,
              col_index,
              check_length(
                vect      = m[cbind(diag_row, diag_col)],
                max_count = max_count
              ),
            ),
            ncol = 3,
            dimnames = list(c("r"), c("row", "col", "cell_count"))
          )
      }

    return(diag_mat)

  } else {   # Default returns an integer with the total diagonal cell count

    # get count of diagonal vector
    diag_count <- check_length(
                      vect      = m[cbind(diag_row, diag_col)],
                      max_count = max_count
                    )

    return(diag_count)

  }

}

diag_summary <- function(
    m,
    indices_df,
    func    = "mean",
    max_count,
    as_df   = FALSE,
    verbose = FALSE
    ) {

  # get function
  func <- match.fun(func)

  # diagonal directions
  diag_dirs <- expand.grid(
    vdir             = c("up", "down"),
    hdir             = c("left", "right"),
    stringsAsFactors = F
  )

  #indices list
  diags <- lapply(1:nrow(indices_df), function(n) {

      if(verbose == TRUE) {
        message(paste0(n, "/", nrow(indices_df)))
      }

      diags <- sapply(1:nrow(diag_dirs), function(y) {

        diag_dist(
          m          = m,
          row_index  = indices_df$row[n],
          col_index  = indices_df$col[n],
          vdirect    = diag_dirs[y, 1],
          hdirect    = diag_dirs[y, 2],
          max_count  = max_count,
          as_matrix  = FALSE,
          id_col     = F,
          verbose    = F
        )
      }) %>%
        func(na.rm = T)
      # data.frame(
      # row        = idx$row[n],
      # col        = idx$col[n],
      # cell_count = .,
      # direction  = c("up_left", "down_left", "up_right", "down_right")
      # )

    })
  return(diags)
}

side_summary <- function(
    m,
    indices_df,
    func    = "mean",
    as_df   = FALSE,
    verbose = FALSE
) {

  # indices_df <- idx
  # func    = "sum"
  # m <- rmat
# z <- 658
  # side counts/lengths directions
  side_lengths <- lapply(1:nrow(indices_df), function(z) {
      if(verbose == TRUE) {
        message(paste0(z, "/", nrow(indices_df)))
      }

      side_count(
        m         = m,
        row_index = indices_df$row[z],
        col_index = indices_df$col[z],
        func      = func,
        verbose   = verbose
      )

    })

    return(side_lengths)
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
    # diag_df <-
    #   data.frame(
    #     diag_row = row_seq[1:min_seq],
    #     diag_col = rev(col_seq)[1:min_seq]
    #     )
    # c(row_seq[1:min_seq],  rev(col_seq)[1:min_seq])
    # M = matrix(rnorm(100),nrow=10,ncol=10)
    # set.seed(123)
    # row_index = sample(10) # 3  8  4  7  6  1 10  9  2  5
    # column_index = sample(10)
    # M
    # cbind(row_index, column_index)
    # M[cbind(row_index, column_index)]
    #
    diag_row = row_seq[1:min_seq]
    diag_col = rev(col_seq)[1:min_seq]

    # mvect  <- m[cbind(diag_row, diag_col)]

    check_length(m[cbind(diag_row, diag_col)])

    # idx_mat <-
      matrix(
        data = c(row_seq[1:min_seq],  rev(col_seq)[1:min_seq]),
        nrow = min_seq,
        ncol = 2
        )
    idx_mat

    m
    # diag_df <-
      data.frame(
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
    cell_res   = 480,
    max_dist   = 20000,
    verbose    = FALSE
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
      cell_res     = cell_res,
      max_dist     = max_dist,
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
#' @param cell_res numeric cell resolution
#' @param max_dist maximum distance to calculate fetch to
#' @param as_df logical, whether return a dataframe or a single count vector. Default is TRUE, returns a dataframe
#' @param verbose logical, whether to print loop messages or not. Default is TRUE
#'
#' @return a dataframe or numeric vector indicating the number of diagonal cells before specific value comes up
#' @export
#'
#' @examples
count_diag <- function(
    m,
    # diag_indices,
    row_vect,
    col_vect,
    stop_value = 0,
    cell_res   = 480,
    max_dist   = 20000,
    as_df      = TRUE,
    verbose    = TRUE
    ) {
  # m <- lw_mat
  # diag_indices <- indices_df[[61]]
  # diag_indices <- indices_df[[2]]
  # diag_indices <- row_nest$indices[[1]]
  # diag_indices <- row_nest$indices[[80]]
  # diag_indices <- row_nest$indices[[81]]

  # max count distance
  # max_count <- floor(max_dist/(cell_res*sqrt(2)))
  max_count <- floor(max_dist/(cell_res))

  # m[diag_indices$diag_row, diag_indices$diag_col]
  # m[ diag_indices$row-1, diag_indices$col]
  # m[(diag_indices$row[1]+1):ncol(m), 1:(diag_indices$col[1]-1)]

  # vectorize row/col indices
  # row_vect <- diag_indices$diag_row
  # col_vect <- diag_indices$diag_col

  # start counter
  counter <- 1

  # loop through diagonal cells until max_count is reached or a 0 is found
  while (counter <= max_count) {

      # value of nth cell
      cell_val <- m[row_vect[counter], col_vect[counter]]

      # if cell value is 0, stop loop
      if (cell_val == stop_value | is.na(cell_val)) {

        if(verbose == TRUE) {
          message(paste0("--- Stopping diagonal cell count --- "))
          message(paste0("--- ", stop_value, " or end of matrix reached ---"))
        }

        break

      # Last iteration message
      } else if(counter >= max_count) {

        message(paste0("--- Max cell count reached: ", max_count, " ---"))

      # print message until last iteration
      } else if(counter < max_count) {

        message(paste0("Diagonal: ", counter, " --- (row ", row_vect[counter], ", col ", col_vect[counter], ")"))

      }

    counter <-  counter + 1

  }

  # remove last loop count
  if (counter >= 1) {

    counter <- counter - 1

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

  # # loop through diagnols and add to cell counter until a 0 is reached
  # for (k in 1:nrow(diag_indices)) {
  #
  #   if(verbose == TRUE) {
  #
  #     message(paste0("Iteration: ", k, " - (row ", diag_indices$diag_row[k], ", col ", diag_indices$diag_col[k], ")"))
  #
  #   }
  #
  #   # if max number of pixels is reached
  #   if(counter > max_count) {
  #
  #     if(verbose == TRUE) {
  #       message(paste0("Counter reached max count:"))
  #       message(paste0("Max count: ", max_count))
  #       message(paste0("Counter: ", counter))
  #     }
  #
  #     break
  #
  #   }
  #
  #   # value of kth southwest cell
  #   cell_val <- m[diag_indices$diag_row[k], diag_indices$diag_col[k]]
  #
  #   # if cell value is 0, stop loop
  #   if (cell_val == stop_value | is.na(cell_val)) {
  #
  #     if(verbose == TRUE) {
  #       message(paste0(stop_value, " detected, stopping counter"))
  #     }
  #
  #     break
  #   }
  #
  #   counter <- counter + 1
  #
  # }

}
#' Check boundaries of matrix column
#' @param m matrix of interest
#' @param row_index numeric row index
#' @param col_index numeric col index
#' @return list of cell indexes
#' @export
check_cols <- function(m,
                       row_index,
                       col_index
                       ) {

  # check if cells are on the edge of raster
  if(row_index == nrow(m)) {

    # if bottom row
    up_cells   <- 1:(row_index-1)
    down_cells <- (row_index):nrow(m)

  } else if(row_index == 1) {

    # if top row
    up_cells   <- 1:(row_index)
    down_cells <- (row_index+1):nrow(m)

  } else {

    # if any other row
    up_cells   <- 1:(row_index-1)
    down_cells <- (row_index+1):nrow(m)

  }

  updown <- list(up_cells, down_cells)

  return(updown)

}

#' Check boundaries of matrix row
#' @param m matrix of interest
#' @param row_index numeric row index
#' @param col_index numeric col index
#' @return list of cell indexes
#' @export
check_rows <- function(m,
                       row_index,
                       col_index
) {

  # check if cells are on the edge of raster
  if(col_index == ncol(m)) {

    # if right most col
    left_cells  <- 1:(col_index-1)
    right_cells <- (col_index):ncol(m)

  } else if(col_index == 1) {

    # if left most col
    left_cells  <- 1:(col_index)
    right_cells <- (col_index+1):ncol(m)

  } else {

    # all other cols
    left_cells  <- 1:(col_index-1)
    right_cells <- (col_index+1):ncol(m)

  }

  leftright <- list(left_cells, right_cells)

  return(leftright)

}
#' Check length of consecutive non-zero cells in vector
#' @param vect numeric vecto of 1s and 0s
#' @param verbose logical, whether messages should be printed. Default is FALSE, no messages print.
#' @return Count of consecutive non-zero cells from a matrix cell, extending out in a given direction
#' @export
check_length <- function(
    vect,
    max_count,
    verbose = FALSE
) {
  # vect <- c(1, 1, 0, 0, 1, NA, 0, NA)
  # vect <- vect3
  # vect <- c(1, 1, 0, 0, 1, 0)
  # vect <- c(1, 1, NA, 0, 1, NA, 0, NA)
  # vect3 <- vect
  # vect <- c(NA, NA, NA, 3, 0)

  # replace NA values w/ 2
  vect[is.na(vect)] <- 2


  # first check if first cell is an NA
  if(vect[1] == 2) {

    # make count index of first zero minus 1
    # count <- length(vect[vect == 2])
    count <- NA

    # rest of side length logic
  } else {

    # if first cell is a 0, give count of 0
    if(vect[1] == 0) {

      count <- 0

      if(verbose == TRUE) {

        message(paste0("Cell count: ", count))

      }

    } else {

        # check for NAs
        na_check <- which(vect == 2)[1]

        # index of first 0 minus 1 gives number of cells before a 0 occurs
        check <- which(vect == 0)[1]

        # check to make sure there is an NA in vector
        if(is.na(na_check)) {

          # if no zero is encountered, make count length of the vector
          if(is.na(check)) {

              count <- length(vect)

          } else { # if zero is encountered

              # make count index of first zero minus 1
              count <- check - 1
          }

        } else {

          # if no zero is encountered, make count length of the vector
          if(is.na(check)) {

              count <- NA

          } else {

              count <- check - 1

          }

      }
    }
  }

    #   # check to make sure there is an NA in vector
    #   if(!is.na(na_check)) {
    #
    #     # if no zero is encountered, make count length of the vector
    #     if(is.na(check)) {
    #
    #       count <- length(vect)
    #
    #     } else { # if zero is encountered
    #
    #       # check that 0 occurs BEFORE any NA values
    #       if(check < na_check) {
    #
    #         # make count index of first zero minus 1
    #         count <- check - 1
    #
    #       } else {
    #
    #         # make count index of first zero minus 1
    #         count <- length(vect[vect == 2])
    #
    #       }
    #
    #     }
    #
    #   } else {
    #
    #     # if no zero is encountered, make count length of the vector
    #     if(is.na(check)) {
    #
    #       count <- length(vect)
    #
    #     } else {
    #
    #       # make count index of first zero minus 1
    #       count <- check - 1
    #
    #     }
    #
    #   }
    #
    # }

  # }
    # convert to integer
    count <- as.integer(count)

    if(verbose == TRUE) {
      message(paste0("Cell count: ", count))
    }



  return(count)

}

#' Check run length encoding output for cell run of zeros
#' @param rle_vect rle object
#' @param verbose logical, whether messages should be printed. Default is FALSE, no messages print.
#' @return Count of consecutive non-zero cells from a matrix cell, extending out in a given direction
#' @export
check_zeros <- function(rle_vect, verbose = FALSE) {

  # rle_vect <- down_rle
  # rev(vcol[up_cells])
  # vcol[down_cells]

  # check if any zeros occur after calculating RLE on a vector
  if(0 %in% unique(rle_vect$values)) {

    if(verbose == TRUE) {
      message(paste0("Calculating consecutive non-zero cell count..."))
    }

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
#' @param cell_res numeric cell resolution
#' @param max_dist maximum distance to calculate fetch to
#' @param func character. R function, either, mean, min, max, or sum. Default is mean calculates mean number of cells around cell
#' @param as_df logical. whether to return a dataframe or not. Default is FALSE, returns numeric vector of mean cell count around cell.
#' @param wide logical, whether dataframe should be returned in wide or long format. Default is FALSE, returns long dataframe column number of matrix cell to calculate cell count to left/right
#' @param verbose logical, whether to print messages to console. Default is FALSE, no messages print
#'
#' @return numeric indicating the average cell count in all directions or a dataframe with consequetive cell counts (non-zero cells) up/down/left/right of the specified cell in a matrix
#' @export
side_count <- function(
    m,
    row_index = NULL,
    col_index = NULL,
    cell_res  = 480,
    max_dist  = 20000,
    # avg_count = TRUE,
    func      = "mean",
    as_df     = FALSE,
    wide      = FALSE,
    verbose   = FALSE
) {
  # avg_count logical. Whether the average number of cells should be returned or cell counts in each direction. Default is TRUE, returns mean cell count around cell.
  # m         = rmat
  # row_index = row_nest$data[[11]]$row
  # col_index = row_nest$data[[11]]$col
  # wide      = FALSE
  # verbose   = T
  # row_nest
  # m         = m
  # row_index = row_nest$data[[4]]$row
  # col_index = row_nest$data[[4]]$col
  # wide      = FALSE

  # get function
  func <- match.fun(func)

  # max count distance
  max_count <- floor(max_dist/cell_res)

  # ---- col (up/down) ----
  # vector of current matrix col
  vcol <- m[, col_index]

  # return index list c(up, down) indices, checks if cells are on the edge of raster
  updown <- check_cols(
    m         = m,
    row_index = row_index,
    col_index = col_index
    )

  # count of up/down cells before a 0, if no zeros occur, returns length of consecutive 1s
  up_count   <- check_length(
    vect      = rev(vcol[updown[[1]]]),
    max_count = max_count
    )

  down_count <- check_length(
    vect      = vcol[updown[[2]]],
    max_count = max_count
    )

  # ---- row (left/right) ----
  # vector of current matrix row
  vrow <- m[row_index, ]

  # return index list c(left, right) indices, checks if cells are on the edge of raster
  leftright <- check_rows(
    m         = m,
    row_index = row_index,
    col_index = col_index
  )

  # count of up/down cells before a 0, if no zeros occur, returns length of consecutive 1s
  left_count  <- check_length(
    vect      = rev(vrow[ leftright[[1]]]),
    max_count = max_count
    )
  right_count <- check_length(
    vect      = vrow[leftright[[2]]],
    max_count = max_count
    )
  # check_length(
  #   vect      = c(1, 1, 1, NA, 0, NA, 0, 0, 0 , 0, NA, NA),
  #   # vect      = c(1, 1, 0, 0, 1, NA, 0, NA),
  #   max_count = max_count
  # )

  # count of cells on up/down/left/right side of cell before a 0 occurs
  side_df <- data.frame(
    row         = row_index,
    col         = col_index,
    direction   = c("up", "down", "left", "right"),
    cell_count  = c(up_count, down_count, left_count, right_count)
    # cell_count  = c(55, 3, 2, 98)
  ) %>%
    na.omit()

  # if any cell counts are above max count distance, convert to max count
  if(length(which(side_df$cell_count > max_count)) != 0) {

    if(verbose == TRUE) {
      message(paste0("updating cell counts to max: ", max_count))
    }

    side_df[side_df$cell_count > max_count, ]$cell_count <- max_count

  }

  # return average cell count around cell
  if(as_df == FALSE) {

    # calculate average in all directions
   fcell_count <-  func(side_df$cell_count, na.rm = T)

   return(fcell_count)

  } else {

    # if wide data requested
    if(wide == TRUE) {

      # count of cells on right and left side of cell before a 0 occurs
      side_df <-
        side_df %>%
        tidyr::pivot_wider(
          names_from  = direction,
          values_from = cell_count
        )

      return(side_df)

    } else {

      # select relevant columns
      side_df <-
        side_df %>%
        dplyr::select(cell_count, direction)

      return(side_df)
      }
  }

}

  # run length encoding of up/down cells
  # up_rle    <- rle(rev(vcol[up_cells]))
  # down_rle  <- rle(vcol[down_cells])
  #
  # # count of up/down cells before a 0, if no zeros occur, returns length of consecutive 1s
  # up_count   <- check_zeros(rle_vect = up_rle)
  # down_count <- check_zeros(rle_vect = down_rle)

  # left_cells  <- 1:col_index
  # right_cells <- col_index:ncol(m)

  # count of up/down cells before a 0, if no zeros occur, returns length of consecutive 1s
  # left_count  <- check_length(vect = rev(vrow[left_cells]))
  # right_count <- check_length(vect = vrow[right_cells])

  # # run length encoding of left/right cells
  # left_rle   <- rle(rev(vrow[left_cells]))
  # right_rle  <- rle(vrow[right_cells])
  #
  # # count of left/right  cells before a 0, if no zeros occur, returns length of consecutive 1s
  # left_count   <- check_zeros(rle_vect = left_rle)
  # right_count  <- check_zeros(rle_vect = right_rle)

  # count of cells on right and left side of cell before a 0 occurs
  # side_df <-
  #   data.frame(
  #     row   = row_index,
  #     col   = col_index,
  #     up    = up_count,
  #     down  = down_count,
  #     left  = left_count,
  #     right = right_count
  #   )

# }

#' Calculate distances by cell counts
#'
#' @param indices_df dataframe of indices
#' @param cell_res numeric cell resolution
#' @param max_dist numeric max distance
#' @param func character R function. Either mean, min, max, or sum
#' @param na_remove logical. arguement passed to na.rm = in R func. Default is TRUE, remove NAs
#' @param verbose logical, print messages. Default is FALSE
#'
#' @return
#' @export
cell_dist <- function(
    indices_df,
    cell_res  = 480,
    max_dist  = 20000,
    func      = "mean",
    na_remove = TRUE,
    verbose   = FALSE
    ) {

  # match function to R function
  func <- match.fun(func)

  # calculate distance from cell counts
  cell_distance <-
    indices_df %>%
    dplyr::group_by(row, col) %>%
    dplyr::summarise(cell_count = func(cell_count, na.rm = na_remove)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      cell_res       = cell_res,
      # distance       = cell_count*cell_res
      distance       = dplyr::case_when(
        cell_count*cell_res > max_dist ~ max_dist,
        TRUE                           ~ cell_count*cell_res
      )
    )

  return(cell_distance)

}


dist_summary <- function(
    m,
    indices_df,
    cell_res  = 480,
    max_dist  = 20000,
    verbose   = FALSE
) {

# verbose = T
  # m = rmat
  # lw_crop
  # #
  # indices_df = idx2[[1]]  # diaganol direction to map over
  # row_nest$data[[33]]$row
  # row_nest$data[[33]]$col
  # side_count(
  #   m         = m,
  #   row_index = row_nest$data[[33]]$row,
  #   col_index = row_nest$data[[33]]$col,
  #   cell_res  = cell_res,
  #   max_dist  = max_dist,
  #   wide      = FALSE,
  #   verbose   = verbose
  # )
  # diagonal directions
  diag_dirs <- c("top_left", "top_right", "bottom_left", "bottom_right")
  #
  # row_nest$indices[[1]]
  # m
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
          cell_res  = cell_res,
          max_dist  = max_dist,
          wide      = FALSE,
          verbose   = verbose
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
            verbose   = verbose
          )
        }) %>%
          dplyr::bind_rows()
      ),
      diag_counts = purrr::map(
        .x = indices,
        .f = ~count_diag_dir(
          m            = m,
          diag_indices = .x,
          cell_res     = cell_res,
          max_dist     = max_dist,
          verbose      = verbose
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
      convert_factor = cell_res,
      # convert_factor = dplyr::case_when(
      #   grepl("_", direction) ~ cell_res*sqrt(2),
      #   TRUE                  ~ cell_res
      # ),
      # distance = cell_count*convert_factor
      distance = dplyr::case_when(
        cell_count*convert_factor > max_dist ~ max_dist,
        TRUE                                 ~ cell_count*convert_factor
      )
    ) %>%
    dplyr::group_by(row_id, row, col) %>%
    dplyr::summarise(
      cell_count = mean(cell_count, na.rm = T),
      distance   = mean(distance, na.rm = T)
    ) %>%
    dplyr::ungroup()

  return(row_dist)

}


# *****************************************************************************
# *****************************************************************************

#' Make fetch summary dstances
#' @description returns a dataframe with fetch lengths for a coastal area
#' @param r_path Path to binary land type raster TIF. Land and water values only.
#' @param r Path to binary land type raster TIF. Land and water values only.
#' @param max_dist numeric maximum distance (meters) to calculate fetch values out to. Default is 20000m (20km)
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
    r_path    = NULL,
    r         = NULL,
    max_dist  = 20000,
    ncores    = 10,
    verbose   = TRUE
) {
  # error if no path or raster are given
  if(is.null(r_path) & is.null(r)) {

    stop(paste0("Please enter a path to a binary landwater raster or a SpatRaster object"))

  }

  if(verbose == TRUE) {
    message(paste0("Calculating Fetch..."))
  }

  # r_path = landtype_files$full_path[1]
  # if no path to raster is given, read it in
  if(!is.null(r_path)) {

    r <- get_landwater(r_path = r_path)

  }
  # plot(r)
    # replace NA raster values w/ 0 for calcs
    # r <-  terra::classify(r, cbind(NA, 0))

    # r <-  terra::classify(r, cbind(NA, 1))
    # cell resolution for distance calcs
    cell_res <- terra::res(r)[1]

    # max count distance
    max_count <- floor(max_dist/(cell_res))
    # max_count <- floor(max_dist/(cell_res*sqrt(2)))

  # You can fill in the NA values using the focal function with the na.rm argument set to FALSE and pad to TRUE.

  # fill  NA values in a raster
  # na_fill <- function(x, i=5) {
  #   if( is.na(x)[i] ) {
  #     return( round(mean(x, na.rm=TRUE),0) )
  #   } else {
  #     return( round(x[i],0) )
  #   }
  # }

  # mapview::mapview(pts) + lw_r
  # pt_buffer <- data.frame(
  #   # lng = 625390,
  #   # lat = 3255040
  #   lng = 625390,
  #   lat = 3275040
  #   # lng = 730390,
  #   # lat = 3240040
  # ) %>%
  #   sf::st_as_sf(coords = c("lng", "lat"), crs = 26915) %>%
  #   # sf::st_buffer(30000) %>%
  #   sf::st_buffer(65000) %>%
  #   sf::st_bbox() %>%
  #   sf::st_as_sfc()
  # # %>%
  #   # terra::vect()
  #
  # mapview::mapview(pt_buffer)
  # lw_crop <-
  #   r %>%
  #   terra::crop( terra::vect(pt_buffer)) %>%
  #   terra::mask( terra::vect(pt_buffer))
  #   # terra::crop(pt_buffer) %>%
  #   # terra::mask(pt_buffer)
  # lw_crop
  # plot(lw_crop)
  # r2 <- raster::focal( raster::raster(r),  w      = matrix(1,5,5),  fun    = na_fill, pad    = T, na.rm  = F, NAonly = T )

  # lw_crop <-  terra::classify(lw_crop, cbind(NA, 1))
  # plot(lw_crop)
  # lw_focal <- terra::focal(r, 297, "max", na.policy="only", na.rm=TRUE)

  # make landwater raster a wide matrix
  rmat <- terra::as.matrix(r, wide = TRUE)
  # rmat <- terra::as.matrix(lw_crop, wide = TRUE)

  # get indices of water points
  idx <-
    which(rmat == 1, arr.ind = T) %>%
    as.data.frame() %>%
    dplyr::arrange(row)

  # verbose = T
  if(verbose == TRUE) {
    message(paste0("Calculating North/South/East/West distances"))
  }
  # system.time(

  # side cell totals list
  side_lst <- side_summary(
    m          = rmat,
    indices_df = idx,
    func       = "mean",
    verbose    = FALSE
  )

  # )

  if(verbose == TRUE) {
    message(paste0("Calculating diagonal distances"))
  }


  # system.time(

  # diagonal totals list
  diag_lst <- diag_summary(
      m          = rmat,
      indices_df = idx,
      func       = "mean",
      max_count  = max_count,
      verbose    = FALSE
    )

  # )

  # add mean diagonal cell counts
  idx$diag_cell_count <- unlist(diag_lst)

  # add mean diagonal cell counts
  idx$side_cell_count <- unlist(side_lst)

  # pivot data to long w/ single column for cell counts
  idx <-
    idx %>%
    tidyr::pivot_longer(
      cols      = c(diag_cell_count, side_cell_count),
      names_to  = "direction",
      values_to = "cell_count"
      )

  # calcualte cell distances from cell counts
  dist_mat <- cell_dist(
    indices_df = idx,
    cell_res   = cell_res,
    max_dist   = max_dist,
    func       = "mean",
    na_remove  = TRUE
  )

  # create fetch raster from gridded matrix
  fetch_r <-
    terra::rast(
      x = matrix(
        data = dplyr::arrange(
          dplyr::left_join(
            expand.grid(
              row = 1:terra::nrow(r),
              col = 1:terra::ncol(r)
              # row = 1:terra::nrow(lw_crop),
              # col = 1:terra::ncol(lw_crop)
            ),
            dplyr::select(dist_mat, row, col, distance),
            by = c("row", "col")
          ),
          row
        )$distance,
        nrow  = terra::nrow(r),
        ncol  = terra::ncol(r),
        # nrow  = terra::nrow(lw_crop),
        # ncol  = terra::ncol(lw_crop),
        byrow = TRUE
      ),
      crs    = terra::crs(r),
      extent = terra::ext(r)
      # crs    = terra::crs(lw_crop),
      # extent = terra::ext(lw_crop)
    )

  return(fetch_r)
  # plot(fetch_r)
  # plot(fetch_r2)
#   plot(fetch_r3)
#
#   # Reclassify fetch values
#   fetch_mat_shallow <- matrix(
#     c(0, 1000, 1,
#       1000, 5000, .5,
#       5000, 20001, .2),
#     ncol=3,
#     byrow = T
#   )
#
#
#   # Reclassify fetch values for deep water
#   fetch_mat_deep <- matrix(
#     c(0,     5000,   1,          # deep water fetch bins
#       5000,  10000, .5,
#       10000, 20001, .2),
#     ncol=3, byrow = T
#   )
#
#   # calculate CV SI
#   # Fetch shallow/deep SI
#   fetch_shallow_cv            <- terra::classify(fetch_r3, fetch_mat_shallow)
#   fetch_deep_cv               <- terra::classify(fetch_r3, fetch_mat_deep)
#   plot(fetch_shallow_cv)
#   plot(fetch_deep_cv)
# # tmp1 <- raster::raster(fetch_r)
# # tmp2 <- raster::raster(fetch_r2)
# tmp3 <- raster::raster(fetch_r3)
# oldfetch <- raster::raster("C:/Users/angus/OneDrive/Desktop/github/cpra_orz/data/fetch/fetch_raster_S07_03_03.tif")
# mapview::mapview(tmp3) + oldfetch + tmp1
# plot(lw)
# mapview::mapview(tmp1) + tmp2 + tmp3
# fetch_r %>%
  # raster::raster() %>%
  # mapview::mapview()

}

#   indices_df <-
#     indices_lst %>%
#     dplyr::bind_rows() %>%
#     dplyr::arrange(row, col) %>%
#     dplyr::group_by(row, col, direction) %>%
#     dplyr::group_split()
#
#   verbose = T
#   indices_df[[1]]$row
#
#   # lapply over each diaganol direction
#   diag_lst <- lapply(1:length(indices_df), function(x) {
#
#     # count on the diagonals
#     count_diag(
#       m            = m,
#       # diag_indices = indices_df[[x]],
#       row_vect     =  indices_df[[x]]$diag_row,
#       col_vect     =  indices_df[[x]]$diag_col,
#       as_df        = TRUE,
#       cell_res     = cell_res,
#       max_dist     = max_dist,
#       verbose      = verbose
#     ) %>%
#       dplyr::mutate(
#         direction = indices_df[[x]]$direction[1],
#         row = indices_df[[x]]$row[1],
#         col = indices_df[[x]]$col[1]
#       )
#   }) %>%
#     dplyr::bind_rows()
#
#   # get indices of water points
#   idx2 <-
#     which(rmat == 1, arr.ind = T) %>%
#     as.data.frame() %>%
#     dplyr::arrange(row) %>%
#     dplyr::group_by(row) %>%
#     dplyr::group_split()
#
#   # **************************
#   # **************************
#
#
#   # max(row_dist$distance)
# # indices_df <- idx[[80]]
#   message(paste0("Calculating fetch distances..."))
#
#   # ncores = 13
#   cl    <- parallel::makeCluster(ncores) #not to overload your computer
#
#   # register parallel backend
#   doParallel::registerDoParallel(cl)
#
#   # run a loop using a parallel backend. ~2x faster
#   fetch_loop <- foreach::foreach(i = 1:length(idx),
#                                  .combine  = "rbind",
#                                  .export   = c('cell_res', 'check_cols', 'check_rows',
#                                                'check_length', 'side_count', 'index_diag',
#                                                'count_diag_dir', 'count_diag' ,'dist_summary'),
#                                  .packages = c("dplyr", "tidyr", "purrr")) %dopar% {
#
#                                   row_dist <- dist_summary(
#                                     m          = rmat,
#                                     indices_df = idx[[i]],
#                                     cell_res   = cell_res,
#                                     max_dist   = max_dist
#                                     )
#                                   row_dist
#
#                                  }
#
#   # stop extra workers
#   doParallel::stopImplicitCluster()
#   parallel::stopCluster(cl)
#
#   # **************************
#   # **************************
#
#   # create fetch raster from gridded matrix
#   fetch_r <-
#     terra::rast(
#       x = matrix(
#         data = dplyr::arrange(
#           dplyr::left_join(
#             expand.grid(
#               row = 1:terra::nrow(r),
#               col = 1:terra::ncol(r)
#               # row = 1:terra::nrow(lw_crop),
#               # col = 1:terra::ncol(lw_crop)
#             ),
#             dplyr::select(fetch_loop, row, col, distance),
#             by = c("row", "col")
#           ),
#           row
#         )$distance,
#         nrow  = terra::nrow(r),
#         ncol  = terra::ncol(r),
#         # nrow  = terra::nrow(lw_crop),
#         # ncol  = terra::ncol(lw_crop),
#         byrow = TRUE
#       ),
#       crs    = terra::crs(r),
#       extent = terra::ext(r)
#       # crs    = terra::crs(lw_crop),
#       # extent = terra::ext(lw_crop)
#     )
#   # fetchy <- raster::raster(fetch_r)
#   # mapview::mapview(fetchy)
#
#   return(fetch_r)

# }
  # **************************
  # **************************


  # create grid matrix to convert to raster
  # grid_df <-  expand.grid(
  #   row = 1:terra::nrow(lw_crop),
  #   col = 1:terra::ncol(lw_crop)
  #   ) %>%
  #   dplyr::left_join(
  #     dplyr::select(fetch_loop, row, col, distance),
  #     by = c("row", "col")
  #   ) %>%
  #   dplyr::arrange(row)
  # expand.grid(
  #   row = 1:terra::nrow(lw_crop),
  #   col = 1:terra::ncol(lw_crop)
  # )
  # dplyr::arrange(
  #   dplyr::left_join(
  #     expand.grid(
  #       row = 1:terra::nrow(lw_crop),
  #       col = 1:terra::ncol(lw_crop)
  #       ),
  #     dplyr::select(fetch_loop, row, col, distance),
  #     by = c("row", "col")
  #     ),
  #   row
  #   )$distance
  #
  # create grid matrix to convert to raster
  # grid_mat <- matrix(
  #   data = dplyr::arrange(
  #     dplyr::left_join(
  #       expand.grid(
  #         row = 1:terra::nrow(lw_crop),
  #         col = 1:terra::ncol(lw_crop)
  #         ),
  #       dplyr::select(fetch_loop, row, col, distance),
  #       by = c("row", "col")
  #       ),
  #     row
  #     )$distance,
  #   # data = grid_df$distance,
  #   # nrow  = terra::nrow(r),
  #   # ncol  = terra::ncol(r),
  #   nrow  = terra::nrow(lw_crop),
  #   ncol  = terra::ncol(lw_crop),
  #   byrow = TRUE
  #   )

  # # replace water cells with mean fetch values
  # rmat2[which(rmat2 == 1, arr.ind = F)] <- fetch_loop$distance
  #
  # # fetch_loop$row
# rmat3 <- rmat
#   # loop over each row in dataframe and replace old water matrix values with mean fetch distances
#   for (j in 1:nrow(fetch_loop)) {
#
#     message(paste0("Row: ", fetch_loop$row[j], " col: ", fetch_loop$col[j]))
#     rmat3[fetch_loop$row[j], fetch_loop$col[j]]  <- fetch_loop$distance[j]
#
#   }

  # # replace 0 values (land) with NA
  # rmat3[which(rmat3 == 0)] <- NA


  # replace 0 values (land) with NA
  # rmat[which(rmat == 0)] <- NA

# }
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
