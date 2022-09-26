
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

#' Calculate percent of cell covered by land SI values (SI 5)
#' @param x numeric value representing percent of land (pct_land)
#' @param ...
#' @return numeric SI 5 values
#' @export
si5_func <- function(x, ...) {
  -x + 1.0
}

#' Calculate sediment deposition SI values (SI 6)
#' @param x numeric value representing sediment deposition in millimeters (sedim)
#' @param ...
#' @return numeric SI 6 values
#' @export
si6_sed_dep_func <- function(x, ...) {
  ifelse(x < 35, 1,
         ifelse(x >= 35 & x < 40, (-0.2*x) + 8.0,
                ifelse(x >= 40, 0, 0)
         )
  )
}


#' Sediment deposition SI values
#' @param x numeric value for sediment deposition (mm)
#' @param ...
#' @return
#' @export
si_sedim_func <- function(x, ...) { #
  ifelse(x == 0, 1,
         ifelse(x >= 0 & x < 40,  1 + (-0.0225*x),
                ifelse(x >= 40, 0.1, 0
                       )
                )
         )
}

#' Calculate shallow water fetch SI values
#' @param x numeric value representing fetch distance in meters
#' @return numeric shallow water fetch SI values
#' @export
si_fetch_shallow <- function(x) {

  # Reclassify fetch values
  ifelse(x >= 0 & x <= 1000, 1,
         ifelse(x > 1000 & x <= 5000, 0.5,
                ifelse(x > 5000, 0.2, 0)
                )
         )

  # Reclassify fetch values
  # fetch_mat_shallow <- matrix(
  #   c(0, 1000, 1,
  #     1000, 5000, .5,
  #     5000, 20001, .2),
  #   ncol=3, byrow = T
  # )
}
#' Calculate road buffer SI values
#' @param x numeric value representing distance from roads (kilometers)
#' @return numeric road buffer SI values
#' @export
si_roads <- function(x) {
  # Reclassify fetch values for deep water
  ifelse(x >= 0 & x <= 5, 1,
         ifelse(x > 5 & x <= 10, 0.5,
                ifelse(x > 10 & x <= 20, 0.2,
                       ifelse(x > 20, 0.1, 0)
                )
         )
  )

  # Reclassify road buffers values
  # road_mat <- matrix(
  #   c(0, 5, 1,
  #     5, 10, 0.5,
  #     10, 20, 0.2,
  #     20, 30, 0.1),
  #   ncol=3, byrow = T
  # )

}

#' Calculate deep water fetch SI values
#' @param x numeric value representing fetch distance in meters
#' @return numeric deep water fetch SI values
#' @export
si_fetch_deep <- function(x) {
  # Reclassify fetch values for deep water
  ifelse(x >= 0 & x <= 5000, 1,
         ifelse(x > 5000 & x <= 10000, 0.5,
                ifelse(x > 10000, 0.2, 0)
         )
  )

  # # Reclassify fetch values for deep water
  # fetch_mat_deep <- matrix(
  #   c(0,     5000,   1,          # deep water fetch bins
  #     5000,  10000, .5,
  #     10000, 20001, .2),
  #   ncol=3, byrow = T
  # )

}
si_fetch <- function(
    r,
    depth_zone = "shallow",
    verbose    = TRUE
) {

  # Calculate shallow water fetch SI
  if(depth_zone == "shallow") {

    if(verbose == TRUE) {
      message(paste0("Calculating shallow water SI values..."))
    }

    # calculate shallow water fetch SI values
    fetch_si <-
      r %>%
      terra::app(fun = si_fetch_shallow) %>%
      stats::setNames(c(gsub("_fetch", "_si_fetch_shallow", names(r))))

  # Calculate deep water fetch SI
  } else if(depth_zone == "deep") {

    if(verbose == TRUE) {
      message(paste0("Calculating deep water SI values..."))
    }

    # calculate deep water fetch SI values
    fetch_si <-
      r %>%
      terra::app(fun = si_fetch_deep) %>%
      stats::setNames(c(gsub("_fetch", "_si_fetch_deep", names(r))))

  # If anything other than "shallow" or "deep", then calculate shallow water fetch SI
  } else {

    if(verbose == TRUE) {
      message(paste0("Depth zone: ", depth_zone, " invalid"))
      message(paste0("Calculating shallow water SI values..."))
    }

    # calculate shallow water fetch SI values
    fetch_si <-
      r %>%
      terra::app(fun = si_fetch_shallow) %>%
      stats::setNames(c(gsub("_fetch", "_si_fetch_shallow", names(r))))

  }

  return(fetch_si)

}

calc_fetch_si <- function(stack_lst, verbose = TRUE) {

  # water depths
  water_depths <- c("shallow", "deep")

  # loop through each fetch raster
  fetch_si <- lapply(1:length(stack_lst), function(i){

    fr <- stack_lst[[i]]

    if(verbose == TRUE) {
      message(paste0(names(fr)))
    }

    # loop through each fetch raster and calculate shallow/deep SI
    si_lst <- lapply(1:length(water_depths), function(z){

      # message(paste0(names(fr), " - ", water_depths[[z]]))
      si_fetch(
        r          = fr,
        depth_zone = water_depths[[z]],
        verbose    = verbose
      )

    }) %>%
      terra::rast()

  })

  # get layers names
  stk_names <- lapply(1:length(stack_lst), function(k){
    names(stack_lst[[k]])
  }) %>%
    unlist()

  # name stack by layer names and make sds
  fetch_si <-
    fetch_si %>%
    stats::setNames(c(stk_names)) %>%
    terra::sds()

  return(fetch_si)
}

calc_sedim_si <- function(
    stack_lst,
    verbose    = TRUE
) {

  # loop through each sedim raster
  sedim_si <- lapply(1:length(stack_lst), function(i){

    sed_dep <- stack_lst[[i]]

    if(verbose == TRUE) {
      message(paste0(names(stack_lst[[i]])))
    }

    # apply SI function to mean sediment deposition
    sed_dep <-
      stack_lst[[i]] %>%
      terra::app(fun = si_sedim_func) %>%
      stats::setNames(c(gsub("_sediment_deposition", "_si_sediment_deposition", names(stack_lst[[i]]))))

  })

  # get layers names
  stk_names <- lapply(1:length(stack_lst), function(k){
    gsub("_sediment_deposition", "_si_sediment_deposition", names(stack_lst[[k]]))
  }) %>%
    unlist()

  # name stack by layer names and make sds
  sedim_si <-
    sedim_si %>%
    stats::setNames(c(stk_names)) %>%
    terra::sds()

  return(sedim_si)

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

depth_zone <- function(
    depth_rast,
    water_depth = "shallow"
    ) {

  if(water_depth == "shallow") {

    # depth_mask
    # mask for shallow waters
    depth_mask <- terra::setValues(
      depth_rast, ifelse(terra::values(depth_rast) == 2, 1, NA)
    )

  } else {

    # mask for deep waters
    depth_mask <- terra::setValues(
      depth_rast, ifelse(terra::values(depth_rast) == 3, 1, NA)
    )

  }

  return(depth_mask)
}

#   plot(depth_rast)
#   plot(shallow_mask)
#   plot(deep_mask)
#
#   fetch_shallow_mask <-
#     fetch_shallow %>%
#     terra::mask(shallow_mask, inverse = T)
#   plot(fetch_shallow)
#   plot(fetch_shallow_mask)
#
#   fetch_deep_mask <-
#     fetch_deep %>%
#     terra::mask(deep_mask, inverse = F)
#   plot(fetch_deep)
#   plot(fetch_deep_mask)
#
#   tmp1 <- raster::raster(fetch_shallow_mask)
#   tmp2 <- raster::raster(fetch_deep_mask)
#   mapview::mapview(tmp1) + tmp2
#
# }

