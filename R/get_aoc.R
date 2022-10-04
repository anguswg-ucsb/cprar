#' Generate Alternative Oyster Cultch (AOC) Rasters
#' @param cv SpatRasterDataset of commericial viability (CV) SpatRasters
#' @param ov SpatRasterDataset of Oyster viability (OV) SpatRasters
#' @param verbose logical, whether to print messages or not. Default is TRUE, prints messages
#' @return SpatRasterDataset of AOC SpatRasters
#' @export
get_aoc <- function(
    cv,
    ov,
    verbose = TRUE
    ) {

  # message
  if(verbose == TRUE) {
    message(paste0("Calculating Alternative Oyster Cultch..."))
  }

  # loop over CV SpatRasterDatasets for each scenario/model run/year range
  aoc_stk <- lapply(1:length(cv), function(i) {

    if(verbose == TRUE) {
      message(paste0("Model: ", gsub("_si_fetch_shallow", "",  names(cv[[i]][[1]]))))
      message(paste0(i, "/", length(cv), " - Commercial viability: ", gsub("_si_fetch_shallow", "",  names(cv[[i]][[1]]))))
    }

    # shallow CV layer for given model version
    cv_shallow   <- cv[[i]][[grep('cv_shallow', names(cv[[i]]), value = T)]]

    # deep CV layer for given model version
    cv_deep      <- cv[[i]][[grep('cv_deep', names(cv[[i]]), value = T)]]

    # combined CV layer for given model version
    cv_combine   <- cv[[i]][[grep('cv', names(cv[[i]]), value = T)]]
    cv_combine   <- cv_combine[[!grepl('shallow|deep', names(cv_combine))]]
    # names(cv[[i]]) %in% paste0(gsub("_si_fetch_shallow", "",  names(cv[[i]][[1]])), "_cv")

    # SI OV years to calculate AOC for
    ov_years     <- ov[[i]][[grep('ov', names(ov[[i]]), value = T)]]

    # Loop over OV SpatRasterDatasets for each scenario/model run/year range
    aoc_rasts <- lapply(1:terra::nlyr(ov_years), function(z) {

      if(verbose == TRUE) {
        message(paste0( z, "/", terra::nlyr(ov_years), " - Oyster viability: ", gsub("_si_ov", "", names(ov_years[[z]]))))
      }

      # Shallow AOC
      aoc_shallow <- (ov_years[[z]] * cv_shallow)**(0.5)

      # Deep AOC
      aoc_deep    <- (ov_years[[z]] * cv_deep)**(0.5)

      # Deep AOC
      aoc_combine <- (ov_years[[z]] * cv_combine)**(0.5)

      # Stack and name AOC rasters
      aoc <-
        c(aoc_shallow, aoc_deep, aoc_combine) %>%
        stats::setNames(c(paste0(gsub("si_ov", "", names(.)), c("aoc_shallow", "aoc_deep", "aoc_combine"))))

    }) %>%
      terra::rast()

    if(verbose == TRUE) {
      message(paste0("--------------------------------------------------"))
    }

    aoc_rasts

  }) %>%
    terra::sds() %>%
    stats::setNames(c(paste0(gsub("ov", "aoc", names(ov)))))

  return(aoc_stk)

}

#' Calculate Mean of AOC values over periods
#' @param aoc SpatRasterDataset containing AOC SpatRasters
#' @param verbose logical, whether messages should print. Default is TRUE, prints messages
#' @return SpatRasterDataset of AOC means for each period
#' @export
get_aoc_mean <- function(
    aoc,
    verbose = TRUE
    ) {

  # aoc_stk
  if(verbose == TRUE) {
    message(paste0("Calculating AOC means..."))
  }

  means_stk <- lapply(1:length(aoc), function(i) {
    if(verbose == TRUE) {
      message(paste0(names(aoc)[i], " - ", i, "/", length(aoc)))
    }

    # shallow mean over period
    aoc_shallow_mean  <-
      aoc[[i]][[grep('shallow', names(aoc[[i]]), value = T)]] %>%
      terra::mean() %>%
      stats::setNames(c(paste0(names(aoc)[i], "_shallow_mean")))

    # deep mean over period
    aoc_deep_mean     <-
      aoc[[i]][[grep('deep', names(aoc[[i]]), value = T)]] %>%
      terra::mean() %>%
      stats::setNames(c(paste0(names(aoc)[i], "_deep_mean")))

    # combined shallow/deep mean over period
    aoc_combine_mean  <-
      aoc[[i]][[grep('combine', names(aoc[[i]]), value = T)]] %>%
      terra::mean() %>%
      stats::setNames(c(paste0(names(aoc)[i], "_combine_mean")))

    aoc_means <- c(aoc_shallow_mean, aoc_deep_mean, aoc_combine_mean)
    aoc_means

  }) %>%
    terra::sds() %>%
    stats::setNames(c(paste0(names(aoc), "_mean")))

  return(means_stk)

  }

#' Calculate Standard Deviation of AOC values over periods
#' @param aoc SpatRasterDataset containing AOC SpatRasters
#' @param verbose logical, whether messages should print. Default is TRUE, prints messages
#' @return SpatRasterDataset of AOC standard deviation for each period
#' @export
get_aoc_sd <- function(
    aoc,
    verbose = TRUE
) {

  if(verbose == TRUE) {
    message(paste0("Calculating AOC standard deviation..."))
  }

  sd_stk <- lapply(1:length(aoc), function(i) {
    if(verbose == TRUE) {
      message(paste0(names(aoc)[i], " - ", i, "/", length(aoc)))
    }

    # shallow std over period
    aoc_shallow_sd  <-
      aoc[[i]][[grep('shallow', names(aoc[[i]]), value = T)]] %>%
      terra::stdev() %>%
      stats::setNames(c(paste0(names(aoc)[i], "_shallow_sd")))

    # deep std over period
    aoc_deep_sd     <-
      aoc[[i]][[grep('deep', names(aoc[[i]]), value = T)]] %>%
      terra::stdev() %>%
      stats::setNames(c(paste0(names(aoc)[i], "_deep_sd")))

    # combined shallow/deep std over period
    aoc_combine_sd  <-
      aoc[[i]][[grep('combine', names(aoc[[i]]), value = T)]] %>%
      terra::stdev() %>%
      stats::setNames(c(paste0(names(aoc)[i], "_combine_sd")))

    aoc_sd <- c(aoc_shallow_sd, aoc_deep_sd, aoc_combine_sd)
    aoc_sd

  }) %>%
    terra::sds() %>%
    stats::setNames(c(paste0(names(aoc), "_sd")))

  return(sd_stk)

}

























