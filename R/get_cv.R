
#' Calculate Commercial viability layer
#' @param roads SpatRaster of roads buffers
#' @param fetch SpatRasterDataset of fetch rasters for each period/model run
#' @param sedim SpatRasterDataset of mean sediment deposition for each period/model run
#' @param depth SpatRasterDataset of binned water depths for each period/model run
#' @param verbose logical, whether messages should print. Default is TRUE, message print
#' @return SpatRasterDataset with each SpatRaster containing 7 layers representing commercial viability and SI inputs
#' @export
get_cv <- function(
    roads,
    fetch,
    sedim,
    depth,
    verbose = TRUE
    ){

  if(verbose == TRUE) {
    message(paste0('Calculating commercial viability layers...'))
  }

  # loop over each scenario/model run/ year range and generate CV raster stack
  cv_stk <- lapply(1:length(fetch), function(i) {


    if(verbose == TRUE) {
      message(paste0(gsub("_si_fetch_shallow", "", names(fetch[[i]][[1]])), " - CV"))
    }

    # rename roads layer to match each CV layer
    tmp_roads <-
      roads %>%
      stats::setNames(c(gsub("_si_fetch_shallow", "_si_roads", names(fetch[[i]][[1]]))))

    # shallow water depth mask
    shallow_mask <- depth_zone(
      depth_rast  = depth[[i]],
      water_depth = "shallow"
    )

    # deep water depth mask
    deep_mask <- depth_zone(
      depth_rast  = depth[[i]],
      water_depth = "deep"
    )

    # shallow fetch SI raster
    fetch_shallow         <- fetch[[i]][[grep('shallow', names(fetch[[i]]), value = T)]]

    # deep fetch SI raster
    fetch_deep            <- fetch[[i]][[grep('deep', names(fetch[[i]]), value = T)]]

    # # calculate shallow/deep CV
    # shallow_cv            <-  (fetch_shallow*sedim[[i]]*roads)**(1/3)
    # deep_cv               <-  (fetch_deep*sedim[[i]])**(1/2)

    # calculate shallow CV
    shallow_cv <-
      ((fetch_shallow*sedim[[i]]*roads)**(1/3)) %>%
      stats::setNames(c(gsub("_si_fetch_shallow", "_cv_shallow", names(fetch[[i]][[1]])))) %>%
      terra::mask(shallow_mask)

    # calculate deep CV
    deep_cv <-
      ((fetch_deep*sedim[[i]])**(1/2)) %>%
      stats::setNames(c(gsub("_si_fetch_deep", "_cv_deep", names(fetch[[i]][[2]])))) %>%
      terra::mask(deep_mask)

    # combine shallow/deep CV
    cv <-
      shallow_cv %>%
      terra::cover(deep_cv) %>%
      stats::setNames(c(gsub("cv_shallow", "cv",  names(.))))

    # mask fetch shallow SI to shallow water
    fetch_shallow <-
      fetch_shallow %>%
      terra::mask(shallow_mask)

    # mask fetch deep SI to deep water
    fetch_deep <-
      fetch_deep %>%
      terra::mask(deep_mask)

    # put all CV layers into raster stack
    cv_lst <- c(fetch_shallow, fetch_deep, tmp_roads, sedim[[i]], shallow_cv, deep_cv, cv)

  })

  # get names for SpatRasterDataset
  stk_names <- gsub("fetch", "cv", names(fetch))

  # name stack by layer names and make sds
  cv_stk <-
    cv_stk %>%
    stats::setNames(c(stk_names)) %>%
    terra::sds()

  return(cv_stk)

}





