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
