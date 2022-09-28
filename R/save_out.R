save_out <- function(
    rasters,
    file_path,
    verbose = TRUE
    ) {

  # check if base folder exists, if not, create it
  if(!dir.exists(file_path) == TRUE) {

      dir.create(file_path)

  }

  # loop through each raster stack in SpatRasterDataset
  for(i in 1:length(rasters)) {


    if(verbose == TRUE) {
      message(paste0("Creating folder: ", names(rasters)[i]))
    }

    # create file directory for model run
    dir.create(paste0(file_path, "/", names(rasters)[i]))

    # model rasters
    stk <- rasters[[i]]

    # loop through each layer in Raster stack and safe out as a TIF
    for(z in 1:terra::nlyr(stk)) {


      if(verbose == TRUE) {
        message(paste0("Saving raster - ", names(stk[[z]]), ".tif", " - ", z, "/", terra::nlyr(stk)))
      }

      # save TIF files
      terra::writeRaster(
        stk[[z]],
        paste0(file_path, "/",  names(rasters)[i], "/", names(stk[[z]]), ".tif")
      )

      }

    }

}



