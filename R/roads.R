
# shp <- readRDS("C:/Users/angus/OneDrive/Desktop/github/cpra_orz/data/roads/road_buffer.rds") %>%
#   dplyr::select(-fid) %>%
#   terra::vect()
# terra::writeVector(shp, "D:/cpra_fixed/road_buffer.gpkg", overwrite = T)
# tmp <- terra::vect("D:/cpra_fixed/road_buffer.gpkg")

# ggplot() +
#   geom_sf(data = dplyr::filter(shp, buffer %in% c(20)), aes(fill =factor(buffer))) +
#   geom_sf(data = dplyr::filter(shp, buffer %in% c(10)), aes(fill =factor(buffer))) +
#   geom_sf(data = dplyr::filter(shp, buffer %in% c(5)), aes(fill =factor(buffer))) +
#   geom_sf(data = dplyr::filter(shp, buffer %in% c(2)), aes(fill =factor(buffer)))

make_road_raster <- function(
    shp_path,
    verbose = TRUE
    ) {


  if(verbose == TRUE) {
    message(paste0("Rasterizing road buffer polygons..."))
  }

  # read in shape file
  shp <- terra::vect(shp_path)

  # rasterize each road buffer
  road_lst <- lapply(1:length(shp), function(y) {
    road_r <- terra::rasterize(
      shp[y],
      make_grid(),
      "buffer"
    ) %>%
      stats::setNames(c(paste0("road_", shp$buffer[y], "km")))

  })
  # %>%
  #   terra::rast()

  # loop through each rasterized road layer and
  for (i in 1:(length(road_lst)-1)) {
    if(verbose == TRUE) {
      message(paste0("raster ", i, " covered by raster ", i+1))
    }

    if(i > 1) {

      road_cover <- terra::cover(tmp_lst[[i-1]] , road_lst[[i+1]])

      tmp_lst[[i]] <- road_cover

    } else {

      road_cover <- terra::cover(road_lst[[i]], road_lst[[i+1]])


      tmp_lst[[i]] <- road_cover

    }

  }

  # grab last item from list
  road_buff <-
    tmp_lst[[length(tmp_lst)]] %>%
    stats::setNames(c("layer"))

  # replacing NA's with 20km buffer value
  road_buff <-  terra::classify(road_buff, cbind(NA, 20))

  return(road_buff)

}


