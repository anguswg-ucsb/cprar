
#   # Reclassify fetch values
  # fetch_mat_shallow <- matrix(
  #   c(0, 1000, 1,
  #     1000, 5000, .5,
  #     5000, 20001, .2),
  #   ncol=3,
  #   byrow = T
  # )

#   # calculate CV SI
#   # Fetch shallow/deep SI
#   fetch_shallow_cv            <- terra::classify(fetch_r, fetch_mat_shallow)
#   fetch_deep_cv               <- terra::classify(fetch_r, fetch_mat_deep)
#   plot(fetch_shallow_cv)
#   plot(fetch_deep_cv)
# tmp1 <- raster::raster(fetch_r)
# oldfetch <- raster::raster("C:/Users/angus/OneDrive/Desktop/github/cpra_orz/data/fetch/fetch_raster_S07_03_03.tif")
# tmp_crop <- raster::raster(lw_crop)
# mapview::mapview(tmp1) + oldfetch +tmp_crop



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
get_fetch_si <- function(r) {

  # fetch_stk$S07_G510_new_FWOA_32_32_fetch$S07_G510_new_FWOA_32_32_fetch %>% plot()
  r <- fetch_stk$S07_G510_new_FWOA_32_32_fetch$S07_G510_new_FWOA_32_32_fetch

  # calculate shallow water fetch SI values
  fetch_shallow <- terra::app(r, fun = si_fetch_shallow)

  # calculate deep water fetch SI values
  fetch_deep    <- terra::app(r, fun = si_fetch_deep)

  plot(r)
  plot(fetch_shallow)
  plot(fetch_deep)
  # rm(tmp1, tmp2, fetch_shallow_cv, fetch_deep_cv)
  # tmp1 <- raster::raster(shallow)
  # tmp2 <- raster::raster(deep)
  # mapview::mapview(tmp1) +tmp2

}

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
