library(ggplot2)
library(rasterVis)
theme_set(theme_bw())
land <-
  land_path %>%
  get_landwater() %>%
  get_land(as_polygon = T, as_sf = T)

r <- final_stk$S07_G510_new_FWOA_12_12$S07_G510_new_FWOA_04_04_si_ov
s1 <- terra::project(s1, "epsg:4326")
## With raster
# gplot(s1) +
#   geom_tile(aes(fill = value))
#   scale_fill_gradient(low = 'white', high = 'blue') +
#   coord_equal()

make_plot <- function(
    stk,
    stk_name,
    land,
    verbose = TRUE
    ) {

  stk <- final_stk$S07_G510_new_FWOA_12_12
  # r <- final_stk$S07_G510_new_FWOA_12_12$S07_G510_new_FWOA_04_04_si_ov
  # r <- terra::project(r, "epsg:4326")

  # extract stack name
  # stk_name <- sub("(\\d)[^0-9]+$", "\\1", names(stk))

  # path to landwater TIF
  land_path <-
    model_dirs %>%
    dplyr::filter(type == "lndtyp") %>%
    dplyr::filter(file_name == stk_name) %>%
    .$full_path

  # Make land polygon for plotting
  land_poly <-
    land_path %>%
    get_landwater() %>%
    get_land(
      as_polygon = TRUE,
      as_sf      = TRUE
      )

  # land_paths$file_name
  # land_simple <-
  #   land_poly %>%
  #   rmapshaper::ms_simplify(keep = 0.25)

  # mapview::npts(land_poly)
  # mapview::npts(land_simple)
  # plot(land_simple$geometry)
  # plot(land_poly$geometry)
  # terra::nlyr(stk)

  # i  <- 1
  # loop through each layer and output plot
  # for(i in 1:terra::nlyr(stk)) {
  #
  #   message(paste0(i, "/", terra::nlyr(stk)))
  #
  #   # convert raster to dataframe for plotting
  #   r_df <-
  #     r %>%
  #     as.data.frame(xy = TRUE) %>%
  #     stats::setNames(c("x", "y", "values"))
  #
  #   # plot raster
  #   ggplot2::ggplot() +
  #     # ggplot2::geom_raster(data = r_df, aes(x = x, y = y, fill = values)) +
  #     ggplot2::geom_tile(data = r_df, aes(x = x, y = y, fill = values)) +
  #     ggplot2::geom_sf(data = land_poly, fill = "grey", col = "transparent") +
  #     # ggplot2::geom_sf(data = land_simple, fill = "grey", col = "transparent") +
  #     # ggplot2::geom_sf(data = land_simple, fill = "grey") +
  #     # geom_raster(data = na.omit(r_df) , aes(x=x, y=y, fill = values)) +
  #     ggplot2::coord_sf() +
  #     ggthemes::theme_map()
  #     viridis::scale_fill_viridis(direction = -1, option = "H",limits = c(0, 1.0)) +
  #     ggplot2::labs(
  #       # title     = "",
  #       title     = paste0(names(r)),
  #       fill      = "",
  #       y         = "",
  #       x         = ""
  #     ) +
  #     ggthemes::theme_map() +
  #     ggplot2::theme(
  #       axis.ticks      = element_blank(),          axis.text.x       = element_blank(),
  #       axis.text.y     = element_blank(),          plot.title        = element_text(size = 20, face = "bold"),
  #       plot.subtitle   = element_text(size = 16),  plot.caption      = element_text(size = 12),
  #       legend.text     = element_text(size = 14),  legend.background = element_blank(),
  #       legend.position = c(0.0, 0.1),              legend.title      = element_text(size = 14, face = "bold")
  #     )
  #
  # }

}

get_plot <- function(
    r,
    # stk_name,
    land_shp,
    pal_limits    = c(0, 1.0),
    pal_option    = "H",
    pal_direction = -1,
    raster_type   = "aoc",
    verbose       = TRUE
) {
  # land_shp <- land_poly
  # r <- stk$S07_G510_new_FWOA_12_12_si_fetch_shallow
  # r <- stk$S07_G510_new_FWOA_12_12_aoc_combine_mean
  r <- stk$
  # names(stk)
  layer_name <- sub(".*(\\d)", "",     names(r))
  layer_name
  # grepl("fetch", layer_name)
  # grepl("road", layer_name)
  # grepl("sd", layer_name)
  # grep("sd", layer_name, value = T)


  # stk <- final_stk$S07_G510_new_FWOA_12_12
  # r <- final_stk$S07_G510_new_FWOA_12_12$S07_G510_new_FWOA_04_04_si_ov
  # r <- terra::project(r, "epsg:4326")

  # extract stack name
  # stk_name <- sub("(\\d)[^0-9]+$", "\\1", names(stk))


  message(paste0(i, "/", terra::nlyr(stk)))

  rmask <-
    r %>%
    terra::mask(terra::vect(land_shp), inverse = T)

  # if(!grepl("roads", layer_name) == TRUE) {

    # r <-
    #   r %>%
    #   terra::mask(terra::vect(land_shp), inverse = T)

  # }
  depth_cat <- raster::ratify(depth)

  lvls              <- levels(depth_cat)[[1]]

  lvls <- lvls %>% filter(ID %in% c(1.00, 2.00, 3.00))
  lvls$status       <- c('depth <= 2 ft',  '2 < depth <= 5 ft', 'depth > 5 ft')
  levels(depth_cat)  <- lvls

  # mapview(depth)
  #  3 = deep, 2 = shallow, 1 = too shallow
  # create xy dataframes of rasters for plotting w/ ggplot2
  depth_df <- depth_cat %>%
    as.data.frame(xy = TRUE) %>%
    setNames(c("x", "y", "layer_status")) %>%
    mutate(
      layer_status = factor(layer_status)
    ) %>%
    setNames(c("long","lat", "layer_status")) %>%
    mutate(
      title        =  "Depth"
    )

  depth_df$layer_status <- factor(depth_df$layer_status, levels =c('depth <= 2 ft',  '2 < depth <= 5 ft', 'depth > 5 ft'))
  # rmask <-
  #   r %>%
  #   terra::mask(terra::vect(land_shp), inverse = T)

  # convert raster to dataframe for plotting
  r_df <-
    r %>%
    # rmask %>%
    as.data.frame(xy = TRUE) %>%
    stats::setNames(c("x", "y", "values"))

  if(grepl("sd", layer_name) == TRUE) {
    # vv <- (terra::values(r))
    # vv <- max(na.omit(terra::values(r)))
    # plot(land_shp$geometry)

    # plot raster
    # rast_plot <-
      ggplot2::ggplot() +
      ggplot2::geom_tile(
        data = r_df,
        aes(x = x, y = y, fill = values)
      ) +
      ggplot2::coord_sf() +
      viridis::scale_fill_viridis(
        direction = -1,
        option    = "D",
        limits    =  c(0, max(na.omit(terra::values(r))))
        ) +
      ggthemes::theme_map() +
      ggplot2::labs(
        title     = "",
        # title     = paste0(names(r)),
        fill      = "",
        y         = "",
        x         = ""
      )

  } else if(grepl("roads", layer_name) == TRUE) {
# layer_name
#     names(r)
    # roads buffer col
    # ggplot2::scale_fill_manual(values = c("yellow", "darkorange",  "red", "darkred")) +
    unique(r_df$values)
    # plot raster
    # rast_plot <-
      ggplot2::ggplot() +
      ggplot2::geom_tile(
        data = r_df,
        aes(x = x, y = y, fill = values)
      ) +
      ggplot2::coord_equal()
      # ggplot2::coord_sf()
      viridis::scale_fill_viridis(
        direction = -1,
        option    = "H",
        limits    = c(0, 1)
      ) +
      ggthemes::theme_map() +
      ggplot2::labs(
        title     = "",
        # title     = paste0(names(r)),
        fill      = "",
        y         = "",
        x         = ""
      )



  } else {
    # plot raster
    # rast_plot <-
    ggplot2::ggplot() +
      ggplot2::geom_tile(
        data = r_df,
        aes(x = x, y = y, fill = values)
      ) +
      # ggplot2::geom_sf(
      #   data = land_shp,
      #   fill  = "black",
      #   col   = "transparent",
      #   alpha = 0.2,
      #   size  = 0.1
      # ) +
      ggplot2::coord_sf() +
      viridis::scale_fill_viridis(
        direction = -1,
        option    = "H",
        limits    = c(0, 1)
      ) +
      ggthemes::theme_map() +
      ggplot2::labs(
        title     = "",
        # title     = paste0(names(r)),
        fill      = "",
        y         = "",
        x         = ""
      )

  }
  # plot raster
  rast_plot <-
    ggplot2::ggplot() +
    ggplot2::geom_tile(
      data = r_df,
      aes(x = x, y = y, fill = values)
      ) +
    ggplot2::geom_sf(
      data = land_shp,
      fill  = "black",
      col   = "transparent",
      alpha = 0.2,
      size  = 0.1
      ) +
    ggplot2::coord_sf() +
    ggthemes::theme_map() +
    ggplot2::labs(
      title     = "",
      # title     = paste0(names(r)),
      fill      = "",
      y         = "",
      x         = ""
    )

  # roads buffer col
  # scale_fill_manual(values = c("yellow", "darkorange",  "red", "darkred")) +

  # water depth
  # scale_fill_manual(values = c("red", "gold",  "dodgerblue3")) +

  # fetch cols
  # scale_fill_manual(values = c ("#C7E9C0", "#41AB5D","#00441B")) +
  # scale_fill_gradientn(colours = (RColorBrewer::brewer.pal(9, "Greens")), limits = c(0, 1))

  # check if standard deviation raster
  if(grepl("sd", names(r)) == TRUE) {



  } else {
    rast_plot +
      viridis::scale_fill_viridis(
        direction = pal_direction,
        option    = pal_option,
        limits    = pal_limits
        )

  }
    # viridis::scale_fill_viridis(
    #   direction = pal_direction,
    #   option    = pal_option,
    #   limits    = pal_limits
    #   )
    # ggspatial::annotation_scale(
    #   text_cex = 1.1,  pad_x = unit(2, "cm"), pad_y = unit(0.1, "cm"),
    #   line_width = 2,text_face = "bold",  tick_height = 0.8,
    #   height = unit(0.2, "cm"), aes(location  = "br", style = "ticks"))
  # +
  #   ggplot2::theme(
  #     axis.ticks      = element_blank(),          axis.text.x       = element_blank(),
  #     axis.text.y     = element_blank(),          plot.title        = element_text(size = 20, face = "bold"),
  #     plot.subtitle   = element_text(size = 16),  plot.caption      = element_text(size = 12),
  #     legend.text     = element_text(size = 14),  legend.background = element_blank(),
  #     legend.position = c(0.0, 0.1),              legend.title      = element_text(size = 14, face = "bold")
  #   )

  # }

}
r_df <-
  s1 %>%
  as.data.frame(xy = TRUE) %>%
  stats::setNames(c("x", "y", "values"))
library(tidyverse)
library(scales)
ggplot() +
  geom_raster(data = r_df, aes(x=x, y=y, fill = values)) +
  # geom_raster(data = na.omit(r_df) , aes(x=x, y=y, fill = values)) +
  coord_equal() +
  viridis::scale_fill_viridis(direction = -1, option = "H",limits = c(0, 1.0)) +
    labs(
      title     = "",
      fill      = "",
      y         = "",
      x         = ""
    ) +
    ggthemes::theme_map() +
    theme(
      axis.ticks      = element_blank(),          axis.text.x       = element_blank(),
      axis.text.y     = element_blank(),          plot.title        = element_text(size = 20, face = "bold"),
      plot.subtitle   = element_text(size = 16),  plot.caption      = element_text(size = 12),
      legend.text     = element_text(size = 14),  legend.background = element_blank(),
      legend.position = c(0.0, 0.1),              legend.title      = element_text(size = 14, face = "bold")
    )

    # scale_fill_gradient2(
    #   low  = ("red"),
    #   mid  = "white",
    #   high = ("blue")
    # )
    # scale_fill_gradient(low = 'white', high = 'blue')
