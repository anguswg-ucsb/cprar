make_plot <- function(
    stk,
    save_path,
    verbose = TRUE
    ) {

  # check if base folder exists, if not, create it
  if(!dir.exists(save_path) == TRUE) {

    dir.create(save_path)

  }

  # loop through all Raster stacks and plot individual layers
  for(i in 1:length(stk)) {

    if(verbose == TRUE) {
      message(paste0("Plotting ", names(stk)[i], " - ", i, "/", length(stk)))
    }

    # create file directory for model run
    dir.create(paste0(save_path, "/", names(stk)[i]))

    # path to landwater TIF
    land_path <-
      model_dirs %>%
      dplyr::filter(type == "lndtyp") %>%
      dplyr::filter(file_name == names(stk)[i]) %>%
      .$full_path

    # land polygon for masking
    land <-
      land_path %>%
      get_landwater() %>%
      get_land(
        as_polygon = TRUE,
        as_sf      = TRUE
        )

    # stack
    st <- stk[[i]]

    # loop through all layers in raster stack
    for(z in 1:terra::nlyr(st)) {

      if(verbose == TRUE) {
        message(paste0(names(st)[z], " - ", z, "/", terra::nlyr(st)))
      }

      # plot layer
      layer_plot <- get_plot(
        r        = st[[z]],
        land_shp = land,
        verbose  = TRUE
      )

      # save out plot
      ggplot2::ggsave(
        filename = paste0(save_path, "/", names(stk)[i], "/", names(st[[z]]), ".png"),
        plot     = layer_plot,
        width    = 14,
        height   = 8
      )
    }

    if(verbose == TRUE) {
      message(paste0("----------------------------------------------------------------"))
    }

  }

}
  # # extract stack name
  # stk_name <- sub("(\\d)[^0-9]+$", "\\1", names(stk))[1]
  #
  # # path to landwater TIF
  # land_path <-
  #   model_dirs %>%
  #   dplyr::filter(type == "lndtyp") %>%
  #   dplyr::filter(file_name == stk_name) %>%
  #   .$full_path
  #
  # # Make land polygon for plotting
  # land_poly <-
  #   land_path %>%
  #   get_landwater() %>%
  #   get_land(
  #     as_polygon = TRUE,
  #     as_sf      = TRUE
  #     )

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

# }

get_plot <- function(
    r,
    land_shp,
    verbose       = TRUE
) {

# # name of layer
  layer_name <- sub(".*(\\d)", "",     names(r))


  if(verbose == TRUE) {
    message(paste0(layer_name))
  }

    # mask to land
    r <-
      r %>%
      terra::mask(terra::vect(land_shp), inverse = T)

    # if layer is water depth, make a classification raster
    if(grepl("water_depth", layer_name) == TRUE) {

      # convert raster to dataframe for plotting
      r_df <-
        r %>%
        # rmask %>%
        as.data.frame(xy = TRUE) %>%
        stats::setNames(c("x", "y", "values")) %>%
        dplyr::mutate(
          class = dplyr::case_when(
            values == 1 ~ 'depth <= 2 ft',
            values == 2 ~ '2 < depth <= 5 ft',
            values == 3 ~ 'depth > 5 ft',
          ),
          class = factor(class,  levels =c('depth <= 2 ft',  '2 < depth <= 5 ft', 'depth > 5 ft'))
        )
    } else {

      # convert raster to dataframe for plotting
      r_df <-
        r %>%
        # rmask %>%
        as.data.frame(xy = TRUE) %>%
        stats::setNames(c("x", "y", "values"))

    }

  if(grepl("sd", layer_name) == TRUE) {
    # vv <- (terra::values(r))
    # vv <- max(na.omit(terra::values(r)))
    # plot(land_shp$geometry)

    # plot raster
    rast_plot <-
      ggplot2::ggplot() +
      ggplot2::geom_tile(
        data = r_df,
        aes(x = x, y = y, fill = values)
      ) +
      # ggplot2::coord_sf() +
      ggplot2::coord_equal() +
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

    return(rast_plot)

  } else if(grepl("roads", layer_name) == TRUE) {

    # plot raster
    rast_plot <-
      ggplot2::ggplot() +
      ggplot2::geom_tile(
        data = r_df,
        aes(x = x, y = y, fill = values)
      ) +
      ggplot2::coord_equal() +
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


    return(rast_plot)

  } else if(grepl("water_depth", layer_name) == TRUE) {

    # plot raster
    rast_plot <-
      ggplot2::ggplot() +
      ggplot2::geom_tile(
        data = r_df,
        aes(x = x, y = y, fill = class)
      ) +
      # ggplot2::coord_equal() +
      ggplot2::scale_fill_manual(values = c("red", "gold",  "dodgerblue3")) +
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


    return(rast_plot)

  } else {

    # plot raster
    rast_plot <-
      ggplot2::ggplot() +
      ggplot2::geom_tile(
        data = r_df,
        aes(x = x, y = y, fill = values)
      ) +
      ggplot2::coord_equal() +
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

    return(rast_plot)

  }



  # roads buffer col
  # scale_fill_manual(values = c("yellow", "darkorange",  "red", "darkred")) +

  # water depth
  # scale_fill_manual(values = c("red", "gold",  "dodgerblue3")) +

  # fetch cols
  # scale_fill_manual(values = c ("#C7E9C0", "#41AB5D","#00441B")) +
  # scale_fill_gradientn(colours = (RColorBrewer::brewer.pal(9, "Greens")), limits = c(0, 1))

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

