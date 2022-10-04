#' Create metadata for SpatRasters
#'
#' @param r SpatRaster data
#' @param output_file character indicating file path to save to
#' @return XML meta data file
#' @importFrom XML newXMLDoc newXMLNode saveXML
#' @importFrom terra ext nrow ncol res crs
#' @export
create_raster_metadata <- function(
    r,
    output_file
    ) {

  # raster extent
  ext            <- terra::ext(r)

  doc            <- XML::newXMLDoc()
  root           <- XML::newXMLNode("raster",     parent=doc)

  abstract_node  <- XML::newXMLNode("abstract",   parent=root)
  supplinf_node  <- XML::newXMLNode("supplinf",   parent=root)
  cntorg_node    <- XML::newXMLNode("cntorg",     parent=root)
  cntper_node    <- XML::newXMLNode("cntper",     parent=root)
  cntpos_node    <- XML::newXMLNode("cntpos",     parent=root)
  addrtype_node  <- XML::newXMLNode("addrtype",   parent=root)
  address_node   <- XML::newXMLNode("address",    parent=root)
  city_node      <- XML::newXMLNode("city",       parent=root)
  state_node     <- XML::newXMLNode("state",      parent=root)
  postal_node    <- XML::newXMLNode("postal",     parent=root)
  country_node   <- XML::newXMLNode("country",    parent=root)
  postal_node    <- XML::newXMLNode("postal",     parent=root)
  country_node   <- XML::newXMLNode("country",    parent=root)
  cntvoice_node  <- XML::newXMLNode("cntvoice",      parent=root)
  cntemail_node  <- XML::newXMLNode("cntemail",  parent=root)

  title_node     <- XML::newXMLNode("title",      parent=root)
  file_node      <- XML::newXMLNode("file_name",  parent=root)
  res_node       <- XML::newXMLNode("resolution", parent=root)
  ext_node       <- XML::newXMLNode("extent",     parent=root)

  XML::newXMLNode("abstract",     attrs=list(type="abstract"), "This layer was developed as part of a project led by Royal Engineering, and funded by CPRA and LDWF to explore oyster resource suitability across the Louisiana coast. The project team developed models to describe the suitability of Louisiana waters for oyster farming using either traditional cultch or alternative oyster cultch methods, and developed geospatial layers to quantify each element of the traditional cultch and AOC models. The model code was developed in R by Lynker, Inc., under a subcontract to Royal Engineering.", parent=abstract_node)

  XML::newXMLNode("supplinf",     attrs=list(type="supplinf"), "Model results were developed based on 10 years of outputs from the 2023 Coastal Master Plan models. The naming convention for these files includes the parameter name, followed by either the model year (03-12) or an indication of whether the output represents a mean or standard deviation of all years. Additional information can be found in the project team report, available on request from CPRA.", parent=supplinf_node)

  XML::newXMLNode("cntorg",       attrs=list(type="cntorg"), "The Coastal Protection and Restoration Authority of LA", parent=cntorg_node)
  XML::newXMLNode("cntper",       attrs=list(type="cntper"), "Rocky Wagner", parent=cntper_node)
  XML::newXMLNode("cntpos",       attrs=list(type="cntpos"), "Planning and Research Division Chief", parent=cntpos_node)
  XML::newXMLNode("addrtype_node",attrs=list(type="addrtype_node"), "mailing", parent=addrtype_node)
  XML::newXMLNode("address",      attrs=list(type="address"), "P.O. Box 44027", parent=address_node)
  XML::newXMLNode("city",         attrs=list(type="city"), "Baton Rouge", parent=city_node)
  XML::newXMLNode("state",        attrs=list(type="state"), "LA", parent=state_node)
  XML::newXMLNode("postal",       attrs=list(type="postal"), "70804", parent=postal_node)
  XML::newXMLNode("country",      attrs=list(type="country"), "US", parent=country_node)
  XML::newXMLNode("cntvoice",     attrs=list(type="cntvoice"), "225-342-1485", parent=cntvoice_node)
  XML::newXMLNode("cntemail",     attrs=list(type="cntemail"), "Rocky.Wager@LA.GOV", parent=cntemail_node)

  XML::newXMLNode("title",    attrs=list(type="title"), names(r), parent=title_node)

  XML::newXMLNode("file_name",    attrs=list(type="file_name"), paste0(names(r), ".tif"), parent=file_node)

  XML::newXMLNode("resolution",   attrs=list(type="resolution"),  paste0(as.character(terra::res(r))[1], " x ", as.character(terra::res(r))[2]), parent=res_node)

  XML::newXMLNode("coord",        attrs=list(type="xmin"), ext[1], parent=ext_node); XML::newXMLNode("coord", attrs=list(type="xmax"), ext[2], parent=ext_node)
  XML::newXMLNode("coord",        attrs=list(type="ymin"), ext[3], parent=ext_node); XML::newXMLNode("coord", attrs=list(type="ymax"), ext[4], parent=ext_node)

  XML::newXMLNode("projection",   terra::crs(r, proj = TRUE), parent=root)

  dim            <- XML::newXMLNode("dimensions", parent=root)

  XML::newXMLNode("cells",        attrs=list(dim="nrow"), terra::nrow(r), parent=dim)

  XML::newXMLNode("cells",        attrs=list(dim="ncol"), terra::ncol(r), parent=dim)

  XML::saveXML(doc, file = output_file)

}

#' Create metadata XML files for layers in SpatialRasterDataset
#'
#' @param srd SpatialRasterDataset of finalized AOC raster layers
#' @param file_path character, base file path to save metadata files to
#' @param verbose logical, whether to print messages or not. Default is TRUE, prints messages
#' @return XML files saved to specified file path
#' @export
finalize_metadata <- function(
    srd,
    file_path,
    verbose = TRUE
    ) {
  srd       = final_stk[1:2]
  srd
  file_path = "D:/cpra_metadata"
  verbose   = TRUE
  # check if base folder exists, if not, create it
  if(!dir.exists(file_path) == TRUE) {

    dir.create(file_path)

  }
  # i <- 1
  # loop through each raster stack in SpatRasterDataset
  for(i in 1:length(srd)) {


    if(verbose == TRUE) {
      message(paste0("Creating folder: ", names(srd)[i]))
    }

    # create file directory for model run
    dir.create(paste0(file_path, "/", names(srd)[i], "_metadata"))

    # model stk
    stk <- srd[[i]]

    # loop through each layer in Raster stack and generate XML metadata files
    for(z in 1:terra::nlyr(stk)) {

      if(verbose == TRUE) {
        message(paste0("Saving raster metadata - ", names(stk[[z]]), ".xml", " - ", z, "/", terra::nlyr(stk)))
      }

      # z <- 1
      # names(stk[[z]])
      # names(stk)
      # # save out meta data files for raster layer
      # create_raster_metadata(
      #   r           = stk[[z]],
      #   output_file = paste0(file_path, "/",  names(srd)[i], "_metadata", "/", names(stk[[z]]), ".xml")
      # )

    }

  }

}





#' Create metadata XML files for layers in SpatialRasterDataset from a template XML file
#'
#' @param srd SpatialRasterDataset of finalized AOC raster layers
#' @param file_path character, base file path to save metadata files to
#' @param template character, path to XML template file to parse
#' @param base_folder character, path to model file directory to parse
#' @param verbose logical, whether to print messages or not. Default is TRUE, prints messages
#' @return XML files saved to specified file path
#' @export
finalize_metadata2 <- function(
    srd,
    file_path,
    template,
    base_folder,
    verbose = TRUE
) {

  # srd       = final_stk[1:2]
  # srd
  # base_folder <- "D:/cpra"
  # file_path = "D:/cpra_metadata"
  # template = "data/metadata_template2.xml"
  # verbose   = TRUE

  # model file directory
  file_dir <-
    base_folder %>%
    parse_directory()

  # check if base folder exists, if not, create it
  if(!dir.exists(file_path) == TRUE) {

    dir.create(file_path)

  }

  # loop through each raster stack in SpatRasterDataset
  for(i in 1:length(srd)) {


    if(verbose == TRUE) {
      message(paste0("Creating folder: ", names(srd)[i]))
    }

    # create file directory for model run
    dir.create(paste0(file_path, "/", names(srd)[i], "_metadata"))

    # model stk
    stk <- srd[[i]]

    # loop through each layer in Raster stack and generate XML metadata files
    for(z in 1:terra::nlyr(stk)) {

      if(verbose == TRUE) {
        message(paste0("Saving raster metadata - ", names(stk[[z]]), ".xml", " - ", z, "/", terra::nlyr(stk)))
      }

      meta_tbl <- make_metadata(
        r        = stk[[z]],
        template = template,
        file_dir = file_dir
        )

      # write metadata table out
      write.table(
        meta_tbl,
        file      = paste0(file_path, "/",  names(srd)[i], "_metadata", "/", names(stk[[z]]), ".xml"),
        quote     = FALSE,
        col.names = FALSE,
        row.names = FALSE
      )
      # names(stk)
      # # save out meta data files for raster layer
      # create_raster_metadata(
      #   r           = stk[[z]],
      #   output_file = paste0(file_path, "/",  names(srd)[i], "_metadata", "/", names(stk[[z]]), ".xml")
      # )

    }

  }

}
# Write meta data files
# meta_path      <-  paste0("data/metadata/", names(aoc), "_",  run_year, ".xml" )

make_metadata <- function(r, template, file_dir) {

  # name of layer
  layer_name <- sub(".*(\\d)", "",    names(r))
  layer_name <- substr(layer_name, 2, nchar(layer_name))

  layer_df <-
    metadata_abstracts() %>%
    dplyr::filter(names == layer_name)

  # scenario text
  scenario    <- stringr::str_extract(names(r), "[^_]*_[^_]*")

  # year range 1
  year1       <- strsplit(gsub(".*(\\d{2}_\\d{2}).*", "\\1", names(r)),
                          "_" )[[1]][1]

  # year range 2
  year2       <- strsplit(gsub(".*(\\d{2}_\\d{2}).*", "\\1", names(r)),
                          "_" )[[1]][2]

  # replace scenario, year1, and year2 in abstract
  abstract <- layer_df$abstracts
  abstract <- gsub("<SCENARIO>", scenario, abstract)
  abstract <- gsub("<YEAR1>", year1, abstract)
  abstract <- gsub("<YEAR2>", year2, abstract)

  # check if <DATASOURCE> needs to be replaced
  if(grepl("<DATASOURCE>", abstract) == TRUE) {

    source_name <- sub("([A-Za-z]+_[A-Za-z]+_[0-9]+_[0-9]+).*", "\\1",   names(r))

    # if file name is wonky
    if(as.numeric(year1) != as.numeric(year2)) {

      source_name <- gsub(year1, year2, source_name)

    }

    # DATASOURCE TEXT
    data_source    <-
      file_dir %>%
      dplyr::filter(file_name == source_name, type == "lndtyp") %>%
      # dplyr::filter(file_name == sub("([A-Za-z]+_[A-Za-z]+_[0-9]+_[0-9]+).*", "\\1",   names(r)), type == "lndtyp") %>%
      .$path %>%
      gsub(".tif", "", .)

    # replace <DATASOURCE> with land file
    abstract <- gsub("<DATASOURCE>", data_source, abstract)

  }

  # file title
  title <- paste0("<title>", names(r), "</title>")

  # pub date
  pub_date <-  paste0("<pubdate>", gsub("-", "", Sys.Date()), "</pubdate>")

  # meta data date
  met_date <- paste0("<metd>", gsub("-", "", Sys.Date()), "</metd>")

  # read in template metadata file
  temp_x     <- readLines(template)

  # replace abstract
  temp_x[12] <- abstract

  # replace title
  temp_x[7]  <- title

  # replace pub date
  temp_x[6] <- pub_date

  # replace meta data date
  temp_x[87] <- met_date

  # template as character
  code <- paste0(as.character(temp_x), collapse = "\n")

  return(code)

}

metadata_abstracts <- function() {
  # Abstracts
  water_depth_abs  <- "<abstract>Depth in feet; derived from 30m depth indundation. Data was sourced from the <DATASOURCE> file from the CMP 2023 Outputs.</abstract>"

  si_fetch_shallow_abs  <- paste0(
    "<abstract>Index of commercial viability for AOC operations in shallow water (assumes smaller boats without refrigeration capability) based on fetch distance. Shallow-water operations are assumed to be conducted via small boats and/or by wading and are restricted to water depths of 2-5 ft. Data was sourced from the <DATASOURCE> file from the CMP 2023 Outputs. Scenario <SCENARIO> year <YEAR1>.</abstract>")

  si_fetch_deep_abs  <- paste0(
    "<abstract>Index of commercial viability for AOC operations in deep water (assumes larger boats with refrigeration capability) based on fetch distance. Deep-water operations are assumed to use larger vessels and are limited to areas with water depths of greater than 5 ft. Data was sourced from the <DATASOURCE> file from the CMP 2023 Outputs. Scenario ",
    "<SCENARIO> year <YEAR1>.</abstract>")

  si_roads_abs  <- paste0(
    "<abstract>Straight line distance (km) to nearest road. Provides a metric for ease of access. The 2020 Census Tiger/Line shapefile for primary and secondary roads was used to estimate distance to roads (access)</abstract>")

  si_sed_dep_abs  <- paste0(
    "<abstract>Sediment deposition rate. Provides a measure of AOC level of effort to reduce effects of sedimentation. Data was sourced from the <DATASOURCE> file from the CMP 2023 Outputs. Scenario ", "<SCENARIO>",  " years <YEAR1> - <YEAR2>.</abstract>")

  cv_shallow_abs  <- paste0(
    "<abstract>Index of commercial viability for AOC operations in shallow water (assumes smaller boats without refrigeration capability) based on fetch and sediment deposition. Shallow-water operations are assumed to be conducted via small boats and/or by wading and are restricted to water depths of 2-5 ft. Scenario <SCENARIO> year <YEAR1>.</abstract>")

  cv_deep_abs  <- paste0(
    "<abstract>Index of commercial viability for AOC operations in deep water (assumes larger boats with refrigeration capability) based on fetch, sediment deposition and distance to roads. Deep-water operations are assumed to use larger vessels and are limited to areas with water depths of greater than 5 ft. Scenario <SCENARIO> year <YEAR1>.</abstract>")

  cv_combine_abs  <- paste0(
    "<abstract>Index of commercial viability for AOC operations combining deep and shallow water AOC layers (assumes larger boats with refrigeration capability) based on fetch, sediment deposition and distance to roads. Scenario <SCENARIO> year <YEAR1>.</abstract>")

  # Abstracts
  aoc_shallow_abs  <- paste0(
    "<abstract>AOC suitability index for shallow water operations (assumes smaller boats without refrigeration capability) based on oyster viability and commercial viability; year <YEAR1>. Shallow-water operations are assumed to be conducted via small boats and/or by wading and are restricted to water depths of 2-5 ft. Scenario <SCENARIO> year <YEAR1>.</abstract>"
  )

  aoc_deep_abs  <- paste0(
    "<abstract>AOC suitability index for deep water operations (assumes larger boats with refrigeration capability) based on oyster viability and commercial viability; year <YEAR1>. Deep-water operations are assumed to use larger vessels and are limited to areas with water depths of greater than 5 ft. Scenario <SCENARIO> year <YEAR1>.</abstract>"
  )

  aoc_combine_abs  <- paste0(
    "<abstract>AOC suitability index combining deep and shallow water AOC layers (assumes larger boats with refrigeration capability) based on oyster viability and commercial viability; year <YEAR1>. Scenario <SCENARIO> year <YEAR1> </abstract>"
  )

  # Abstracts
  aoc_shallow_mean_abs  <- paste0(
    "<abstract>AOC suitability index for shallow water operations (assumes smaller boats without refrigeration capability) based on oyster viability and commercial viability; Scenario <SCENARIO> mean years <YEAR1> - <YEAR2>. Shallow-water operations are assumed to be conducted via small boats and/or by wading and are restricted to water depths of 2-5 ft.</abstract>"
  )
  aoc_shallow_sd_abs  <- paste0(
    "<abstract>AOC suitability index for shallow water operations (assumes smaller boats without refrigeration capability) based on oyster viability and commercial viability; Scenario <SCENARIO> standard deviation years <YEAR1> - <YEAR2>. Shallow-water operations are assumed to be conducted via small boats and/or by wading and are restricted to water depths of 2-5 ft.</abstract>"
  )
  aoc_deep_mean_abs  <- paste0(
    "<abstract>AOC suitability index for deep water operations (assumes larger boats with refrigeration capability) based on oyster viability and commercial viability; Scenario <SCENARIO> mean years <YEAR1> - <YEAR2>. Deep-water operations are assumed to use larger vessels and are limited to areas with water depths of greater than 5 ft.</abstract>"
  )
  aoc_deep_sd_abs  <- paste0(
    "<abstract>AOC suitability index for deep water operations (assumes larger boats with refrigeration capability) based on oyster viability and commercial viability; Scenario <SCENARIO> standard deviation years <YEAR1> - <YEAR2>. Deep-water operations are assumed to use larger vessels and are limited to areas with water depths of greater than 5 ft.</abstract>"
  )

  aoc_combine_mean_abs  <- paste0(
    "<abstract>AOC suitability index combining deep and shallow water AOC layers (assumes larger boats with refrigeration capability) based on oyster viability and commercial viability; Scenario <SCENARIO> mean years <YEAR1> - <YEAR2>.</abstract>"
  )

  aoc_combine_sd_abs  <- paste0(
    "<abstract>AOC suitability index combining deep and shallow water AOC layers (assumes larger boats with refrigeration capability) based on oyster viability and commercial viability; Scenario <SCENARIO> standard deviation years <YEAR1> - <YEAR2>.</abstract>"
  )

  # Abstracts
  si_sal_cool_abs  <- paste0("<abstract>Suitability index based on cool month (October through March) minimum salinity; Scenario <SCENARIO> model year <YEAR1> </abstract>")
  si_sal_warm_abs  <- paste0("<abstract>Suitability index based on warm month (April through September) minimum salinity; Scenario <SCENARIO> model year <YEAR1> </abstract>")
  si_sal_avg_abs   <- paste0("<abstract>Suitability index based on annual average salinity; Scenario <SCENARIO> model year <YEAR1> </abstract>")
  si_ms_abs        <- paste0("<abstract>Suitability index based monthly minimum salinity; Scenario <SCENARIO> model year <YEAR1> </abstract>")
  si_ov_abs        <- paste0("<abstract>Oyster viability index based monthly minimum salinity and annual average salinity; Scenario <SCENARIO> model year <YEAR1> </abstract>")


  # combine abstracts into dataframe
  abstract_df <- data.frame(
    abstracts = c(water_depth_abs, si_fetch_shallow_abs, si_fetch_deep_abs, si_roads_abs,
                  si_sed_dep_abs, cv_shallow_abs, cv_deep_abs, cv_combine_abs,
                  aoc_shallow_abs, aoc_deep_abs, aoc_combine_abs,
                  si_sal_cool_abs, si_sal_warm_abs, si_sal_avg_abs, si_ms_abs, si_ov_abs,
                  aoc_shallow_mean_abs, aoc_shallow_sd_abs, aoc_deep_mean_abs, aoc_deep_sd_abs,
                  aoc_combine_mean_abs, aoc_combine_sd_abs
                  ),
    names     = c("water_depth", "si_fetch_shallow", "si_fetch_deep", "si_roads", "si_sediment_deposition", "cv_shallow", "cv_deep", "cv", "aoc_shallow",
                  "aoc_deep", "aoc_combine", "si_cool", "si_warm", "si_avg", "si_ms", "si_ov",
                  "aoc_shallow_mean", "aoc_shallow_sd", "aoc_deep_mean", "aoc_deep_sd", "aoc_combine_mean", "aoc_combine_sd")
  )

    return(abstract_df)

}
















