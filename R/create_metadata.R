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

  XML::newXMLNode("abstract",     attrs=list(type="abstract"), "This layer was developed as part of a project led by Royal Engineering, and funded by CPRA and LDWF to explore oyster resource suitability across the Louisiana coast. The project team developed models to describe the suitability of Louisiana waters for oyster farming using either traditional cultch or alternative oyster cultch methods, and developed geospatial layers to quantify each element of the traditional cultch and AOC models. The model code was developed in R by Lynker, Inc., under a subcontract to Royal Engineering. The full model code is available at https://github.com/anguswg-ucsb/cprar", parent=abstract_node)

  XML::newXMLNode("supplinf",     attrs=list(type="supplinf"), "Model results were developed based on 10 years of outputs from the 2023 Coastal Master Plan models. The naming convention for these files includes the parameter name, followed by either the model year (01-10) or an indication of whether the output represents a mean or standard deviation of all years. Additional information can be found in the project team report, available on request from CPRA.", parent=supplinf_node)

  XML::newXMLNode("cntorg",       attrs=list(type="cntorg"), "The Coastal Protection and Restoration Authority of LA", parent=cntorg_node)
  XML::newXMLNode("cntper",       attrs=list(type="cntper"), "Brian Lezina", parent=cntper_node)
  XML::newXMLNode("cntpos",       attrs=list(type="cntpos"), "Planning and Research Division Chief", parent=cntpos_node)
  XML::newXMLNode("addrtype_node",attrs=list(type="addrtype_node"), "mailing", parent=addrtype_node)
  XML::newXMLNode("address",      attrs=list(type="address"), "P.O. Box 44027", parent=address_node)
  XML::newXMLNode("city",         attrs=list(type="city"), "Baton Rouge", parent=city_node)
  XML::newXMLNode("state",        attrs=list(type="state"), "LA", parent=state_node)
  XML::newXMLNode("postal",       attrs=list(type="postal"), "70804", parent=postal_node)
  XML::newXMLNode("country",      attrs=list(type="country"), "US", parent=country_node)
  XML::newXMLNode("cntvoice",     attrs=list(type="cntvoice"), "225.342.1475", parent=cntvoice_node)
  XML::newXMLNode("cntemail",     attrs=list(type="cntemail"), "brian.lezina@la.gov", parent=cntemail_node)

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

      # save out meta data files for raster layer
      create_raster_metadata(
        r           = stk[[z]],
        output_file = paste0(file_path, "/",  names(srd)[i], "_metadata", "/", names(stk[[z]]), ".xml")
      )

    }

  }

}



























