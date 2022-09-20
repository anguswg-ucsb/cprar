#' Parse Year and Model
#' @param base_folder highest level directory housing all the model folders
#' @return data.frame
#' @export
#' @importFrom tidyr extract
#' @importFrom dplyr mutate
parse_directory = function(base_folder){

  x <- data.frame(
    paths = basename(list.dirs(base_folder))[-1]
    )

  tidyr::extract(
    x,
    col   = paths,
    into  = c("model_version", "names"),
    regex = "^^((?:[^_]*){1})\\s?(.*)$"
    ) %>%
    dplyr::mutate(
      base_folder = base_folder,
      path        = paste0(base_folder, "/", model_version, names)
      )

}

#' Parse model directory
#' @param dir highest level directory housing all the model folders
#' @return data.frame
#' @export
#' @importFrom stringr str_extract
#' @importFrom utils tail
#' @importFrom dplyr group_by mutate ungroup relocate
parse_files = function(dir){

  file_df <- data.frame(
          model_dir = basename(dir),
          full_path = list.files(dir, full.names = T),
          path      = list.files(dir)
          ) %>%
    dplyr::group_by(path) %>%
    dplyr::mutate(
      scenario = stringr::str_extract(path, "(?<=S)[0-9]+"),
      ext      = gsub("\\.", "", stringr::str_extract(path, "\\.[^_]+$")),
      type     = gsub(stringr::str_extract(path, "\\.[^_]+$"), "", tail(unlist(strsplit(path, '_')), 1))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::relocate(scenario, type, ext, model_dir, path, full_path)

  return(file_df)

}

# library(tidyverse)
# base_folder <- "D:/cpra"
#
# model_dirs <- parse_directory(base_folder = base_folder)
# model_files <- parse_files(dir = model_dirs$path[1])
#
#






















