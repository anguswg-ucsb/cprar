#' Parse Year and Model
#' @param base_folder highest level directory housing all the model folders
#' @return data.frame
#' @export
#' @importFrom tidyr extract
#' @importFrom dplyr mutate group_by ungroup relocate
parse_directory = function(base_folder){


  x <- data.frame(
    paths = basename(list.dirs(base_folder))[-1]
    )

  x <- tidyr::extract(
    x,
    col   = paths,
    into  = c("model_version", "names"),
    regex = "^^((?:[^_]*){1})\\s?(.*)$"
    ) %>%
    dplyr::mutate(
      base_folder = base_folder,
      path        = paste0(base_folder, "/", model_version, names)
      )

  message(paste0("Parsing directory..."))

  file_df <- lapply(1:nrow(x), function(n) {

    message(paste0(basename(x$path[n])))

    data.frame(
      model_dir = basename(x$path[n]),
      full_path = list.files(x$path[n], full.names = T),
      path      = list.files(x$path[n])
    ) %>%
      dplyr::group_by(path) %>%
      dplyr::mutate(
        scenario = stringr::str_extract(path, "(?<=S)[0-9]+"),
        ext      = gsub("\\.", "", stringr::str_extract(path, "\\.[^_]+$")),
        type     = gsub(stringr::str_extract(path, "\\.[^_]+$"), "", tail(unlist(strsplit(path, '_')), 1)),
        year     = as.numeric(substr(gsub(".*(\\d{2}_\\d{2}).*", "\\1", path), 1, 2))
      ) %>%
      dplyr::ungroup() %>%
      dplyr::relocate(scenario, year, type, ext, model_dir, path, full_path)

  }) %>%
    dplyr::bind_rows()

   return(file_df)

}

#' Parse model directory
#' @param dir highest level directory housing all the model folders
#' @return data.frame
#' @export
#' @importFrom stringr str_extract
#' @importFrom utils tail
#' @importFrom dplyr group_by mutate ungroup relocate
parse_files = function(dir){
# dir <- base_folder
  file_df <- data.frame(
          model_dir = basename(dir),
          full_path = list.files(dir, full.names = T),
          path      = list.files(dir)
          ) %>%
    dplyr::group_by(path) %>%
    dplyr::mutate(
      scenario = stringr::str_extract(path, "(?<=S)[0-9]+"),
      ext      = gsub("\\.", "", stringr::str_extract(path, "\\.[^_]+$")),
      type     = gsub(stringr::str_extract(path, "\\.[^_]+$"), "", tail(unlist(strsplit(path, '_')), 1)),
      year     = as.numeric(substr(gsub(".*(\\d{2}_\\d{2}).*", "\\1", path), 1, 2))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::relocate(scenario, year, type, ext, model_dir, path, full_path)


  return(file_df)

}

#' Parse a Masterplan file name and extract info
#' @description Given a path with the general Masterplan path construction, a dataframe will be returned with scenario number, extension type, file content type, and the masterplan year
#' @param path character path to Masterplan file
#' @return dataframe
#' @export
path_info <- function(path) {

  # path <- paths[1]

  path_details <-
    data.frame(
      scenario = stringr::str_extract(path, "(?<=S)[0-9]+"),
      ext      = gsub("\\.", "", stringr::str_extract(path, "\\.[^_]+$")),
      type     = gsub(stringr::str_extract(path, "\\.[^_]+$"), "", tail(unlist(strsplit(path, '_')), 1)),
      year     = as.numeric(substr(gsub(".*(\\d{2}_\\d{2}).*", "\\1", path), 1, 2))
      ) %>%
    dplyr::mutate(
      year     = dplyr::case_when(
        year < 10 ~ paste0("0", year),
        TRUE      ~ as.character(year)
      )
    )

  return(path_details)

}
# library(tidyverse)
# base_folder <- "D:/cpra"
#
# model_dirs <- parse_directory(base_folder = base_folder)
# model_files <- parse_files(dir = model_dirs$path[1])
#
#






















