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
    dplyr::mutate(base_folder = base_folder)

  # x %>%
  #   dplyr::group_by(paths) %>%
  #   dplyr::mutate(
  #     model_version =     regmatches(paths, regexpr("_", paths), invert = TRUE)[[1]][1],
  #     names         =     regmatches(paths, regexpr("_", paths), invert = TRUE)[[1]][2]
  #
  #   ) %>%
  #   dplyr::ungroup()

}

library(tidyverse)
base_folder <- "D:/cpra"
# base_folder <- "D:/cpra/G510_new_FWOA"
# df <- data.frame(x = c(NA, "a-b", "a-d", "b-c", "d-e"))
# df %>% extract(x, "A")
# df %>% extract(x,
#                c("A", "B"),
#                "([[:alnum:]]+)-([[:alnum:]]+)"
#                )
