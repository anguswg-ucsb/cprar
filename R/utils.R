#' Return sequential year numbers corresponding with model years
#'
#' @return dataframe with the model year and the corresponding sequential year and decade
#' @export
model_years <- function() {

  year_df <- data.frame(
    model_year = c(3:12, 23:32),
    num_year   = 1:20,
    decade     = rep(c("decade_1", "decade_2"), each = 10)
  )

  return(year_df)

}
