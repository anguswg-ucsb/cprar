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

# You can fill in the NA values using the focal function with the na.rm argument set to FALSE and pad to TRUE.
# fill  NA values in a raster
na_fill <- function(x, i=5) {
  if( is.na(x)[i] ) {

    return( round(mean(x, na.rm=TRUE),0) )

    } else {

      return( round(x[i],0) )

    }
  }
