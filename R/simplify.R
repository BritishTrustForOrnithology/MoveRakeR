#' Method to simplify Track objects to essential data
#'
#' Simplification of telemetry dataset to retain TagID, DateTime, longitude, and latitude columns,
#' plus any extra column the user wishes.
#'
#' @param data Input data object, of class \code{data.frame}, \code{tibble-family} or \code{data.table}, with required columns: TagID, DateTime, longitude, latitude.
#' @export
simplify <- function(data, keep = NULL, verbose = FALSE){

  kp_variables <- c("TagID", "DateTime", "longitude", "latitude")

  if(!is.null(keep)){
    kp_variables <- c(kp_variables, keep)
  }

  data <- subset(data, select = kp_variables)

    return(data)

}

