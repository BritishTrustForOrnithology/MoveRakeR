#' Calcualte the x and y min-max box ranges
#'
#' This is a simple way of just getting the xy bounds of the data in \code{Track} datasets.
#'
#' @param data A \code{Track} family object, or just a simple data.frame or tibble.
#' @param p4s A numeric or \code{sf} \code{CRS} argument, here defaulting to the epsg string 3035.
#'
#' @return A list of length 2 with element one being the minimum and maximum x
#' range, and the second the y range.
#'
#' @seealso [MoveRakeR::plot.Track]
#'
#' @examples
#'
#' indata <- yourdata # data.frame with a minimum of columns named TagID, DateTime, longitude, latitude
#' data <- Track(indata) # optional to convert to in-house class (most functions will still run on non track data but generic S3 methods will not be available for plot and summary)
#'
#' # run get_bounds function
#' llyrb = get_bounds(indata, p4s = p4)
#'
#' @export
get_bounds <- function(data, p4s = 3035){

  if(is_TrackStack(data)){
    data <- do.call('rbind',data)
  }

  if(is_TrackMultiStack(data)){
    cap = list()
    for(i in 1:length(data)){
      cap[[i]] <- do.call('rbind',data[[i]])
    }
    data <- do.call('rbind',cap)
  }

  bb <- sf::st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326)
  bb <- sf::st_transform(bb, p4s)

  lly = sf::st_bbox(bb)
  lly_x <- c(lly[1],lly[3])
  lly_y <- c(lly[2],lly[4])

  names(lly_x) <- names(lly_y) <- c("min","max")
  llyrb <- rbind(lly_x,lly_y)
  row.names(llyrb) <- c("Longitude","Latitude")

  return(list(xRa = c(llyrb[1,1],llyrb[1,2]), yRa = c(llyrb[2,1],llyrb[2,2])))

}




