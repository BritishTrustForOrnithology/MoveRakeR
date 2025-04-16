#' Pressure sensor ("Pr") constructor classes for MoveRakeR
#'
#' @description Functions to append object classes of \code{Pr}, \code{PrStack} and \code{PrMultiStack},
#' obtained respectively using their respective namesake functions respectively. this is a specific
#' class for the UvA pressure sensor data.
#'
#' @details  Three special object classes are returned from initial the read in function
#' \code{\link{read_pressure_UvA}}. \code{Pr} is typically representative
#' of an individual animal's time-series, but an 'rbinded' \code{Pr}
#' object of several animals, i.e. multiple TagIDs, will also be accepted as a \code{Track}
#' object and processed for each individual, thus \code{Pr} objects represent a single \code{data.frame} class;
#' \code{PrStack} and \code{PrMultiStack} objects are nested \code{list} formats, and (as currently implemented)
#' \emph{must be treated as one individual per data.frame}. These stacked listed methods
#' are intended for efficient application of routines for groups of animals. Objects can be coerced into
#' \code{Pr}, \code{PrStack} and \code{PrMultiStack} classes but they require the following
#' minimum column names: \strong{TagID}, \strong{DateTime} and \strong{pressure_avg}.
#' Further checking of whether objects belong to special classes are provided by:
#' \code{link{is_Pr}},
#' \code{link{is_PrStack}} and \code{\link{is_PrMultiStack}}, and coercing of two
#' \code{PrStack} objects is provided by \code{\link{pressure_multi_stack}}.
#'
#' @seealso [MoveRakeR::Track], [MoveRakeR::Accn], [MoveRakeR::pressure_multi_stack], [MoveRakeR::is_Pr], [MoveRakeR::is_PrStack], [MoveRakeR::is_PrMultiStack]
#'
#' @param data An input base::data.frame for a pressure sensor dataset that is to be
#' coerced to the designated class.
#' @return For tracking data, returns a \code{Pr}, \code{PrStack} or
#' \code{PrMultTrackStack}
#'
#' @export
Pr <- function(data){
  if(is.null(data$TagID) | is.null(data$DateTime) | is.null(data$pressure_avg)){
    stop("Can't construct Pr object: fields are missing or mispelled")}

  obj <- structure(.Data = data, class = c("Pr","data.frame"))
  return(obj)
}
#' @rdname Pr
#' @export
Pr2 <- function(data){
  if(is.null(data$TagID) | is.null(data$DateTime) | is.null(data$pressure_avg)){
    stop("Can't construct Pr object: fields are missing or mispelled")}

  obj <- structure(.Data = data, class = c("Pr2","data.frame"))
  return(obj)
}
#' @rdname Pr
#' @export
PrStack <- function(data){
  if(!is.list(data))
    stop("Can't construct PrStack object: data are not a list")
  if(is.null(data[[1]]$TagID) | is.null(data[[1]]$DateTime) | is.null(data[[1]]$pressure_avg)){
    stop("Can't construct PrStack object: fields are missing or mispelled")
  }
  for(j in 1:length(data)){
    data[[j]] <- structure(.Data = data[[j]], class = c("Pr","data.frame"))
  }
  obj <- structure(.Data = data, class = c("Pr","PrStack","list"))
  return(obj)
}
#' @rdname Pr
#' @export
PrMultiStack <- function(data){
  # expects lists in lists
  if(!is.list(data)[[1]]){
    stop("Can't construct PrMultiStack object: data are not a list")}
  if(is.null(data[[1]][[1]]$TagID) | is.null(data[[1]][[1]]$DateTime) | is.null(data[[1]][[1]]$pressure_avg)){
    stop("Can't construct PrMultiStack object: fields are missing or mispelled")
  }
  for(i in 1:length(data)){
    for(j in 1:length(data[[i]])){
      data[[i]][[j]] <- structure(.Data = data[[i]][[j]], class = c("Pr","data.frame"))
    }
    data[[i]] <- structure(.Data = data[[i]], class = c("Pr","PrStack","list"))
  }
  obj <- structure(.Data = data, class = c("Pr","PrStack","PrMultiStack","list"))
  return(obj)
}
#' is_Pr
#'
#' Check that an object is of class \code{\link{Pr}}, class \code{\link{PrStack}} or class \code{\link{PrMultiStack}}.
#'
#' @param x Object to be assesed.
#' @param strict For is_ functions, logical for should the class check search strictly for the
#' specific sub-class, e.g. PrStack, or PrMultiStack (default = TRUE), but if FALSE
#' then the function checks as a normal "is" or inherits for the super class, e.g. Pr (even if object is a PS or PMS).
#' @return Logical \code{TRUE} if \code{x} belongs to class \code{\link{Pr}}, \code{\link{PrStack}} or \code{\link{PrMultiStack}}, otherwise returns \code{FALSE}.
#' @export
is_Pr <- function(x, strict = TRUE){
  if(strict == TRUE){
    inherits(x,"Pr") & !inherits(x,"Pr2") & !inherits(x,"PrStack") & !inherits(x,"PrMultiStack")
  } else{
    inherits(x,"Pr")
  }
}
#' @rdname is_Pr
#' @export
is_PrStack <- function(x, strict = TRUE){
  if(strict == TRUE){
    inherits(x,"PrStack") & !inherits(x,"PrMultiStack")
  } else{
    inherits(x,"PrStack")
  }
}
#' @rdname is_Pr
#' @export
is_PrMultiStack <- function(x, strict = TRUE){
  if(strict == TRUE){
    inherits(x,"PrMultiStack")
  } else{
    inherits(x,"PrMultiStack")
  }
}
#' @rdname is_Pr
#' @export
is_Pr2 <- function(x, strict = TRUE){
  if(strict == TRUE){
    inherits(x,"Pr2") & !inherits(x,"Pr") & !inherits(x,"PrStack") & !inherits(x,"PrMultiStack")
  } else{
    inherits(x,"Pr2")
  }
}

#' pressure_multi_stack
#'
#' Coerce \code{PrStack} class objects into a \code{PrMultiStack} class object.
#'
#' @param data Object to be assesed.
#'
#' @return Return new \code{data} coerced to a \code{\link{PrMultiStack}} class.
#' @export
pressure_multi_stack <- function(data = NULL){
  if(!is_PrStack(data[[1]])){stop("data should be a PrStack class object")}
  data <- PrMultiStack(data) # data should be a list(x,y)
  return(data)
}

#' @noRd
is.Pr <- is_Pr
is.PrStack <- is_PrStack
is.PrMultiStack <- is_PrMultiStack








