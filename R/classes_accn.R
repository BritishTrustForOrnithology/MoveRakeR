#' Accelerometer ("Accn") constructor classes for MoveRakeR
#'
#' @description Functions to append object classes of \code{Accn}, \code{AccnStack} and \code{AccnMultiStack},
#' obtained respectively using their respective namesake functions respectively. this is a specific
#' class for the UvA accelerometer sensor data.
#'
#' @details  Three special object classes are returned from initial the read in function
#' \code{\link{read_accn_UvA}}. The class \code{Accn} is typically representative
#' of an individual animal's time-series, but an 'rbinded' \code{Accn}
#' object of several animals, i.e. multiple TagIDs, will also be accepted and processed for each TagID,
#' thus \code{Accn} objects represent a single \code{data.frame} class;
#' \code{AccnStack} and \code{AccnMultiStack} objects are nested \code{list} formats, and (as currently implemented)
#' \emph{must be treated as one individual per data.frame}. These stacked listed methods
#' are intended for efficient application of routines for groups of animals. Objects can be coerced into
#' \code{Accn}, \code{AccnStack} and \code{AccnMultiStack} classes but they require the following
#' minimum column names: \strong{TagID}, \strong{DateTime} and relevant xyz metrics
#' for a minimum 3D accelerometer: \strong{x_acceleration}, \strong{y_acceleration}, \strong{z_acceleration}.
#' Further checking of whether objects belong to special classes are provided by:
#' \code{link{is_Accn}},
#' \code{link{is_AccnStack}} and \code{\link{is_AccnMultiStack}}, and coercing of two
#' \code{AccnStack} objects is provided by \code{\link{accn_multi_stack}}. See \code{?Track} for details
#' of \code{Track} family classes.
#'
#' @seealso [MoveRakeR::Track], [MoveRakeR::Pr], [MoveRakeR::accn_multi_stack], [MoveRakeR::is_Accn], [MoveRakeR::is_AccnStack], [MoveRakeR::is_AccnMultiStack]
#'
#' @param data An input base::data.frame for an accelerometer dataset that is to be
#' coerced to the designated class.
#' @return For tracking data, returns a \code{Accn}, \code{AccnStack} or
#' \code{AccnMultTrackStack}
#'
#' @export
Accn <- function(data){
  if(is.null(data$TagID) | is.null(data$DateTime) | is.null(data$x_acceleration) | is.null(data$y_acceleration) | is.null(data$z_acceleration)){
    stop("Can't construct Accn object: fields are missing or mispelled")}

  obj <- structure(.Data = data, class = c("Accn","data.frame"))
  return(obj)
}
#' @rdname Accn
#' @export
Accn2 <- function(data){
  if(is.null(data$TagID) | is.null(data$DateTime) | is.null(data$x_acceleration) | is.null(data$y_acceleration) | is.null(data$z_acceleration)){
    stop("Can't construct Accn object: fields are missing or mispelled")}

  obj <- structure(.Data = data, class = c("Accn2","data.frame"))
  return(obj)
}
#' @rdname Accn
#' @export
AccnStack <- function(data){
  if(!is.list(data))
    stop("Can't construct AccnStack object: data are not a list")
  if(is.null(data[[1]]$TagID) | is.null(data[[1]]$DateTime) | is.null(data[[1]]$x_acceleration) | is.null(data[[1]]$y_acceleration) | is.null(data[[1]]$z_acceleration)){
    stop("Can't construct AccnStack object: fields are missing or mispelled")
  }
  for(j in 1:length(data)){
    data[[j]] <- structure(.Data = data[[j]], class = c("Accn","data.frame"))
  }
  obj <- structure(.Data = data, class = c("Accn","AccnStack","list"))
  return(obj)
}

#' @rdname Accn
#' @export
AccnMultiStack <- function(data){
  # expects lists in lists
  if(!is.list(data)[[1]]){
    stop("Can't construct AccnMultiStack object: data are not a list")}
  if(is.null(data[[1]][[1]]$TagID) | is.null(data[[1]][[1]]$DateTime) | is.null(data[[1]][[1]]$x_acceleration) | is.null(data[[1]][[1]]$y_acceleration) | is.null(data[[1]][[1]]$z_acceleration)){
    stop("Can't construct AccnMultiStack object: fields are missing or mispelled")
  }
  for(i in 1:length(data)){
    for(j in 1:length(data[[i]])){
      data[[i]][[j]] <- structure(.Data = data[[i]][[j]], class = c("Accn","data.frame"))
    }
    data[[i]] <- structure(.Data = data[[i]], class = c("Accn","AccnStack","list"))
  }
  obj <- structure(.Data = data, class = c("Accn","AccnStack","AccnMultiStack","list"))
  return(obj)
}

#' is.Accn
#'
#' Check that an object is of class \code{\link{Accn}}, class \code{\link{AccnStack}} or class \code{\link{AccnMultiStack}}.
#'
#' @param x Object to be assesed.
#' @param strict For is_ functions, logical for should the class check search strictly for the
#' specific sub-class, e.g. AccnStack, or AccnMultiStack (default = TRUE), but if FALSE
#' then the function checks as a normal "is" or inherits for the super class, e.g. Accn (even if object is a AS or AMS).
#' @return Logical \code{TRUE} if \code{x} belongs to class \code{\link{Accn}}, \code{\link{AccnStack}} or \code{\link{AccnMultiStack}}, otherwise returns \code{FALSE}.
#' @export
is_Accn <- function(x, strict = TRUE){
  if(strict == TRUE){
    inherits(x,"Accn") & !inherits(x,"Accn2") & !inherits(x,"AccnStack") & !inherits(x,"AccnMultiStack")
  } else{
    inherits(x,"Accn")
  }
}
#' @rdname is_Accn
#' @export
is_AccnStack <- function(x, strict = TRUE){
  if(strict == TRUE){
    inherits(x,"AccnStack") & !inherits(x,"AccnMultiStack")
  } else{
    inherits(x,"AccnStack")
  }
}
#' @rdname is_Accn
#' @export
is_AccnMultiStack <- function(x, strict = TRUE){
  if(strict == TRUE){
    inherits(x,"AccnMultiStack")
  } else{
    inherits(x,"AccnkMultiStack")
  }
}
#' @rdname is_Accn
#' @export
is_Accn2 <- function(x, strict = TRUE){
  if(strict == TRUE){
    inherits(x,"Accn2") & !inherits(x,"Accn") & !inherits(x,"AccnStack") & !inherits(x,"AccnMultiStack")
  } else{
    inherits(x,"Track2")
  }
}

#' accn_multi_stack
#'
#' Coerce \code{AccnStack} class objects into a \code{AccnMultiStack} class object.
#'
#' @param data Object to be assesed.
#'
#' @return Return new \code{data} coerced to a \code{\link{AccnMultiStack}} class.
#' @export
accn_multi_stack <- function(data = NULL){
  if(!is_AccnStack(data[[1]])){stop("data should be a AccnStack class object")}
  data <- AccnMultiStack(data) # data should be a list(x,y)
  return(data)
}

#' @noRd
is.Accn <- is_Accn
is.AccnStack <- is_AccnStack
is.AccnMultiStack <- is_AccnMultiStack










