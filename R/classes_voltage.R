#' Voltage ("Vo") and voltage communication ("VoCom") constructor classes for MoveRakeR
#'
#' @description Functions to append object classes of \code{Vo}, \code{VoStack} and \code{VoMultiStack},
#' and \code{VoCom}, \code{VoComStack} and \code{VoComMultiStack},
#' obtained respectively using their respective namesake functions respectively. this is a specific
#' class for the UvA voltage ("Vo") and communication including voltage data ("VoCom") data.
#'
#' @details  Three special object classes are returned from initial the read in function
#' \code{\link{read_voltage_UvA}}. \code{Vo} is typically representative
#' of an individual animal's time-series, but an 'rbinded' \code{Vo}
#' object of several animals, i.e. multiple TagIDs, will also be accepted as a \code{Vo}
#' object and processed for each individual, thus \code{Vo} objects represent a single \code{data.frame} class;
#' \code{VoStack} and \code{VoMultiStack} objects are nested \code{list} formats, and (as currently implemented)
#' \emph{must be treated as one individual per data.frame}. These stacked listed methods
#' are intended for efficient application of routines for groups of animals. Objects can be coerced into
#' \code{Vo}, \code{VoStack} and \code{VoMultiStack} classes but they require the following
#' minimum column names: \strong{TagID}, \strong{DateTime} and \strong{vsll} and \strong{vbat}.
#' All the above is also true for the \code{VoCom} class which is here specified as a separate class
#' as it comes from a different part of the UvA-BiTS database with a different layout,
#' although it also contains voltage information, hence treated like a voltage class. The "VoCom"
#' class requires the column headings: \strong{TagID}, \strong{DateTime} and \strong{vbat} and \strong{bytes_communicated}.
#' Further checking of whether objects belong to special classes are provided by:
#' \code{link{is_Vo}}, \code{link{is_VoCom}},
#' \code{link{is_VoStack}}, \code{link{is_VoComStack}},
#' \code{\link{is_VoMultiStack}}, and \code{\link{is_VoComMultiStack}}and coercing of two
#' \code{VoStack} objects is provided by \code{\link{voltage_multi_stack}}.
#'
#' @seealso [MoveRakeR::Track], [MoveRakeR::Accn], [MoveRakeR::pressure_multi_stack], [MoveRakeR::is_Pr], [MoveRakeR::is_PrStack], [MoveRakeR::is_PrMultiStack]
#'
#' @param data An input base::data.frame for a voltage dataset from UvA-BiTS that is to be
#' coerced to the designated class.
#' @return For tracking data, returns a \code{Vo}, \code{VoStack} or
#' \code{VoMultTrackStack} from the "ee_energy_limited" UvA-BiTS database view, or
#' a \code{VoCom}, \code{VoComStack} or \code{VoComMultTrackStack} from the "ee_comm_limited" UvA-BiTS database view.
#' @export

Vo <- function(data){
  if(is.null(data$TagID) | is.null(data$DateTime) | is.null(data$vsll) | is.null(data$vbat)){
    stop("Can't construct Vo object: fields are missing or mispelled")}

  obj <- structure(.Data = data, class = c("Vo","data.frame"))
  return(obj)
}
#' @rdname Vo
#' @export
VoCom <- function(data){
  if(is.null(data$TagID) | is.null(data$DateTime) | is.null(data$vbat) | is.null(data$bytes_communicated)){
    stop("Can't construct VoCom object: fields are missing or mispelled")}

  obj <- structure(.Data = data, class = c("VoCom","data.frame"))
  return(obj)
}
#' @rdname Vo
#' @export
Vo2 <- function(data){
  if(is.null(data$TagID) | is.null(data$DateTime) | is.null(data$vsll) | is.null(data$vbat)){
    stop("Can't construct Vo2 object: fields are missing or mispelled")}

  obj <- structure(.Data = data, class = c("Vo2","data.frame"))
  return(obj)
}
#' @rdname Vo
#' @export
VoCom2 <- function(data){
  if(is.null(data$TagID) | is.null(data$DateTime) | is.null(data$vbat) | is.null(data$bytes_communicated)){
    stop("Can't construct VoCom2 object: fields are missing or mispelled")}

  obj <- structure(.Data = data, class = c("VoCom2","data.frame"))
  return(obj)
}
#' @rdname Vo
#' @export
VoStack <- function(data){
  if(!is.list(data))
    stop("Can't construct VoStack object: data are not a list")
  if(is.null(data[[1]]$TagID) | is.null(data[[1]]$DateTime) | is.null(data[[1]]$vsll) | is.null(data[[1]]$vbat)){
    stop("Can't construct VoStack object: fields are missing or mispelled")
  }
  for(j in 1:length(data)){
    data[[j]] <- structure(.Data = data[[j]], class = c("Vo","data.frame"))
  }
  obj <- structure(.Data = data, class = c("Vo","VoStack","list"))
  return(obj)
}
#' @rdname Vo
#' @export
VoMultiStack <- function(data){
  # expects lists in lists
  if(!is.list(data)[[1]]){
    stop("Can't construct VoMultiStack object: data are not a list")}
  if(is.null(data[[1]][[1]]$TagID) | is.null(data[[1]][[1]]$DateTime) | is.null(data[[1]][[1]]$vsll) | is.null(data[[1]][[1]]$vbat)){
    stop("Can't construct VoMultiStack object: fields are missing or mispelled")
  }
  for(i in 1:length(data)){
    for(j in 1:length(data[[i]])){
      data[[i]][[j]] <- structure(.Data = data[[i]][[j]], class = c("Vo","data.frame"))
    }
    data[[i]] <- structure(.Data = data[[i]], class = c("Vo","VoStack","list"))
  }
  obj <- structure(.Data = data, class = c("Vo","VoStack","VoMultiStack","list"))
  return(obj)
}
#' @rdname Vo
#' @export
VoComStack <- function(data){
  if(!is.list(data))
    stop("Can't construct VoComStack object: data are not a list")
  if(is.null(data[[1]]$TagID) | is.null(data[[1]]$DateTime) | is.null(data[[1]]$vbat) | is.null(data[[1]]$bytes_communicated)){
    stop("Can't construct VoComStack object: fields are missing or mispelled")
  }
  for(j in 1:length(data)){
    data[[j]] <- structure(.Data = data[[j]], class = c("VoCom","data.frame"))
  }
  obj <- structure(.Data = data, class = c("VoCom","VoComStack","list"))
  return(obj)
}
#' @rdname Vo
#' @export
VoComMultiStack <- function(data){
  # expects lists in lists
  if(!is.list(data)[[1]]){
    stop("Can't construct VoComMultiStack object: data are not a list")}
  if(is.null(data[[1]][[1]]$TagID) | is.null(data[[1]][[1]]$DateTime) | is.null(data[[1]][[1]]$vbat) | is.null(data[[1]][[1]]$bytes_communicated)){
    stop("Can't construct VoComMultiStack object: fields are missing or mispelled")
  }
  for(i in 1:length(data)){
    for(j in 1:length(data[[i]])){
      data[[i]][[j]] <- structure(.Data = data[[i]][[j]], class = c("VoCom","data.frame"))
    }
    data[[i]] <- structure(.Data = data[[i]], class = c("VoCom","VoComStack","list"))
  }
  obj <- structure(.Data = data, class = c("VoCom","VoComStack","VoComMultiStack","list"))
  return(obj)
}

#' is_Vo
#'
#' Check that an object is of class \code{\link{Vo}}, class \code{\link{VoStack}} or class \code{\link{VoMultiStack}}.
#'
#' @param x Object to be assesed.
#' @param strict For is_ functions, logical for should the class check search strictly for the
#' specific sub-class, e.g. VoStack, or VoMultiStack (default = TRUE), but if FALSE
#' then the function checks as a normal "is" or inherits for the super class, e.g. Vo (even if object is a VS or VMS).
#' @return Logical \code{TRUE} if \code{x} belongs to class \code{\link{Vo}}, \code{\link{VoStack}} or \code{\link{VoMultiStack}}, otherwise returns \code{FALSE}.
#' @export
is_Vo <- function(x, strict = TRUE){
  if(strict == TRUE){
    inherits(x,"Vo") & !inherits(x,"Vo2") & !inherits(x,"VoStack") & !inherits(x,"VoMultiStack")
  } else{
    inherits(x,"Vo")
  }
}
#' @rdname is_Vo
#' @export
is_VoStack <- function(x, strict = TRUE){
  if(strict == TRUE){
    inherits(x,"VoStack") & !inherits(x,"VoMultiStack")
  } else{
    inherits(x,"VoStack")
  }
}
#' @rdname is_Vo
#' @export
is_VoMultiStack <- function(x, strict = TRUE){
  if(strict == TRUE){
    inherits(x,"VoMultiStack")
  } else{
    inherits(x,"VoMultiStack")
  }
}
#' @rdname is_Vo
#' @export
is_Vo2 <- function(x, strict = TRUE){
  if(strict == TRUE){
    inherits(x,"Vo2") & !inherits(x,"Vo") & !inherits(x,"VoStack") & !inherits(x,"VoMultiStack")
  } else{
    inherits(x,"Vo2")
  }
}

#' is_VoCom
#'
#' Check that an object is of class \code{\link{VoCom}}, class \code{\link{VoComStack}} or class \code{\link{VoComMultiStack}}.
#'
#' @param x Object to be assesed.
#' @param strict For is_ functions, logical for should the class check search strictly for the
#' specific sub-class, e.g. VoStack, or VoMultiStack (default = TRUE), but if FALSE
#' then the function checks as a normal "is" or inherits for the super class, e.g. VoCom (even if object is a VComS or VComMS).
#' @return Logical \code{TRUE} if \code{x} belongs to class \code{\link{VoCom}}, \code{\link{VoComStack}} or \code{\link{VoComMultiStack}}, otherwise returns \code{FALSE}.
#' @export
is_VoCom <- function(x, strict = TRUE){
  if(strict == TRUE){
    inherits(x,"VoCom") & !inherits(x,"VoCom2") & !inherits(x,"VoComStack") & !inherits(x,"VoComMultiStack")
  } else{
    inherits(x,"VoCom")
  }
}
#' @rdname is_VoCom
#' @export
is_VoComStack <- function(x, strict = TRUE){
  if(strict == TRUE){
    inherits(x,"VoComStack") & !inherits(x,"VoComMultiStack")
  } else{
    inherits(x,"VoComStack")
  }
}
#' @rdname is_VoCom
#' @export
is_VoComMultiStack <- function(x, strict = TRUE){
  if(strict == TRUE){
    inherits(x,"VoComMultiStack")
  } else{
    inherits(x,"VoComMultiStack")
  }
}
#' @rdname is_VoCom
#' @export
is_VoCom2 <- function(x, strict = TRUE){
  if(strict == TRUE){
    inherits(x,"VoCom2") & !inherits(x,"VoCom") & !inherits(x,"VoComStack") & !inherits(x,"VoComMultiStack")
  } else{
    inherits(x,"VoCom2")
  }
}

#' voltage_multi_stack
#'
#' Coerce \code{VoStack} class objects into a \code{VoMultiStack} class object.
#'
#' @param data Object to be assesed.
#'
#' @return Return new \code{data} coerced to a \code{\link{VoMultiStack}} class.
#' @export
voltage_multi_stack <- function(data = NULL){
  if(!is_VoStack(data[[1]])){stop("data should be a VoStack class object")}
  data <- VoMultiStack(data) # data should be a list(x,y)
  return(data)
}

#' @noRd
is.Vo <- is_Vo
is.VoStack <- is_VoStack
is.VoMultiStack <- is_VoMultiStack
#' @noRd
is.VoCom <- is_VoCom
is.VoComStack <- is_VoComStack
is.VoComMultiStack <- is_VoComMultiStack

#' Track2Vo
#'
#' Conversion between \code{Track} and \code{Vo} classes
#'
#' The \code{Track2Vo} function is required for use of the plot generic S3 method for \code{Vo} objects,
#' to first create a \code{Vo} object from you track data based on columns that are assumed to be existing
#' within the \code{Track} data.
#'
#' @param data Track object to be converted.
#' @param vsll column in your data relating to solar voltage charging (e.g. voltage or percentage)
#' @param vbat column in your data relating to the battery charge (e.g. voltage, or percentage)
#'
#' @example
#' Vo_obj = Track2Vo(data, vbat = "tag-voltage", vsll = "battery-charging-current")
#'
#' Vo_obj_subset = Vo_obj[Vo_obj$TagID == "animal_1",] # subsetting for one animal if wanted
#' track_data = data[data$TagID == "animal_1",]
#'
#' # plot with track data alongside for additional day/night bars, with a subset of time period
#' plot_st = plot(Vo_obj_subset, dataTrack = track_data, start = "2022-04-05 00:00:00", end = "2022-04-10 00:00:00")
#'
#' @export
Track2Vo <- function(data, vsll = NULL, vbat = NULL){

  w = which(names(data) %in% c("TagID","DateTime"))

  if(length(w) != 2){
    stop("Needs TagID and DateTime columns defined")
  }

  # detect columns flagged as vsll or vbat
  vbatcol = which(names(data) %in% vbat)
  vsllcol = which(names(data) %in% vsll)

  # always keep TagID and DateTime
  data0 <- data[,names(data) %in% c("TagID","DateTime")]

  if(length(vbatcol) == 0){
    message("No vbat column found matching supplied name")
    data0$vbat <- NA
  } else{
    data0$vbat = data.frame(data)[,vbatcol]
  }

  if(length(vsllcol) == 0){
    message("No vbat column found matching supplied name")
    data0$vsll <- NA
  } else{
    data0$vsll = data.frame(data)[,vsllcol]
  }
  row.names(data0) <- 1:nrow(data0)

  data0 <- Vo(data0)

  return(data0)
}






