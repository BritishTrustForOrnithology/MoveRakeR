#' Conversions to and from MoveRakeR data classes
#'
#' It may be necessary to rework the structure of \code{Track}, \code{TrackStack} and \code{TrackMultiStack} objects,
#' or convert from one type to another aggregating across variables. For example plotting purposes,
#' or should the user have read in the data but then want to regroup structures by a column
#' in the data, such as incubation vs chick rearing in birds, or years etc, that is if the user
#' is interested in using base R operations. Importantly, however \code{MoveRakeR} encourages use of tidy approaches
#' and so makes use of \code{dplyr} and \code{tidyr}, with the grouping structure
#' essentially taking the place of the previous way of doing things in this R package that was
#' essentially applying functions over lists, hence the 'stacks' and 'multistacks'. These forms have been retained
#' though for now but may be deprecated in the future.
#'
#' These functions provide flexibility to convert between base R listed forms for \code{Track}-family objects.
#' Function \code{Track2TrackStack} allows a TrackStack object to be built from a Track object using a variable to build each Track element.
#' \code{Track2TrackMultiStack} operates at the higher level and expects two named variables
#' to build both the listed elements of TrackStack objects and the variable to group multiple TrackStack objects.
#' Both these functions therefore need labelled columns in the Track object for the groupings.
#'
#' Note, currently NOT implemented: TrackStack2TrackMultiStack
#'
#' @param data A \code{Track}, \code{TrackStack} or \code{TrackMultiStack} object, depending on the
#' conversion types to and from.
#' @param by A single vector for \code{Track2TrackStack} and a two length vector for
#' \code{Track2TrackMultiStack} denoting the name of the columns which to make the transposition.
#' @param ord Currently only "seq" is available as an option for ordering the levels in sequential
#' order for \code{Track2TrackMultiStack}, but bespoke ordering is possible for the \code{Track2TrackStack}
#' where \emph{ord} accepts a concatenated numeric vector denoting the order in which to place the
#' elements for the intended TrackStack outputted object.
#' @param nm Specific to TMS to TS and TMS to Track conversions, defaulting to "_stack_" inserted for animal names
#' reflecting which stack they were combined from to higher level listing.
#' @param rename logical defaulting to TRUE for if renaming using \code{nm} should be used.
#' @return A \code{Track}, \code{TrackStack} or \code{TrackMultiStack} depending on the function
#' and conversion choices.
#'
#' @export
Track2TrackMultiStack <- function(data, by = NULL, ord = "seq"){
  # currently only sequential ordering is implemented

  if(!is_Track(data)){stop("Expecting a Track class object of a single data.frame")}
  message(paste0("Contructing a TrackMultiStack object from Track object using columns ", by[1], " and ", by[2]))

  w1 <- which(names(data) == by[1])
  w2 <- which(names(data) == by[2])

  if(is.null(by)){stop("Needs two by variables to be specified")}
  if(length(by) > 2) {stop("Only two listed levels implemented")}
  if(length(by) < 2) {stop("Needs a variable for the Track and TrackStack elements repectively")}
  if(length(w1) == 0 | length(w2) == 0){stop("No columns found mathing the names provided")}

  if(ord == "seq"){
    #ord <- list(c(1:length(levs1)),c(1:length(levs2)))
    data. <- data[order(data[,w1]),]
    data. <- data.[order(data.[,w2]),]
  }
  levs1 <- as.factor(as.vector(unique(data.[,w1])))
  levs2 <- as.factor(as.vector(unique(data.[,w2])))

  # but of course splitting across stacks etc by ANOTHER variable may mean that
  # some elements drop out, i.e. not commmon to all!

  # make the TMS object
  # TrackMultiStack

  obj <- list()
  j <- 1
  for(j in 1:length(levs2)){
    obj[[j]] <- list()
    for(i in 1:length(levs1)){
      obj[[j]][[i]] <- data.[data.[,w1] == as.vector(levs1[i]) & data.[,w2] == as.vector(levs2[j]),]
    }
    obj[[j]] <- structure(.Data = obj[[j]], class = c("Track","TrackStack","list"))
    names(obj[[j]]) <- levs1
  }

  # check for blank listed entries

  for(j in 1:length(levs2)){
    obj[[j]] <- obj[[j]][sapply(obj[[j]], function(x) dim(x)[1]) > 0]
    obj[[j]] <- structure(.Data = obj[[j]], class = c("Track","TrackStack","list"))


  }

  obj <- structure(.Data = obj, class = c("Track","TrackStack","TrackMultiStack","list"))
  names(obj) <- levs2

  return(obj)

}

#'
#' @rdname Track2TrackMultiStack
#' @export
Track2TrackStack <- function(data, by = NULL, ord = "seq"){

  if(!is_Track(data)){stop("Expecting a Track class object of a single data.frame")}
  message(paste0("Contructing a TrackStack object from Track object using column ", by))

  if(is.null(by)){stop("Needs a variable to be specified for the Track objects in the stack")}

  # make a listed format from tag IDs in a Track object OR ANY OTHER COLUMN!
  # and name the track stack by the factor of that column

  w <- which(names(data) == by)
  if(length(w) == 0){stop("No column found mathing the name provided")}

  # bit of a safety net just converting everthing to a factor
  # specify a bespoke order of the data
  levs <- as.factor(as.vector(unique(data[,w])))

  if(!is.null(ord)){
    if(ord == "seq"){
      levs <- levs[order(levs)]
    } else {
      if(length(ord) != length(levs)){stop("Length or ord must equal that of levels")}
      levs <- levs[order(levs)[ord]] # bespoke order, but assuming order is after levels HAVE BEEN SORTED!
    }
  } else{ # defaulting to sequence order of factor
    levs <- levs[order(levs)]
  }

  data[,w] <- as.factor(as.vector(data[,w]))

  # TrackStack
  obj <- list()
  for(i in 1:length(levs)){obj[[i]] <- data[data[,w] == levs[i],]}
  names(obj) <- levs
  obj <- structure(.Data = obj, class = c("Track","TrackStack","list"))
  return(obj)

  # check for blank listed entries
  obj <- obj[sapply(obj, function(x) dim(x)[1]) > 0]
  obj <- structure(.Data = obj, class = c("Track","TrackStack","list"))

  return(obj)

}

#'
#' @rdname Track2TrackMultiStack
#' @export
TrackStack2Track <- function(data){

  # convert TrackStacks to a single Track object - really a simple rbind!
  if(!is_TrackStack(data)){stop("Expecting a TrackStack class object of a list of data.frames")}

  obj <- structure(.Data = do.call('rbind',data), class = c("Track","data.frame"))

  return(obj)
}

#'
#' @rdname Track2TrackMultiStack
#' @export
TrackMultiStack2TrackStack <- function(data, rename = TRUE, nm = "_stack_"){

  # convert TrackStacks to a single Track object - really a simple rbind!
  if(!is_TrackMultiStack(data)){stop("Expecting a TrackMultiStack class object of multiple lists of data.frames")}

  # needs a stack of the two track stacks into one
  obj <- NULL
  i <- 1
  for(i in 1:length(data)){
    if(rename == TRUE){
      names(data[[i]]) <- paste0(names(data[[i]]),nm,i)
    }
    obj <- c(obj,data[[i]])
  }

  length(obj)

  for(j in 1:length(obj)){
    obj[[j]]$TagID <- names(obj)[j]
  }

  obj <- structure(.Data = obj, class = c("Track","TrackStack","list"))

  return(obj)
}

#'
#' @rdname Track2TrackMultiStack
#' @export
TrackMultiStack2Track <- function(data, rename = TRUE, nm = "_stack_"){

  # convert TrackStacks to a single Track object - really a simple rbind!
  if(!is_TrackMultiStack(data)){stop("Expecting a TrackMultiStack class object of multiple lists of data.frames")}

  # needs a stack of the two track stacks into one
  obj <- NULL
  i <- 1
  for(i in 1:length(data)){
    if(rename == TRUE){
      names(data[[i]]) <- paste0(names(data[[i]]),nm,i)
    }
    obj <- c(obj,data[[i]])
  }

  for(j in 1:length(obj)){
    obj[[j]]$TagID <- names(obj)[j]
  }

  obj <- structure(.Data = do.call('rbind',obj), class = c("Track","data.frame"))


  return(obj)
}

#' @rdname Track2TrackMultiStack
#' @export
TripMultiStack2Trip <- function(data, rename = TRUE, nm = "_stack_"){

  # convert TrackStacks to a single Track object - really a simple rbind!
  if(!is_TripMultiStack(data)){stop("Expecting a TripMultiStack class object of multiple lists of data.frames")}

  # needs a stack of the two track stacks into one
  obj <- NULL
  i <- 1
  for(i in 1:length(data)){
    if(rename == TRUE){
      names(data[[i]]) <- paste0(names(data[[i]]),nm,i)
    }
    obj <- c(obj,data[[i]])
  }

  for(j in 1:length(obj)){
    obj[[j]]$TagID <- names(obj)[j]
  }

  obj <- structure(.Data = do.call('rbind',obj), class = c("Trip","data.frame"))


  return(obj)
}

#' @rdname Track2TrackMultiStack
#' @export
TripStack2Trip <- function(data){

  # convert TrackStacks to a single Track object - really a simple rbind!
  if(!is_TripStack(data)){stop("Expecting a TripStack class object of a list of data.frames")}

  obj <- structure(.Data = do.call('rbind',data), class = c("Trip","data.frame"))

  return(obj)
}

#' @rdname Track2TrackMultiStack
#' @export
Trip2TripMultiStack <- function(data, by = NULL, ord = "seq"){
  # currently only sequential ordering is implemented

  if(!is_Trip(data)){stop("Expecting a Trip class object of a single data.frame")}
  message(paste0("Contructing a TripMultiStack object from Trip object using columns ", by[1], " and ", by[2]))

  w1 <- which(names(data) == by[1])
  w2 <- which(names(data) == by[2])

  if(is.null(by)){stop("Needs two by variables to be specified")}
  if(length(by) > 2) {stop("Only two listed levels implemented")}
  if(length(by) < 2) {stop("Needs a variable for the Trip and TripStack elements repectively")}
  if(length(w1) == 0 | length(w2) == 0){stop("No columns found mathing the names provided")}

  if(ord == "seq"){
    #ord <- list(c(1:length(levs1)),c(1:length(levs2)))
    data. <- data[order(data[,w1]),]
    data. <- data.[order(data.[,w2]),]
  }
  levs1 <- as.factor(as.vector(unique(data.[,w1])))
  levs2 <- as.factor(as.vector(unique(data.[,w2])))

  # but of course splitting across stacks etc by ANOTHER variable may mean that
  # some elements drop out, i.e. not commmon to all!

  # make the TMS object
  # TrackMultiStack

  obj <- list()
  j <- 1
  for(j in 1:length(levs2)){
    obj[[j]] <- list()
    for(i in 1:length(levs1)){
      obj[[j]][[i]] <- data.[data.[,w1] == as.vector(levs1[i]) & data.[,w2] == as.vector(levs2[j]),]
    }
    obj[[j]] <- structure(.Data = obj[[j]], class = c("Trip","TripStack","list"))
    names(obj[[j]]) <- levs1
  }

  # check for blank listed entries

  for(j in 1:length(levs2)){
    obj[[j]] <- obj[[j]][sapply(obj[[j]], function(x) dim(x)[1]) > 0]
    obj[[j]] <- structure(.Data = obj[[j]], class = c("Trip","TripStack","list"))


  }

  obj <- structure(.Data = obj, class = c("Trip","TripStack","TripMultiStack","list"))
  names(obj) <- levs2

  return(obj)

}


