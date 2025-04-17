#' Get unique TagIDs from Track, TrackStack or TrackMultiStack object
#'
#' Simple \code{unique()} application across listed formats supplied, useful for summarising
#' and assessing how to plot individual animals, e.g. using the S3 generic \code{plot} function for \code{Track} objects. The S3 method
#' for this class has only one function associated with it, being only useful for the Track-family.
#'
#' @param data A \code{Track}, \code{TrackStack}, or \code{TrackMultiStack} object.
#' @param ... further arguments passed to or from other methods.
#' @export
get_TagIDs <- function(data, ...){
  UseMethod("get_TagIDs", data)
}

#' @rdname get_TagIDs
#' @param justlist logical defaults to FALSE, to list all animals in the object
#' regardless if animals are repeated, e.g. across \code{TrackMultiStacks} and returns a charcter
#' vector for all names
#' @param TMS_join logical defaulting to TRUE specific to TMS objects whether to show TagID names in each component stack
#' @param verbose logical (default TRUE) to return detailed messaging.
#'
#' @return A vector containing unique TagIDs in the dataset(s).
#'
#' @export
get_TagIDs.Track <- function(data,justlist=FALSE,TMS_join = TRUE, verbose = TRUE){
  # could be done through the names of the data too, but done on TagID column

  # Check classes, error capture
  if(!is(data, "Track")){ # using the generic is S3 method, will check if Track, TrackStack or TrackMultiStack now
    stop("Data are not a valid Track, TrackStack or TrackMultiStack object")
  }

  if(is_Track(data)){
    if(verbose){
      message("Getting TagIDs from Track object")
    }
    out <- unique(data$TagID)
  }
  if(is_TrackStack(data)){
    if(verbose){
      message("Getting TagIDs from TrackStack object")
    }

    if(!justlist){
      out <- unique(as.vector(unlist(lapply(data,function(x){unique(x$TagID)}))))
    } else{
      out <- names(data)
    }

  }

  if(is_TrackMultiStack(data)){
    if(verbose){
      message("Getting TagIDs from TrackMultiStack object")
    }

    if(!justlist){
      out2 <- list()
      for(i in 1:length(data)){
        out2[[i]] <- as.vector(unlist(lapply(data[[i]],function(x){unique(x$TagID)})))
      }
      if(TMS_join){
        out <- unique(unlist(out2))
      } else{
        out <- out2 # won't actually unique anything, just returns the names in each stack
      }

    }
    if(justlist){
      out2 <- list()
      for(i in 1:length(data)){
        out2[[i]] <- names(data[[i]])
      }
      if(TMS_join){
        out <- unlist(out2)
      } else{
        out <- out2 # just returns the names in each stack
      }
    }

  }

  return(out)

}


