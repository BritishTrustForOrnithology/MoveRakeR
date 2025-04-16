#' Summarise \code{Track}, \code{TrackStack} and \code{TrackMultiStack} class objects
#'
#' A function using the S3 \code{summary} method for \code{Track-family} objects,
#' that gives a simple summary using \code{base::summary}.
#'
#' \code{summary} was designed for easy indication as to whether data match
#' the user's expectation, such as upon read in from databases, or after manipulation stages,
#' e.g. checking the date ranges per animal are as expected if start and end times were specified in
#' \code{\link{read_track_MB}} and \code{\link{read_track_UvA}}.
#' NOTE: output from \code{summary} is to be incorporated in a shiny viewing of the data.
#'
#' @rdname summary
#' @param data A \code{Track}, \code{TrackStack}, or \code{TrackMultiStack} object.
#' @return An object of class \code{TrackSummary}, \code{TrackSummaryStack} or \code{TrackSummaryMultiStack}, containing
#' \code{base::summary} information for all columns in the \code{Track} data.frame objects. Additional slots
#' of dimension of the data \code{dim_data}, number of animals, \code{animals}, years included for
#' animals, \code{years}, and specific TagIDs extracted, \code{TagIDs}.
#' @param Simplify Logical defaulting to TRUE for whether to simplify the data to only
#' core data columns of: "TagID", "DateTime", "longitude", "latitude".
#' @param extra_vars for a concatenation of column names that should be retained in simplification
#' of the Track data, in addition to the core columns specified; not this argument is only relevant if
#' simplify = TRUE.
#' @param simple_view Logical (default = TRUE) for if just a simple table of fixes, datetime range,
#' number of fixes and long-lat range is required.
#'
#' @examples
#' indata <- yourdata # data.frame with a minimum of columns named TagID, DateTime, longitude, latitude
#' data <- Track(indata) # optional to convert to in-house class (most functions will still run on non track data but generic S3 methods will not be available for plot and summary)
#'
#' summary(data)
#'
#' @export
#' @method summary Track
summary.Track <- function(object, Simplify = TRUE, extra_vars = NULL, simple_view = TRUE){

  data <- object # in keeping with name of first arg in generic summary method

  if(!is(data, "Track")){
    stop("Data are not a valid Track, TrackStack or TrackMultiStack object")
  }

  if(Simplify){
    data <- simplify(data, keep = extra_vars)
  }

  .summary_track <- function(data){
    # sumarise animal through generic base::summary data add a couple of attributes

    .risub <- function(x,n){substring(x,nchar(x)-n+1)}

    if(simple_view == FALSE){
      data <- lapply(data, function(x) subset(x, select = -TagID))
      dataout <- lapply(data,summary.data.frame)
    }

    dimdata <- data.frame(do.call('rbind',lapply(data,function(x){dim(x)[1]})))
    names(dimdata)[c(1)] <- c("n_fixes")
    ddrange <- data.frame(do.call('rbind',lapply(data,function(x){as.character(range(x$DateTime))})))
    dimdata$start <- datetime(ddrange$X1)
    dimdata$end <- datetime(ddrange$X2)
    dimdata$dur <- round(difftime(dimdata$end,dimdata$start, unit = 'days'),2)

    xrange <- data.frame(do.call('rbind',lapply(data,function(x){as.character(range(x$longitude))})))
    yrange <- data.frame(do.call('rbind',lapply(data,function(x){as.character(range(x$latitude))})))

    dimdata$xrange <- paste0("(",round(as.numeric(xrange$X1),2),", ", round(as.numeric(xrange$X2),2),")")
    dimdata$yrange <- paste0("(",round(as.numeric(yrange$X1),2),", ", round(as.numeric(yrange$X2),2),")")

    dimdata$TagID <- do.call('rbind',lapply(data, function(x) as.vector(unique(x$TagID))))[,1]
    dimdata <- dimdata[order(dimdata$TagID),]

    if(!is.null(extra_vars)){

      for(i in 1:length(extra_vars)){

        # check if numeric or integer
        check_integer <- function(x){if(any(!x%%1 == 0)){FALSE} else{TRUE}}

        if(is.numeric(data[[1]][names(data[[1]]) == extra_vars[i]][[1]])){

          if(check_integer(data[[1]][names(data[[1]]) == extra_vars[i]][[1]]) == TRUE){

            Ra <- do.call('rbind',
                          lapply(data, function(x){
                            range(x[names(x) == extra_vars[i]])
                          }
                          )
            )

            L <- do.call('rbind',
                          lapply(data, function(x){
                            length(unique(x[names(x) == extra_vars[i]])[[1]])

                          }
                          )
            )

            nm <- paste0("(",round(Ra[,1],2),", ",round(Ra[,2],2),", n=",L[,1],")")
            dimdata <- cbind(dimdata, nm)
            names(dimdata)[which(names(dimdata) == "nm")] <- paste0(extra_vars[i],"_range")

          }

          if(check_integer(data[[1]][names(data[[1]]) == extra_vars[i]][[1]]) == FALSE){

            Ra <- do.call('rbind',
                          lapply(data, function(x){
                            range(x[names(x) == extra_vars[i]])
                          }
                          )
            )
            nm <- paste0("(",round(Ra[,1],2),", ",round(Ra[,2],2),")")
            dimdata <- cbind(dimdata, nm)
            names(dimdata)[which(names(dimdata) == "nm")] <- paste0(extra_vars[i],"_range")


          }


        } else{ # character, factor etc
          Ra <- do.call('rbind',
            lapply(data, function(x){
              levs <- as.character(unique(x[names(x) == extra_vars[i]][[1]]))
              if(length(levs) == 2){
                stlis1 <- paste0(substr(levs[1],1,3),"~",.risub(levs[1],2))
                stlis2 <- paste0(substr(levs[1],1,3),"~",.risub(levs[2],2))
                stlis <- paste0(stlis1, ", ", stlis2)
                paste0("<chr> ", length(levs), " levs: ", stlis)
              } else
                if(length(levs) > 2){
                  stlis <- paste0(substr(levs[1],1,3),"~",.risub(levs[1],2), ", ...")
                  paste0("<chr> ", length(levs), " levs: ", stlis)
                }
                else{
                stlis <- paste0(substr(levs[1],1,3),"~",.risub(levs[1],2))
                paste0("<chr> ", length(levs), " lev: ", stlis)
              }

             }
            )
          )

          nm <- Ra[,1]
          dimdata <- cbind(dimdata, nm)
          names(dimdata)[which(names(dimdata) == "nm")] <- paste0(extra_vars[i],"_range")

        }

      }

    }

    if(!simple_view){
      attributes(dataout)$dim_data <- dimdata
      names(dataout) <- as.vector(do.call('rbind',lapply(data,function(x) unique(x$TagID)))[,1])

      attributes(dataout)$animals <- length(data)
      attributes(dataout)$years <- lapply(data,function(x){unique(as.numeric(substr(x$DateTime,1,4)))})
      attributes(dataout)$TagIDs <- as.vector(do.call('rbind',lapply(data,function(x){unique(x$TagID)}))[,1])
      return(dataout)
    } else{
      return(dimdata)
    }

  }


  if(is_Track(data)){

    # create list format of Track objects for consistent output with TrackStack
    data2 <- list()
    for(i in 1:length(unique(data$TagID))){
      data2[i] <- list(subset(data,TagID == unique(data$TagID)[i]))
    }
    message(paste0("Summary for ", length(data2)," Track objects (animals)"))

    dataout <- .summary_track(data2)
    class(dataout) <- append("TrackSummary",class(dataout))

    return(dataout)
  }

  else if(is_TrackStack(data)){
    message(paste0("Summary for ", length(data)," Track objects (animals)"))

    dataout <- .summary_track(data)
    class(dataout) <- append("TrackSummaryStack",class(dataout))

    return(dataout)

  }

  else if(is_TrackMultiStack(data)){

    anikeep <- list()
    for(j in 1:length(data)){
      anikeep[[j]] <- length(data[[j]])
    }

    message(paste0("Summary for ", length(data)," TrackStack objects (", paste(unlist(anikeep), collapse = ", "), " animals, repectively)"))

    dataout <- lapply(data,.summary_track)
    class(dataout) <- append("TrackSummaryMultiStack",class(dataout))

    return(dataout)


  }


}
