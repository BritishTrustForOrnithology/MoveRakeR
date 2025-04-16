#' Convert a Track object to a move or moveStack object
#'
#' \code{Track2move} acts as a bridge between \code{MoveRakeR} and the \code{move}
#' R package (Kranstauber et al. 2021), converting \emph{Track} objects into
#' a \emph{move:move} or emph{move:moveStack} object.
#'
#' @param data A \code{Track} family object (no S3 methods are coded).
#' @param group_anims whether to treat animals with the same 'TagID' as
#' the same individual animal (e.g. if previously separated into different TrackStacks
#' due to treatment differently across years, etc. defaults to TRUE.If TRUE, ONE \code{move::move}
#' object will first be created for the single animal prior to combining into the \code{move::moveStack};
#' if false, they will be treated as separate "TagIDs" in the the resulting move:: object.
#'
#' @details
#' The \code{MoveRakeR} workflow may only take the user so far in their analytical journey. Function
#' \code{Track2move} takes the class back to \code{move} for further use of that R package.
#' Note that \code{Track2move} first converts datasets first to a \code{TrackStack} to apply an internal function
#' to further convert each \code{Track} object element to a \code{move} object for each animal (TagID). Also note,
#' that if only one animal (TagID) is to be translated to \code{move} format, then a \code{move::move} object will
#' be returned rather than a \code{moveStack} object. This still uses \code{sp::CRS()}, so apologies, this
#' is not dovetailed with \code{move2} and \code{sf}.
#'
#' @return
#' A \code{move::moveStack} object for further use with the the \code{move} package. See
#' package documentation and \url{https://cran.r-project.org/web/packages/move/vignettes/move.html#dynamic-brownian-bridge-movement-model-dbbmm}
#' for more detailed usage. In many ways, \code{Track2move} is the opposite of \code{Movebank2Track} that
#' reads in a static Movebank-downloaded csv to convert to a \code{Track} object.
#'
#' @seealso [MoveRakeR::sub_samp]
#'
#' @references
#' Bart Kranstauber, Marco Smolla and Anne K Scharf (2021). move: Visualizing and Analyzing
#' Animal Track Data. R package version 4.0.6. \url{https://bartk.gitlab.io/move/}
#'
#' @examples
#' # -------------------------------------------------------------------- #
#' # covert to a move::moveStack object
#' myStack = Track2move(data = data)
#'
#' # opens up the move playbox:
#' move::timestamps(myStack)
#'
#' ## Thinning example - note this is also done in MoveRakeR with some additional flexibility and bootstrapping
#' thintime <- move::thinTrackTime(myStack[["1"]], interval = as.difftime(30, units='mins'), tolerance = as.difftime(5, units='mins'))
#' data_ <- subset_TMS(data,TagID = "1")
#' data_ <- sub_samp(data_, dt = 1800, tol = 0.16667) # BTOTT track_subsamp test
#'
#' # The treatment of these functions between move and MoveRakeR is different (resulting in different no's rows).
#' # MoveRakeR has a tolerance value but it is for looking at all consecutive
#' # gaps and allows fixes to be treated as consecutive valid recording (i.e. not a gap)
#' # up to val+(val*tol), whereas the move::thinTrackTime function sort of does the reverse
#' # and allows lower value gaps DOWN to the tolerance value, I think. Get in touch if interested further.
#'
#' thintime5 <- move::thinTrackTime(myStack[["1"]], interval = as.difftime(5, units='mins'), tolerance = as.difftime(1, units='mins'))
#' sub_ <- sub_samp(data_, dt = 300)
#'
#' # No methods are available in MoveRakeR so adivsable to got to other packages like move or adehabitatLT for this
#' interp5min <- move::interpolateTime(myStack[["1"]], time=as.difftime(5, units="mins"), spaceMethod='greatcircle')
#' sp::plot(interp5min, col="red",pch=20, main="By number of locations")
#' move::points(myStack[["1"]])
#' move::points(interp5min)
#'
#' ######
#' @import sp
#' @import move
#' @export
Track2move <- function(data, group_anims = TRUE){

  # if(is_TrackMultiStack(data)){
  #    data = TrackMultiStack2Track(data)
  #    #data$stack = do.call('rbind',strsplit(data$TagID,"_stack_"))[,2]
  #    data$TagID = do.call('rbind',strsplit(data$TagID,"_stack_"))[,1]
  #  }

  # making elements as TrackStack class for applying move class creation (works better that way)
  if(is_TrackMultiStack(data)){
    data = TrackMultiStack2TrackStack(data, renam=FALSE)
  }
  if(is_Track(data)){
    data = Track2TrackStack(data, by = "TagID")
  }

  if(group_anims == TRUE){
    message("Grouping animals across any stacks used in BTOTT")
    # if TRUE then any stacking differences are lost, i.e. animals will not be kept
    # as separate - if FALSE then move:: will group the animals automatically
    # although you can also make this happen by having renam = TRUE in TrackMultiStack2TrackStack
    # combine duplicate TagIDs that may have been from different stacks
    #ww <- names(which(table(names(data)) > 1))

    # any names present in the TrackStack?
    if(is.null(names(data))){

      nms <- list()
      for(i in 1:length(data)){
        nms[[i]] <- as.vector(unique(data[[i]]$TagID)[1])
      }
      names(data) <- unlist(nms)

    }

    # go through these and make a single Track for the same TagIDs
    yy <- as.list(get_TagIDs(data, verbose = FALSE))
    rbind_Track <- function(x){
      da <- subset_TMS(data, TagIDs = x)
      do.call('rbind',da)
    }
    data = TrackStack(lapply(yy,rbind_Track))

  }

  # clear the Track class, but safest to build the moveStack from applying over list
  # otherwise sometimes it does not make the class right

  do_one_Move <- function(x){
    x <- as.data.frame(x)
    move::move(x = x$longitude, y = x$latitude, time = x$DateTime,
               #proj = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"),
               proj=sp::CRS("+proj=longlat +ellps=WGS84"),
               #proj = sf::st_crs(4326), # NOT POSSIBLE!
               data = x, animal = x$TagID, sensor = "GPS")
  }

  if(length(data) > 1){
    message("Translating multiple animals to move::moveStack class")
    MS <- lapply(data, do_one_Move)
    myStack <- move::moveStack(MS,forceTz="UTC")

  } else{
    message("Translating single animal to move::move class")
    myStack <- do_one_Move(data[[1]])
  }


  return(myStack)


}

