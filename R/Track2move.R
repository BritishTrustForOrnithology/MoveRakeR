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
#' Function \code{Track2move} takes the Track class back to \code{move} for further use of that R package.
#' Note that \code{Track2move} will return a \code{moveStack} object for multiple animals and
#' a \code{Move} object for one animal. This still uses \code{sp::CRS()}, so apologies, this
#' is not dovetailed with \code{move2} and \code{sf}.
#'
#' @return
#' A \code{move::moveStack} or \code{move::Move} object for further use with the the \code{move} package.
#' In many ways, \code{Track2move} is the opposite of \code{Movebank2Track} that
#' reads in a static Movebank-downloaded csv to convert to a \code{Track} object.
#'
#' @seealso [MoveRakeR::sub_samp], [MoveRakeR::Movebank2Track]
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
Track2move <- function(data){

  tids = unique(data$TagID)

  out_list <- list()
  for(i in 1:length(tids)){
    x <- data[data$TagID %in% tids[i],]
    x <- as.data.frame(x)

    print(nrow(x))
    out_list[[i]] <- move::move(x = x$longitude, y = x$latitude, time = x$DateTime,
                                proj=sp::CRS("+proj=longlat +ellps=WGS84"),
                                data = x, animal = x$TagID, sensor = "GPS")

  }

  if(length(out_list) > 1){
    message("Translating multiple animals to move::moveStack class")
    myStack <- move::moveStack(out_list,forceTz="UTC")

  } else{
    message("Translating single animal to move::move class")
    myStack <- out_list[[i]]
  }

  return(myStack)

}

