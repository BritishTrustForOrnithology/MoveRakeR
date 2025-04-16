#' Ploting arrows along a Track
#'
#' @description
#' Plotting directional movement arrows along the route of an animal.
#'
#' @details
#' The function \code{plot_track_arrows} plots arrows along a
#' route of an animal indicating direction of movement. This function works by using basic
#' trigonometry to plot Cartesian coordinates for interpolated
#' re-locations along an animal's trajectory. The number of arrows
#' can be adjusted along the route by altering the \emph{corr_no_arr}
#' argument and the size length of the arrows either side of the line can be
#' altered with the \emph{corr_size_arr} argument. Further, the user can specify
#' the angle of the arrow by adjusting the \code{tol} argument. The latter
#' specifies a certain amount of tolerance around the direction of the animal
#' that is added and subtracted from the turning angle of movement between fixes, i.e.
#' increasing 'tol' widens the angle of the arrow. The function operates
#' by first interpolateing the track in space using \code{adehabitatLT},
#' i.e. so that arrows can be drawn along any route of the animal between
#' observed fixes.
#'
#' The function calls three other functions the user need not worry about,
#' but just for information: \code{get_angle_coords} to do the mathematics
#' of the arrow calculations, \code{arrow_plot_prep},
#' which calls \code{get_angle_coords} for each interpolated fix,
#' and \code{arrow_transform} which converts the coordinates to the chosen
#' projection system.
#'
#' The main \code{plot_track_arrows} function therefore does the
#' spatial interpolation, calling the function stack, and
#' also makes use of other \code{MoveRakeR}
#' functions \code{project_points} and \code{get_bounds}. Arrows are
#' plotted using base graphics using the \code{segments} function. The \code{plot_track_arrows} function also needs the function \emph{gap_section}, or
#' equivalent naming of gapsections from other functions or user-manual identification,
#' to provide as 'bursts' to the \code{adehabitatLT} functions for interpolation. This is
#' to make sure no interpolation is done over excessive gaps in the dataset.
#'
#' The \code{plot_track_arrows} function is also now
#' used directly in the \code{plot.Track} function specifying arrows directly
#' in the main plotter at a higher level, with a call to \code{plot_track_arrows}. See examples.
#'
#' @param data A \code{Track} family object.
#' @param ... further arguments passed to or from other methods.
#' @export
plot_track_arrows <- function(data, ...){
  UseMethod("plot_track_arrows", data)
}

#' @rdname plot_track_arrows
#' @param x Input data into sub-functions \code{arrow_plot_prep} and \code{arrow_transform};
#' user need not use this, as called in function stack.
#' @param angle Turning angle to be assessed for adding and subtracting bearing
#' from to form the arrow in the plots; used only in function \code{get_angle_coords}; user
#' should not have to worry about this.
#' @param tol 'Tolerance' value in radians for the mount of angle to add and subtract from the
#' direction of movement to form the arrow head; defaults to 0.5.
#' @param r Direct size value of the length of arrows to plot;
#' defaults to 500 metres (assuming UTM projection) in the sub-functions of \code{get_angle_coords} and
#' \code{arrow_plot_prep} but is calculated dynamically in the
#' \code{plot_track_arrows} function so does not need to be edited directly, instead controlled
#' by the \emph{corr_size_arr} argument. However, an \emph{r} value can be supplied directly
#' to the \code{arrow_plot_prep} if need be. This value will therefore take on a different
#' meaning - see below. A further complexity is that for groups
#' of animals, the same size arrows are needed across all, hence a common
#' extent is needed. Thus, in the \code{arrow_plot_prep} function, if a \code{TrackStack}
#' or \code{TrackMultiStack} is detected, the mn_extent / mn_extent^corr_size_arr
#' formula will be taken for the across animal extent automatically, using the \emph{corr_size_arr}.
#' But if only a \code{TracK} is supplied and if \emph{r} is not NULL, then the
#' \emph{r} will need to be the mn_extent / mn_extent^corr_size_arr resultant value supplied
#' directly, hence e.g. could be if you want to manually use a wider custom extent for the arrow size.
#' This latter option is most handy for automatic use in the \code{plot.Track} function
#' that works on a bird-by-bird basis so needed direct use of \emph{r} in that instance.
#' @param p4s A numeric or \code{sf} \code{CRS} argument, here defaulting to the UTM 31N (32631).
#' @param col Colour of the arrow to be plotted, defaults to "black".
#' @param corr_size_arr Value to control the size of arrows along the
#' trajectory, defaults to 0.35; this is used as an exponent
#' in relation the mean extent (m) of the animal: r = m / m^corr_size_arr,
#' used further to dynamically plot the size of the arrow over changing spatial
#' extents - thus fed into worker functions \code{arrow_plot_prep} and \code{plot_track_arrows}
#' to adjust the \emph{r} parameter.
#' @param corr_no_arr Value to control the number of arrows to plot
#' along the trajectory. The number of rows of the interpolated dataset
#' via \code{adehabitatLT} calculation, is first adjusted by proportion thus
#' \emph{N_keep = nrows*corr_no_arr}, with \emph{corr_no_arr} defaulting to a value of 0.005
#' retaining one fix in every 200 interpolated;
#' this is used further to give row number identifiers for fixes along the route to retain,
#' is used in the equation: \emph{row_keep = seq(2, nrows, (nrows/N_keep))}, and therefore acts as a means of retaining the number of rows of
#' the interpolated fixes to retain for plotting. Note, \emph{N_keep} can
#' also be supplied directly, in which case a direct value is fed in for the
#' number of rows to keep, otherwise, if NULL, will be calculated automatically as above.
#' @param N_keep A direct supplied value for number of arrows to plot (or rather
#' number of rows in interpolated data to keep for plotting), defaults to NULL for automatical calculation
#' based on the \emph{corr_no_arr} argument.
#' @param geo_transform Logical (defaults to TRUE) to transform the coordinates of the arrow lines to WGS84 geographic cartesian.
#' @param u The \emph{u} argument in function \code{adehabitatLT::redisltraj}, here set to
#' 100 m (default) for constant spatial interpolation at fine scales, but may not
#' be fine enough for some very detailed data sections.
#' @param nnew The \emph{nnew} argument in function \code{adehabitatLT::redisltraj},
#' here set to 500 by default.
#' @param verbose Logical defaulting to TRUE for detailed messaging.
#'
#' @examples
#' indata <- yourdata # data.frame with a minimum of columns named TagID, DateTime, longitude, latitude
#' data <- Track(indata) # optional to convert to in-house class (most functions will still run on non track data but generic S3 methods will not be available for plot and summary)
#' p4 <- 4326
#'
#' ### European shape as an example
#' # R packages not bundled with MoveRakeR
#' #remotes::install_github("ropensci/rnaturalearthhires")
#' #install.packages('rnaturalearth')
#' library(rnaturalearthhires)
#' library(rnaturalearth)
#'
#' spdf_world <- ne_download(scale = 10, type = "countries")
#' Europe <- spdf_world[spdf_world$SUBREGION %in% c("Western Europe","Eastern Europe","Northern Europe","Southern Europe"),]
#' Europe_sf = st_crop(Europe, xmin = -25, ymin=32, xmax=35, ymax=70)
#' Europe_sf <- sf::st_transform(Europe_sf, p4)
#' plot(Europe_sf$geometry)
#'
#' # resample to get as 5 min trajectory for simplicity
#' data = sub_samp(data, 300, tol = 0.4, drop = FALSE)
#'
#' # projection
#' p4s = 32631
#'
#' opar = par()
#' par(mar = c(0,0,0,0))
#' par(oma = c(0,0,0,0))
#'
#' # plot using the plot method for 'Track' objects
#' plot(Europe_sf$geometry, border = "black", col = "wheat", xlim = c(-4,-2.6), ylim = c(53.8,54.4))
#' plot(data, ADD=TRUE)
#' plot(Europe_sf$geometry, border = "black", add=TRUE)
#'
#' # test adding arrows
#' plot_track_arrows(data, col = "red")
#' plot_track_arrows(data, col = "grey")
#'
#' plot_track_arrows(data, col = "black", corr_size_arr = 0.45) # smaller
#' plot_track_arrows(data, col = "black", corr_size_arr = 0.45, corr_no_arr = 0.010) # smaller and more numerous
#' plot_track_arrows(data, col = "black", corr_size_arr = 0.45, corr_no_arr = 0.050) # smaller and even more numerous
#'
#' # which can look a bit ugly for lots of lines!
#'
#' # --------------------------- #
#' # The plot_track_arrows function is also implemented in the main
#' # plot function for Track objects (S3 method)
#'
#' plot(Europe_sf$geometry, border = "black", col = "wheat", xlim = c(-4,-2.6), ylim = c(53.8,54.4))
#' plot(data, ADD=TRUE, anims = "uniq", arrows = TRUE, Points = TRUE, corr_size_arr = 0.38)
#'
#' # Use in the plot function takes the colours of the Tracks per animal
#' # or per Stack (for TMS objects)
#' # See the ?plot.Track help for more details
#'
#' ############################## END ###########################
#'
#' @importFrom adehabitatLT as.ltraj redisltraj
#' @import sf
#' @export
plot_track_arrows.Track <- function(data, tol = 0.5, u = 100, nnew = 500, r = NULL, N_keep = NULL, col = "black",
                                    corr_no_arr = 0.005, corr_size_arr=0.35, geo_transform = TRUE, verbose = TRUE,
                                    #p4s = sp::CRS("+proj=utm +zone=31 ellps=WGS84")
                                    p4s = 32631
                                    ,...){

  if(!is(data, "Track")){
    stop("Data are not a valid Track object")
  }

  .plot_track_arrows <- function(data, ...){

    if(verbose == TRUE){message(unique(data$TagID))}

    onebird <- droplevels(data)
    onebird_sf <- sf::st_as_sf(onebird, coords = c("longitude", "latitude"), crs = 4326)
    bb <- sf::st_transform(onebird_sf, p4s)

    coords = as.data.frame(sf::st_coordinates(bb))

    onebird$X <- coords$X
    onebird$Y <- coords$Y

    onebird <- onebird[!duplicated(onebird$DateTime),] # done also in plot.Track

    lt = adehabitatLT::as.ltraj(subset(onebird, select = c(X,Y)), date = onebird$DateTime, id = onebird$TagID, burst = onebird$gapsec)
    if(length(which(sapply(lt,function(x) nrow(x) == 1))) > 0){
      lt_ <- lt[-which(sapply(lt,function(x) nrow(x) == 1))]
    } else{
      lt_ <- lt
    }

    lt_2 <- adehabitatLT::redisltraj(l = lt_, u = u, type = "space", nnew = nnew)

    # -------------------- #
    # Scaling the arrow length to whatever extent we want to plot based on extent
    ss = get_bounds(data, p4s)
    xlims = max(ss$xRa) - min(ss$xRa)
    ylims = max(ss$yRa) - min(ss$yRa)
    mn_extent = mean(xlims,ylims)

    # need a quadratic adjustment probably with correction over increasing distance
    # or actually an exponentional?

    if(is.null(r)){
      rr = mn_extent / mn_extent^corr_size_arr
    } else{
      rr <- r
    }

    #if(sf::st_is_longlat(sf::st_crs(p4s))){
    #  rr = rr * 1000
    #}

    arrow2plot <- do.call('rbind',lapply(lt_2, arrow_plot_prep, tol = tol, r = rr))

    # -------------------- #
    # NUMBER of arrows to plot
    # number of GPS points is silly as could be 3s fixes biasing
    #nnew = round(nrow(data)/3)
    #row_keep = round(seq(2,nrow(arrow2plot),nrow(arrow2plot) / nnew))

    if(is.null(N_keep)){
      # we have interpolated spatially so this is better to use
      N_keep <- length(1:nrow(arrow2plot)) * corr_no_arr
    }

    #row_keep = round(seq(2,nrow(arrow2plot),nnew))
    row_keep = round(seq(2,nrow(arrow2plot),nrow(arrow2plot) / N_keep))

    arrow2plot$rn = 1:nrow(arrow2plot)
    ap2 <- arrow2plot[arrow2plot$rn %in% row_keep,]

    ap2 <- ap2[!is.na(ap2$X1),]
    ap2 <- ap2[!is.na(ap2$X2),]
    ap2 <- ap2[!is.na(ap2$Y1),]
    ap2 <- ap2[!is.na(ap2$Y2),]
    ap2 <- ap2[!is.na(ap2$x),]
    ap2 <- ap2[!is.na(ap2$y),]

    if(geo_transform == TRUE){
      # back to geographic coordinates
      ap3 = arrow_transform(ap2, p4s = 32631)
    } else{
      ap3 <- ap2
    }

    # plot the segments
    segments(x0 = ap3$x, y0 = ap3$y, x1 = ap3$X1, y1 = ap3$Y1, col = col)
    segments(x0 = ap3$x, y0 = ap3$y, x1 = ap3$X2, y1 = ap3$Y2, col = col)

    return(ap3)
  }

  ##########

  if(verbose == TRUE){
    message("plot_track_arrows: Adding directional arrows")
  }

  ##########
  if(is_Track(data)){

   .plot_track_arrows(data, r = r)

  }

  # reclaculate r value for use across animals
  if(is_TrackStack(data) | is_TrackMultiStack(data)){
    # get bounds of the whole track and assess overall
    # arrow size
    ss = get_bounds(data, p4s)
    xlims = max(ss$xRa) - min(ss$xRa)
    ylims = max(ss$yRa) - min(ss$yRa)
    mn_extent = mean(xlims,ylims)
    r = mn_extent / mn_extent^corr_size_arr

  }

  if(is_TrackStack(data)){

    if(length(col) == 1){
      lapply(data, .plot_track_arrows)
    }
    if(length(col) > 1){
      for(i in 1:length(data)){
        .plot_track_arrows(data, col = col[i], r = r)
      }
    }

  }

  if(is_TrackMultiStack(data)){


    if(length(col) == 1){
      for(i in 1:length(data)){
        lapply(data[[i]], .plot_track_arrows, r = r)
      }
    }

    if(length(col) > 1){

      lenn = lapply(data, length)

      gg = hh = list()
      for(k in 1:length(lenn)){
        gg[[k]] = rep(k,lenn[[k]])
        hh[[k]] = seq(1:lenn[[k]])
      }
      df = data.frame(j = do.call('c',gg), i = do.call('c', hh))
      df$colno <- 1:nrow(df)
      df$col = col

      j = 1
      for(j in 1:length(data)){

        col_ <- df[df$j == j,]

        i = 1
        for(i in 1:length(data[[j]])){
          col__ <- col_[col_$i == i,]
          .plot_track_arrows(data[[j]][[i]], col = col__$col, r = r)

        }
      }

    }
  }


}

#' @rdname plot_track_arrows
#' @export
get_angle_coords <- function(angle, tol = 0.3, r = 500){ # r = 50 metres in UTM coords
  a2 = ifelse(angle < 1, angle+pi, (angle+pi)-(2*pi))

  #out = data.frame(theta = angle, theta_opp = a2, l_opp = a2-tol, u_opp = a2+tol)

  # then use those new upper and lower angles from the second point to
  # draw lines back from the relocation

  x1 = r*cos(a2-tol)
  y1 = r*sin(a2-tol)

  x2 = r*cos(a2+tol)
  y2 = r*sin(a2+tol)

  # which means you just add those x and ys to the coordinate
  out = data.frame(theta = angle, theta_opp = a2, l_opp = a2-tol, u_opp = a2+tol,
                   x1,y1,x2,y2)

  return(out)
}

#' @rdname plot_track_arrows
#' @export
arrow_plot_prep <- function(x, tol = 0.5, r = 500){

  row.names(x) <- 1:nrow(x)

  gac <- get_angle_coords(x$abs.angle, tol = tol, r = r)
  gac = gac[-nrow(gac),]
  gac_1 <- gac[1,]
  gac_1[1,] <- NA
  gac <- rbind(gac_1,gac)

  new = cbind(x,gac)

  # calculate new coordinates
  new$X1 <- new$x+new$x1
  new$X2 <- new$x+new$x2

  new$Y1 <- new$y+new$y1
  new$Y2 <- new$y+new$y2

  new <- subset(new,select = c(x,y,date,X1,X2,Y1,Y2))

  # reduce dataset by X amount? later

  return(new)

}

#' @rdname plot_track_arrows
#' @export
arrow_transform <- function(x,
                            #p4s = sp::CRS("+proj=utm +zone=31 ellps=WGS84")
                            p4s = 32631
                            ){


  .arrow_geo <- function(z, xy = c("x","y")){

    zz = z[,which(names(z) %in% xy)]

    zz = sf::st_as_sf(z, coords = xy, crs = p4s)
    zzz = sf::st_transform(zz, 4326)

    df = as.data.frame(sf::st_coordinates(zzz))
    #names(df) <- c("x", "y")

    #df = data.frame(x,y)
    names(df) = xy

    return(df)

  }

  initial_coords <- .arrow_geo(x, xy = c("x","y"))

  # arrow1
  a1 <- .arrow_geo(x, xy = c("X1","Y1"))

  # arrow2
  a2 <- .arrow_geo(x, xy = c("X2","Y2"))

  xx <- cbind(initial_coords,a1,a2)
  xx$DateTime <- x$date

  return(xx)
}



