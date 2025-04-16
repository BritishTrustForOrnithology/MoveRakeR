#' Produce spatial visualisations of tracking data
#'
#' A generic plotting function \code{plot} available as an S3 method for \code{Track} objects
#' of class \code{Track}, \code{TrackStack} or \code{TrackMultiStack}
#'
#' This \code{plot} function can help with generic plotting covering a range of complexities. The function uses base graphics, with outputs depending
#' on the user's choice. Lines, points or lines+points options are available, with options
#' for colouration of points and lines based on a colour palette or specific vectors of colours.
#' The function is also compatible with migration datasets, independent of trip analysis. Through use of
#' the \code{"ADD"} argument, the user can load in any shapefile they wish to then plot points/lines over.
#' Options also exist to colour individuals the same colour in repeated TrackStacks in a TrackMultiStack
#' and it is suggested the user use the in-built functions should they need to subset particular animals/groups
#' a priori.
#'
#' The user can also plot arrows in the plotting function via
#' a call to the standalone function \code{plot_track_arrows}. This adds a
#' series of arrows angles at 0.5 radians either sie of the direction of movement.
#' The angle and the number plotted here are fixed but further flexibility on both
#' of those elements are available in \code{plot_track_arrows}. The user can, however,
#' adjust the size of the arrows directly in the plot main function here, via use of
#' the \emph{corr_size_arr} argument, which is based on the extent of the suited of animals
#' plotted. Arrows are automatically coloured the same as the Track lines.
#'
#' @rdname plot
#' @param data A \code{Track-family}, object, one of, \code{Track}, \code{TrackStack} or \code{TrackMultiStack} object.
#' @param gap Logical (defaulting to FALSE) should gapsections in the \code{"gapsec"} column be used for line-plotting;
#' this requires gapsections to  have been identified e.g. through \code{clean.GPS}
#' or \code{gap.section}, and the function will only plot lines of consecutive fixes within
#' your chosen gap boundary, i.e. to avoid plotting lines across space joining fixes you don't want to.
#' @param Points Logical (default TRUE) as to whether points are to be plotted on the map
#' @param arrows Logical defaults to FALSE, as to whether directional arrows are desired,
#' calls separate function \code{plot_track_arrows}.
#' @param corr_size_arr See \code{plot_track_arrows} for details of this parameter;
#' controls the size of the arrow length based on the extent of animals, default 0.35.
#' @param N_keep See \code{plot_track_arrows} for details; \emph{WARNING: not currently implemented};
#' should control the number of arrows to plot, but here is NULL meaning an automatic calculation is made per animal.
#' For more flexibility on this, use the \code{plot_Track_arrows} outside the main plotting function.
#' @param sta Logical (default TRUE) for ploting a triangle for the start location of animal(s).
#' @param end Logical (default TRUE) for ploting an upside-down triangle for the end location of animal(s).
#' @param maxdists Logical (default TRUE) for plotting maximum distance points of animal(s) as a square.
#' @param maxdist_trip Logical (default FALSE) for plotting small squares at the distal point (max distance from the start location)
#' for animal(s); only works of a column called "tripNo" is found in the data.
#' @param Lines Logical (defaulting to TRUE) as whether lines are to be plotted on the map.
#' @param Legend Logical (defaulting to FALSE) should a legend be added to the plot. Note, this argument
#' offers only basic line, point colouration and cex options for the legend for individual animal IDs. Should the user
#' want anything extra or more bespoke, then set this to FALSE and apply your own after the function has plotted.
#' @param Xlim The numeric x range of the plotting extent as a concatenated string, that can be a user-fed value or
#' here suggested making use of the \code{get_bounds} function to assess the range of individual
#' groups of animals. Defaults to range of data.
#' @param Ylim As above, the numeric y range of the plotting extent, defaults to range of data.
#' Note if ADD=FALSE, the map upon which lines and points are plotted overrides any x and y ranges set.
#' @param cex_p Numeric \code{par(cex)} value for the point size, set to 0.4 by default.
#' @param cex_leg Numeric \code{par(cex)} value for the legend size, set to 1 by default.
#' @param leg_pos Vector of four options to feed the legend argument: "topright", "topleft", "bottomright", "bottomleft",
#' set to "topright" as default.
#' @param pcol Option for a specific \emph{point} colour as a single colour argument, to overide
#' any option in argument in \code{col} - i.e. should the user want coloured lines per animal
#' but a specific single colour for points; note only one colour is allowed in this argument.
#' @param Lwd Numeric \code{par} value for the line width defaulting to 1.
#' @param ADD Logical (defaulting to FALSE) to add the lines/points for the current
#' plot onto an existing graphic already loaded in the plot window. This enables base maps of the user to be loaded
#' so the function is not proscriptive in requiring base mapping first.
#' @param col The colour palette to use for the plotting, defaulting to an errant "NULL".
#' \code{col} can be a colour palette function such as \code{grDevices::colorRampPalette}, for which
#' the number of animals in the \code{Track family} of objects is first assessed and then used to plot.
#' Alternatively, the user can specify a vector of colours, e.g. \code{c("red", "blue", ...)}
#' but this must match the length of the number of animals being plotted, with the exception of a single colour that
#' can be supplied and this will be taken to colour all the animals in the object the same.
#' @param anims A value specific to \code{TrackMultiStack} objects, defaulting to NULL (but requires a specification for TMS objects).
#' This argument must be one of \code{"uniq"} or \code{"stack"} and determines how animals in
#' the component tracks in the Stacks of the MultiStack object will be plotted. \code{"uniq"} indicates a separate colour
#' is required for every animal across all stacks, with further TMS argument \code{"SameColAcrossStacks"} used
#' to determine if the same animal IDs in different stacks should be coloured the same. The \code{"stack"} option
#' expects a colour of length the same as the number of stacks of animals, and permits
#' colouring all animals in the individual stack the same, but each stack to then be coloured differently.
#' @param SameColAcrossStacks As noted above, the \code{anims} argument \code{"uniq"} can accept specific treatment
#' to colour TagIDs the same across different TrackStack objects in the multistack configuration. This option thus reduces the number
#' of colours used, so the colour-palette will be adjusted to the reduced number automatically, i.e. should say
#' six individual Track elements (in stacks) become five if one TagID is the same. Similarly, the first
#' five colours of a fixed colour list in the \code{col} argument will be taken should more have been specified.
#' @param CombStackTagID Logical (defaulting to FALSE) as to whether individual animal TagIDs should
#' be coloured the same within Tracks and TrackStack objects.
#' @param p4s A numeric or \code{sf} \code{CRS} argument, here defaulting to the UTM 31N (32631).
#' @param verbose logical defaulting to TRUE for detailed messaging.
#'
#' @return A plotted surface in an open window, but that can be part of code by the user to export to external
#' graphics devices such as png, jpeg etc.
#'
#' @seealso [MoveRakeR::plot_track_arrows]
#'
#' @examples
#'
#' indata <- yourdata # data.frame with a minimum of columns named TagID, DateTime, longitude, latitude
#' indata <- Track(indata) # optional to convert to in-house class (most functions will still run on non track data but generic S3 methods will not be available for plot and summary)
#' p4 <- 3035
#'
#' # With a very basic rectangle around the colony for trips
#' indata <- indata %>% clean_GPS() %>%
#'   define_trips(method="rect", plot=TRUE,lls = c(-3.2, 54.0553,-3.1689, 54.0437), p4s = p4)
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
#' ## colour ramp
#' Cpal <- grDevices::colorRampPalette(c("red", "green", "blue"))
#'
#' # Mostly the user will be concerned with a single data.frame tibble of many animals - these
#' # are Track classes. If any listing is used to separate out animals in a list (TrackStack) or
#' # further mult-listing (TrackMultiStack), then the plot S3 generic can handle those other formats - see below examples.
#'
#' #############
#' # TRACK OBJECTS
#' # plotting as a palette
#' # NOT RUN
#' xRa = c(minx, maxx) # define bounds perhaps
#' yRa = c(miny, maxy)
#'
#' plot(Europe_sf$geometry, xlim=xRa, ylim=yRa,col="wheat")
#' plot(indata,ADD=TRUE,col = Cpal, p4s = p4, Legend = TRUE)
#'
#' # OR as defined colours of your own
#' plot(Europe_sf$geometry, xlim=xRa, ylim=yRa,col="wheat")
#' plot(indata,ADD=TRUE,col = c("blue"), p4s = p4)
#'
#' # ADD TRACK ARROS:
#'
#' plot(indata, ADD=FALSE, anims = "uniq", arrows = TRUE, Points = TRUE, corr_size_arr = 0.38)
#'
#' ##############
#' # TRACKSTACK OBJECTS (e.g. first [[stack]] in the TMS)
#' # plotting as a palette and plotting the first TrackStack object of the TMS object
#'
#' plot(Europe_sf$geometry, xlim=xRa, ylim=yRa,col="wheat")
#' plot(indata_stack,ADD=TRUE,col = Cpal, p4s = p4, Legend = TRUE)
#'
#' plot(indata_stack,ADD=FALSE,col = Cpal, p4s = 27700)
#'
#  #OR as defined colours of your own
#' plot(Europe_sf$geometry, xlim=xRa, ylim=yRa,col="wheat")
#' plot(indata_stack,ADD=TRUE,col = c("red","royalblue","orange","yellow"), p4s = p4)
#'
#' ############################
#' # TRACK MULTI STACK OBJECTS
#' ### Stack option for two colours for two stacks, e.g. all animals grouped as one colour
#'
#' plot(Europe_sf$geometry, xlim=xRa, ylim=yRa,col="wheat")
#' plot(indata_multi_stack,ADD=TRUE,col = Cpal, anims = "stack", p4s = p4)
#'
#' # or feeding in direct colours
#' plot(indata_multi_stack,ADD=TRUE,col = c("red","blue"), anims = "stack", p4s = p4)
#'
#' # if one colour provided, all animals coloured the same
#' plot(indata_multi_stack,ADD=TRUE,col = "red", anims = "stack", p4s = p4)
#'
#' ### Unique option
#' plot(Europe_sf$geometry, xlim=xRa, ylim=yRa,col="wheat")
#' plot(indata_multi_stack,ADD=TRUE,col = Cpal, anims = "uniq", p4s = p4)
#'
#' # separate colour option must match length of birds summed across both stacks
#' # function works out division of colours by bird numbers in each stack
#'
#' # again one colour just colours all
#' plot(indata_multi_stack,ADD=TRUE,col = "red", anims = "uniq", p4s = p4)
#'
#' # plotting the same colours for individual animals in different stacks
#' # this is done in the function by the term: SameColAcrossStacks = TRUE
#' # Also there is an option to combine animals within a stack
#'
#' # Keeping the animal as separate colours in the two stacks
#' plot(ukmap$geometry, xlim=lls$xRa, ylim=lls$yRa,col="wheat")
#' plot(indata_multi_stack,ADD=TRUE,col = Cpal, anims = "uniq", SameColAcrossStacks = FALSE, p4s = p4)
#'
#' # or...same colours across stacks
#' plot(Europe_sf$geometry, xlim=xRa, ylim=yRa,col="wheat")
#' plot(indata_multi_stack,ADD=TRUE,col = Cpal, anims = "uniq", SameColAcrossStacks = TRUE, p4s = p4)
#'
#' # Not currently possible to separate out arrow colours from point or line colours
#' # here; such flexibiliity can be got through separate use of the plot_track_arrows function.
#' # The arrow argument defaults to FALSE so need to turn back on. Control
#' # also available for size of arrow across the Track via the "corr_size_arr" argument,
#' # but any further use of number of arrow or the angle of the arrow use the
#' # plot_track_arrows function directly after calling plot. Or ask the Developer if that's not good enough.
#'
#' ############################ END ###########################
#'
#' @rawNamespace import(data.table, except = c(month, hour, quarter, week, year, wday, second, minute, mday, yday, isoweek))
#' @importFrom sfheaders sf_linestring
#' @importFrom geosphere distRhumb
#' @import sf
#' @export
#' @method plot Track
plot.Track <- function(data, gap = FALSE,
                       Points = TRUE, arrows = FALSE, corr_size_arr = 0.35,
                       N_keep = NULL, sta = TRUE, end = TRUE, maxdists = TRUE,
                       maxdist_trip = FALSE, Lines = TRUE, Legend = FALSE,
                       Xlim = NULL, Ylim = NULL, cex_p = 0.4,
                       cex_leg = 1, leg_pos = "topright", pcol = NULL, Lwd = 1,
                       ADD = FALSE, col = NULL, anims = NULL,
                       CombStackTagID = FALSE, SameColAcrossStacks = FALSE, verbose=TRUE,
                       p4s = 4326
                       ){

  if(is.null(col)){
    col <- colorRampPalette(c("red", "green", "blue"))
  }

  # cpalette = a vector of colours or a palette
  cpalette <- col

  # avoid recursion on CRS
  p4sin <- p4s

  if(!is(data, "Track")){
    stop("Data are not a valid Track object")
  }

  # --------------------------- #
  # for arrows, get bounds of whole track for equal sized arrows
  # this is done in the track arrow function applied to TS and TMS
  # object but here we have to go through the T route, so needs
  # appraising first outside the function - sorry overly complex.
  if(arrows){

    #ss = get_bounds(data, p4s = sp::CRS("+proj=utm +zone=31 +ellps=WGS84 +units=m +no_defs"))
    ss = get_bounds(data, p4s = 32631)

    xlims = max(ss$xRa) - min(ss$xRa)
    ylims = max(ss$yRa) - min(ss$yRa)
    mn_extent = mean(xlims,ylims)
    rr = mn_extent / mn_extent^corr_size_arr

    #if(sf::st_is_longlat(sf::st_crs(p4s))){
    #  rr = rr * 1000
    #}

    if(verbose){
        message(paste0("Adding directional arrows, size ", round(rr), " m"))
      }
  }
  # --------------------------- #

  # sub plot routine per animal
  .plot <- function(data, Col, Add=FALSE){

    # gapsections? i.e. may not want to plot across large gaps in your data, e.g. if logger stops
    # so look for gapsec column, use to identify gapssections > 1 and plot those
    # note ALL points are plotted though, even isolated ones


    if(Lines){

      if(gap){

        if(!any(names(data) == "gapsec")){stop("Needs a column called gapsec: Use clean_GPS or gap_section functions")}

        # # # # # # # # #  BUG IDENTIFIED 26/09/2024 # # # # # # # #
        # CANNOT ACCEPT LINESTRINGS OF LENGTH 1!
        #Error in scan(text = lst[[length(lst)]], quiet = TRUE) :
        #        scan() expected 'a real', got 'IllegalArgumentException:'
        #Error in (function (msg)  :
        #                  IllegalArgumentException: point array must contain 0 or >1 elements
        #
        # assess gapsec lengths and drop those with length 1

        gaps2use = unique(data$gapsec)[table(data$gapsec) > 1]

        ## new sf: not needing an apply if using sfheaders here
        pp.l <- sfheaders::sf_linestring(data[data$gapsec %in% gaps2use,], x = "longitude", y = "latitude", linestring_id = "gapsec")

        # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

      } else{
        pp.l <- sfheaders::sf_linestring(data, x = "longitude", y = "latitude")
      }

      pp.l <- sf::st_set_crs(pp.l, 4326)
      pp.l <- sf::st_transform(pp.l, crs = p4s)
    }

    # get all points
    pp <- sf::st_as_sf(data, coords = c("longitude", "latitude"), crs =4326)
    pp <- sf::st_transform(pp, crs = p4s)

    if(!Add){
      #plot.new()

      if(Lines){
        if(!is.null(pp.l)){
          #sp::plot(pp.l,col=Col,add=FALSE,lwd=Lwd,xlim=Xlim,ylim=Ylim)
          par(mar=c(1,1,1,1))
          plot(pp.l$geometry, col = Col, add = FALSE, lwd = Lwd ,xlim = Xlim, ylim = Ylim)

          }
      }

      if(Points){
        if(!is.null(pcol)){
          # taking a single background point colour
          if(length(pcol) > 1){stop("A single background point colour is expected")}
            pcol <- pcol
        } else{pcol <- Col}
        if(Lines){
          #sp::plot(pp,col=Col,bg=pcol,pch=21,cex=cex_p,add=TRUE,xlim=Xlim,ylim=Ylim)
          plot(pp$geometry, col = Col, bg = pcol, pch = 21, cex = cex_p, add = TRUE , xlim = Xlim, ylim = Ylim)

        } else{
          #sp::plot(pp,col=Col,bg=pcol,pch=21,cex=cex_p,add=FALSE,xlim=Xlim,ylim=Ylim)
          plot(pp$geometry, col = Col, bg = pcol, pch = 21, cex = cex_p, add = FALSE, xlim = Xlim, ylim = Ylim)
          }
      }

    } else if(Add){
      if(Lines){
        if(!is.null(pp.l)){
          #sp::plot(pp.l,col=Col,lwd=Lwd,add=TRUE)
          plot(pp.l$geometry, col = Col, lwd = Lwd, add = TRUE)
          }
      }
      if(Points){
        if(!is.null(pcol)){
          # taking a single background point colour
          if(length(pcol) > 1){stop("A single background point colour is expected")}
          pcol <- pcol
        } else{pcol <- Col}
        #sp::plot(pp,col=Col,bg=pcol,pch=21,cex=cex_p,add=TRUE)
        plot(pp$geometry, col = Col, bg = pcol, pch = 21, cex = cex_p, add = TRUE)

      }
    }

    # ---------------------------------------------------------------------------------- #
    ### plot a triangle for start of track, and circle for end
    # get start/end coord for bird
    # (note previously this did not work with projected data, updated 26/10/2023)

    #data <- LBBGWalB201416[[1]][[3]][1:1000,]
    #data <- data[order(data$TagID, data$DateTime),]
    #plot.Track(LBBGWalB201416[[1]][[3]][1:1000,])

    if(sta){
      plot(pp[1,]$geometry, pch = 24, cex = 2, add=TRUE)
      #points(na.omit(data$latitude)[1]~na.omit(data$longitude)[1], pch = 24, cex = 2)
    }
    if(end){
      plot(pp[nrow(pp),]$geometry, pch = 25, cex = 2, add=TRUE)
      #points(na.omit(data$latitude)[nrow(data)]~na.omit(data$longitude)[nrow(data)], pch = 25, cex = 2)
    }
    if(maxdists){

      # not using sp::SpatiLPoints anymore so has to be alternative input - data = vector of two columns long lat
      if(sf::st_is_longlat(pp)){
        dists = geosphere::distRhumb( sf::st_coordinates(pp), p2 = sf::st_coordinates(pp[1,]))
      } else{
        dists = sf::st_distance(pp, pp[1,])[,1]
      }

      # which is the max point?
      wh <- which(dists == max(dists))[[1]]
      # add max distance points to plot
      plot(pp[wh,]$geometry, pch = 22, cex = 2, add=TRUE)

      # for plotting max distance per trip
      if(maxdist_trip){

        if(exists("tripNo", data)){

          dists = data.table::data.table(data.frame(dists, tripNo = data$tripNo)) # use this in trip_stats function!!!? Quicker than my approach
          wh = dists[, .I[dists == max(dists)], by=tripNo]$V1
          plot(pp[wh,]$geometry, pch = 22, cex = 2, add=TRUE)

        } else{
          warning("No tripNos specified in data, skipping maxdist labelling")
        }
      }

    }

  }

  ### if Xlim and Ylim are NULL, set to the local extent of the data
  ### of course over-ridden should the base map already have been plotted and ADD=TRUE

  lls <- get_bounds(data, p4s=p4sin)
  if(is.null(Xlim)){Xlim <- lls$xRa}
  if(is.null(Ylim)){Ylim <- lls$yRa}

  ### TrackStack Object
  if(is_TrackStack(data)){
    message(paste("Plotting basic lines for TrackStack object of", length(data), "animals"))

    # get colour palette by number of birds in the TrackStack

    if(is.function(cpalette)){
      cols <- list()
      for(i in 1:length(data)){
        cols[[i]] <- cpalette(length(data))[i]
      }
    } else{
      # then assume vector BUT CHECK IT MATCHES LENGTH OF DATA
      if(length(cpalette) != length(data)){
        if(length(cpalette) > 1){stop("Number of colours provided does not match length of data")}
      }

      if(length(cpalette) == 1){cpalette = rep(cpalette,length(data))} # make one colour for all individuals

      cols <- list()
      for(i in 1:length(data)){
        cols[[i]] <- cpalette[i]
      }

    }

    if(any(table(names(data)) > 1)){
      message("Multiple TagIDs detected in different Track elements")

      if(multTagIDcomb == TRUE){
        message("Combining Tracks of the same TagIDs to one Track")
        d. <- data[names(data) == names(which(table(names(data)) > 1))]
        d <- do.call('rbind',d.)
        d <- unique(d)
        d <- d[order(d$DateTime),]
        # replace in the original list

        data2 <- data[names(data) != names(which(table(names(data)) > 1))]

        data <- TrackStack(c(data2,list(d))) # making a TrackStack temporarily
        names(data) <- lapply(data,function(x) unique(x$TagID))

        # adjust colours to only take first x colours provided
        cols <- cols[1:length(data)] # complex this as to order of things as user could know the duplication exists and want to specify the length ahead of this!! currently not implemented
      } else {
        message("Retaining separate Tracks of the same TagIDs for plotting")
      }

    }

    #lls <- get_bounds(data,p4s=p4sin) # get_bounds function gets the extent of the data under current projection

    for(b in 1:length(data)){
      if(b == 1){
        if(ADD){ # if plotting a basemap of your own, then add all track on top
          .plot(data[[b]],Col=cols[[b]],Add=TRUE)
        } else if(!ADD){ # not plotting a base map, then take first bird plot to the exten of all birds, and then add birds sequentially
          .plot(data[[b]],Col=cols[[b]],Add=FALSE)
        }
        } else{.plot(data[[b]],Col=cols[[b]],Add=TRUE)}



      if(arrows){

        ## need to decide if to geo_transform
        if(sf::st_is_longlat(sf::st_crs(p4s))){

          pp4s = 32631
          gtransform = TRUE

        } else{
          pp4s <- p4s
          gtransform = FALSE
        }
        plot_track_arrows(data[[b]], col = cols[[b]], r = rr, verbose = FALSE,
                          geo_transform = gtransform,
                          p4s = pp4s,
        )

      }

    }


    ### legend here
    if(Legend){
      if(Lines & !Points){
        legend(leg_pos,legend = names(data),cex=cex_leg,lwd=Lwd,col = unlist(cols),bty="n")
      }
      if(Lines & Points){
        legend(leg_pos,legend = names(data),cex=cex_leg,lwd=Lwd,pch=21,pt.bg=unlist(cols),col = unlist(cols),bty="n")
      }
      if(!Lines & Points){
        legend(leg_pos,legend = names(data),cex=cex_leg,pch=21,pt.bg=unlist(cols),col = unlist(cols),bty="n")
      }
    }



  }

  ### Track object
  if(is_Track(data)){
    message(paste("Plotting basic lines for Track object of", length(unique(data$TagID)), "animals"))

    # NOTE: for a TRACK object if multiple tagIDs are listed, then the function will
    # just plot all of one animal, BUT, sort here first by DateTime and TagID just in case the user feeds in a concatenated Track of the same animal with no flag up of what each section of data represents
    # Function will automatically then plot data as one per animal of one colour across all
    data <- unique(data[order(data$TagID,data$DateTime),])

    if(is.function(cpalette)){
      cols <- list()
      i = 1
      for(i in 1:length(unique(data$TagID))){
        cols[[i]] <- cpalette(length(unique(data$TagID)))[i]
      }
    } else{
      # then assume vector BUT CHECK IT MATCHES LENGTH OF DATA > 1
      if(length(cpalette) != length(unique(data$TagID))){
        if(length(cpalette) > 1){stop("Number of colours provided does not match length of data")}
      }

      if(length(cpalette) == 1){cpalette <- rep(cpalette,length(unique(data$TagID)))} # make one colour for all individuals

      cols <- list()
      for(i in 1:length(unique(data$TagID))){
        cols[[i]] <- cpalette[i]
      }

    }

    ## bounds
    #lls <- get_bounds(data,p4s=p4sin)

    b = 1
    for(b in 1:length(unique(data$TagID))){
      if(b == 1){
        if(ADD){ # if plotting a basemap of your own, then add all track on top
          .plot(data[data$TagID==unique(data$TagID)[b],],Col=cols[[b]],Add=TRUE) #,Xlim=lls$xRa,Ylim=lls$yRa assuming not needing to xlim and ylim if bsemap already called in open R code
        } else if(!ADD){ # not plotting a base map, then take first bird plot to the exten of all birds, and then add birds sequentially
          .plot(data[data$TagID==unique(data$TagID)[b],],Col=cols[[b]],Add=FALSE)
        }
      } else{.plot(data[data$TagID==unique(data$TagID)[b],],Col=cols[[b]],Add=TRUE)}


      if(arrows){

        ## need to decide if to geo_transform
        if(sf::st_is_longlat(sf::st_crs(p4s))){

          pp4s = 32631
          gtransform = TRUE

        } else{
          pp4s <- p4s
          gtransform = FALSE
        }

        plot_track_arrows(data = Track(data[data$TagID==unique(data$TagID)[b],]), col = cols[[b]], r = rr, verbose = FALSE,
                          geo_transform = gtransform,
                          p4s = pp4s,
        )

      }

    }

    if(Legend){
      ### legend here
      if(Lines & !Points){
        legend(leg_pos,legend = unique(data$TagID),cex=cex_leg,lwd=Lwd,col = cols[[b]],bty="n")
      }
      if(Lines & Points){
        legend(leg_pos,legend = unique(data$TagID),cex=cex_leg,lwd=Lwd,pch=21,pt.bg=cols[[b]],col = cols[[b]],bty="n")
      }
      if(!Lines & Points){
        legend(leg_pos,legend = unique(data$TagID),cex=cex_leg,pch=21,pt.bg=Col,col = cols[[b]],bty="n")
      }
    }

  }

  ### TrackMultiStack object
  if(is_TrackMultiStack(data)){
    cond <- FALSE # for checking if animal duplication is carried through

    if(is.null(anims)){stop("'anims' argument must be specified as: 'uniq' or 'stack'")}

    #data <- list(c(indata2[[1]],list(indata2[[1]][[1]])),c(indata2[[1]],list(indata2[[1]][[1]])))
    #names(data[[1]]) <- c("5854", "5845", "5958", "5854")
    #names(data[[2]]) <- c("5854", "5845", "5958", "5854")

    # check the stacks for duplicate IDs WITHIN the stacks
    for(f in 1:length(data)){

      if(any(table(names(data[[f]])) > 1)){
        message("Multiple TagIDs detected in different Track elements for individual TrackStacks")

        if(CombStackTagID){
          cond <- TRUE # for later asignment of colours under this special circumstance....
          message("Combining Tracks of the same TagIDs to one Track")
          d. <- data[[f]][names(data[[f]]) == names(which(table(names(data[[f]])) > 1))]
          d <- do.call('rbind',d.)
          d <- unique(d)
          d <- d[order(d$DateTime),]
          # replace in the original list

          data2 <- data[[f]][names(data[[f]]) != names(which(table(names(data[[f]])) > 1))]

          data3 <- TrackStack(c(data2,list(d))) # making a TrackStack temporarily
          names(data3) <- lapply(data3,function(x) unique(x$TagID))

          data[[f]] <- data3 # replace the StrackStack in the MultiStack with this adjusted list

        } else {
          message("Retaining separate Tracks of the same TagIDs for plotting")
        }

      }
    }

    # check animal count across the stacks
    anikeep <- lapply(data,FUN = function(x){length(x)})

    message(paste0("Plotting basic lines for ", length(data)," TrackStack objects (", paste(unlist(anikeep), collapse = ", "), " animals, repectively)"))


    # ---------------------- #
    ## this is assuming all different colours for all birds in one plot combined across stacks
    if(anims == "uniq"){

      if(verbose){message("Adding single colours for each animals")}
      all <- sum(do.call('rbind',anikeep))

      # different approach to TS and T objects, looking at bird distribution across the stacks and colour acquisition
      if(is.function(cpalette)){
        cpal.all <- cpalette(all)
      } else if(!is.function(cpalette)){
        if(length(cpalette) != all){
          if(length(cpalette) > 1){
            if(!cond){stop("Number of colours provided does not match length of data")}
            }
        }
        cpal.all <- cpalette
      }

      # if only one colour fed in, assume that the all animals in > 1 stacks will be of that colour
      if(length(cpal.all) == 1){
        cpal.all <- rep(cpal.all,all)
      }
      if(cond){ # then you have amalgamated duplicate animals above and need to take ONLY the first x elements supplied in original colour list
        cpal.all <- cpal.all[1:all]

      }

      # set up colour list to match TMS format
      ii <- do.call('rbind',anikeep)[,1]
      #ii <- c(6,9,5)
      iii <- cumsum(ii)
      init = 1
      i = 1
      xx <- list()
      yy <- list()
      for(i in 1:length(iii)){
        xx[[i]] <- list(); for(q in 1:ii[i]){xx[[i]][[q]] <- cpal.all[init:iii[i]][q]} # colours
        yy[[i]] <- list(); for(q in 1:ii[i]){yy[[i]][[q]] <- names(data[[i]])[q]} # TagIDs
        #xx[[i]] = cpal.all[init:iii[i]]
        init = 1 + iii[i]
      }
      cols <- xx
      #### checking animal TagIDs across stacks now
      # If there are TagIDs common across multiple stacks, then USE THE SAME COLOUR FOR EACH INDIVIDUAL
      # (but that has to be the user's choice)

      if(SameColAcrossStacks){
        da <- data.frame(TagIDs = unlist(yy), cols = unlist(xx))
        da <- da[!duplicated(da$TagIDs),] # keeping the first colour of animal in first list

        # regenerate the list but replacing duplicate animals with the first colour
        zz <- yy
        for(i in 1:length(zz)){
          for(q in 1:length(zz[[i]])){
            zz[[i]][[q]] <- da[da$TagIDs == yy[[i]][[q]],]$cols
          }
        }
        cols <- zz

      }


    }

    # ---------------------- #
    ## one colour for each stack (multiple animals in the same stack not applying here)
    if(anims == "stack"){
      if(verbose){message("Grouping animals by colours for each TrackStack")}
      if(is.function(cpalette)){
        cpal.all <- cpalette(length(data))
      } else if(!is.function(cpalette)){
        if(length(cpalette) != length(data)){
          if(length(cpalette) > 1){stop("Number of colours provided does not match length of data")}
        }
        cpal.all <- cpalette
      }

      # if only one colour fed in, assume that the all animals in > 1 stacks will be of that colour
      if(length(cpal.all) == 1){
        cpal.all <- rep(cpal.all,length(data))
      }

      xx <- list()
      i <- 1
      for(i in 1:length(data)){
        xx[[i]] <- list()
        for(j in 1:length(data[[i]])){
        xx[[i]][[j]] <- cpal.all[i]
        }
      }

      cols <- xx

    }

    # ---------------------- #
    # all diff colours, get length of whole combined stack
    #i = 1; b = 1


    for(i in 1:length(data)){

      for(b in 1:length(data[[i]])){
        if(b == 1 & i == 1){
          if(ADD){ # if plotting a basemap of your own, then add all track on top
            .plot(data[[i]][[b]],Col=cols[[i]][[b]],Add=TRUE) #,Xlim=lls$xRa,Ylim=lls$yRa assuming not needing to xlim and ylim if bsemap already called in open R code
          } else if(!ADD){ # not plotting a base map, then take first bird plot to the exten of all birds, and then add birds sequentially
            .plot(data[[i]][[b]],Col=cols[[i]][[b]],Add=FALSE)
          }
        }
        else{.plot(data[[i]][[b]],Col=cols[[i]][[b]],Add=TRUE)}


        if(arrows){

          ## need to decide if to geo_transform
          if(sf::st_is_longlat(sf::st_crs(p4s))){

            pp4s = 32631
            gtransform = TRUE

          } else{
            pp4s <- p4s
            gtransform = FALSE
          }
          plot_track_arrows(data[[i]][[b]], col = cols[[i]][[b]], r = rr, verbose = FALSE,
                            geo_transform = gtransform,
                            #p4s = sp::CRS("+proj=utm +zone=31 +ellps=WGS84 +units=m +no_defs")
                            p4s = pp4s,
          )

        }

      }
    }



    # so there is a chance that from process above, the colours may be adjusted if there is
    # a bird in one stack AND the other, so to plot in legend we only want one colour per bird
    # doing as you do above for uniq colours then, we can....do the same as SameAcrossAllStacks
    # but this time, colours, as colours are thing that you doubled up if there was a bird the same in both stacks
    # of course if chosing the stack option colourign by stack, then probably this legend option is not very visually appealing!


    if(Legend){

      # get colour bird lists used
      ll <- list()
      for(i in 1:length(data)){ll[[i]] <- names(data[[i]])}
      ll <- unlist(ll)
      da2 <- data.frame(TagIDs = ll, cols = unlist(cols))
      da2 <- da2[!duplicated(da2$cols),] # keeping the first colour of animal in first list


      if(anims == "stack"){
        # adjust the TagID to not be labelled as animals but stacks groups
        da2$TagID <- paste0("Group",1:length(da2$TagID))
      }

      if(Lines & !Points){
        legend(leg_pos,legend = da2$TagID,cex=cex_leg,lwd=Lwd,col = da2$cols,bty="n")
      }
      if(Lines & Points){
        legend(leg_pos,legend = da2$TagID,cex=cex_leg,lwd=Lwd,pch=21,pt.bg=da2$cols,col = da2$cols,bty="n")
      }
      if(!Lines & Points){
        legend(leg_pos,legend = da2$TagID,cex=cex_leg,pch=21,pt.bg=da2$cols,col = da2$cols,bty="n")
      }

    }



  }

  if(!Points & !Lines){warning("Nothing selected for plotting")}
  par(mar=c(4,4,2,2))

}












