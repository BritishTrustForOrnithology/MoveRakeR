#' Produce line plot of voltage data from a "Vo" object
#'
#' @description
#' A generic plotting function \code{plot} available as an S3 method for objects of class \code{Vo}
#' and \code{VoTrack}.
#'
#' @details
#' The S3 generic \code{plot} function can be used for \code{Vo} and \code{VoStack} objects to
#' produce a visualisation of UvA-BiTS (not Movetech at the moment) tag voltages, to see
#' how tags are performing, e.g. just after deployment to check sustainability of sampling protocols.
#' The function in its default form
#' will accept a single \code{Vo} and \code{VoStack} object under the "data" argument, and
#' will then plot the "vbat" and "vsll" columns denoting battery and solar charging voltages
#' as blue and red lines respectively (secondary axis for solar voltage).
#'
#' The function uses \code{ggplot} so each TagID supplied has its own facet.
#' But note, too many birds plotted at the same time will make this visualisation difficult.
#'
#' The function also accepts "start" and "end" arguments (in UTC) if the user wishes
#' to subset the plot to be within specific DateTimes (one set across all birds only).
#'
#' The option also exists to bring in a \code{Track} or \code{TrackStack} object
#' under the "dataTrack" argument, to work
#' alongside in the plot to produce rectangles of day and night (grey shaded) phases on the
#' plot using the R package \code{suncalc}.
#'
#' Further, using the Track objects allows the user to also investigate the sammpling rate
#' of tags to see if particular sampling schedules were sustainable, with option then if
#' the argument dt = TRUE, to plot the sampling rate (dt) column in track data alongside either the
#' vbatt or vsll of the Vo data; the dt line will always appear on the secondary access as the
#' red line.
#'
#' No doubt there could be bugs in this, so do let the package maintainer know if anything doesn't work as intended.
#'
#' @seealso [MoveRakeR::read_voltage_UvA], [MoveRakeR::read_track_UvA]
#'
#' @param data A \code{Vo} or \code{VoStack} object.
#' @param dataTrack An (optional, default NULL) A \code{Track} or \code{TrackStack} object for further plotting
#' of sunrise/sunset grey rectangles on plots and used also for getting sampling rates tags were
#' specified too using the DateTime stamp gaps between successive fixes (dt).
#' @param start Optional UTC start time (default NULL) for subsetted DateTimes (UTC) specified as a character
#' with format e.g.: "yyyy-mm-dd hh:mm:ss", although the function \code{MoveRakeR:datetime} is called
#' so "dd-mm-YYYY hh:mm:ss" and "dd/mm/YYYY hh:mm:ss" and two digit years should work too.
#' @param end As with start (default NULL) but the end UTC DateTime; note one or other of start and end
#' can be specified independently.
#' @param roll_gap_thres A numeric integer value for the maximum gap between nearest matched volateg data DateTimes
#' and the track data DateTimes, defaulting to 3600 s, and anything more than that will be dropped, i.e.
#' may be that the tag was not yet deployed, still recording voltage data but no GPS.
#' @param dt Logical (default FALSE) for whether the "dt" column in the Track data should be used to
#' plot in the secondary y-axis; only works if a Track-family object (TMS not accepted) is supplied
#' in the "dataTrack" argument.
#' @param max_dt The maximum dt value to retain int he plot (experimental) i.e. to remove all dt's greater than
#' this value; note lines are still drawn between gaps in the lineplot; only works if the
#' "dataTrack" argument is not NULL.
#' @param dt_var The variable to plot on the main y-axis in the plotting alongside "dt", defaulting
#' to the battery voltage (vbat) in the Vo dataset, but can accept the vsll solar charging data as well; only
#' works if the "dataTrack" argument is not NULL.
#'
#' @return
#' A ggplot reflecting the user's inputs an wishes.
#'
#' @examples
#'
#' db_file <- "GPS" # what you named the ODBC link on set up
#' db <- RODBC::odbcConnect(db_file) # currently required "db" to be named globally
#'
#' TagIDs = c('1', '2', '3')
#' start = c("2021-06-21 06:00:00")
#' end = c("2021-07-25 09:00:00")
#' dataVo <- read_voltage_UvA(TagIDs, start=start, end=end) # form = energy is default
#' dataTrack <- read_track_UvA(TagIDs, start=start, end=end)
#'
#' data = do.call('rbind',dataVo) # function will accept a TrackStack and VoStack as well....
#' dataTrack = do.call('rbind',dataTrack)
#'
#' ### plot simple battery vs solar charge, note hard to see with the timescale chosen
#' plot(dataVo)
#'
#' ### zoom in on the timeline
#' plot(dataVo, start = "2021-06-22 00:01:00", end = "2021-06-28 00:01:00")
#'
#' ### add in Track data for day/night rectangles#'
#' plot(data = dataVo, dataTrack = dataTrack, start = "2021-06-22 00:01:00", end = "2021-06-28 00:01:00")
#'
#' ### try a dt sampling rate plot
#' plot(dataVo, dataTrack, dt = TRUE) # note dt only works if a Track/TrackStack is supplied
#' plot(data = dataVo, dataTrack = dataTrack, dt = TRUE, start = "2021-06-22 00:01:00", end = "2021-06-28 00:01:00")
#'
#' ### see other options for toggling the thresholds of: max dt plotted, the max gap between the
#' ### voltage data and nearest Track data on merge (roll_gap_thres),
#' ### and plotting the solar charge instead of vbat default alongside dt using the dt_var variable.
#'
#' @export
#' @method plot Vo
plot.Vo <- function(data, dataTrack=NULL, start = NULL, end = NULL, roll_gap_thres = 3600, dt = FALSE, max_dt = NULL, dt_var = "vbat"){

  # only handles TrackStacks and Tracks!

  if(is_VoStack(data)){data = do.call('rbind',data)}

  # subset data through start and ends
  if(!is.null(start)){
    start = datetime(start)
    data <- data[data$DateTime >= start,]
  }
  if(!is.null(end)){
    end = datetime(end)
    data <- data[data$DateTime <= end,]
  }
  if(nrow(data) == 0){stop("No data for start/end range found in voltage data")}

  if(dt == TRUE & is.null(dataTrack)){
    stop("If specifying a sampling rate dt plot, needs a Track or TrackStack data object supplied")
  }

  if(!is.null(dataTrack)){

    if(is_TrackStack(dataTrack)){dataTrack = TrackStack2Track(dataTrack)}

    if(!is.null(start)){
      dataTrack <- dataTrack[dataTrack$DateTime >= start,]
    }
    if(!is.null(end)){
      dataTrack <- dataTrack[dataTrack$DateTime <= end,]
    }
    if(nrow(dataTrack) == 0){stop("No data for start/end range found in Track data")}

    # add dts?
    if(dt == TRUE){
      dataTrack <- dtt(dataTrack, verbose = FALSE)
    }

    # add sunrise/sunset
    #dataTrack <- sun_calc(dataTrack)

    dat <- data.frame(date = lubridate::date(dataTrack$DateTime),
                      lat = dataTrack$latitude,
                      lon = dataTrack$longitude)

    getem <- suncalc::getSunlightTimes(data = dat, keep = c("sunrise","sunset"))

    dataTrack$sunrise <- getem$sunrise
    dataTrack$sunset <- getem$sunset

    # merge the nearest geographical position with the battery data

    ls1 <- list()
    bds <- unique(data$TagID)
    i = 1
    for(i in 1:length(bds)){
      dataTrack2 <- data.table::data.table(dataTrack[dataTrack$TagID == bds[i],], key = c("DateTime"))  # interpolated data
      dataTrack2$DateTime_Track <- dataTrack2$DateTime

      data2 <- unique(data.table::data.table(data[data$TagID == bds[i],], key = c("DateTime")))  # raw data
      data3 <- dataTrack2[data2, list(TagID, DateTime, DateTime_Track) , roll = "nearest", allow.cartesian=TRUE ]
      datasub <- data[data$TagID == bds[i],]

      datasub <- unique(merge(datasub,data3, by = c("TagID","DateTime")))
      #datasub$DateTime_Track <- data3$DateTime_Track

      # merge lat long of the Track, and the dtt of the Track!
      if(dt == TRUE){
        dataTrack3 <- subset(dataTrack2, select = c(DateTime_Track,longitude,latitude,sunrise,sunset,dt))
      } else{
        dataTrack3 <- subset(dataTrack2, select = c(DateTime_Track,longitude,latitude,sunrise,sunset))
      }
      datasub <- merge(datasub,dataTrack3, by = "DateTime_Track")
      datasub$roll_gap <- as.vector(difftime(datasub$DateTime_Track, datasub$DateTime, units = "secs"))
      datasub$roll_gap <- ifelse(datasub$roll_gap < 0,datasub$roll_gap *-1,datasub$roll_gap )

      ls1[[i]] <- unique(datasub)

    }
    data <- do.call('rbind',ls1)

    # if roll_gap is > a dt roll_gap of X, then NA lat,long,dt
    data$latitude <- ifelse(data$roll_gap > roll_gap_thres, NA, data$latitude)
    data$longitude <- ifelse(data$roll_gap > roll_gap_thres, NA, data$longitude)
    if(dt == TRUE){
      data$dt <- ifelse(data$roll_gap > roll_gap_thres, NA, data$dt)
    }

  }

  # ------------------------------------ #
  .rescale <- function(x, a, b, c=min(x), d=max(x)){ #new.min, new.max, old.min, old.max
    y = a + (b - a) * (x - c)/(d - c)
    return(y)
  }

  ### the standard Vo plotting of voltage and solar charging
  if(dt == FALSE){

    # get the range of values across the full voltage dataset
    # then scale the solar to this as it varies by a lot more and plot
    # as a secondary y axis

    a = min(data$vbat); b = max(data$vbat); c = min(data$vsll); d = max(data$vsll)
    data$vsll_2 <- .rescale(data$vsll, a, b)

    y1n <- "Battery voltage (V)"
    y2n <- "Solar voltage (v)"
    scaled_breaks <- ggplot2::waiver()

    wb = which(names(data) == "vbat")

  }

  ### The custom dt plot of sampling rates alongside one other of the variables, like battery or solar charging
  # but which way around should we plot this, Track or Vo? I guess still from when the battry took readings....

  if(dt == TRUE){

    # get which variable you want to plot on the MAIN yaxis (dt specified on second)
    data <- data[!is.na(data$dt),]
    data <- data[data$dt > 0,]

    if(!is.null(max_dt)){
      data <- data[data$dt <= max_dt,]
    }

    wd = which(names(data) == "dt") # the dt we always want under this option for 2ndary axis
    wb = which(names(data) == dt_var) # the battery value on first y axis

    a = min(data[,wb]); b = max(data[,wb]); c = min(data[,wd]); d = max(data[,wd])
    data$vsll_2 <- .rescale(data[,wd], a, b) # keep the naming the same so can plot second y for dt == TRUE/FALSE

    y1n <- names(data)[wb]
    if(y1n == "vbat"){y1n <- "Battery voltage (V)"}
    if(y1n == "vsll"){y1n <- "Solar voltage (v)"}
    y2n <- "Sampling rate (s)"

    # Vector of desired breakpoints for secondary axis for "dt"
    #sec_breaks <- seq(0, 1, 1/6)
    if(d < 1000000){d2 = 1000000; scaled_breaks <- seq(0,1000000,200000)}
    if(d < 500000){d2 = 500000; scaled_breaks <- seq(0,500000,100000)}
    if(d < 200000){d2 = 200000; scaled_breaks <- seq(0,200000,50000)}
    if(d < 100000){d2 = 100000; scaled_breaks <- seq(0,100000,20000)}
    if(d < 50000){d2 = 50000; scaled_breaks <- seq(0,50000,10000)}
    if(d < 40000){d2 = 40000; scaled_breaks <- seq(0,40000,10000)}
    if(d < 30000){d2 = 30000; scaled_breaks <- seq(0,30000,5000)}
    if(d < 20000){d2 = 20000; scaled_breaks <- seq(0,20000,5000)}
    if(d < 15000){d2 = 15000; scaled_breaks <- seq(0,15000,5000)}
    if(d < 10000){d2 = 10000; scaled_breaks <- seq(0,10000,2000)}
    if(d < 5000){d2 = 5000; scaled_breaks <- seq(0,5000,1000)}
    if(d < 2500){d2 = 2500; scaled_breaks <- seq(0,2500,500)}
    if(d < 2000){d2 = 2000; scaled_breaks <- seq(0,2000,500)}
    if(d < 1500){d2 = 1500; scaled_breaks <- seq(0,1500,500)}
    if(d < 1000){d2 = 1000; scaled_breaks <- seq(0,1000,200)}
    if(d < 500){d2 = 500; scaled_breaks <- seq(0,500,100)}
    if(d < 100){d2 = 100; scaled_breaks <- seq(0,100,20)}

    #scaled_breaks <- d2 * sec_breaks
  }

  ### plotting multi-bird facet

  grobj = ggplot2::ggplot() +
    ggplot2::geom_line(data = data, ggplot2::aes_(x = ~DateTime, y = ~data[,wb]), color = "steelblue",size = 1) +
    ggplot2::geom_line(data = data, ggplot2::aes_(x = ~DateTime, y = ~vsll_2), color = "red",size = 1) +
    #geom_point(color="steelblue")

    # Custom the Y scales:
    # a = min(vbat), b = max(vbat), c = min(vsll), d = max(vsll)
    # x = (a*(d-2*c)+(b*c)+(y*(c-d))) / (a-b)
    # where x = vsll and y = vbat
    #x = (a*(d-2*c)+(b*c)+(.*(c-d))) / (a-b)

    ggplot2::scale_y_continuous(y1n,
                       #sec.axis = sec_axis(trans=~ (rVbat[1] + (rVbat[2] - rVbat[1]) * (. - min(data$vsll))/(max(data$vsll) - min(data$vsll))),  name="Solar voltage (V)")
                       #sec.axis = sec_axis(trans=~ ~./scaleFactor,  name="Solar voltage (V)")
                       sec.axis = ggplot2::sec_axis(trans=~(a*(d-2*c)+(b*c)+(.*(c-d))) / (a-b),
                                           name=y2n,
                                           breaks = scaled_breaks)
    )

  grobj2 = grobj + ggplot2::facet_grid(as.formula(paste("TagID", " ~ .")), labeller = "label_both")
  grobj2 = grobj2 + ggplot2::facet_wrap(as.formula(paste("TagID", " ~ .")),strip.position="top",ncol = 1, nrow=length(unique(data$TagID)))
  grobj2 <- grobj2 + ggplot2::theme_bw()
  grobj2 <- grobj2 + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5),text = ggplot2::element_text(size=10))
  #grobj2 <- grobj2 + ggplot2::theme(panel.grid.major = ggplot2::element_blank(),panel.grid.minor = ggplot2::element_blank(),axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5))
  grobj2 <- grobj2 + ggplot2::geom_point(size=1)


  grobj2 <- grobj2 + ggplot2::theme(axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)))
  grobj2 <- grobj2 + ggplot2::theme(plot.margin=unit(c(0.5,1,0.5,0.5), "cm"))

  grobj2 <- grobj2 + ggplot2::theme(axis.title.y.right = ggplot2::element_text(vjust= 4))

  # add dashed line for charging of solar at ca. 1.8 volts
  if(y2n == "Solar voltage (v)"){
    grobj2 <- grobj2 + ggplot2::geom_hline(yintercept = a + (b - a) * (1.8 - c)/(d - c), color = "grey", linetype = "dashed", size = 1)
  }

  #y = 2
  #x = (a*(d-2*c)+(b*c)+(y*(c-d))) / (a-b)
  # y = a + (b - a) * (x - c)/(d - c)

  ### plot day/night rectangles
  if(!is.null(dataTrack)){

    # these form the vertical rects, but needs to be different per bird if they are in diff locations!
    sr <- data.frame(SR = unique(lubridate::round_date(data$sunrise,unit = "10 minutes")))
    sr$date <- lubridate::date(sr$SR)
    ss <- data.frame(SS = unique(lubridate::round_date(data$sunset,unit = "10 minutes")))
    ss$date <- lubridate::date(ss$SS)
    srss <- merge(sr,ss,by = "date",all=TRUE)
    srss <- srss[!duplicated(srss$date),]

    i = 1
    ls2 <- list()
    for(i in 2:nrow(srss)){
      ls2[[i]] <- data.frame(SS = srss[i-1,]$SS, SR = srss[i,]$SR)
    }
    srss2 <- do.call('rbind',ls2)
    names(srss2)[1] <- "x1"
    names(srss2)[2] <- "x2"
    srss2$y1 <- a
    srss2$y2 <- b

    grobj2 <- grobj2 + ggplot2::geom_rect(data = srss2, mapping = ggplot2::aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2),
                                 color = NA, alpha = 0.1)

  }

  return(grobj2)

}










