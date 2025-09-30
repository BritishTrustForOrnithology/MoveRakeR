#' Trajectory speed filter for GPS data
#'
#' Annotate or filter data that is apparently too fast for real animal movement based on a pre-defined threshold.
#'
#' @details
#' A common task when dealing with GPS movement data is to consider fixes in the dataset that
#' may be moving much faster than expected based on the upper limit of known movement speeds for the study animal.
#' There are such filters available in a variety of R packages, each with their own methodology that the user is
#' also directed to, such as \code{trip::speedfilter} and trip::sd() (Sumner et al. 2009, Sumner 2011).
#' Comparisons to other functions in neighbouring R packages will likely
#' give slightly different results. As with all such annotation and filter functions, care is needed to make sure the results are as expected, and
#' it should be stressed the MoveRakeR function \code{speed_filt} is not a filter by a speed value
#' The MoveRakeR approach provides further options alongside these other R packages and works by iteratively remove GPS fixes that violate a threshold.
#' This is particularly useful should several consecutive erroneous points be identified (i.e. displaced from where they should be).
#' Caution is also needed when considering GPS sampling rates as maximum finer-scale (or coarser-scale) changes between consecutive
#' fixes could vary among different scales. Finer scale sampling for stationary animals may reflect natural GPS error that
#' could yield otherwise erroneous travel speeds and so may need considering rate-by-rate. This pattern can also
#' be examined further using the function \code{speed_jit}.
#'
#' Here the default value in the speed filter is 50 m/s and this will depend on the species studied.
#' The value should be chosen wisely as mentioned above, taking a value well within the natural bounds of movement
#' of the animal will result in fixes being dropped that are otherwise valid due to the cumulative nature of the filter.
#' It is recommended therefore that a conservative value be selected above the maximum known speed of the animal, again paying
#' attention to artefacts that could occur by sampling rate.
#'
#' The \code{speed_filt} uses the function \code{traj_speed} to compute trajectory speed between consecutive fixes
#' within animals. This computation can be carried out 'within_gap' should gaps already be present in the data.
#'
#' @param data Input data object, with required columns: TagID, DateTime, longitude, latitude.
#' @param sp_thres The value (m/s) upon which to carry out the filter assessment, defaults to 50 m/s.
#' @param annotate TRUE or FALSE (default) for whether to annotate (TRUE) or filter (FALSE) the data by the sp_thres condition.
#' @param hasgaps TRUE (default) or FALSE whether to check for existing gaps having been defined already in the data,
#' and if found use those gaps, if not reverting to \code{gap_section} defaults; FALSE also takes the \code{gap_section} defaults.
#' @param within_gap For the \code{traj_speed} function, this checks for existence of gapsec column, if not, it fails. As the
#' \code{traj_speed} function is called from within \code{speed_filt}, which itself always uses a gapsection even if it is function defaults,
#' this allows \code{traj_speed} to be used without fail.
#' @param verbose TRUE (default) or FALSE whether to provide within-function messaging.
#' @param detailed_verbose TRUE or FALSE (default) whether more detailed messaging is to be provided.
#'
#' @return
#' Either an annotated dataset with an additional column of 'speed_rm' (1,0) for whether data were 'flagged', or a filtered
#' dataset with such rows having been removed.
#'
#' @seealso [MoveRakeR::turn_filt]
#'
#' @examples
#' ### loading a raw data format
#' indata <- yourdata # data.frame with a minimum of columns named TagID, DateTime, longitude, latitude
#' data <- Track(indata)
#'
#' dat_speed_fil = speed_filt(data, annotate = TRUE, detailed_verbose = FALSE)
#'
#' data_ = traj_speed(dat_speed_fil, within_gap = FALSE) # use of traj_speed
#'
#' attr(data_, "speed_filt")
#' sum(dat_speed_fil$speed_rm) # n = X fixes dropped
#'
#' # Compare also to other functions from other packages
#' # e.g. the trip package and function trip::speedfilter()
#'
#' # This may take a little while (for a test of a 750K row dataset ca 10 mins on
#' # an Intel(R) Core(TM) i3-3220 CPU @ 3.30GHz 8GB machine depending on memory use) to coerce big data:
#' data_in_trip <- data_in %>% data.frame() %>% # to remove the MoveRakeR::Track class
#'  rename(x = longitude, y = latitude, trip_id = TagID, time= DateTime)  %>% # for trip
#'  relocate(c(x,y,time, trip_id),.before = Type) %>% # make sure order is correct x,y,time,trip_id
#'  mutate(time = as.POSIXct(time)) %>% trip::trip()
#' test_sp_filt <- trip::speedfilter(data_in_trip, max.speed = 50*3.6, test = FALSE) # m/s to km/h
#' data_in_trip$outlier <- ifelse(test_sp_filt,1,0)
#'
#' @export
speed_filt<- function(data, sp_thres = 50, annotate = FALSE, hasgaps = TRUE, verbose = TRUE, detailed_verbose = FALSE){

  # get attributes from previous function runs
  attr_list <- get_attributes(data)

  data_dp <- data

  # use existing gaps if found
  if(hasgaps){

      # if hasgaps = TRUE
      # if no gapsection found, call gapsection for defaults, drop = FALSE, else use existing gapsec
      if(!exists("gapsec",data_dp)){
        if(verbose){
          message("----- Adding gap sections as none found -----\n",
                  "-- Using defaults: \n",
                  "-> GAP = 28800 s\n",
                  "-> tol = 0.2")
        }
        data_dp <- gap_section(data_dp, verbose = FALSE, attr = FALSE)
      } else{
        if(verbose){message("----- Using pre-existing gapsec column found in data -----")}
      }

  } else{

    # Even if there are previous gapsections, override with new choices
    data_dp <- gap_section(data_dp, verbose = FALSE, attr = FALSE)

  }


  #x = subdat
  .speed_filt <- function(x){

    thres <- sp_thres+20 # initialise while loop with value higher than selected as sp_thres
    thres_to_meet <- sp_thres
    dat <- x # retained for debugging

    if(length(dat$DateTime) > 1){ # only works on >1 gapsecs, i.e. can't calculate a dt value between DateTimes for row number == 1

      while(thres > thres_to_meet){
        #print("foobar")
        if(detailed_verbose){message("====== Starting threshold: ",round(thres,2), " ======")}

        dat <- dat %>% mutate(rn = row_number())

        # Use the MoveRakeR traj_speed filter - needs forwards and reverse e.g. if point is erroneous at the start or a section
        data2a <- traj_speed(dat)

        # this will have NAs for the start traj speed bird animal/gapsection
        data2a$traj_speed[is.na(data2a$traj_speed)] <- 0

        data2a <- data2a %>% mutate(traj_speed_rev = lead(traj_speed, default= 0)) # pad a 0 at the end of reversed row

        # conditions
        data2a$cond1 <- ifelse(data2a$traj_speed > thres,1,0)
        data2a$cond2 <- ifelse(data2a$traj_speed_rev > thres,1,0)

        ############################
        ## assess if any values are above the threshold and remove ONLY ONE (to allow reassessment in the while loop)
        ## but also assess if there are any issues at start and end, i.e. will ONLY be double X's
        ## if only the start and ends as '10' / '01' then there are NO fixes to remove

        #subset(data2a, select = c(cond1,cond2)) # double 1's are the issue!
        double_ones = data2a[data2a$cond1 == 1 & data2a$cond2 == 1,]

        # ok if there simply are no fixes above the threshold then it doesn't matter about conditions above
        # first check that over-arching condition
        if(any(data2a$traj_speed > thres_to_meet)){

          # are there any issues at start or end with the first or last fix being the likely issue?
          # or generally really obvious fixes being identified via forward and backwards assessment?
          if(nrow(double_ones) > 0){
            if(detailed_verbose){message("---> double-X point removal <---")}

            # select the double 1s then!
            above_thresh = double_ones #if more than one, i.e. start then first of these will be picked up below and second should be picked up on nt while loop (v rare I expect)
          } else{

            # otherwise we have no issues at the start and end and so reverting to a forward approach as did before
            above_thresh = data2a[data2a$traj_speed >= thres_to_meet,]
            if(detailed_verbose){message("---> Forward deletion --->")}
          }

        } else{
          if(detailed_verbose){message("--- no fixes to drop from this gapsection ---")}
          above_thresh = data2a[data2a$traj_speed >= thres_to_meet,] # will be zero nrow
        }

        rns_keep <- data2a$rn

        if(nrow(above_thresh) > 0){
          if(verbose){message("~~~ point(s) identified to drop. Dropping point no ",above_thresh$rn[1],": ", above_thresh$DateTime[1], " ~~~")}
          # then take out the FIRST ONLY ONTHIS LOOP
          rns_keep <- rns_keep[rns_keep != above_thresh$rn[1]] # take out first in the datetime order - could be two points far away in datetime, in which case new ltraj assignment should still find the second etc on next while loop
        }

        ## now select rownames as a subset i.e. matching those that meet above thres.to.meet condition
        dat <- dat[dat$rn %in% rns_keep,]

        # reassess trajectory speed across the TagID/gapsection
        data2aa = traj_speed(dat)

        ## check if there is still a point beyond thres_to_meet via max of REMAINING dt/dists
        if(!is.na(data2aa$dist[1])){
          thres <- max((data2aa$dist / data2aa$dt),na.rm=TRUE)
          if(detailed_verbose){message("====== thres after: ", round(thres,2), " ======")}
          #dat <- data2aa # I think we probably assign this back to dat right??
        }
        if(is.na(data2aa$dist[1])){
          break
        }

      }

    }

    return(dat)
  }

  ##############################
  # replace in position? better than rbinding
  #q = 1; g = 1
  data_dp$rn = 0
  data_dp$rn2 = 1:nrow(data_dp)

  # remember fixes before removal of points from speed filter
  vv = data.frame(table(data_dp$TagID))

  ##############################
  # LOOP

  # if we are labelling fixes, add a column to capture this
  if(annotate){
    data_dp$speed_rm <- 0
  }

  tid = unique(data_dp$TagID)
  for(q in 1:length(tid)){
    if(verbose){message("TagID: ", tid[q])}
    gs = unique(data_dp[data_dp$TagID %in% tid[q],]$gapsec)
    for(g in 1:length(gs)){
      if(detailed_verbose){message("G A P   S E C T I O N   ", g, ", TagID: ", tid[q])}
      subdat = data_dp[data_dp$TagID %in% tid[q] & data_dp$gapsec %in% gs[g],]
      subdat0 = .speed_filt(subdat)

      # remove non-matching rows from original
      rdrop = subdat$rn2[which(!subdat$rn2 %in% subdat0$rn2)] # rows removed

      if(length(rdrop) > 0){

        if(!annotate){
          if(detailed_verbose){message("DROPPING FIXES")}
          data_dp = data_dp[!data_dp$rn2 %in% rdrop,]
        } else{
          if(detailed_verbose){message("LABELLING FIXES")}

          data_dp$speed_rm <- ifelse(data_dp$rn2 %in% rdrop, 1, data_dp$speed_rm)

        }

      }
    }

    if(verbose){

      if(!annotate){
        dd = nrow(data_dp[data_dp$TagID %in% tid[q],] )
        orig_n = vv[vv$Var1 %in% tid[q],]$Freq

        if(dd < orig_n){
          message("TagID: ", tid[q], ": Dropped: ", orig_n - dd, " fixes due to speed filter")
        } else{
          message("TagID: ", tid[q], ": No fixes dropped due to speed filter")
        }
      }

    }

  }


  # ------------------------------------------------------ #
  data_out <- Track(tibble(data_dp))

  # --------------------------------------------------------- #
  # assign attributes that may have been present at the start
  data_out <- give_attributes(data_out, attr_list)

  # ------------------------------------------------------ #
  # new attributes on current function run

  if(is.null(attr(data_out, "speed_filt") )){
    attr(data_out, "speed_filt") <- "speed_filt"
  }

  attr(attr(data_out, "speed_filt"), "sp_thres") <- sp_thres
  attr(attr(data_out, "speed_filt"), "annotate") <- annotate
  attr(attr(data_out, "speed_filt"), "hasgaps") <- hasgaps

  data_out <- data_out %>% group_by(TagID)
  data_out <- structure(.Data = data_out, class = c("Track","grouped_df", "tbl_df","tbl","data.frame"))

  return(data_out)


}

#' @rdname speed_filt
#' @export
traj_speed <- function(data, within_gap = FALSE){

  # get attributes from previous function runs
  attr_list <- get_attributes(data)

  if(within_gap){
    # check for existence of gapsec column, if not, fail
    if(exists("gapsec", data)){
      data = data %>% group_by(TagID, gapsec)
    } else{
      stop("gapsec column not found in data - did you run function gap_section()?")
    }
  } else{
    data = data %>% group_by(TagID)
  }

  # ---------------- #
  # distance and time calculations between fixes
  # Quicker to use disHaversine, geosphere::distVincentySphere seems the same

  #data = data %>%
  #  #group_by(TagID) %>%
  #  mutate(dist = c(geosphere::distHaversine(cbind(longitude, latitude)), NA),
  #         dt = as.vector(difftime(lead(DateTime), DateTime, units = 'secs'))
  #  )

  data <- data %>%
    mutate(
      dist = if (n() > 1) {
        c(geosphere::distHaversine(cbind(longitude, latitude)), NA)
      } else {
        NA_real_
      },
      dt = as.numeric(difftime(lead(DateTime), DateTime, units = "secs"))
    )

  data <- data %>% mutate(traj_speed = dist / dt,
                          traj_speed = lag(traj_speed, default= NA_real_)) # pad with '1' at the start, removing last row


  # --------------------------------------------------------- #
  # assign attributes that may have been present at the start
  data <- give_attributes(data, attr_list)

  return(data)

}




