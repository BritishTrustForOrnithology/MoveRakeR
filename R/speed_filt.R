#' Trajectory speed filter for GPS data
#'
#' Annotate or filter data that is apparently too fast for real animal movement based on a pre-defined threshold.
#'
#' @details
#' A common task when dealing with GPS movement data is to consider fixes in the dataset that
#' may be moving much faster than expected based on the upper limit of known movement speeds for the study animal.
#' There are such filters available in a variety of R packages, each with their own methodology that the user is
#' also directed to, such as \code{trip::speedfilter()} and trip::sd() (Sumner et al. 2009, Sumner 2011).
#' Comparisons to other functions in neighbouring R packages will likely
#' give slightly different results. As with all such annotation and filter functions, care is needed to make sure the results are as expected, and
#' it should be stressed the \code{speed_filt()} is not a direct filter by a speed value as such. The \code{speed_filt()} function works
#' by iteratively removing GPS fixes that violate the upper speed threshold. This process is particularly useful
#' should several consecutive erroneous points be identified (i.e. displaced from where they should be).
#' Caution is also needed when considering GPS sampling rates as maximum finer-scale (or coarser-scale) changes between consecutive
#' fixes could vary among different scales. Finer scale sampling for stationary animals may reflect natural GPS error that
#' could yield otherwise erroneous travel speeds and so may need considering rate-by-rate. This pattern can also
#' be examined further using the function \code{speed_jit()}. Alternatively, smoothing can be explored (see below).
#'
#' Here the default value in the speed filter is 50 m/s and this will depend on the species studied.
#' The value should be chosen wisely as mentioned above, taking a value well within the natural bounds of movement
#' of the animal will result in fixes being dropped that are otherwise valid due to the cumulative nature of the filter.
#' It is recommended therefore that a conservative value be selected above the maximum known speed of the animal, again paying
#' attention to artefacts that could occur by sampling rate.
#'
#' The raw trajectory speed filter (default) is sensitive to single bad spikes, small short-scale GPS jitter and
#' possibly projection artefacts. This is ideal for catching obvious spikes in the data that may be enough alone.
#' However, the raw trajectory approach may not capture instances where there may be localised jitter and
#' several fixes in a row may be slightly 'wrong'. In this case, considering a further smooth or RMS (root mean square)
#' is useful to answer a slightly different question in that: "is there sustained implausible movement in the
#' neighbourhood for this fix?". And so further options are available to use RMS or smoothing of trajectory speed
#' instead of or alongside (preferable) to the default setting of using the raw trajectory speed values.
#' For more information see \code{traj_speed_sm}. Using RMS/smoothing alongside the raw traj. speed integrates information
#' from multiple adjacent fixes (if McConnell, = 2 fixes either side excluding the current fix), which dampens the
#' one-off GPS noise and reflects local movement context. As above, this can help identify if the raw traj speed is
#' not wrong but there is sustained local inconsistency. McConnell exudes the centre point to avoid double-counting
#' the error i.e. a bad fix can inflate it's own smoothed value, so this asks whether the neighbours around the fix
#' imply unrealistic movement. This is therefore a context detector not a spike detector.
#'
#' Methods are available to compute the root mean square over varying moving window sizes with options of how
#' to treat edge conditions and inclusion/exclusion of window centre-points (noting the above), as well as
#' bespoke functions that can be fed through to the filter. However, \strong{beware of using the smoothing/RMS options
#' alone with dropping fixes}.
#'
#' Also beware if identifying spikes in erroneous points excluding the centre-point. It is included as a flexible option here whether to do so
#' but the McConnell was designed to identify transient spikes in speed, not to smooth over the exact point itself.
#' The idea is to compute a local RMS around a point, but using its neighbors to avoid the spike “contaminating”
#' its own value. Therefore, use the `include_centre` option with caution.
#'
#' If method = "none", i.e. no smoothing, the \code{speed_filt} uses the function \code{traj_speed()} to compute trajectory speed between consecutive fixes
#' within animals. Otherwise, if smoothing is used, the \code{traj_speed_sm()} function is used.
#' The computations can be carried out 'within_gap' should gaps already be present in the data.
#'
#' Annotate is set to TRUE by default as it may be up to the user how to proceed having flagged fixes, e.g. whether
#' just the traj speed and RMS/smoother trend revelers are sufficient on their own. Setting annotate to FALSE will drop
#' any fixes flagged.
#'
#' Note: The same speed threshold ceiling is applied to both raw and smoothed trajectory speeds.
#' Depending on the degree of smoothing, this may result in some high-speed spikes being missed or excessive points
#' being flagged. Users may wish to adjust the threshold when using hybrid filtering.
#' In hybrid mode, note that points flagged in the raw and smoothed trajectories may not overlap much;
#' the combined speed_rm column (either/or) can therefore be larger than either speed_rm_raw or speed_rm_sm
#' individually, indicating that relatively few points are flagged by both methods simultaneously."
#'
#' @param data Input data object, with required columns: TagID, DateTime, longitude, latitude.
#' @param sp_thres The value (m/s) upon which to carry out the filter assessment, defaults to 50 m/s.
#' @param hybrid Logical, as to whether to use a hybrid speed filter, using both raw
#' trajectory speed and a smooth/RMS filter; defaults to FALSE giving trajectory speed filtered using raw data.
#' If set to TRUE, the default is then the hybrid approach with the mcconnell filter.
#' @param method One of four options relating to whether smoothing in the trajectory speed is required.
#' This is one of "none" (default), no smoothing, and three further method names relating to root mean square
#' calculations stemming from the McConnell et al. (1992) approach, used either directly as that "mcconnell"
#' or two further flexible extensions to the RMS with "flexi_clip" adjusting number of points (points_each_side)
#' within the filter at the edges to avoid NAs, and "flexi_vec" as a faster vectorised form but that has potential edge variations to flexi_clip,
#' with 'include_centre' consideration for the midpoint (excluded from mcconnell); for more details on
#' method arguments, see \code{traj_speed_sm()}.
#' @param points_each_side integer, default = 2. As in see \code{traj_speed_sm()}, for RMS methods the number of points either side for "flexi_clip" and "flexi_vec".
#' @param include_centre logical, default TRUE. As in see \code{traj_speed_sm()}, for RMS methods whether to include the centre point in "flexi_clip" and "flexi_vec".
#' @param fun As in \code{traj_speed_sm()}, the name of a bespoke function argument supplied, default NULL. This
#' overrides choices in `method`, `points_each_side` and `include_centre`.
#' @param fun_args As in \code{traj_speed_sm()}, arguments to be passes to `fun` as a list(), default empty list, activated if fun is not NULL
#' and overrides any choices in method, points_each_side and include_centre.
#' @param annotate Logical default TRUE for whether to annotate (TRUE) or filter (FALSE) the data by the sp_thres condition.
#' @param hasgaps TRUE (default) or FALSE whether to check for existing gaps having been defined already in the data,
#' and if found use those gaps, if not reverting to \code{gap_section} defaults; FALSE also takes the \code{gap_section()} defaults.
#' @param within_gap For the \code{traj_speed()} function, this checks for existence of gapsec column, if not, it fails. As the
#' \code{traj_speed} function is called from within \code{speed_filt()}, which itself always uses a gapsection even if it is function defaults,
#' this allows \code{traj_speed()} to be used without fail.
#' @param fallback logical, default TRUE. If a smoothing method or fun is used, yet too-few data are available in a section of points,
#' i.e. TagID/gapsec, the traj_speed smooth values may all be NA. This fallback option allows a defaulting back to using
#' raw 'unsmoothed' trajectory speed to continue use in the filter (if TRUE), otherwise if FALSE, the \code{speed_filt()} will
#' skip the small section of points entirely.
#' @param verbose TRUE (default) or FALSE whether to provide within-function messaging.
#' @param detailed_verbose TRUE or FALSE (default) whether more detailed messaging is to be provided.
#'
#' @return
#' Either an annotated dataset with an additional column of 'speed_rm' (1,0) for whether data were 'flagged', or a filtered
#' dataset with such rows having been removed. In the case of the hybrid annotation approach, two additional columns are included
#' detailing annotation of rows from the raw trajectory speed ('speed_rm_raw') and RMS/smooth functions ('speed_rm_sm'), respectively, with the
#' 'speed_rm' representing a '1' where either of those additional columns is a '1'.
#'
#' @seealso [MoveRakeR::turn_filt], [MoveRakeR::traj_speed_sm]
#'
#' @references
#'
#' McConnell, B. J. and Chambers, C. and Fedak, M. A. (1992) Foraging ecology of southern elephant seals in
#' relation to the bathymetry and productivity of the southern ocean. \emph{Antarctic Science}, \strong{4}, 393-398.
#'
#' @examples
#' ### loading a raw data format
#' indata <- yourdata # data.frame with a minimum of columns named TagID, DateTime, longitude, latitude
#' data <- Track(indata)
#'
#' #### Default, no hybrid traj_speed + smooth or RMS
#' # hybrid is FALSE by default and method = "none", i.e. to just use raw traj speed, with annotate = TRUE
#' dat_speed_fil = speed_filt(data, detailed_verbose = TRUE) # fastest of course
#'
#' # lowering the speed ceiling:
#' dat_speed_fil2 = speed_filt(data, detailed_verbose = FALSE, sp_thres = 35)
#'
#' # example DROPPING the fixes
#' test2b = speed_filt(data, annotate = FALSE, detailed_verbose = TRUE)
#' nrow(test2b)
#' nrow(data)
#'
#' ##### Hybrid activation:
#' dat_speed_fil = speed_filt(data, hybrid = TRUE, annotate = TRUE, detailed_verbose = FALSE) # hybrid filter mconnell + raw traj speed
#'
#' attr(dat_speed_fil, "speed_filt")
#' sum(dat_speed_fil$speed_rm) # n = X fixes that could be dropped (annotate = TRUE)
#'
#' # Or smoothing on its own without raw traj_speed considered at all (method indicating smoothed, or rather RMS wanted):
#' dat_speed_fil = speed_filt(data, hybrid = FALSE, method = "mcconnell", annotate = TRUE, detailed_verbose = FALSE) # hybrid filter mconnell + raw traj speed
#'
#' # lowering the speed ceiling:
#' dat_speed_fil2 = speed_filt(data, hybrid = TRUE, annotate = TRUE, detailed_verbose = FALSE, sp_thres = 35)
#'
#' # obviously use with caution as a single ceiilng is used for hybrid
#' sum(dat_speed_fil2$speed_rm)
#' sum(dat_speed_fil2$speed_rm_raw)
#' sum(dat_speed_fil2$speed_rm_sm)
#'
#' # e.g. a too low threshold will bring more raw fixes into play that may be legitimate.
#' # see notes in details.
#'
#' # use of traj_speed() and traj_speed_sm() direct on data
#' data_ = traj_speed(dat_speed_fil, within_gap = FALSE) # use of traj_speed
#' data_ = traj_speed_sm(dat_speed_fil, within_gap = FALSE) # use of traj_speed_sm using defaults
#'
#' ### Options using other built in RMS
#'
#' test1 = speed_filt(data, detailed_verbose = TRUE) # <--- default method = "mcconnell" + raw traj_speed
#'
#' # other similar hybrid options with the other methods:
#' test2 = speed_filt(data, method = "flexi_clip", detailed_verbose = TRUE)
#' test3 = speed_filt(data, method = "flexi_vec", detailed_verbose = TRUE)
#' test4 = speed_filt(data, method = "mcconnell", detailed_verbose = TRUE)
#'
#' # bespoke smoothing with a specified function, e.g. rolling median:
#' funr <- function(x, y, k = 5){
#'   frollapply(y, n = k, FUN = median, align = "center", fill = NA)
#' }
#' test5 = speed_filt(data, hybrid = TRUE, detailed_verbose = TRUE, fun = funr, fun_args = list(k = 5)) # smoother and raw traj
#' test6 = speed_filt(data, hybrid = FALSE, detailed_verbose = TRUE, fun = funr, fun_args = list(k = 5)) # just the smoother
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
speed_filt <- function(data, sp_thres = 50,
                      hybrid = FALSE,
                      method = c("none","mcconnell", "flexi_clip", "flexi_vec"),
                      points_each_side = 2,
                      include_centre = TRUE,
                      fun = NULL,
                      fun_args = list(),
                      annotate = TRUE,
                      hasgaps = TRUE,
                      verbose = TRUE,
                      fall_back = TRUE,
                      detailed_verbose = FALSE){

  # get attributes from previous function runs
  attr_list <- get_attributes(data)

  data_dp <- data

  method <- method[1]

  if(is.null(fun)){
    if(!method %in% c("none","flexi_clip", "mcconnell", "flexi_vec")){
      stop("method needs to be one of: 'none', 'flexi_clip', 'mcconnell' or 'flexi_vec' ")
    }
  }

  if(detailed_verbose){
    message(strrep("*", getOption("width")))
    message("speed_filt() function with detailed messaging (set detailed_verbose to FALSE if not wanted)")
    message(strrep("*", getOption("width")))
    verbose <- TRUE
  }


  # if hybrid smoother requested, but method is none and fun is null, default to no hybrid, i.e. will be traj speed only
  if(hybrid){
    if(method == "none" & is.null(fun)){
      if(verbose) message("Hybrid requested but no smoothing method supplied. Defaulting to raw-only speed")
      hybrid <- FALSE
    } else{
      # note if hybrid chosen, this will use BOTH traj speed and smoothed traj speed after passing first check for method above
      if(verbose){message("Hybrid approach: \n- using both raw traj_speed and the chosen smooth/RMS approach")}
      }
  } else{
    if(method == "none" & is.null(fun)){
      if(verbose){message("Using raw traj_speed for the filter")}
    } else{
      if(verbose){message("Using RMS/smoothed traj_speed for the filter")}
    }

  }

  if(!is.null(fun)){

    Fun = fun
    Fun_args = fun_args

    if(verbose){message("Using smoothed trajectory speed with a custom function")}

    # if a traj_speed column already exists, store this and don't override - this function uses traj_speed as a name to do the processing
    if(exists("traj_speed", data_dp)){ data_dp$traj_speed_ = data_dp$traj_speed }

  } else{

    Method = method
    Points_each_side = points_each_side
    Include_centre = include_centre

    if(method != "none"){
      if(verbose){message("- RMS trajectory speed:")}

      if(method == "mcconnell"){
        include_centre = FALSE # always the case
      }

      if(include_centre){
        ex_in <- "incl."
        np = (points_each_side*2)+1
      } else{
        ex_in <- "excl."
        np = (points_each_side*2)
      }

      if(verbose){ message("   - method: ", method, " (window per side = ", points_each_side, ", ", ex_in, " centre = ", np, " points)")}

      # if a traj_speed column already exists, store this and don't override - this function uses traj_speed as a name to do the processing
      if(exists("traj_speed", data_dp)){ data_dp$traj_speed_ = data_dp$traj_speed }

    } else{
      if(verbose){ message("Using raw trajectory speed for filtering")}

    }

  }

  # -------------------------------------------------------- #
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

  # --------------------------------------------------------- #
  # worker function
  # --------------------------------------------------------- #

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

        # No bespoke function supplied, or no built-in RMS method
        # Use the MoveRakeR traj_speed filter - needs forwards and reverse e.g. if point is erroneous at the start or a section

        # if hybrid is TRUE, then by default the filter will use the smooth pathway, this overrides that choice to also consider unsmoothed
        if(override == 1){
          data2a <- traj_speed(dat)
        } else{

          if(is.null(fun) & method == "none"){
            data2a <- traj_speed(dat)

          } else{

            # no function,
            if(is.null(fun) & method %in% c("flexi_clip", "mcconnell", "flexi_vec")){

              data2a <- traj_speed_sm(dat,
                                      method = Method,
                                      within_gap = TRUE, # always using the gapsecs defined at higher level from speed_filt()
                                      points_each_side = Points_each_side,
                                      include_centre = Include_centre,
                                      verbose = FALSE)

              # with a bespoke function
            } else if(!is.null(fun)){

              data2a <- traj_speed_sm(dat,
                                      within_gap = TRUE, # always using the gapsecs defined at higher level from speed_filt()
                                      fun = Fun,
                                      fun_args = Fun_args,
                                      verbose = FALSE)
            }


            # match arguments check the hasgaps or usegaps, and change the RMS variable returned to traj_speed
            # so filter can work...
            data2a$traj_speed = data2a$traj_speedSM


          }

        }


        if(all(is.na(data2a$traj_speed ))){

          # check for traj_speed - override the smoothing method...
          if(detailed_verbose){message("Too few GPS fixes to assess with chosen smoothing method (traj_speed = all NA).")}

          if(fall_back){
            if(detailed_verbose){message("Defaulting to using raw traj_speed")}
            data2a <- traj_speed(data2a)
          } else{
            break
          }

        }

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

        # ------------------------------------------------------ #
        # reassess trajectory speed across the TagID/gapsection

        if(override == 1){
          data2aa <- traj_speed(dat)
        } else{

          if(is.null(fun) & method == "none"){
            data2aa <- traj_speed(dat)

          } else{

            # no function,
            if(is.null(fun) & method %in% c("flexi_clip", "mcconnell", "flexi_vec")){

              data2aa <- traj_speed_sm(dat,
                                       method = Method,
                                       within_gap = TRUE, # always using the gapsecs defined at higher level from speed_filt()
                                       points_each_side = Points_each_side,
                                       include_centre = Include_centre,
                                       verbose = FALSE)

              # with a bespoke function
            } else if(!is.null(fun)){

              data2aa <- traj_speed_sm(dat,
                                       within_gap = TRUE, # always using the gapsecs defined at higher level from speed_filt()
                                       fun = Fun,
                                       fun_args = Fun_args,
                                       verbose = FALSE)


            }

            data2aa$traj_speed = data2aa$traj_speedSM

          }

        }

        if(all(is.na(data2aa$traj_speed ))){
          # check for traj_speed - override the smoothing method...
          #if(detailed_verbose){message("Too few GPS fixes to assess with chosen method (traj_speed = all NA).")}

          if(fall_back){
            #if(detailed_verbose){message("Defaulting to using raw traj_speed")}
            data2aa <- traj_speed(data2aa)
          } else{
            break
          }
        }

        ## check if there is still a point beyond thres_to_meet via max of REMAINING dt/dists
        if(!is.na(data2aa$dist[1])){

          #thres <- max((data2aa$dist / data2aa$dt),na.rm=TRUE)
          thres <- max(data2aa$traj_speed,na.rm=TRUE)

          if(detailed_verbose){message("====== thres after: ", round(thres,2), " ======")}
          #dat <- data2aa # I think we probably assign this back to dat right??
        }
        if(is.na(data2aa$dist[1])){
          break
        }

      }

    }

    # check if traj_speed_ renaming happened and put back variables as they should be
    if(exists("traj_speed_", dat)){
      data2a$traj_speedSM = data2a$traj_speed # put back RMS as what it should be names
      dat$traj_speed = data2a$traj_speed_  # name previously kept traj_speed_ back to traj_speed real
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

  # then here we decide on smooth, unsmoothed or both by also using override....

  # if we are labelling fixes, add a column to capture this
  #if(annotate){

  if(!hybrid){
    data_dp$speed_rm <- 0
  } else{
    data_dp$speed_rm_sm <- 0
    data_dp$speed_rm_raw <- 0
    data_dp$speed_rm <- 0
  }
  #}

  tid = unique(data_dp$TagID)
  #q = 2
  for(q in 1:length(tid)){
    message(strrep("-", getOption("width")))
    if(verbose){message("TagID: ", tid[q])}
    #message(strrep("-", getOption("width")))

    gs = unique(data_dp[data_dp$TagID %in% tid[q],]$gapsec)
    #g = 1
    if(detailed_verbose){message("---- G A P   S E C T I O N S:  n = ", length(gs), " ----")}
    for(g in 1:length(gs)){
      if(detailed_verbose){message("- S E C T I O N   ", g, ", TagID: ", tid[q])}
      subdat = data_dp[data_dp$TagID %in% tid[q] & data_dp$gapsec %in% gs[g],]

      #### here....
      if(hybrid){
        # do both
        if(detailed_verbose){
          message(strrep("*", getOption("width")/2))
          message("'Smoothed' traj_speed")
          message(strrep("*", getOption("width")/2))
        }
        override = 0
        subdat0 = Track(.speed_filt(subdat)) # smoothed

        if(detailed_verbose){
          message(strrep("*", getOption("width")/2))
          message("'Raw' traj_speed")
          message(strrep("*", getOption("width")/2))
        }
        override = 1
        subdat00 = .speed_filt(subdat) # raw traj_speed

      } else{
        override = 0
        subdat0 = .speed_filt(subdat) # run with either traj speed RAW or the smoothed

      }

      # remove non-matching rows from original
      rdrop0 = logical(0)
      rdrop = subdat$rn2[which(!subdat$rn2 %in% subdat0$rn2)] # either raw or smoothed depending on hybrid TRUE / FALSE
      if(hybrid){ rdrop0 = subdat$rn2[which(!subdat$rn2 %in% subdat00$rn2)] } # this will be the raw traj under hybrid

      if(length(rdrop) > 0 | length(rdrop0) > 0){

        if(!annotate){
          if(detailed_verbose){message("---- DROPPING FIXES ---")}

          if(hybrid){

            # # # # # # #
            # for printing below:
            #if(detailed_verbose){
              df = data.frame(sm = ifelse(data_dp$rn2 %in% rdrop, 1, data_dp$speed_rm))
              df$unsm = ifelse(data_dp$rn2 %in% rdrop0, 1, data_dp$speed_rm)
              df$both = ifelse(df$unsm & df$sm == 1, 1, 0)
              csum = data.frame(t(colSums(df)))

            #}


            # # # # # # #

            data_dp = data_dp[!data_dp$rn2 %in% rdrop,]
            data_dp = data_dp[!data_dp$rn2 %in% rdrop0,]

          } else{
            data_dp = data_dp[!data_dp$rn2 %in% rdrop,]
          }

        } else{
          if(detailed_verbose){message("LABELLING FIXES")}

          if(hybrid){

            data_dp$speed_rm_sm <- ifelse(data_dp$rn2 %in% rdrop, 1, data_dp$speed_rm_sm)
            data_dp$speed_rm_raw <- ifelse(data_dp$rn2 %in% rdrop0, 1, data_dp$speed_rm_raw)

          } else{
            data_dp$speed_rm <- ifelse(data_dp$rn2 %in% rdrop, 1, data_dp$speed_rm)
          }

        }

      }
    }

    if(hybrid){
      data_dp$speed_rm <- ifelse(data_dp$speed_rm_raw == 1 | data_dp$speed_rm_sm == 1, 1, 0)
    }

    if(verbose){

      if(!annotate){
        dd = nrow(data_dp[data_dp$TagID %in% tid[q],] )
        orig_n = vv[vv$Var1 %in% tid[q],]$Freq

        if(dd < orig_n){
          if(orig_n - dd == 1){ pnm = "fix" } else{ pnm = "fixes"}
          message("TagID: ", tid[q], ": Dropped: ", orig_n - dd, " ", pnm, " due to speed filter")

          if(hybrid){
            if(verbose){message("Under hybrid raw traj_speed + 'smoothed' traj_speed:")}
            if(verbose){message("- Fixes flagged: raw, n = ", csum$unsm, ", 'smoothed', n = ", csum$sm, " and both, n = ", csum$both)}
          }
        } else{
          message("TagID: ", tid[q], ": No fixes dropped due to speed filter")
        }
      }

    }

  } ####


  # ------------------------------------------------------ #
  data_out <- Track(tibble(data_dp))

  if(!annotate){
    if(!hybrid){
      data_out <- data_out %>% dplyr::select(-rn, -rn2, -speed_rm)
    } else{
      data_out <- data_out %>% dplyr::select(-rn, -rn2, -speed_rm, -speed_rm_sm, -speed_rm_raw)
    }

  } else{
    data_out <- data_out %>% dplyr::select(-rn, -rn2)
  }

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

  if(detailed_verbose){
    message(strrep("*", getOption("width")))
  }


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


