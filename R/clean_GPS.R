#' Cleaning the tracking data
#'
#' \code{clean_GPS} is provided to allow some basic cleaning of the raw tracking data to be
#' carried out, i.e. after having read in the raw data using
#' \code{\link{read_track_UvA}} and/or \code{\link{read_track_MB}}. \code{clean_GPS} is used
#' to take out less certain positional fixes, filter out fixes associated with unrealistic trajectory speeds, and further
#' define date-time sections (strings of points) where tags were deemed have valid recording in relation to the programmed GPS sampling rate.
#'
#' @details
#' A single function \code{clean_GPS} is provided that cycles through ten main steps to either annotate
#' potential 'bad' data rows, or filter such rows, thus 'cleaning' the GPS data.
#'
#' There many reasons why (here focusing on GPS) animal-borne telemetry locations may have error, such as
#' internal firmware settings of tags e.g. connection parameters with satellites, sampling rate
#' of tags, number of satellites, satellites position in the sky, along with external factors
#' such as temperature, humidity etc. Here, we assume the user is mainly interested in 2D space,
#' so we retain all vertical information in columns \code{altitude} and \code{altitude_agl}. Further filters for such
#' z-dimension columns may be needed by the user.
#'
#' Instead here, we carry out basic cleaning to annotate or filter fixes that may be identified less reliable.
#' The steps within \code{clean_GPS} are often very simple trivial data manipulations, provided here as a convenience tool.
#' The user may wish to break out of the \code{clean_GPS} workflow entirely. Annotation is also at the heart of \code{clean_GPS}
#' as it is expected that other functions from neighbouring R packages may come in handy to assess alongside this process.
#'
#' Within \code{clean_GPS}, there are multiple steps identified. These mainly surround issues of potential measurement error, regarding:
#' 1. Missing location information - any missing latitude/longitude information (NAs) are first identified and flagged/filtered;
#' 2. Label/drop rows where all columns in the data are NA.
#' 3. Minimum number of GPS fixes per animal.
#' 4. A value for the minimum number of satellites can be provided below which data are flagged/filtered;
#' 5. A Movebank-specific condition based on rows flagged in the database as 'flt_switch' problem rows can be flagged/dropped;
#' 6. Potential measurement error through satellite position, via the GPS 'hdop' and 'pdop' values if present in the data, with a threshold value above which data are flagged/filtered;
#' 7. Gap section labeling using the \code{gap_section} function (data annotation only), flagging user-defined gaps in the data and in turn:
#' 8. Identification of or removal of 'orphaned' GPS fixes that have only one GPS fix per gap section (potentially problematic for further analyses);
#' 9. Duplicate date time stamps - a rather obvious step but reserved for later in the process here, as if duplicate rows are found and the annotate argument is TRUE, the data are returned early before any speed or tuning filters that requires such non-duplication of DateTimes;
#' 10. Trajectory (ground) speed filter, that calls both \code{traj_speed} to calculate speeds within identified gap sections, and \code{speed_filt} to carry out labeling/filtering of points beyond unrealistic perceived animal travel speeds; and
#' 11. A turning angle filter, using function \code{turn_filt} to annotate/filter fixes more ore less than a modulus(radians) turning angle between successive fixes.
#'
#' The turning filters also depends on \code{adehabitatLT::as.ltraj} for efficiently computing turning angles and
#' should be used with a high degree of caution given GPS location of stopped animal movement reflects GPS noise.
#'
#' Ground speeds will of course vary by species - so it may be best to view
#' the distribution, and the function \code{rake_outlie} can be useful to identify outliers through simple numeric statistics.
#' Similarly for all perceived biases in the data, viewing of the distribution of the variables that could be driving
#' such variation may be useful to view using \code{rake_outlie}, such as the pdop and hdop thresholds, if they are deemed important.
#'
#' Ultimately these filtering and annotating steps are a little subjective, and depends on the purpose of the analysis.
#' It is not the intention of \code{MoveRakeR} however to re-invent existing methods, but add to the tools available that we can assess
#' biases, and so the user is urged to consider other packages such as \code{trip}, \code{traise}, \code{move/move2} and \code{AMT}.
#'
#' The \code{clean_GPS} workflow in \code{MoveRakeR} is not the first of its kind to have dealt with such similar issues.
#' There are a number of packages that have functions that may be of use to consider as annotation of the data alongside
#' \code{clean_GPS}, such as function \code{trip::sda} and \code{trip::speedfilter} that have methods for speed/turning angle point assessment;
#' the function in s \code{MoveRakeR::clean_GPS} differ for trajectory speed in using a while loop to sequentially assess potentially
#' erroneous fixes that may be above the maximum speed for the species, as strings of erroneous points can occur, thus being displaced
#' from where they likely should have been; hence the  \code{MoveRakeR::speed_filt} by design also considers a displacement effect indirectly.
#'
#' Trajectory speed filters naturally have drawbacks as they are tied intrinsically to the sampling rate of the tag. Information
#' on instantaneous speed may be preferable, although not provided in \code{clean_GPS} as a filtering/annotating option.
#' Coarser GPS sampling rates may make it harder to discern potentially erroneous fixes given a wider time elapsed over what the animal could have been doing and
#' at very fine spatio-temporal scales, i.e. with fast sampling less than 60 seconds, a small change in GPS position has a greater influence on trajectory speed
#' than for coarser sampling schedules. This could be resolved by increasing the speed filter threshold value for particular sampling rates
#' but it may be better to investigate the error potential in traj_speed calculations at these finer scale
#' and the potential sensitivity of the data to such flat decisions. This relationship can be examined further
#' using the \code{tspeed_jit} function.
#'
#' @param data Input data object, with required columns: TagID, DateTime, longitude, latitude.
#' @param min_fix Minimum number of fixes to identify as valid or retain per animal. The default is 0,
#' i.e. all fixes are retained. This parameter is used in conjunction with min_fix_when to determine where in the process this is applied.
#' @param min_fix_when Where to apply the minimum number of fixes per animal (min_fix) criterion. Two options are
#' available: "after_NA" carries out this assessment after initial NA rows and NA lat longs have been removed, and "after_all"
#' makes the calculation after all other filters have been applied.
#' @param NA_latlong logical condition (default TRUE), whether to annotate or remove NA latitude, longitude records,
#' although check if this is compatible with further analyses downstream.
#' @param sat_check Logical whether to filter data for satellite count, defaults to TRUE, and if TRUE,
#' the data needs a column called matching that supplied under 'sat_col'.
#' @param drop_sats Numeric integer value for the minimum number of GPS satellites to drop from the database.
#' Defaults to 3, meaning only 4 and above are flagged as suitable, or retained in the data.
#' @param sat_col The column to search for satellite counts, defaults to "satellites_used".
#' @param flt_switch logical argument, whether to annotate/exclude
#' MoveBank data erroneous flt_switch values. Warning though this can remove fixes erroneously if not careful
#' so it is suggested if not sure to look at Movebank directly and do this separately. This is set to
#' FALSE by default i.e. no flt_switch filtering done.
#' @param pdop Default is numeric 0 indicating do nothing, but if greater than zero, the function will
#' look for a column named pdop, gps.pdop (or gps_pdop) or PDOP and annotate or keep only data less than that value. Note
#' if NAs are present these are retained in the outputs.
#' @param hdop Default is numeric 0 indicating do nothing, but if greater than zero, the function will
#' look for a column named hdop, gps.hdop (or gps_hdop) or HDOP and annotate or keep only data less than that value. Note
#' if NAs are present these are retained in the outputs.
#' @param hasgaps A logical vector whether the data being fed in already has gap sections defined from another process.
#' If FALSE, \code{clean_GPS} will assign its own gap sections based on the values in GAP and tol.
#' @param GAP Numeric integer in seconds. The function can label sections of assumed continuous GPS recording
#' for the individual animal, using the \code{\link{gap_section}} function.
#' The argument \code{GAP} here is the cut-off i.e. temporal hiatus between two
#' consecutive GPS fixes, which defaults to 28800 seconds (eight hours), although note, the tolerance parameter
#' will also be incorporated (see tol argument).
#' This is a subjective choice for the user, and will depend also on GPS sampling
#' resolution. If a precise GAP is needed with no tolerance, set tol to 0. Units are in seconds.
#' @param tol A tolerance is also applied to the GAP value, useful for example to give lee-way around
#' the gap criteria relating to imprecision of sampling rate in GPS deviating from the initial rate specified.
#' By default this is set as 0.2, i.e. to give a true value used in delineation as: GAP +/- (GAP*0.2) s
#' @param drop_single_gap logical defaults to FALSE, but user can specify TRUE to annotate/retain single gap sections
#' that by default get removed from the speed filter as isolated fixes that are at too coarser rate.
#' as determined by the \code{GAP} argument. This replaces the previous 'drop' argument in MoveRakeR <1.0.2.
#' @param dup_DT logical defaulting to TRUE to flag/remove duplicate DateTimes, but note that this will flag/remove
#' any duplicated rows if you are working with trips that 'come-and-go' from a central place.
#' See function: \code{add_cag_trips}. However also note that if you select to flag duplicate DateTimes and not
#' remove them i.e. annotate = TRUE, the function will stop because the speed filter requires them to be unique.
#' Hence this check is the last one made before the speed filter, so other annotations can be flagged.
#' @param speed_filt logical defaulting to TRUE as to whether to use the speed filter in the cleaning steps.
#' Due to potential differences in how users may want to apply this cleaning process, they may want
#' to do their own cleaning, such as using instantaneous speed instead. In which case, \code{clean_GPS}
#' can be used to ONLY do other parts of the clean_GPS workflow. Further cleaning such as
#' using accuracy of positions, altitude may be advisable and are not coded in as options here.
#' @param sp_thres Numeric value for the minimum threshold (m/s) to be used in the speed filter, which requires
#' inspection and will vary by species. This will depend on the sampling resolution
#' of your data. The function then uses a while loop to remove all points not
#' meeting the threshold level until all offending fixes are removed.
#' @param turnFilt An optional filter, default set to NULL (no filter applied) to annotate or filter greater or lesser than a certain turning angle of the animal,
#' but beware for GPS fixes for a stationary animal, turning angles represent GPS error that can be erratic, so this may
#' need further behavioural consideration. If supplied, turn_filt should be a single value with a maximum of pi, that
#' represents the modulus of the radian turning angle; note also that if turn_filt_dir = "less_than" and turn_filt = pi, then
#' all fixes would be removed; A value of  turn_filt = pi/4 and turn_filt_dir = "less_than" would retain all fixes less_than turn_filt.
#' @param turnFilt_dir As with turn_filt, a direction indicating greater or lesser than the turn_filt value to annotate/retain values.
#' @param annotate Logical defaulting to FALSE, in which case data will be \strong{filtered}, however if TRUE, data
#' will be \code{annotated} instead retaining all rows, and different columns will be added for each of the \code{clean_GPS}
#' manipulation steps representing rows flagged under each condition (see 'Value' section).
#' @param attrib Logical set to FALSE if detailed attributes should be attached
#' for numbers of fixes removed in various stages from \code{clean_GPS}.
#' These can be accessed afterwards using: attr(data, "data_summary").
#' @param reproject As above, a logical value whether or not to reproject lat-long WGS84
#' coordinates to the new desired projection as specified via, \code{p4s}.
#' @param p4s The coordinate reference system projection, should it be needed, defaults to NW Europe epsg:3035.
#' @param verbose Logical argument if run-time messages are wanted.
#' @param detailed_verbose Logical argument if the very detailed process of the speed filtering
#' part of the function is required. Defaults to FALSE.
#'
#' @seealso [MoveRakeR::rake], @seealso [MoveRakeR::gap_section], [MoveRakeR::sub_samp], [MoveRakeR::traj_speed], [MoveRakeR::speed_filt], [MoveRakeR::turn_filt]
#'
#' @return The function returns a tibble format with a \code{Track} class.
#' Resultant output depends on the argument \code{annotate}; if TRUE, then data are filtered for
#' the required processing steps selected in \code{clean_GPS}, with additional columns in the objects if not already included:
#' including: \strong{dt}, time difference (secs) between consecutive GPS fixes,
#' \strong{gap}, a binary (1,0) for fixes where a gap was identified and \strong{gapsec}
#' as a numeric integer of sequentially-labelled gapsection strings of telemetry fixes. If the \code{annotate} argument is
#' TRUE, then various columns can be added flagging as a 1 or 0 (some with NA conditions) whether a fix
#' was labelled as 'incorrect' through the thresholds supplied. These columns are named accordingly for conditions:
#' - tid_rm (minimum no. animals)
#' - NA_latlong_rm (lat/long point NA)
#' - all_NA_rm (NA rows)
#' - sat_NA (NA of rows without satellites)
#' - sat_rm (number of satellites)
#' - flt_rm (flt_switch)
#' - gps_pdop_rm (pdop)
#' - gps_hdop_rm (hdop)
#' - singlegap_rm ('orphaned' gapsections)
#' - dup_rm (duplicate DateTimes)
#' - speed_rm (speed filter)
#' - angle_rm (angle filter)
#' - combined_rm, A combined remove column using conditions across all annotation rows
#'
#' @examples
#'
#' indata <- yourdata # data.frame with a minimum of columns named TagID, DateTime, longitude, latitude
#' data <- Track(indata) # optional to convert to in-house class (most functions will still run on non track data but generic S3 methods will not be available for plot and summary)
#' data <- data %>% clean_GPS() # accepting the defaults of the clean_GPS function
#'
#' # Various examples of uses:
#'
#' # as an annotation tool, in detail:
#' data_an <- clean_GPS(data,
#'          min_fix = 5, min_fix_when = "after_NA",       # keep birds with 5 fixes or more, after NA annotation applied
#'          NA_latlong = TRUE,                            # drop NA lat-longs
#'          sat_check = TRUE, drop_sats=3,                # drop fixes with  3 satellites
#'          flt_switch = FALSE,                           # ignored here
#'          pdop = 6, hdop = 6,                           # drop any rows above a value of 6
#'          hasgaps = FALSE, GAP = 28800, tol = 0,        # no gaps previously defined, use an 8 h gap + zero tolerance
#'          drop = FALSE,                                 # keep all gap sections identified with 1 fix (but note these are dropped anyway in speed filter if we are not annotating!)
#'          dup_DT = TRUE,                                # remove duplicate DateTimes
#'          speedFilt = TRUE, sp_thres = 50,              # speed filter activated with a maximum of 50 m/s
#'          turnFilt = NULL, turnFilt_dir = "less_than",  # No angle turning filter used
#'          annotate = TRUE, attrib = TRUE,               # no annotation = dropping rows not meeting conditions, attributes of no. rows per stage retained as an attribute in final data
#'          reproject = FALSE, p4s = 3035,                # no reprojection of lat longs
#'          verbose = TRUE, detailed_verbose = FALSE)     # simple messaging used
#'
#' #' # check out where fixes where dropped:
#' attr(data_an,"data_summary")
#'
#' # If annotating the data, switching annotate to TRUE above, additional rows flagging conditions would be added
#' data_an$combined_rm # the combined row
#'
#' # Outside of the clean_GPS() function
#' # Using the speed filter, outside of clean_GPS() with annotation:
#'
#' # define gapsections
#' data <- gap_section(data,GAP=28800, tol = 0.2)
#'
#' # use speed filter, with the above gaps, annotating the data
#' data_speed_filt <- speed_filt(data, annotate = TRUE, verbose = TRUE, detailed_verbose = TRUE)
#'
#' # setting that part of clean_GPS to FALSE, to annotate with other elements e.g. nsats
#' data_2 <- clean_GPS(data_speed_filt, speed_filt = FALSE, annotate = TRUE, hasgaps = TRUE) # using gaps already present in the data
#'
#' # Clean_GPS also gives a messge if some satellite count data are found to be NA
#' # and these data are retained in the filter of number of satellites.
#'
#' # can also summarise the data, using a further function summary
#' #summary(data)
#'
#' # see also 'raking' the data to check for potential outliers using a very basic quantile (here annotating)
#' data_test <- rake_outlie(data = data, var = "traj.speed", u_max = NULL, q = c(0.01,0.99), method = "quantile",
#'     plot = FALSE, annotate = TRUE, summary = TRUE, verbose = FALSE)
#'
#' data_test$outlier_i_traj.speed_q  # outliers outside individual range
#' data_test$outlier_ia_traj.speed_q # outliers outside individual range + outside the all-animal range
#'
#' # other approaches
#' trip::sda()
#'
#' @seealso [MoveRakeR::read_track_MB], [MoveRakeR::tspeed_jit], [MoveRakeR::read_track_UvA], [MoveRakeR::gap_section], [MoveRakeR::summary]
#'
#' @export
clean_GPS <- function(data,
                      min_fix = 0, min_fix_when = c("after_NA", "after_all"),
                      NA_latlong = TRUE,
                      sat_check = TRUE, drop_sats=3, sat_col = "satellites_used",
                      flt_switch = FALSE,
                      pdop = 0, hdop = 0,
                      hasgaps = FALSE, GAP = 28800, tol = 0.2, drop_single_gap = FALSE,
                      dup_DT = TRUE,
                      speedFilt = TRUE, sp_thres = 50,
                      turnFilt = NULL, turnFilt_dir = c("less_than", "greater_than"),
                      annotate = FALSE, attrib = TRUE,
                      reproject = FALSE, p4s = 3035,
                      verbose = TRUE, detailed_verbose = FALSE, ...){

  data_dp <- tibble(data) %>% ungroup()

  turnFilt_dir = turnFilt_dir[1]
  min_fix_when = min_fix_when[1]

  if(exists("drop", inherits = FALSE)){
    drop_single_gap = drop # legacy code of drop from previous funcion version retained
    message("Newer version of clean_GPS() in MoveRakeR v 1.02+ uses 'drop_single_gap' instead of 'drop'")
    cat(rep(" ",  getOption("width")), "\n", sep = "")
  }

  # --------------------------------------- #
  # assessment of relevant minimum namings in data
  cond <- ifelse(all(c("TagID","DateTime", "longitude", "latitude") %in% names(data_dp)), TRUE, FALSE) # required columns
  if(!cond){
    stop("Missing column names! Check input data for named columns: 'TagID','DateTime','longitude','latitude'")
  }

  # get attributes from previous function runs
  attr_list <- get_attributes(data)
  # --------------------------------------- #

  p4sin = p4s

  ##########
  if(verbose){

    message("*************** Object of ", length(unique(data$TagID)), " animals, ", nrow(data), " rows ***************")

    if(annotate){
      message("----- ANNOTATING FLAGGED FIXES IDENTIFIED AS OUTLIERS -----")
    } else{
      message("----- DROPPING FLAGGED FIXES IDENTIFIED AS OUTLIERS -----")
    }

    if(NA_latlong){
      message("----- Checking for NA lat-long rows -----")
    }

    if(sat_check){

      if(drop_sats < 3){
        message("----- Satellite count being used, but 'drop_sats' is < 3, so likely nothing will happen -----")
      } else{
        message("----- Satellite count being used, threshold = ", drop_sats, " sats -----")
      }

    }

    if(flt_switch){
      message("----- Applying flt_switch process -----")
    }

    if(pdop > 0){
      message("----- Processing fixes with a pdop value > ", pdop, " (searching for relevant column names) -----")
    }
    if(hdop > 0){
      message("----- Processing fixes with a pdop value > ", hdop, " (searching for relevant column names) -----")
    }

    if(hasgaps){
      message("----- Searching for existing gap section labelling -----")
    } else{
      message("----- Defining new gap sections -----")
      message("----> Using a tol value of ", tol, "; GAP = ", GAP, " " ,"secs", "; Real GAP = ",GAP," + ", GAP,"*",tol, " = ", GAP+(GAP*tol), " ", "secs ---")
    }

    if(drop_single_gap){
      message("----- Processing single gapsection fixes -----")
    }

    if(dup_DT){
      message("----- Assessing presence of duplicate DateTime stamps for fixes -----")
    }

    if(speedFilt){
      message("----- Applying trajectory speed process, using an upper threshold of ", sp_thres, " m/s -----")
    }

    if(!is.null(turnFilt)){
      message("----- Applying turning angle process, assessing values ", turnFilt_dir, " a threshold of ", turnFilt, " radians -----")
    }

    # --------------------- #

    if(!annotate){
      if(attrib){
       message("----- addding in an attribute summary of the rows -----")
      }
    }
    if(reproject){
      message("----- Reprojecting GPS fixes using sf crs: ", p4s, " -----")
    }

  }


  # ------------------------------------------------------------------------------------------------------- #

  # the real gao being used (potentially given selections)
  GAP = GAP+(GAP*tol)

  data_dp <- tibble(data)
  #data_dp <- tibble(BTOTTdata::LBBGWalB201416[[1]][[1]])

  # # # # #
  #0. Initial tally of data length (primarily a summary for if annotate == FALSE)
  data_s <- data_dp %>% count(TagID)

  # ------------------------------------------------------------------- #
  # Initial row name labeller for annotating indexers
  data_dp$rn <- 1:nrow(data_dp)

  # ------------------------------------------------------------------- #
  # 1. Remove any NA lat longs

  if(NA_latlong){
    # remove any NA latitude or longitudes

    if(annotate){
      # annotate the rows
      data_dp$NA_latlong_rm <- 0

      if(any(is.na(data_dp$latitude) | is.na(data_dp$longitude))){
        data_dp$NA_latlong_rm <- ifelse( is.na(data_dp$latitude) | is.na(data_dp$longitude),1, data_dp$NA_latlong_rm)
      }
    } else{
      # drop the rows
      data_dp <- data_dp %>% filter(!is.na(latitude) & !is.na(longitude))

      # # # # #
      # count the total remaining data after dropping or even if nothing dropped....
      #data_ll <- data_dp %>%  count(TagID)
      data_s <- data_dp %>% count(TagID) %>% rename(NA_latlong_n = n) %>% left_join(data_s, by = "TagID")

      # # # # #
    }

  }


  # ------------------------------------------------------------------- #
  # 2. Removes rows that are completely NA across all columns
  if(annotate){

    # Add flag column: 1 if all NA, 0 otherwise
    data_dp <- data_dp %>%
      mutate(all_NA_rm = if_else(if_all(everything(), is.na), 1L, 0L))

  } else{
    data_dp = data_dp %>% filter(if_any(everything(), ~ !is.na(.)))

    if(all(is.na(data_dp))){
      stop("No data, NA data.frame")
    }

    # # # # #
    #data_s$all_NA <- data_dp %>% count(TagID) %>% .$n
    data_s <- data_dp %>% count(TagID) %>% rename(all_NA_n = n) %>% left_join(data_s, by = "TagID")

    # # # # #

  }

  # -------------------------------------------------------------- #
  # 3a. Birds with too few fixes (before all other tests)

  if(min_fix_when == "after_NA"){

    if(min_fix > 0){

      data_mins = data_dp %>% group_by(TagID) %>%
        summarise(n = n()) %>%
        mutate(flag_ = ifelse(n < min_fix, 1L, 0L))

      # which animals have less than the min number
      tid_rm <- data_mins[data_mins$flag_ == 1L,]$TagID
      data_dp$tid_rm <- ifelse(data_dp$TagID %in% tid_rm, 1L, 0L)

      if(!annotate){
        data_dp <- data_dp %>% filter(tid_rm == 0L)

        # # # # #
        data_s <- data_dp %>% count(TagID) %>% rename(tid_rm_n_before = n) %>% left_join(data_s, by = "TagID")
        # # # # #

        data_dp <- data_dp %>% select(-tid_rm)
      }

    }

  }


  # ------------------------------------------------------------------- #
  # 4 Remove <x satellites, check for NAs

  # first check if there are numeric data in the "satellites_used" column to work with
  if(sat_check){

    #if(!exists("satellites_used",data)){
    #  stop("For flagging min N satellite count, make sure a column called: 'satellites_used' is present, or if no sat data, then add in blank column with this name OR set sat_check argument to FALSE")
    #}

    if(!any(names(data) %in% sat_col)){
      warning("No satellite data column found matching that supplied under dat_col, skipping this step! ...")
    }

    #data$nsats_used = data$satellites_used
    #data <- data %>% select(-satellites_used)
    #sat_col <- "nsats_used"

    data_dp <- data_dp %>% rename(satellites_used = !!sym(sat_col)) # temporary renaming....

    if(annotate){
      data_dp$sat_rm <- 0
      data_dp$sat_NA <- 0

      # NA sat count
      if(any(is.na(data_dp$satellites_used))){
        data_dp$sat_NA <- ifelse( is.na(data_dp$satellites_used),1, data_dp$sat_NA)
      }

      # sats < min thresh
      if(any(data_dp$satellites_used <= drop_sats)){
        data_dp$sat_rm <- ifelse( data_dp$satellites_used <= drop_sats,1, data_dp$sat_rm)
        #data_dp$sat_rm <- ifelse( is.na(data_dp$sat_rm),NA, data_dp$sat_rm)
      }

    } else{

      data_sc <- data_dp %>% filter(is.na(satellites_used)) %>% count(TagID)

      if(nrow(data_sc) > 0){

        pc = round((sum(data_sc$n) / nrow(data_dp))*100,2)
        if(verbose){message("NA satellite data for ", nrow(data_sc)," TagIDs", " (total of ", sum(data_sc$n), " fixes, ", ifelse(pc < 1, "< 1 %", paste0(pc, "%")), " data): NA data will be retained")}

      }

      # drop satellites less than min threshold
      data_dp <- data_dp %>% filter(satellites_used > drop_sats | is.na(satellites_used))

      # # # # #
      # count up the sat_ns if drop option activated
      #data_s$sat_n <- data_dp %>% count(TagID) %>% .$n
      data_s <- data_dp %>% count(TagID) %>% rename(sat_n = n) %>% left_join(data_s, by = "TagID")

      # # # # #
    }

    data_dp <- data_dp %>% rename(!!sat_col := satellites_used) # put back to what user supplied data as
  }

  # ------------------------------------------------------------------- #
  # 5. Flt_switch - switch != 0 removed if Move data

  if(flt_switch){

    if(!exists("Type", data)){
      warning("No Type column found but 'flt_switch' is TRUE; if you have MoveBank data and want values flt_switch != 0 removed, add in a column called 'Type' with 'MoveBank' for all values: this allows assessment of the 'flt_switch' column (here an underscore not dot)")
    } else{

      data_dp <- data_dp %>% group_by(TagID) %>% arrange(TagID, DateTime) %>%
        mutate(flt_switch = ifelse(is.na(flt_switch), -99, flt_switch)) %>% # if Movebank, check for flt_switch column being NA, then set to -99 for dropping
        mutate(flt_switch = ifelse(flt_switch != 0, -99, flt_switch))   # also if anything isn't zero then treat as a flagged bad data

      if(annotate){
        data_dp <- data_dp %>%
          mutate(flt_rm = ifelse(Type == "MoveBank" & flt_switch == -99, 1, 0))

      } else{
        data_dp <- data_dp %>%
          subset(Type != "MoveBank" | flt_switch != -99) # DROP THE -99s if the column is called Movebank

        # # # # #
        # count tally
        #data_s$flt_n <- data_dp %>% count(TagID) %>% .$n
        data_s <- data_dp %>% count(TagID) %>% rename(flt_n = n) %>% left_join(data_s, by = "TagID")

        # # # # #
      }

    }

  }

  # ------------------------------------------------------------------- #
  # 6a. pdop
  # check for existence of pdop named column or variants
  #data_dp <- Track(data_raw)

  data_dp <- data_dp %>% group_by(TagID) %>% arrange(TagID, DateTime)


  if(pdop > 0){

    if(!exists("pdop", data_dp)){
      if(!exists("gps.pdop", data_dp)){
        if(!exists("PDOP", data_dp)){
          if(!exists("gps_pdop", data_dp)){
            warning("No column found matching pdop, gps.pdop, gps_pdop or PDOP, name to one of those.")
          } else{
            if(annotate){
              data_dp <- data_dp %>%
                mutate(gps_pdop_rm = case_when(is.na(gps_pdop) ~ NA_integer_, gps_pdop >= pdop ~ 1L, gps_pdop < pdop ~ 0L))
            } else{
              data_dp <- data_dp %>% filter(gps_pdop < pdop | is.na(gps_pdop))
            }
          }
        } else{
          if(annotate){
            data_dp <- data_dp %>%
              mutate(gps_pdop_rm = case_when(is.na(PDOP) ~ NA_integer_, PDOP >= pdop ~ 1L, PDOP < pdop ~ 0L))
          } else{
            data_dp <- data_dp %>% filter(PDOP < pdop | is.na(PDOP))
          }
        }
      } else{
        if(annotate){
          data_dp <- data_dp %>%
            mutate(gps_pdop_rm = case_when(is.na(gps.pdop) ~ NA_integer_, gps.pdop >= pdop ~ 1L, gps.pdop < pdop ~ 0L))
        } else{
          data_dp <- data_dp %>% filter(gps.pdop < pdop | is.na(gps.pdop))
        }
      }
    } else{
      if(annotate){
        data_dp <- data_dp %>%
          mutate(gps_pdop_rm = case_when(is.na(pdop) ~ NA_integer_, pdop >= pdop ~ 1L, pdop < pdop ~ 0L))
      } else{
        data_dp <- data_dp %>% filter(pdop < pdop | is.na(pdop))
      }
    }


    # # # # #
    if(!annotate){

      data_s <- data_dp %>% count(TagID) %>% rename(gps_pdop_n = n) %>% left_join(data_s, by = "TagID")
      #data_s$pdop_n <- data_dp %>% count(TagID) %>% .$n
    }
    # # # # #

  }


  # ------------------------------------------------------------------- #
  # 6b. hdop
  # check for existence of pdop named column or variants

  if(hdop > 0){

    if(!exists("hdop", data_dp)){
      if(!exists("gps.hdop", data_dp)){
        if(!exists("HDOP", data_dp)){
          if(!exists("gps_hdop", data_dp)){
            warning("No column found matching hdop, gps.hdop, gps_hdop or HDOP, name to one of those.")
          } else{
            if(annotate){
              data_dp <- data_dp %>%
                mutate(gps_hdop_rm = case_when(is.na(gps_hdop) ~ NA_integer_, gps_hdop >= pdop ~ 1L, gps_hdop < pdop ~ 0L))
            } else{
              data_dp <- data_dp %>% filter(gps_hdop < pdop | is.na(gps_hdop))
            }
          }
        } else{
          if(annotate){
            data_dp <- data_dp %>%
              mutate(gps_hdop_rm = case_when(is.na(HDOP) ~ NA_integer_, HDOP >= pdop ~ 1L, HDOP < pdop ~ 0L))
          } else{
            data_dp <- data_dp %>% filter(HDOP < pdop | is.na(HDOP))
          }
        }
      } else{

        if(annotate){
          data_dp <- data_dp %>%
            mutate(gps_hdop_rm = case_when(is.na(gps.hdop) ~ NA_integer_, gps.hdop >= pdop ~ 1L, gps.hdop < pdop ~ 0L))
        } else{
          data_dp <- data_dp %>% filter(gps.hdop < pdop | is.na(gps.hdop))
        }
      }
    } else{
      if(annotate){
        data_dp <- data_dp %>%
          mutate(gps_hdop_rm = case_when(is.na(hdop) ~ NA_integer_, hdop >= pdop ~ 1L, hdop < pdop ~ 0L))
      } else{
        data_dp <- data_dp %>% filter(hdop < pdop | is.na(hdop))
      }

    }
    # # # # #
    if(!annotate){
      data_s <- data_dp %>% count(TagID) %>% rename(gps_hdop_n = n) %>% left_join(data_s, by = "TagID")
      #data_s$hdop_n <- data_dp %>% count(TagID) %>% .$n
    }
    # # # # #
  }

  # ungroup from above pdop hdop conditions
  data_dp <- data_dp %>% ungroup()

  # ------------------------------------------------------------------- #
  # 7. Assign gapsections (via 'gap' identification)
  # tolerance already added above
  # drop assessment separated below

  if(!hasgaps){
    data_dp <- gap_section(data_dp, GAP=GAP, tol = 0, drop_single_gap=FALSE, verbose = FALSE, attr = FALSE)
  } else{

    # check for necessary data
    if(!exists("gapsec", data_dp) & !exists("gap", data_dp) ){

      if(verbose){message("Although hasgaps = TRUE, no gapsections were found; Using defaults (check this is what you want!)")}
      data_dp <- gap_section(data_dp, GAP=GAP, tol = 0, drop_single_gap=FALSE, verbose = FALSE, attr = FALSE)
    } else{
      if(verbose){message("Using pre-defined gap sections found in the data")}
    }

  }
  #table(data_dp[data_dp$TagID %in% tid[5],]$gapsec)


  #########################################################
  # 8. if drop = TRUE then drop OUT single gapsection fixes as cannot be assessed in the speed filter
  if(drop_single_gap){

    if(annotate){
      data_dp <- data_dp %>%
        group_by(TagID, gapsec) %>%
        mutate(singlegap_rm = if_else(n() == 1, 1L, 0L)) %>%  # 1 if only one row in group
        ungroup()

    } else{
      data_dp = data_dp %>% group_by(TagID,gapsec) %>% mutate(nn = n()) %>%
        filter(nn > 1) %>% # drop any single gapsecs
        dplyr::select(-nn) %>%
        ungroup()

      # # # # #
      data_s <- data_dp %>% count(TagID) %>% rename(singlegap_n = n) %>% left_join(data_s, by = "TagID")
      #data_sr <- data_dp %>%  count(TagID)
      # # # # #
    }

  }

  # ------------------------------------------------------------------- #
  # 9. Filter out duplicated DateTimes
  # not sure if needed but this would drop out any added extra tripnumbering rows!
  if(dup_DT){

    if(annotate){
      data_dp <- data_dp %>%
        group_by(TagID) %>%
        mutate(dup_rm = if_else(duplicated(DateTime) | duplicated(DateTime, fromLast = TRUE), 1L, 0L)) %>%
        ungroup()

      data_dp <- give_attributes(data_dp, attr_list)

#      # --------------------------------------------------------- #
#      #### retain attributes of the choices made
#      # main attribute entry for the function
#      # this only modifies general attributes alongside clean_gps
#      if(is.null(attr(data_dp, "general") )){
#        attr(data_dp, "general") <- "general"
#      }
#      if(is.null(attr(data, "clean_gps") )){
#        attr(data_dp, "clean_gps") <- "clean_gps"
#      }
#
#      # sub_attributes for arguments
#      attr(attr(data_dp, "general"), "GAP") <- get("GAP")
#      attr(attr(attr(data_dp, "general"), "GAP"),"function") <- "clean_gps"
#
#      attr(attr(data_dp, "general"), "unit") <- "secs"
#      attr(attr(attr(data_dp, "general"), "unit"),"function") <- "clean_gps"
#
#      attr(attr(data_dp, "general"), "drop") <- get("drop")
#      attr(attr(attr(data_dp, "general"), "drop"),"function") <- "clean_gps"
#
#      attr(attr(data_dp, "general"), "p4s") <- get("p4s")
#      attr(attr(attr(data_dp, "general"), "p4s"),"function") <- "clean_gps"
#
#      attr(attr(data_dp, "clean_gps"), "drop_sats") <- get("drop_sats")
#      attr(attr(data_dp, "clean_gps"), "sp_thres") <- get("sp_thres")
#      attr(attr(data_dp, "clean_gps"), "sat_check") <- get("sat_check")
#      attr(attr(data_dp, "clean_gps"), "flt_switch") <- get("flt_switch")
#      attr(attr(data_dp, "clean_gps"), "NA_latlong") <- get("NA_latlong")
#      attr(attr(data_dp, "clean_gps"), "dup_DT") <- get("dup_DT")
#      attr(attr(data_dp, "clean_gps"), "annotate") <- get("annotate")
#      attr(attr(data_dp, "clean_gps"), "turn_filt") <- get("turn_filt")
#      attr(attr(data_dp, "clean_gps"), "turn_filt_dir") <- get("turn_filt_dir")
#
#      data_dp <- structure(.Data = data_dp, class = c("Track","grouped_df", "tbl_df","tbl","data.frame"))
#      return(data_dp) # here we stop the function because we need unique DateTimes

    } else{
      data_dp = data_dp %>% filter(!duplicated(DateTime))

      # # # # #
      # count the total remaining data after dropping or even if nothing dropped....
      data_s <- data_dp %>% count(TagID) %>% rename(dup_n = n) %>% left_join(data_s, by = "TagID")
      #data_uq <- data_dp %>%  count(TagID)
      # # # # #
    }

  }

  ###########
  # reproject or if internal X column not found, project data as UTM metres needed for dist cals ideally below in adehabitat
  if(reproject | !exists("X",data_dp)){

    data_sf <- sf::st_as_sf(data_dp, coords = c("longitude", "latitude"), crs = 4326)
    data_sf <- sf::st_transform(data_sf, p4s)
    coords = sf::st_coordinates(data_sf)
    data_dp$X <- coords[,1]
    data_dp$Y <- coords[,2]

    #data_dp <- data_dp[!duplicated(data_dp$DateTime),]

  }

  #########################################################
  # 10. Trajectory speed filter
  if(speedFilt){

    if(verbose){message("----- Custom speed filter -----")}

    # use the existing gaps in the data, which may have been freshly calculated above, or in hasgaps = TRUE argument in clean_GPS() using pre-existing gaps
    data_dp = speed_filt(data_dp, sp_thres = sp_thres, annotate = annotate, verbose = verbose, detailed_verbose = detailed_verbose, hasgaps = TRUE)

    # speed filter row check if not annotating
    if(!annotate){
      # # # # #
      #data_s$spe_n <- data_dp %>% count(TagID) %>% .$n
      data_s <- data_dp %>% count(TagID) %>% rename(speed_n = n) %>% left_join(data_s, by = "TagID")
      # # # # #

      if(sum(data_s$n) - nrow(data_dp) > 0){
        if(verbose){message("~@~@~@~@~@ Dropped ", sum(data_s$n) - nrow(data_dp), " fixes after all choices ~@~@~@~@~@")}
      } else{
        if(verbose){message("~@~@~@~@~@ No fixes dropped :) ~@~@~@~@~@")}
      }

    }

  }

  ##########################################################
  # 11. Turning angle filter
  if(!is.null(turnFilt)){

    data_dp <- turn_filt(data_dp, turnFilt = turnFilt, turnFilt_dir = turnFilt_dir,
                         annotate = annotate, verbose = verbose, hasgaps = TRUE) # using pre-identified gaps from step #6

    data_dp <- data_dp %>% select(-any_of(c("init_row", "cc", "row_id")))

    if(!annotate) {

        # # # # #
        # count tally
        #data_s$ang_rm <- tibble(ltraj_df) %>% count(TagID) %>% .$n
        data_s <- data_dp %>% count(TagID) %>% rename(angle_n = n) %>% left_join(data_s, by = "TagID")
        # # # # #

    }
  }

  # -------------------------------------------------------------- #
  # 3b. Birds with too few fixes (before all other tests)

  if(min_fix_when == "after_all"){

    if(min_fix > 0){

      data_mins = data_dp %>% group_by(TagID) %>%
        summarise(n = n()) %>%
        mutate(flag_ = ifelse(n < min_fix, 1L, 0L))

      # which animals have less than the min number
      tid_rm <- data_mins[data_mins$flag_ == 1L,]$TagID
      data_dp$tid_rm <- ifelse(data_dp$TagID %in% tid_rm, 1L, 0L)

      if(!annotate){
        data_dp <- data_dp %>% filter(tid_rm == 0L)

        # # # # #
        data_s <- data_dp %>% count(TagID) %>% rename(tid_rm_n_before = n) %>% left_join(data_s, by = "TagID")
        # # # # #

        data_dp <- data_dp %>% select(-tid_rm)
      }

    }

  }


  ##########################################################
  # if selecting to annotate, search for annotate labelled columns and flag a combined version of all...
  #
  #1. NA_latlong: NA_latlong_rm
  #2. Duplicated rows all_NA_rm
  #3. N sats: sat_rm
  #4. Fltswitch flt_rm
  #5. a/b gps_pdop_rm, gps_hdop_rm
  #6. (gapsection)
  #7. drop in gapsection style (orphaned gaps): singlegap_rm
  #8. dup_DT (DateTimes) dup_rm
  #9. speed filter: speed_rm
  #10. turn filter: angle_rm

  if(annotate){

    nms <- c("tid_rm", "NA_latlong_rm", "all_NA_rm", "sat_rm", "flt_rm", "gps_pdop_rm", "gps_hdop_rm", "singlegap_rm", "dup_rm", "speed_rm", "angle_rm")

    setDT(data_dp)
    #cols <- which(names(data_dp) %in% nms)
    cols <- intersect(nms, names(data_dp))

    if (length(cols) == 0L) {
      data_dp[, combined_rm := NA_integer_]
    } else {
      # Convert selected columns to a matrix
      mat <- as.matrix(data_dp[, ..cols])

      # Vectorized
      data_dp[, combined_rm := fifelse(rowSums(mat == 1, na.rm = TRUE) > 0, 1L,
                                  fifelse(rowSums(mat == 0, na.rm = TRUE) > 0, 0L, NA_integer_))]
    }


  }


  ##########################################################
  if(nrow(data_dp) > 0){
    data_dp <- data_dp %>% group_by(TagID) %>%
      mutate(dt = as.vector(difftime(lead(DateTime), DateTime, units = 'secs'))) # I guess put this back as it may have been?

  }

  if(exists("rn", data_dp)){ data_dp <- data_dp %>% dplyr::select(-c(rn)) }
  if(exists("rn2", data_dp)){ data_dp <- data_dp %>% dplyr::select(-c(rn2)) }

  if(!is.null(data_dp)){

    if(!annotate){
      if(attrib){
        attributes(data_dp)$data_summary <- data_s # add in the summary of the rows left after the filtering
      }
    }

  }

  # --------------------------------------------------------- #
  # assign attributes that may have been present at the start
  data_dp <- give_attributes(data_dp, attr_list)

  # --------------------------------------------------------- #
  #### retain attributes of the choices made
  # main attribute entry for the function
  # this only modifies general attributes alongside clean_gps
  if(is.null(attr(data_dp, "general") )){
    attr(data_dp, "general") <- "general"
  }
  if(is.null(attr(data, "clean_gps") )){
    attr(data_dp, "clean_gps") <- "clean_gps"
  }

  # sub_attributes for arguments
  attr(attr(data_dp, "general"), "GAP") <- get("GAP")
  attr(attr(attr(data_dp, "general"), "GAP"),"function") <- "clean_gps"

  attr(attr(data_dp, "general"), "unit") <- "secs"
  attr(attr(attr(data_dp, "general"), "unit"),"function") <- "clean_gps"

  attr(attr(data_dp, "general"), "drop_single_gap") <- get("drop_single_gap")
  attr(attr(attr(data_dp, "general"), "drop_single_gap"),"function") <- "clean_gps"

  attr(attr(data_dp, "general"), "p4s") <- get("p4s")
  attr(attr(attr(data_dp, "general"), "p4s"),"function") <- "clean_gps"

  attr(attr(data_dp, "clean_gps"), "drop_sats") <- get("drop_sats")
  attr(attr(data_dp, "clean_gps"), "sp_thres") <- get("sp_thres")
  attr(attr(data_dp, "clean_gps"), "sat_check") <- get("sat_check")
  attr(attr(data_dp, "clean_gps"), "flt_switch") <- get("flt_switch")
  attr(attr(data_dp, "clean_gps"), "min_fix") <- get("min_fix")
  attr(attr(data_dp, "clean_gps"), "min_fix_when") <- get("min_fix_when")
  attr(attr(data_dp, "clean_gps"), "NA_latlong") <- get("NA_latlong")
  attr(attr(data_dp, "clean_gps"), "dup_DT") <- get("dup_DT")
  attr(attr(data_dp, "clean_gps"), "annotate") <- get("annotate")
  attr(attr(data_dp, "clean_gps"), "turn_filt") <- get("turn_filt")
  attr(attr(data_dp, "clean_gps"), "turn_filt_dir") <- get("turnFilt_dir")

  data_dp <- structure(.Data = data_dp, class = c("Track","grouped_df", "tbl_df","tbl","data.frame"))
  return(data_dp)

}


#' @rdname clean_GPS
#' @export
get_attributes <- function(data){

  assign_rates_attr <- attr(data, "assign_rates")
  general_attr <- attr(data, "general")
  clean_GPS_attr <- attr(data, "clean_gps")
  define_trips_attr <- attr(data, "define_trips")
  sub_samp_attr <- attr(data, "track_subsamp")
  rake_one_attr <- attr(data, "rake_outlie")
  rake_one_attr <- attr(data, "rake_outlie")
  speed_filt_attr <- attr(data, "speed_filt")
  turn_filt_attr <- attr(data, "turn_filt")
  trip_stats_attr <- attr(data, "trip_stats")

  attr_list <- list()
  attr_list[[1]] <- if(!is.null(assign_rates_attr)){assign_rates_attr } else{NA}
  attr_list[[2]] <- if(!is.null(general_attr)){general_attr}else{NA}
  attr_list[[3]] <- if(!is.null(clean_GPS_attr)){clean_GPS_attr}else{NA}
  attr_list[[4]] <- if(!is.null(define_trips_attr)){define_trips_attr}else{NA}
  attr_list[[5]] <- if(!is.null(sub_samp_attr)){sub_samp_attr}else{NA}
  attr_list[[6]] <- if(!is.null(rake_one_attr)){rake_one_attr}else{NA}
  attr_list[[7]] <- if(!is.null(speed_filt_attr)){speed_filt_attr}else{NA}
  attr_list[[8]] <- if(!is.null(turn_filt_attr)){turn_filt_attr}else{NA}
  attr_list[[9]] <- if(!is.null(trip_stats_attr)){trip_stats_attr}else{NA}

  names(attr_list) <- c("assign_rates_attr", "general_attr", "clean_GPS_attr",
                        "define_trips_attr", "sub_samp_attr", "rake_outlie_attr",
                        "speed_filt_attr", "turn_filt_attr",
                        "trip_stats_attr")

  return(attr_list)

}

#' @rdname clean_GPS
#' @export
give_attributes <- function(data, attr_list){

  # looks for attr_list from get_attributes()
  # add them to data if not NA

  if(!is.na(attr_list[[1]])){  attr(data, "assign_rates") <- attr_list[[1]]}
  if(!is.na(attr_list[[2]])){  attr(data, "general") <- attr_list[[2]]}
  if(!is.na(attr_list[[3]])){  attr(data, "clean_gps") <- attr_list[[3]]}
  if(!is.na(attr_list[[4]])){  attr(data, "define_trips") <- attr_list[[4]]}
  if(!is.na(attr_list[[5]])){  attr(data, "track_subsamp") <- attr_list[[5]]}
  if(!is.na(attr_list[[6]])){  attr(data, "rake_outlie") <- attr_list[[6]]}
  if(!is.na(attr_list[[7]])){  attr(data, "speed_filt") <- attr_list[[7]]}
  if(!is.na(attr_list[[8]])){  attr(data, "turn_filt") <- attr_list[[7]]}

  return(data)

}

























