#' Cleaning the tracking data
#'
#' \code{clean_track} is provided to allow some basic cleaning of the raw tracking data to be
#' carried out, i.e. after having read in the raw data using
#' \code{\link{read_track_UvA()}} and/or \code{\link{read_track_MB()}}. \code{clean_track()} is used
#' to address a variety of issues with tracking data such as (1) sampling issues like including data duplicates,
#' missing data, errant spatio-temporal data spans, numbers of fixes per animal, and gaps between valid strings
#' of points; as well as (2) further issues relating to measurement error and unrealistic movement trajectories.
#'
#' @details
#'
#' \strong{Introduction}
#'
#' A wrapper function \code{clean_track()} is provided that cycles through several main steps to either annotate
#' potential 'bad' data rows, or filter such rows, thus 'cleaning' the GPS data. As an initial upfront disclaimer,
#' \code{clean_track()} calls many other \code{MoveRakeR} functions and each of these can be also used in
#' their own right outside of \code{clean_track()}. Therefore, the \code{clean_track()} wrapper may be more useful once you are sure the process
#' of annotating or filtering data is "locked in" and you have done all the necessary checks. \code{clean_track()}
#' may therefore be more useful as a workflow-tidier, or if you are just certain of how to clean the data then that's totally fine.
#' As always, this is a gradient from fully open code (i.e. outside of \code{MoveRakeR}) to using
#' the \code{MoveRakeR}-family of functions through to then using the fully-wrapped up process of \code{clean_track()}.
#'
#' There many reasons why (here focusing on GPS) animal-borne telemetry locations may have error, such as
#' internal firmware settings of tags e.g. connection parameters with satellites, sampling rate
#' of tags, number of satellites, satellites position in the sky, along with external factors
#' such as temperature, humidity etc. Here, we assume the user is mainly interested in 2D space,
#' so we retain all vertical information in columns \code{altitude} and \code{altitude_agl}. Further
#' filters for such z-dimension columns may be needed by the user.
#'
#' Here, we carry out basic cleaning to annotate or filter fixes that may be identified less reliable.
#' The steps within \code{clean_track} are often more simple trivial data manipulations, that can be accomplished in your own way
#' However, many of these steps can quickly get more complicated than originally intended. \code{MoveRakeR} therefore
#' is there to help if that is the case. Annotation is also at the heart of \code{clean_track}
#' as it is expected that other functions from neighbouring R packages may come in handy to assess
#' alongside this process.
#'
#' \strong{The general process}
#'
#' Within \code{clean_track}, there are four main 'groups' of cleaning defined here:
#'
#' 1. Duplicate and missing data Location and spatio-temporal spans of data;
#' 2. Sample size and minimum numbers of animals and fixes per animal, and data gaps between points;
#' 3. Error covariates, such a number of satellites, dilution of precision or other accuracy measures;
#' 4. Further location filters based on speed/ and/or turning angle (building on 2. above);
#'
#' Under each one of these banners, \code{clean_track} handles (in certain order):
#' Location and timestamp issues
#' - Missing location - any missing latitude/longitude information (NAs) are first identified and flagged/filtered;
#' - Missing data - Label/drop rows where all columns in the data are NA.
#' - Duplicate date issues - using the function \code{duplicate_track()} duplicate rows are either annotated or resolved.
#' - Data spans are assessed using the \code{tag_spans()} function, which can assess errant future dates, specific dates
#' supplied by the user, and a priori spatial extent expectations.
#'
#' Sample sizes and data gaps
#' - Minimum number of GPS fixes per animal;
#' - Gap section labeling using the \code{gap_section} function (data annotation only), flagging user-defined gaps in the data and in turn:
#' - Identification of or removal of 'orphaned' GPS fixes that have only one GPS fix per gap section (potentially problematic for further analyses);
#'
#' Error based on covariates
#' - A value for the minimum number of satellites can be provided below which data are flagged/filtered;
#' - Satellite position, via the GPS 'hdop' and 'pdop' values if present in the data, with a threshold value above which data are flagged/filtered;
#' - A Movebank-specific condition based on rows flagged in the database as 'flt_switch' problem rows can be flagged/dropped;
#' - Bespoke accuracy filters via available columns in the data, via the \code{filt_err()} function.
#'
#' Speed and turning angle filters
#' - Trajectory (ground) speed filter, that calls both \code{traj_speed()} to calculate speeds within identified gap sections, and \code{speed_filt()} to
#' carry out labeling/filtering of points beyond unrealistic perceived animal travel speeds. Root-mean square or
#' other smoothing operations can be fed through to \code{traj_speed()} via \code{clean_track()};
#' - A turning angle filter, using function \code{turn_filt()} to annotate/filter fixes more ore less than a modulus(radians) turning angle between successive fixes.
#' The turning filter also depends on \code{adehabitatLT::as.ltraj} for efficiently computing turning angles and
#' should be used with a high degree of caution given GPS location of stopped animal movement reflects GPS noise.
#'
#'
#' \strong{Further details}
#'
#' As a bove, the defaults of \code{clean_track()} should ideally not be relied upon 'out-of-the-box'. Selection of parameters should be carefully
#' considered and scutinised (i.e. raking) through the use of the \code{ShinyRakeR} app.
#'
#' Contexts will likely vary greatly and will be highly species- specific.
#' Ground speeds will of course vary considerably by species - so it may be best to view
#' the distribution, with the help of function \code{rake_outlie}, to identify outliers through simple numeric statistics.
#' Similarly for all perceived biases in the data, viewing of the distribution of the variables that could be driving
#' such variation may be useful to view using \code{rake_outlie}, such as the pdop and hdop thresholds, if they are deemed important.
#'
#' The raw trajectory speed filter (default) is sensitive to single 'bad' spikes. However, the speed filtering process can be
#' expanded to consider instances where several fixes in a row may be slightly 'wrong' but falling below the extreme outlying cases
#' identified via raw trajectory speed. It may therefore be necceassary to ask a slightly different question:
#' "is there sustained implausible movement in the' neighbourhood for this fix?".
#' Further arguments can therefore be supplied to \code{clean_track} to address this, by using
#' a root mean square or smoothing of trajectory speed, which alongside the raw trajectory speed amounts to a "hybrid" approach
#' to isolate individual bad spikes as well as local context-dependent neighbourhood error. See also \code{trip::speedfilter}
#' for a similar approach.
#'
#' Trajectory speed filters naturally have drawbacks as they are tied intrinsically to the sampling rate of the tag. Information
#' on instantaneous speed may be preferable, although not provided in \code{clean_track} as a filtering/annotating option.
#' Coarser GPS sampling rates may make it harder to discern potentially erroneous fixes given a wider time elapsed over what the animal could have been doing and
#' at very fine spatio-temporal scales, i.e. with fast sampling less than 60 seconds, a small change in GPS position has a greater influence on trajectory speed
#' than for coarser sampling schedules. It may therefore be needed to investigate the error potential in traj_speed calculations at these finer scale
#' and the potential sensitivity of the data to single value decisions. This relationship can be examined further
#' using the \code{tspeed_jit} function. Note this may be resolved partly by considering a root mean square/smoothing
#' operation within the speed filter (see examples), increasing robustness over different sampling regimes.
#'
#' The \code{clean_track} workflow in \code{MoveRakeR} is not the first of its kind to have dealt with such similar issues.
#' There are a number of packages that have functions that may be of use to consider as annotation of the data alongside
#' \code{clean_track}, such as function \code{trip::sda} and \code{trip::speedfilter} that have methods for speed/turning angle point assessment;
#' the function in s \code{MoveRakeR::clean_track} differ for trajectory speed in using a while loop to sequentially assess potentially
#' erroneous fixes that may be above the maximum speed for the species, as strings of erroneous points can occur, thus being displaced
#' from where they likely should have been; hence the  \code{MoveRakeR::speed_filt} by design also considers a displacement effect indirectly.
#'
#' Ultimately these filtering and annotating steps are a little subjective, and depends on the purpose of the analysis.
#' It is not the intention of \code{MoveRakeR} however to re-invent all existing methods, but instead add to the tools available that we can assess
#' biases, and so the user is also directed to other packages such as \code{trip}, \code{traipse}, \code{move/move2} and \code{AMT}.
#'
#' \emph{NOTE as of v 1.1.3.90000}
#' \code{clean_track()} used to be called \code{clean_GPS()} in previous \code{MoveRakeR} releases.
#' The function is the same, this is just a name change, and users can still
#' call \code{clean_GPS()}, but please consider that older name deprecated.
#'
#' \strong{Wider discussion on data cleaning/raking}
#'
#' There are different coding approaches to tackle cleaning of GPS data. It is fully appreciated users may have their own
#' approaches to do this. If these process are done in 'open code', following the \code{ExMove} workflow (Langley et al. 2024)
#' this is useful starting point. The user may then hit repeatable issues we all face, for which the functions in
#' \code{MoveRakeR} seek to simplify and capture to avoid verbose coding. These functions are then
#' brought together in \code{clean_track()} as an overall wrapper. In between these steps, there is also the
#' defined 'raking' of data to explore what happens if you choose option X, or parameter Y, i.e. how much data
#' is dropped, do these exclude certain individuals or time periods etc. Data cleaning can be seen on a continuum of approaches ranging
#' from the fully open-code approaches (that may result in repeated coding of the same sorts of issues), to a fully wrapped up
#' approach of \code{MoveRakeR::clean_track} as a single function for which use 'out-of-the-box') is risky without first delving into the
#' error present in the data. It is also recognised that \code{MoveRakeR} is coded around examples on real GPS data for which we
#' have detected. However, there may be unknown issues we have not yet come across, that a user may switch to open coding of
#' data cleaning. Do feel free to get in touch if there are any steps that could be included.
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
#' @param hdop_check Logical (default FALSE), whether to activate the hdop filter.
#' @param pdop_check Logical (default FALSE), whether to activate the pdop filter.
#' @param hdop_val Default is numeric 7, if hdop_check = TRUE, the function will look for a column named under the 'hdop_col' argument,
#' and keep only data less than that value.  NAs are present these are retained in the outputs.
#' @param pdop_val Default is numeric 7, if pdop_check = TRUE, the function will look for a column named under the 'pdop_col' argument,
#' and keep only data less than that value.  NAs are present these are retained in the outputs.
#' @param hdop_col Character variable (default "gps.hdop") directing the column to be used for the hdop filter.
#' @param pdop_col Character variable (default "gps.pdop") directing the column to be used for the pdop filter.
#' @param flt_switch logical argument, whether to annotate/exclude
#' MoveBank data erroneous flt_switch values. Warning though this can remove fixes erroneously if not careful
#' so it is suggested if not sure to look at Movebank directly and do this separately. This is set to
#' FALSE by default i.e. no flt_switch filtering done.
#' @param custom_filt Logical (default FALSE) whether to use any further custom columns to filter/annotate by.
#' This process if fully flexible for numeric or character/factor variables and uses the \code{file_err()} function
#' hence to use this option, the necessary arguments must be specified for that function. These are the names
#' of the variables to use via \strong{nms}, the lower bound via \strong{vals_l}, the upper bound via
#' \code{vals_u}, and optionally the \strong{inclusive} argument, for the upper and lower bound condition. These arguments
#' are supplied to \code{filt_err} via the dots (...) argument within \code{clean_track}. Note also,
#' if custom_filt is TRUE, checks are first made for use of prior built-in filters via the number of satellite, pdop,
#' hdop and flt_switch checks, and if any duplicate column names are found in the 'nms' argument those having been
#' used already, these are omitted from the custom filter.
#' @param hasgaps A logical vector whether the data being fed in already has gap sections defined from another process.
#' If FALSE, \code{clean_track} will assign its own gap sections based on the values in GAP and tol.
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
#' @param duplicates logical defaulting to TRUE to call \code{duplicate_track()} to address data duplicates
#' Also note that this will flag/remove any duplicated rows if you are working with trips that 'come-and-go'
#' from a central place; see function: \code{add_cag_trips}.
#' @param dup_method Corresponding to \code{duplicate_track()} argument. Global conflict–resolution
#' dup_method used for all duplication steps unless overridden by a further method-specific duplication call (see examples).
#' dup_method is one of \code{"best_quality"}, \code{"first"}, \code{"last"}, \code{"random"}, or step-specific dup_methods
#' where applicable. Defaults to "random" if nothing is provided; further formal arguments for \code{duplicate_track()}
#' can be passed via the '...' of \code{clean_track()}.
#' @param tagspans logical defaulting to TRUE to call \code{tag_spans()} whether to consider a priori reasons why
#' data may be erroneous in time or space; further formal arguments for \code{duplicate_track()}
#' can be passed via the '...' of \code{tag_spans()}.
#' @param measure corresponds to the equivalent argument in \code{tag_spans()} one of c("spatial", "temporal") fro
#' consideration of temporal and/or spatial extent flagging or resolving.
#' @param speed_filt logical defaulting to TRUE as to whether to use the speed filter and call \code{speed_filt()}.
#' \code{clean_track} calls the \code{speed_filt} function to do this, and in turn further arguments can be fed
#' through to \code{speed_filt} from \code{clean_track} via the dots (...) argument; these include the option to
#' use further root mean square or smoothing options such as through the \strong{method} argument of \code{speed_filt}
#' and whether to use both raw trajectory speed and the 'smoothed' version via the \strong{hybrid} argument.
#' @param sp_thres As supplied to \code{speed_filt()}. A numeric value for the minimum threshold (m/s) to be used in the speed filter, which requires
#' inspection and will vary by species. This will depend on the sampling resolution
#' of your data. The function then uses a while loop to remove all points not
#' meeting the threshold level until all offending fixes are removed.
#' @param turnFilt An optional filter, default set to NULL (no filter applied) to annotate or filter using \code{turn_filt()};
#' turning angles greater or lesser than a certain "turn_filt" turning angle provided are flagged/filtered,
#' but beware for GPS fixes for a stationary animal, as turning angles represent GPS error that can be erratic, and so this may
#' need further behavioural consideration. If supplied, turn_filt should be a single value with a maximum of pi, that
#' represents the modulus of the radian turning angle; note also that if turn_filt_dir = "less_than" and turn_filt = pi, then
#' all fixes would be removed; A value of  turn_filt = pi/4 and turn_filt_dir = "less_than" would retain all fixes less_than turn_filt.
#' @param turnFilt_dir As supplied to \code{turn_filt()}, a direction indicating greater or lesser than the turn_filt value to annotate/retain values.
#' @param annotate Logical defaulting to FALSE, in which case data will be \strong{filtered}, however if TRUE, data
#' will be \code{annotated} instead retaining all rows, and different columns will be added for each of the \code{clean_track}
#' manipulation steps representing rows flagged under each condition (see 'Value' section).
#' @param attrib Logical set to FALSE if detailed attributes should be attached
#' for numbers of fixes removed in various stages from \code{clean_track}.
#' These can be accessed afterwards using: attr(data, "data_summary").
#' @param reproject As above, a logical value whether or not to reproject lat-long WGS84
#' coordinates to the new desired projection as specified via, \code{p4s}.
#' @param p4s The coordinate reference system projection, should it be needed, defaults to NW Europe epsg:3035.
#' @param verbose Logical argument if run-time messages are wanted.
#' @param detailed_verbose Logical argument if the very detailed process of the speed filtering
#' part of the function is required. Defaults to FALSE.
#' @param ... Additional arguments that can be passed on to function, which at the moment is limited mainly to
#' \code{speed_filt()}, where additional arguments: "hybrid", "fall_back", "include_centre", "method",
#' "fun", "fun_args" and "points_each_side" can be passed on lexically feeding through to lower functions.
#'
#' @seealso [MoveRakeR::rake], @seealso [MoveRakeR::gap_section], [MoveRakeR::filt_err], [MoveRakeR::traj_speed], [MoveRakeR::traj_speed_sm], [MoveRakeR::speed_filt], [MoveRakeR::turn_filt]
#'
#' @return The function returns a tibble format with a \code{Track} class.
#' Resultant output depends on the argument \code{annotate}; if TRUE, then data are filtered for
#' the required processing steps selected in \code{clean_track}, with additional columns in the objects if not already included:
#' including: \strong{dt}, time difference (secs) between consecutive GPS fixes,
#' \strong{gap}, a binary (1,0) for fixes where a gap was identified and \strong{gapsec}
#' as a numeric integer of sequentially-labelled gapsection strings of telemetry fixes. If the \code{annotate} argument is
#' TRUE, then various columns can be added flagging as a 1 or 0 (some with NA conditions) whether a fix
#' was labelled as 'incorrect' through the thresholds supplied. These columns are named accordingly for conditions:
#' - tid_rm (minimum no. animals);
#' - NA_latlong_rm (lat/long point NA);
#' - all_NA_rm (NA rows);
#' - sat_NA (NA of rows without satellites);
#' - sat_rm (number of satellites);
#' - flt_rm (flt_switch);
#' - gps_pdop_rm (pdop);
#' - gps_hdop_rm (hdop);
#' - singlegap_rm ('isolated' gapsections);
#' - dup_rm (duplicate DateTimes);
#' - speed_rm (speed filter);
#' - angle_rm (angle filter);
#' - if using the 'custom_filt' argument, any additional columns used will be flagged with a suffix "_rm";
#' - if using the duplication method, then a further five columns ar possible relating to specific
#' issues identified from the duplication process: dup_parallel_rm (parallel issue), dup_time_rm (same DateTime, different coordinates),
#' dup_coords_rm (same coordinates, different DateTime), dup_time_coords_rm (both DateTime and coords the same)
#' and all_dup_rm (all row duplicates);
#' - if using tagspans a further six columns are possible: future_date_rm (future date found), temporal_date_rm (dates exceed supplied range),
#' minx_rm (< minx), miny_rm (< miny), maxx_rm (> maxx), maxy_rm (> maxy)
#' and then depending on conditions returned:
#' - 'combined_rm' is the result of the combined flags (1,0,NA) across all "_rm" columns.
#'
#' @examples
#'
#' indata <- yourdata # data.frame with a minimum of columns named TagID, DateTime, longitude, latitude
#' data <- Track(indata) # optional to convert to in-house class (most functions will still run on non track data but generic S3 methods will not be available for plot and summary)
#' data <- data %>% clean_track() # accepting the defaults of the clean_track function
#'
#' ############################### DISCLAIMER ####################################
#' # clean_track() is a wrapper for several other MoveRakeR functions
#' # It is advisable to carry out piecemeal checks of data before using clean_track() but thereafter if
#' # the filtering or annotating process is "locked in" and you are sure of the steps used, then
#' # clean_track() could be useful as shorthand to help clean up workflows
#'
#' # --------------------------------------------------------------------------- #
#' # Example of basic use
#' # --------------------------------------------------------------------------- #
#' # as an annotation tool:
#' data_an <- clean_track(data,
#'  min_fix = 0, min_fix_when = "after_NA",                       # keep birds with 5 fixes or more, after NA annotation applied
#'  NA_latlong = TRUE,                                            # drop NA lat-longs
#'  sat_col = "satellites_used", sat_check = TRUE, drop_sats = 3, # drop fixes with 3 satellites
#'  pdop_check = FALSE, pdop_val = 7, pdop_col = "gps.pdop",      # drop any rows above a value of 7 IF pdop_check TRUE
#'  hdop_check = FALSE, hdop_val = 7, hdop_col = "gps.hdop",      # drop any rows above a value of 7 IF hdop_check TRUE
#'  flt_switch = FALSE,                                           # flt_switch custom argument, ignored here
#'  custom_filt = FALSE,                                          # should any custom filtering be done, no here but nms, vals_l, vals_u would then be needed (see below)
#'  hasgaps = FALSE, GAP = 28800, tol = 0.2,                      # no gaps previously defined, if TRUE use GAP and tol args to redefine
#'  drop_single_gap = FALSE,                                      # keep all gap sections identified with 1 fix (note these are dropped anyway in speed filter if not annotating)
#'  duplicates = TRUE, dup_method = "random",                     # activate duplicate filter, defaulting to a random pick in dup_method
#'  tagspans = TRUE, measure = c("spatial", "temporal"),          # activate the tagspans data checks (default spatial and temporal)
#'  speedFilt = TRUE, sp_thres = 50,                              # activate basic speed filtering using only raw GPS traj speed - see further options for smoothed and RMS consideration
#'  turnFilt = NULL, turnFilt_dir = "less_than",                  # no use of turn filter under default
#'  annotate = TRUE,                                              # default is usually FALSE but TRUE annotates rather than filters
#'  attrib = TRUE, reproject = FALSE, p4s = 3035,                 # attributes and p4s conditions
#'  verbose = TRUE, detailed_verbose = FALSE)                     # verbosity
#'
#' # If you are NOT annotating, you can check where fixes were dropped via:
#' attr(data_an,"data_summary")
#' # otherwise you can assess this using the '_rm' columns in the annotated dataset:
#' data_an$combined_rm # the combined annotation column
#'
#' # --------------------------------------------------------------------------- #
#' # Further use of custom filtering via the dots(...) argument of clean_track()
#' # --------------------------------------------------------------------------- #
#' # The below call activates 'custom_filt' and looks for columns speed_accuracy, flag ("wrong" or "correct"), and satellites_used
#' # but... notices how sat_check is already TRUE and satellites_used is specified there so would be ignored by
#' # the custom filterer. Note also the vals_l and vals_u notation; here anything >= 11 speed_accuracy would be filtered as 'incorrect' (annotate = FALSE = destructive filtering)
#' # If sat_check was FALSE, then anything less than or equal to 3 satellites would be dropped via the vals_l argument.
#' # If the flag column is "wrong", then these would be dropped. vals_l and vals_u are best supplied as lists if multiple
#' # columns are to be used.
#'
#' data$flag <- "0" # IF YOU DO NOT HAVE A FLAG COLUMN (used in this example), otherwise filt_err() will fail.
#'
#' cust_filt = clean_track(data,
#'                 sat_check = TRUE, drop_sats=3, sat_col = "satellites_used",
#'                 duplicates = TRUE,  tagspans = TRUE,
#'                 annotate = FALSE, # <--- i.e. dropping data that are flagged as errant
#'                 pdop_check = TRUE, pdop_val = 9, pdop_col = "gps.pdop",
#'                 hdop_check = TRUE, hdop_val = 9, hdop_col = "gps.hdop",
#'                 custom_filt = TRUE,
#'                 nms = c("speed_accuracy", "flag", "satellites_used"),
#'                 vals_l = list(NULL,NULL,3),
#'                 vals_u = list(11,"wrong",NULL),
#'                 drop_single_gap = TRUE, hasgaps = TRUE, GAP = 15000, tol = 0.2,
#'                 speedFilt = TRUE, sp_thres = 50)
#'
#' # Custom functions can be fed through to any column you want - see filt_err() for more details
#' # Here, a potential custom function use on the 'flt_switch' in the data
#' flag_flt_switch <- function(x) {
#'  flag <- as.integer(!(is.na(x) | x == 0))
#'  return(flag)
#' }
#'
#' cust_filt2 = clean_track(data,
#'                 sat_check = TRUE, drop_sats=3, sat_col = "satellites_used",
#'                 duplicates = TRUE,  tagspans = TRUE,
#'                 annotate = FALSE,
#'                 pdop_check = TRUE, pdop_val = 9, pdop_col = "gps.pdop",
#'                 hdop_check = TRUE, hdop_val = 9, hdop_col = "gps.hdop",
#'                 custom_filt = TRUE,
#'                 nms = c("speed_accuracy", "flag", "satellites_used", "flt_switch"), # <-------- "flt_switch" now added to c() of nms
#'                 vals_l = list(NULL,NULL,4,NULL),                                    # <-------- extra NULL lower condition
#'                 vals_u = list(11,"wrong",NULL,flag_flt_switch),                     # <-------- 'flag_flt_switch' function specified
#'                 drop_single_gap = TRUE, hasgaps = TRUE, GAP = 15000, tol = 0.2,
#'                 speedFilt = TRUE, sp_thres = 50)
#'
#' # --------------------------------------------------------------------------- #
#' # Speed filter further option via the dots(...) argument of clean_track()
#' # --------------------------------------------------------------------------- #
#' # Similar to the use of the 'custom_filt' argument above, the speed filterer in clean_track can also accept
#' # further arguments via '...' that are fed through to speed_filt(). In the example below we annotate the data
#' # and use the speed filterer but also specify that we want to consider a hybrid approach using raw
#' # trajectory speed AND the root mean square "mcconnell" method; this uses two fixes either side back and forward (excluding the current fix)
#' # The RMS/smooth alongside the raw traj speed is useful to also search for potentially erroneous
#' # sustained implausible fixes around the current fix indicating local neighbourhood inconsistency
#' # as well as 'spikes' in the data from the raw trajectory filter.
#'
#' cust_speed = clean_track(data,
#'                 sat_check = TRUE, drop_sats=3, sat_col = "satellites_used",
#'                 duplicates = TRUE,  tagspans = TRUE,
#'                 annotate = FALSE,
#'                 pdop_check = TRUE, pdop_val = 7, pdop_col = "gps.pdop",
#'                 hdop_check = TRUE, hdop_val = 7, hdop_col = "gps.hdop",
#'                 speedFilt = TRUE, sp_thres = 50, method = "mcconnell", hybrid = TRUE) # <------ supplied to speed_filt() via '...'
#'
#' # --------------------------------------------------------------------------- #
#' # Duplicates
#' # --------------------------------------------------------------------------- #
#' # This calls the duplicates_track() function
#' # However the only global argument for this process in clean_track() is
#' # dup_method = c("random", "best_quality", "first", "last", "mean")
#' # But you can make use of duplicates_track() by specifying any argument that function needs by just
#' # specifying them in the clean_track() function which will be fed through as above with other process via the "..."
#'
#' # You can use the single functions anyway but for example specifically using
#' # "best quality" as the dup_method BUT then also adding which columns through further variables:
#'
#' data_dups = clean_track(data, duplicates = TRUE, annotate = TRUE,
#'   dup_method = "best_quality",
#'   quality_vars = c("satellites_used", "accuracy", "pdop", "hdop"),
#'   quality_directions = c(1,1,-1,-1)
#' )
#'
#' # or even more bespoke, using a specific method that is allowed for a particular dup_method
#' # these are passed through the 'dots' of the duplicate_track() essentially double-dotted from clean_track():
#' data_dups = clean_track(data, duplicates = TRUE,
#'   dup_method_same_time = "centroid",
#'   annotate = FALSE,
#'   verbose = TRUE # default
#' )
#' # which should show "Using dup_method: centroid" in the Duplicate data checker if verbose is TRUE
#'
#' # --------------------------------------------------------------------------- #
#' # Tag data spans
#' # --------------------------------------------------------------------------- #
#' # Tag data span checks are computed using the tag_spans() function
#' # This also makes use of a single global argument in clean_track() but as above
#' # clean_track() accepts the necessary further functions within tag_spans():
#'
#' data_tagspans <- clean_track(data,
#'     tagspans = TRUE,
#'     measure = c("spatial", "temporal"), # <- general summaries in tag_spans() cannot be used for clean_track()
#'     xra = c(-10,5), yra = c(25,65),
#'     dates = c("2014-01-01 00:00:01", "2021-12-31 23:59:59"),
#'     annotate = FALSE
#'   )
#'
#' # --------------------------------------------------------------------------- #
#' # Outside of the clean_track() function
#' # --------------------------------------------------------------------------- #
#' # As noted above, clean_track() is a wrapper for several other MoveRakeR functions
#' # It is advisable to carry out piecemeal checks of data before using clean_track()
#' # If the user wants maximal control over the process and in particular the ORDER in
#' # which different cleaning steps are applied then best to do outside of clean_track()
#'
#' # Example, using the speed filter, outside of clean_track() with annotation:
#'
#' # define gapsections
#' data <- gap_section(data,GAP=28800, tol = 0.2)
#'
#' # use speed filter, with the above gaps, annotating the data
#' data_speed_filt <- speed_filt(data, annotate = TRUE, verbose = TRUE, detailed_verbose = TRUE)
#'
#' # setting that part of clean_track to FALSE, to annotate with other elements e.g. nsats
#' data_2 <- clean_track(data_speed_filt, speed_filt = FALSE, annotate = TRUE, hasgaps = TRUE) # using gaps already present in the data
#'
#' # clean_track also gives a message if some satellite count data are found to be NA
#' # and these data are retained in the filter of number of satellites.
#'
#' # See also processes for
#' #duplicate_track(data)
#' #tag_spans(data)
#' #filt_err(data)
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
clean_track <- function(data,
                      min_fix = 0,
                      min_fix_when = c("after_NA", "after_all"),
                      NA_latlong = TRUE,
                      sat_check = TRUE, drop_sats=3, sat_col = "satellites_used",
                      pdop_check = FALSE, pdop_val = 7, pdop_col = "gps.pdop",
                      hdop_check = FALSE, hdop_val = 7, hdop_col = "gps.hdop",
                      flt_switch = FALSE,
                      custom_filt = FALSE,
                      hasgaps = FALSE, GAP = 28800, tol = 0.2, drop_single_gap = FALSE,
                      duplicates = TRUE, # will do a default check of issues but resolving (annotate = FALSE), requires feed in of necessary duplicate_track() args; deafults to random picks
                      dup_method = c("random", "best_quality", "first", "last", "mean"), # for duplicate_track()
                      tagspans = TRUE, # will use default of tag_spans to ONLY check for future dates, unless ... is used feeding to tag_spans
                      measure = c("spatial", "temporal"), # for tag_spans()
                      speedFilt = TRUE, sp_thres = 50, # plus any additional columns for ... fed to speed_filt()
                      turnFilt = NULL, turnFilt_dir = c("less_than", "greater_than"),
                      annotate = FALSE, attrib = TRUE,
                      reproject = FALSE, p4s = 3035,
                      verbose = TRUE, detailed_verbose = FALSE, ...){

  data_dp <- tibble(data) %>% ungroup()

  turnFilt_dir = turnFilt_dir[1]
  min_fix_when = min_fix_when[1]

  if(exists("drop", inherits = FALSE)){
    drop_single_gap = drop # legacy code of drop from previous funcion version retained
    message("Newer version of clean_track() in MoveRakeR v 1.02+ uses 'drop_single_gap' instead of 'drop'")
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
    message(strrep("*", getOption("width")))
    message(centre_line(paste0("Object of ", length(unique(data$TagID)), " animals, ", nrow(data), " rows")))
    message(strrep("*", getOption("width")))

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

    if(pdop_check){
      message("----- Processing fixes with a pdop value > ", pdop_val, " (searching for relevant column names) -----")
    }
    if(hdop_check){
      message("----- Processing fixes with a pdop value > ", hdop_val, " (searching for relevant column names) -----")
    }

    if(custom_filt){
      if(any(flt_switch,pdop_check,hdop_check,sat_check)){
        message("----- Custom error filter also applied (searching for any duplication column names with built-in filters) -----")
      } else{
        message("----- Custom error filter applied -----")
      }
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

    if(duplicates){
      message("----- Assessing presence of duplicate DateTimes and locations -----")
    }

    if(tagspans){
      message("----- Assessing a priori tag_spans for xy and DateTime validity -----")
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

    #message(strrep("*", getOption("width")))


  }

  # ------------------------------------------------------------------------------------------------------- #

  # the real gao being used (potentially given selections)
  GAP = GAP+(GAP*tol)

  # # # # #
  #0. Initial tally of data length (primarily a summary for if annotate == FALSE)
  data_s <- data_dp %>% count(TagID)

  # tracking rows created with a _rm suffix:
  rm_cols_created <- NULL

  # ------------------------------------------------------------------- #
  # Initial row name labeller for annotating indexers
  # ------------------------------------------------------------------- #

  data_dp$rn <- 1:nrow(data_dp)


  # ------------------------------------------------------------------- #
  # 1. Remove any NA lat longs, obviously not correct
  # ------------------------------------------------------------------- #

  if(NA_latlong){
    # remove any NA latitude or longitudes

    if(verbose){
      message(centre_line("NA_latlong checker"))
      }

    if(annotate){
      # annotate the rows
      data_dp$NA_latlong_rm <- 0

      if(any(is.na(data_dp$latitude) | is.na(data_dp$longitude))){
        data_dp$NA_latlong_rm <- ifelse( is.na(data_dp$latitude) | is.na(data_dp$longitude),1, data_dp$NA_latlong_rm)

        rm_cols_created <- c(rm_cols_created, "NA_latlong_rm")

        if(verbose){

          n_found <- sum(data_dp$NA_latlong_rm,na.rm=TRUE)
          pc <- round((n_found / nrow(data_dp))*100, 2)

          if(n_found > 0){
            message("- Annotating ", n_found, " rows with NA lat-longs, (", ifelse(pc < 1, "< 1 %", paste0(pc, "%")), " data)")
          } else{
            message("- No data with NA lat longs (all rows, NA_latlong_rm = 0)")
          }

        }

      } else{
        if(verbose){
          message("- No data with NA lat longs (all rows, NA_latlong_rm = 0)")
        }

      }
    } else{

      # in data_s,  # n is the original tally, but this is sequential.... so need rolling update
      n_as_is <- nrow(data_dp)

      # drop the rows
      data_dp <- data_dp %>% dplyr::filter(!is.na(latitude) & !is.na(longitude))

      # # # # #
      # count the total remaining data after dropping or even if nothing dropped....
      data_s <- data_dp %>%  ungroup() %>% group_by(TagID) %>% count(TagID) %>% rename(NA_latlong_n = n) %>% left_join(data_s, by = "TagID")

      if(verbose){

        n_found <- n_as_is - sum(data_s$NA_latlong_n)
        pc <- round((n_found / n_as_is)*100, 2)

        if(n_found > 0){
          message("- Removing ", n_found, " rows with NA lat-longs, (", ifelse(pc < 1, "< 1 %", paste0(pc, "%")), " data)")
        } else{
          message("- No data with NA lat longs (nothing to remove)")
        }
      }

      # # # # #
    }

  }


  # ------------------------------------------------------------------- #
  # 2. Remove rows that are completely NA across all columns
  # ------------------------------------------------------------------- #
  if(verbose){
    message(centre_line("NA all row checker"))
    }

  if(annotate){

    # Add flag column: 1 if all NA, 0 otherwise
    data_dp <- data_dp %>%
      mutate(all_NA_rm = if_else(if_all(everything(), is.na), 1L, 0L))

    rm_cols_created <- c(rm_cols_created, "all_NA_rm")

    if(verbose){

      n_found <- sum(data_dp$all_NA_rm,na.rm=TRUE)
      pc <- round((n_found / nrow(data_dp))*100, 2)

      if(n_found > 0){
        message("- Annotating ", n_found, " rows with all NA data, (", ifelse(pc < 1, "< 1 %", paste0(pc, "%")), " data)")
      } else{
        message("- No data with NAs over all rows (all_NA_rm = 0)")
      }

    }


  } else{

    n_as_is <- nrow(data_dp)

    data_dp = data_dp %>% dplyr::filter(if_any(everything(), ~ !is.na(.)))

    if(all(is.na(data_dp))){
      stop("No data, NA data.frame")
    }

    # # # # #
    data_s <- data_dp %>% ungroup() %>% group_by(TagID) %>% count(TagID) %>% rename(all_NA_n = n) %>% left_join(data_s, by = "TagID")

    if(verbose){

      n_found <- n_as_is - sum(data_s$all_NA_n)
      pc <- round((n_found / n_as_is)*100, 2)

      if(n_found > 0){
        message("- Removing ", n_found, " rows with all NA data, (", ifelse(pc < 1, "< 1 %", paste0(pc, "%")), " data)")
      } else{
        message("- No NA data (nothing to remove)")
      }
    }

    # # # # #

  }

  # ------------------------------------------------------------------- #
  # 3. Assess duplicates
  # ------------------------------------------------------------------- #
  # I am now billing this as a very early thing that should be done in the processing, as it could be that
  # you have two parallel data streams which should be checked before any "arrange(TagID, DateTime)" ....

  # test
  #data_dp <- data
  #data_s <- data_dp %>% count(TagID)
  #data_dp = duplicate_track(data, dup_method = "first",annotate = FALSE)
  #n_as_is <- nrow(data_dp)

  if(duplicates){

    if(verbose){
      message(centre_line("Duplicate data checker"))
      }

    dup_method <- match.arg(dup_method)

    ##############
    dup_method_overrides <- c(
      "dup_method_parallel",
      "dup_method_same_time",
      "dup_method_same_loc",
      "dup_method_exact"
    )

    dot_args <- list(...)
    override_args <- dot_args[names(dot_args) %in% dup_method_overrides]

    # in duplicate_track() NULLs are handled via missing(var) and validation errors if is.null(var)
    # this means NULL is not allowed in duplicate_track() but may be in other functions, but here therefore:
    override_args <- override_args[!vapply(override_args, is.null, logical(1))]

    dot_args <- list(...) # capture all dots
    dup_args <- names(formals(duplicate_track)) # formal arguments of duplicate_track
    dup_dot_args <- dot_args[names(dot_args) %in% dup_args]  # only keep those relevant to duplicate_track
    args <- c("quality_vars", "quality_fun", "quality_directions", "centroid_fun", "time_fun") # args we want to manage / populate defaults for
    defaults <- lapply(formals(duplicate_track)[args], eval) # get evaluated defaults from duplicate_track
    dup_dot_args <- modifyList(defaults, dup_dot_args) # merge user-supplied with defaults (user-supplied takes precedence)
    dup_dot_args <- dup_dot_args[!vapply(dup_dot_args, is.null, logical(1))] # as above

    n_as_is <- nrow(data_dp)

    # safer:
    call_args <- c(
      list(
        data = data_dp,
        dup_method = dup_method,
        annotate = annotate,
        verbose = verbose
      ),
      dup_dot_args,
      override_args
    )

    data_dp <- do.call(duplicate_track, call_args)

    if(annotate){

      # rename the duplicate_track() returned columns
      # parallel_issue = dup_parallel_rm
      # dup_time_issue = dup_time_rm
      # dup_coords_issue = dup_coords_rm
      # dup_time_coords_issue = dup_time_coords_rm
      # all_dup_issue = all_dup_rm

      # New <- Old
      data_dp <- data_dp %>% rename(dup_parallel_rm = parallel_issue,
                                     dup_time_rm = dup_time_issue,
                                     dup_coords_rm = dup_coords_issue,
                                     dup_time_coords_rm = dup_time_coords_issue,
                                     all_dup_rm = all_dup_issue)

      rm_cols_created <- c(rm_cols_created, "dup_parallel_rm", "dup_time_rm", "dup_coords_rm", "dup_time_coords_rm", "all_dup_rm")

    } else{

      # simple sum up of already reduced tibble
      # overall duplicate tally = just the total rows minus those dropped overall from the
      # way duplicate_track() does things sequentially (not providing breakdown of where things dropped here)

      data_s <- data_dp %>% ungroup() %>% group_by(TagID) %>% count(TagID) %>% rename(dup_n = n) %>% left_join(data_s, by = "TagID")

      if(verbose){

        n_found <- n_as_is - sum(data_s$dup_n)
        pc <- round((n_found / n_as_is)*100, 2)

        if(n_found > 0){
          message("- Removing ", n_found, " rows with duplicate data issues, (", ifelse(pc < 1, "< 1 %", paste0(pc, "%")), " data)")
        } else{
          message("- No duplicate data issues found (nothing to remove)")
        }
      }

    }
  }

  # ------------------------------------------------------------------- #
  # 4a. Birds with too few fixes (before all other tests)
  # ------------------------------------------------------------------- #
  # debatable when this should come in

  if(min_fix_when == "after_NA"){

    if(verbose){
      message(centre_line("Min animal fix checker (before other tests)"))
      }

    if(min_fix > 0){

      data_mins = data_dp %>% group_by(TagID) %>%
        summarise(n = n()) %>%
        mutate(flag_ = ifelse(n < min_fix, 1L, 0L))

      # which animals have less than the min number
      tid_rm <- data_mins[data_mins$flag_ == 1L,]$TagID
      data_dp$tid_rm <- ifelse(data_dp$TagID %in% tid_rm, 1L, 0L)

      rm_cols_created <- c(rm_cols_created, "tid_rm")

      if(annotate){

        if(verbose){

          n_found <- sum(data_dp$tid_rm,na.rm=TRUE)
          pc <- round((n_found / nrow(data_dp))*100, 2)

          if(n_found > 0){
            message("- Annotating ", n_found, " rows for animals with too few fixes ",  "(", ifelse(pc < 1, "< 1 %", paste0(pc, "%")), " data; min thresh = " , min_fix, " fixes)")
          } else{
            message("- No animals flagged with too few fixes")
          }

        }

      } else{

        n_as_is <- nrow(data_dp)

        data_dp <- data_dp %>% dplyr::filter(tid_rm == 0L)

        # # # # #
        data_s <- data_dp %>% ungroup() %>% group_by(TagID) %>% count(TagID) %>% rename(tid_rm_n_before = n) %>% left_join(data_s, by = "TagID")
        # # # # #

        if(verbose){

          n_found <- n_as_is - sum(data_s$tid_rm_n_before)
          pc <- round((n_found / n_as_is)*100, 2)

          if(n_found > 0){
            message("- Removing ", n_found, " rows for animals with too few fixes ",  "(", ifelse(pc < 1, "< 1 %", paste0(pc, "%")), " data; min thresh = " , min_fix, " fixes)")
          } else{
            message("- No data removed, no animals had too few fixes")
          }
        }

        data_dp <- data_dp %>% select(-tid_rm)
      }

    } else{

      if(verbose){

        if(annotate){
          message("- No animals flagged with too few fixes")
        } else{
          message("- No data removed, no animals had too few fixes")
        }

      }
    }

  }


  # ------------------------------------------------------------------- #
  # 5. Tag spans
  # ------------------------------------------------------------------- #
  # carry out some similar checks to tag_spans from rake()

  if(tagspans){

    if(verbose){
      message(centre_line("Tag spans checker"))
      }

    if(length(measure) == 1){

      if(measure == "general"){

        if(verbose){
          message("- The general summary element of tag_spans() is meaningless for clean_track(), skipping ...")
        }

      }

    }

    if(length(measure) > 1){
      if(any(measure == "general")){
        w = which(measure == "general")
        measure = measure[-w]
      }

      dot_args <- list(...) # capture all dots
      tag_span_args <- names(formals(tag_spans)) # formal arguments of duplicate_track
      tag_span_dot_args <- dot_args[names(tag_span_args) %in% tag_span_args]  # only keep those relevant to duplicate_track

      args <- c("xra", "yra", "dates", "buffer_days", "tz") # args we want to manage / populate defaults for

      defaults <- lapply(formals(tag_spans)[args], eval) # get evaluated defaults from duplicate_track
      tag_span_dot_args <- modifyList(defaults, tag_span_dot_args) # merge user-supplied with defaults (user-supplied takes precedence)

      n_as_is <- nrow(data_dp)

      # general summary is meaningless in clean_track(), so only allowing only annotate TRUE/FALSE
      data_dp <- tag_spans(data_dp,
                           measure = measure,
                           xra = tag_span_dot_args$xra,
                           yra = tag_span_dot_args$yra,
                           dates = tag_span_dot_args$dates,
                           buffer_days = tag_span_dot_args$buffer_days,
                           tz = tag_span_dot_args$tz,
                           annotate = annotate,
                           verbose=verbose,
                           messages = FALSE # hard-coded
      )


      if(annotate){

        #rename variables
        rename_map <- c(
          future_date_rm       = "future_issue",
          temporal_date_rm     = "temporal_issue",
          minx_rm              = "minx_issue",
          miny_rm              = "miny_issue",
          maxx_rm              = "maxx_issue",
          maxy_rm              = "maxy_issue"
        )

        # Only keep rename pairs where the *old* column exists
        rename_map_existing <- rename_map[rename_map %in% names(data_dp)]

        data_dp <- data_dp %>%  rename(!!!rename_map_existing)

        rm_cols_created <- c(rm_cols_created, names(rename_map_existing)
        )

      } else{

        # simple sum up of already reduced tibble via annotate = FALSE in tag_spans() above

        data_s <- data_dp %>% ungroup() %>% group_by(TagID) %>% count(TagID) %>% rename(dup_spans = n) %>% left_join(data_s, by = "TagID")

        if(verbose){

          n_found <- n_as_is - sum(data_s$dup_spans)
          pc <- round((n_found / n_as_is)*100, 2)

          if(n_found > 0){
            message("- Removing ", n_found, " rows with data violating spatio-temporal checks, (", ifelse(pc < 1, "< 1 %", paste0(pc, "%")), " data)")
          } else{
            message("- No data issues that violate spatio-temporal checks found (nothing to remove)")
          }
        }

      }

    }

  }

  # ------------------------------------------------------------------- #
  # 6 Remove <x satellites, check for NAs
  # ------------------------------------------------------------------- #

  # first check if there are numeric data in the "satellites_used" column to work with
  if(sat_check){

    if(verbose){
      message(centre_line("Min satellite count checker"))
      }

    if(!any(names(data_dp) %in% sat_col)){
      if(verbose){
        message("- No satellite data column found matching that supplied under dat_col, skipping this step! ")
      }
      warning("No satellite data column found matching that supplied under 'dat_col'")
    } else{

      #data_dp <- data
      #annotate=FALSE

      # main function args
      #sat_col = "satellites_used"
      #drop_sats = 4

      # sole annotation to start with using the filt_err() function (annotate = TRUE) by default
      data_dp <- filt_err(
        data_dp,
        nms = sat_col,
        vals_l = drop_sats,
        vals_u = NULL,
        all_flag = FALSE
      )

      # ----------------------------------------------------- #
      # rename to sat_rm
      # ----------------------------------------------------- #
      rm_col <- paste0(sat_col, "_rm")
      data_dp <- data_dp %>% rename(sat_rm = !!sym(rm_col))

      rm_cols_created <- c(rm_cols_created, "sat_rm")

      # ----------------------------------------------------- #
      # Flag NAs
      # ----------------------------------------------------- #
      new_col <- "sat_NA"
      if(!new_col %in% names(data_dp)) data_dp[[new_col]] <- 0L
      if(any(is.na(data_dp[[sat_col]]))){
        data_dp[[new_col]] <- ifelse(is.na(data_dp[[sat_col]]), 1L, data_dp[[new_col]])
      }

      # ----------------------------------------------------- #
      # Annotation / dropping routine with verbosity
      # ----------------------------------------------------- #
      if(annotate){

        if(verbose){

          n_found <- sum(data_dp$sat_rm,na.rm=TRUE)
          pc <- round((n_found / nrow(data_dp))*100, 2)

          if(n_found > 0){
            message("- Annotating ", n_found, " rows with too few satellites ",  "(", ifelse(pc < 1, "< 1 %", paste0(pc, "%")), " data; min sat thresh = " , drop_sats, ")")
          } else{
            message("- No rows with too few satellites")
          }

        }

      } else{
        # --- If annotate = FALSE, drop flagged rows and report ---

        #data_s <- data_dp %>% count(TagID) # for testing

        n_as_is <- nrow(data_dp)

        # Count remaining sats per TagID (dynamic)
        sat_n <- data_dp %>%
          ungroup() %>%
          group_by(TagID) %>%
          dplyr::filter(sat_rm != 1 | is.na(sat_rm)) %>%
          count(TagID, name = "sat_n")

        # Count NA sats
        sat_NA <- data_dp %>%
          ungroup() %>%
          group_by(TagID) %>%
          dplyr::filter(sat_NA == 0) %>%
          count(TagID, name = "sat_NA")

        # Merge everything cleanly
        data_s <- data_s %>%
          left_join(sat_n,  by = "TagID") %>%
          left_join(sat_NA, by = "TagID") %>%
          tidyr::replace_na(list(sat_n = 0, sat_NA = 0))


        sat_NA2 <- data_dp %>%
          dplyr::filter(sat_NA == 1) %>%
          count(TagID, name = "sat_NA")

        if(nrow(sat_NA2) > 0){

          pc <- round((sum(sat_NA2$sat_NA) / nrow(data_dp))*100, 2)
          if(verbose){
            message(
              "- NA satellite data for ", nrow(sat_NA2), " TagIDs",
              " (total of ", sum(sat_NA2$sat_NA), " fixes, ",
              ifelse(pc < 1, "< 1 %", paste0(pc, "%")), " data): NA data will be retained"
            )
          }
        }

        if(verbose){

          n_found <- n_as_is - sum(data_s$sat_n)
          pc <- round((n_found / n_as_is)*100, 2)

          if(n_found > 0){
            message("- Removing ", n_found, " rows with too few satellites ",  "(", ifelse(pc < 1, "< 1 %", paste0(pc, "%")), " data; min sat thresh = " , drop_sats, ")")
          } else{
            message("- No rows with too few satellites to drop")
          }
        }

        # Drop rows flagged as 1, keep 0 or NA
        data_dp <- data_dp %>%
          dplyr::filter(is.na(sat_rm) | sat_rm != 1) %>%
          dplyr::select(c(-sat_rm, -sat_NA))

      }
    }
  }

  # ------------------------------------------------------------------- #
  # 7. Flt_switch - switch != 0 removed if Move data
  # ------------------------------------------------------------------- #

  if(flt_switch){

    if(verbose){
      message(centre_line("flt_switch checker"))
      }

    if(!exists("Type", data)){
      warning("No Type column found but 'flt_switch' is TRUE; if you have MoveBank data and want values flt_switch != 0 removed, add in a column called 'Type' with 'MoveBank' for all values: this allows assessment of the 'flt_switch' column (here an underscore not dot)")
    } else{

      data_dp <- data_dp %>% group_by(TagID) %>% #arrange(TagID, DateTime) %>%
        mutate(flt_switch = ifelse(is.na(flt_switch), -99, flt_switch)) %>% # if Movebank, check for flt_switch column being NA, then set to -99 for dropping
        mutate(flt_switch = ifelse(flt_switch != 0, -99, flt_switch))   # also if anything isn't zero then treat as a flagged bad data

      if(annotate){
        data_dp <- data_dp %>%
          mutate(flt_rm = ifelse(Type == "MoveBank" & flt_switch == -99, 1, 0))

        rm_cols_created <- c(rm_cols_created, "flt_rm")

        if(verbose){

          n_found <- sum(data_dp$flt_rm,na.rm=TRUE)
          pc <- round((n_found / nrow(data_dp))*100, 2)

          if(n_found > 0){
            message("- Annotating ", n_found, " rows matching flt_switch condition ",  "(", ifelse(pc < 1, "< 1 %", paste0(pc, "%")), " data)")
          } else{
            message("- No rows matching flt_switch condition")
          }

        }

      } else{

        n_as_is <- nrow(data_dp)

        data_dp <- data_dp %>%
          subset(Type != "MoveBank" | flt_switch != -99) # DROP THE -99s if the column is called Movebank

        # # # # #
        data_s <- data_dp %>% ungroup() %>% group_by(TagID) %>% count(TagID) %>% rename(flt_n = n) %>% left_join(data_s, by = "TagID")

        if(verbose){

          n_found <- n_as_is - sum(data_s$flt_n)
          pc <- round((n_found / n_as_is)*100, 2)

          if(n_found > 0){
            message("- Removing ", n_found, " rows matching flt_switch condition ",  "(", ifelse(pc < 1, "< 1 %", paste0(pc, "%")), " data)")
          } else{
            message("- No rows matching flt_switch condition to drop")
          }
        }

        # # # # #
      }

    }

  }

  # ------------------------------------------------------------------- #
  # 8a. pdop
  # check for existence of pdop named column or variants
  #data_dp <- Track(data_raw)
  # ------------------------------------------------------------------- #

  data_dp <- data_dp %>% group_by(TagID) #%>% arrange(TagID, DateTime)

  #pdop = TRUE
  #pdop_val = 7
  #pdop_col = "gps.pdop"
  #data_dp$gps.pdop

  if(pdop_check){

    if(verbose){
      message(centre_line("Dilution of precision: pdop checker"))
      }

    if(!any(names(data) %in% pdop_col)){
      warning("No pdop data column found matching that supplied under pdop_col, skipping this step! ...")
    } else{
      data_dp <- filt_err(
        data_dp,
        nms = pdop_col,
        vals_l = NULL,
        vals_u = pdop_val, # upper value
        all_flag = FALSE
      )

      # rename to gps_pdop_rm
      rm_col <- paste0(pdop_col, "_rm")
      data_dp <- data_dp %>% rename(gps_pdop_rm = !!sym(rm_col))

      rm_cols_created <- c(rm_cols_created, "gps_pdop_rm")

      if(annotate){
        if(verbose){

          n_found <- sum(data_dp$gps_pdop_rm,na.rm=TRUE)
          pc <- round((n_found / nrow(data_dp))*100, 2)

          if(n_found > 0){
            message("- Annotating ", n_found, " rows less than the pdop value: ", pdop_val, " (", ifelse(pc < 1, "< 1 %", paste0(pc, "%")), " data)")
          } else{
            message("- No rows matching max pdop condition")
          }

        }
      } else{

        n_as_is <- nrow(data_dp)

        # count up the rows meeting the condition
        data_s <- data_dp %>%
          ungroup() %>%
          group_by(TagID) %>%
          count(TagID) %>%
          rename(gps_pdop_n = n) %>%
          left_join(data_s, by = "TagID")  %>%
          tidyr::replace_na(list(gps_pdop_n = 0))

        if(verbose){

          n_found <- n_as_is - sum(data_s$gps_pdop_n)
          pc <- round((n_found / n_as_is)*100, 2)

          if(n_found > 0){
            message("- Removing ", n_found, " rows less than the pdop value: ", pdop_val, " (", ifelse(pc < 1, "< 1 %", paste0(pc, "%")), " data)")
          } else{
            message("- No rows matching max pdop condition to drop")
          }
        }

        data_dp <- data_dp %>%
          dplyr::filter(gps_pdop_rm != 1 | is.na(gps_pdop_rm)) %>%
          dplyr::select(c(-gps_pdop_rm))



      }
    }
  }

  # ------------------------------------------------------------------- #
  # 8b. hdop
  # check for existence of hdop named column or variants
  # ------------------------------------------------------------------- #

  if(hdop_check){

    if(verbose){
      message(centre_line("Dilution of precision: hdop checker"))
      }

    if(!any(names(data) %in% hdop_col)){
      warning("No hdop data column found matching that supplied under hdop_col, skipping this step! ...")
    } else{

      data_dp <- filt_err(
        data_dp,
        nms = hdop_col,
        vals_l = NULL,
        vals_u = hdop_val, # upper value
        all_flag = FALSE
      )

      rm_col <- paste0(hdop_col, "_rm")
      data_dp <- data_dp %>% rename(gps_hdop_rm = !!sym(rm_col))

      rm_cols_created <- c(rm_cols_created, "gps_hdop_rm")

      if(annotate){
        if(verbose){

          n_found <- sum(data_dp$gps_hdop_rm,na.rm=TRUE)
          pc <- round((n_found / nrow(data_dp))*100, 2)

          if(n_found > 0){
            message("- Annotating ", n_found, " rows less than the hdop value: ", hdop_val, " (", ifelse(pc < 1, "< 1 %", paste0(pc, "%")), " data)")
          } else{
            message("- No rows matching max hdop condition")
          }

        }
      } else{

        n_as_is <- nrow(data_dp)

        data_s <- data_dp %>%
          ungroup() %>%
          group_by(TagID) %>%
          count(TagID) %>%
          rename(gps_hdop_n = n) %>%
          left_join(data_s, by = "TagID")  %>%
          tidyr::replace_na(list(gps_hdop_n = 0))

        if(verbose){

          n_found <- n_as_is - sum(data_s$gps_hdop_n)
          pc <- round((n_found / n_as_is)*100, 2)

          if(n_found > 0){
            message("- Removing ", n_found, " rows less than the hdop value: ", hdop_val, " (", ifelse(pc < 1, "< 1 %", paste0(pc, "%")), " data)")
          } else{
            message("- No rows matching max hdop condition to drop")
          }
        }

        data_dp <- data_dp %>%
          dplyr::filter(gps_hdop_rm != 1 | is.na(gps_hdop_rm)) %>%
          dplyr::select(c(-gps_hdop_rm))

      }
    }
  }

  # ungroup from above pdop hdop conditions
  data_dp <- data_dp %>% ungroup()

  # ------------------------------------------------------------------- #
  # 9. Accuracy via custom filtering/flagging
  # ------------------------------------------------------------------- #
  # bespoke accuracy routine alongside other variables

  if(custom_filt){

    if(verbose){
      message(centre_line("Custom error column checker"))
      }

    # capture dot arguments
    dot_args <- list(...)
    filt_args <- names(formals(filt_err))
    filt_dot_args <- dot_args[names(dot_args) %in% filt_args]

    #browser()
    if(length(filt_dot_args) > 0){

      args <- c("nms", "vals_l", "vals_u", "inclusive")

      fml <- formals(filt_err)

      defaults <- lapply(fml[args], function(f) {
        # Case 1: missing argument → treat as NULL
        if (is.symbol(f) && deparse(f) == "") {
          return(NULL)
        }

        # Case 2: argument provided but is a bare symbol → treat as NULL
        if (is.symbol(f)) {
          return(NULL)
        }

        # Case 3: regular default → evaluate normally
        eval(f)
      })

      #defaults <- lapply(formals(filt_err)[args], eval)
      filt_dot_args <- modifyList(defaults, filt_dot_args)

      # ------------------------------------------ #
      # initial checking of length match validity
      #nms <- c("speed_accuracy", "flag", "satellites_used", "flt_switch")
      #vals_l = list(NULL,NULL,4,NULL)
      #vals_u = list(11,"wrong",NULL,flag_flt_switch)

      nms    <- filt_dot_args$nms
      vals_l <- filt_dot_args$vals_l
      vals_u <- filt_dot_args$vals_u

      if(length(vals_l) != length(vals_u)){
        message("vals_l and vals_u are differnt lengths, skipping custom filtering")
      }
      if(length(vals_l) != length(nms)){
        message("vals_l and nms are differnt lengths, skipping custom filtering")
      }
      if(length(vals_u) != length(nms)){
        message("vals_u and nms are differnt lengths, skipping custom filtering")
      }

      # ------------------------------------------ #
      ### remove previously used filtering cols
      remove_if_used <- function(nms, vals_l, vals_u, col) {
        if (col %in% nms) {
          message("Column ", col, " has already been used — removing its custom filter rules")
          idx <- which(nms == col)
          nms    <- nms[-idx]
          vals_l <- vals_l[-idx]
          vals_u <- vals_u[-idx]
        }
        list(nms = nms, vals_l = vals_l, vals_u = vals_u)
      }

      if(sat_check){
        tmp <- remove_if_used(nms, vals_l, vals_u, sat_col)
        nms    <- tmp$nms
        vals_l <- tmp$vals_l
        vals_u <- tmp$vals_u
      }

      if(pdop_check){
        tmp <- remove_if_used(nms, vals_l, vals_u, pdop_col)
        nms    <- tmp$nms
        vals_l <- tmp$vals_l
        vals_u <- tmp$vals_u
      }

      if(hdop_check){
        tmp <- remove_if_used(nms, vals_l, vals_u, hdop_col)
        nms    <- tmp$nms
        vals_l <- tmp$vals_l
        vals_u <- tmp$vals_u
      }

      if(flt_switch){

        # if logical TRUE, maybe the column is called "flt_switch"?
        col_to_check <- if(is.logical(flt_switch) && flt_switch) "flt_switch" else as.character(flt_switch)

        if(col_to_check %in% nms) {
          tmp <- remove_if_used(nms, vals_l, vals_u, col_to_check)
          nms    <- tmp$nms
          vals_l <- tmp$vals_l
          vals_u <- tmp$vals_u
        }
      }

      # update the list
      filt_dot_args$nms    <- nms
      filt_dot_args$vals_l <- vals_l
      filt_dot_args$vals_u <- vals_u

      # ------------------------------------------ #
      # proceed to custom filter if nms length > 0
      if(length(nms) > 0){

        rm_cols_created <- c(rm_cols_created, paste0(nms, "_rm")) # remember choices for grand clean_track() annotation

        data_dp <- filt_err(
          data = data_dp,
          verbose  = verbose,
          annotate = TRUE,
          all_flag = FALSE,

          # dots args
          nms       = filt_dot_args$nms,
          vals_l    = filt_dot_args$vals_l,
          vals_u    = filt_dot_args$vals_u,
          inclusive = filt_dot_args$inclusive
        )

        # then if annotate.....
        #n = "speed_accuracy"
        #data_dp$test <- 0
        #rm_col = "test"
        # ------------------------------------------ #

        if(annotate){
          if(verbose){

            for(n in nms){
              rm_col <- paste0(n, "_rm")

              n_found = data_dp %>% summarise(n = sum(!!sym(rm_col), na.rm=TRUE)) %>% .$n
              pc <- round((n_found / nrow(data_dp))*100, 2)

              if(n_found > 0){
                message("- Annotating ", n_found, " rows flagged by: ", n, " (", ifelse(pc < 1, "< 1 %", paste0(pc, "%")), " data)")
              } else{
                message("- No rows matching condition for ", n, " to annotate")
              }
            }

          }
        } else{

          # consider the variable drops back against the full data, not sequential here?
          n_as_is <- nrow(data_dp)

          for(n in nms){
            rm_col <- paste0(n, "_rm")
            count_col <- paste0(n, "_n")

            data_dp0 <- data_dp %>% dplyr::filter(!!sym(rm_col) == 0 | is.na(!!sym(rm_col) ))

            data_s <- data_dp0 %>%
              ungroup() %>%
              group_by(TagID) %>%
              count(TagID) %>%
              rename(!!count_col := n) %>%
              left_join(data_s, by = "TagID")  %>%
              tidyr::replace_na(setNames(list(0), count_col))

            if(verbose){

              n_removed <- n_as_is - sum(data_s[[count_col]], na.rm = TRUE)
              pc <- round((n_removed / n_as_is) * 100, 2)

              if(n_removed > 0) {
                message(
                  "- Removing ", n_removed, " rows flagged by ", n,
                  " (", ifelse(pc < 1, "< 1 %", paste0(pc, "%")), " of data)"
                )
              } else {
                message("- No rows matching condition for ", n, " to drop")
              }
            }

            data_dp <- data_dp %>%
              dplyr::filter((!!sym(rm_col)) != 1 | is.na((!!sym(rm_col)))) %>%
              dplyr::select(-all_of(rm_col))

          }
        }

      }
    } else{
      if(verbose){message("- No provided arguments for custom filtering via filt_err(), please provide these")}
    }




  }

  # ------------------------------------------------------------------- #
  # 10. Assign gapsections (via 'gap' identification)
  # ------------------------------------------------------------------- #
  # tolerance already added above
  # drop assessment separated below

  if(verbose){
    message(centre_line("Checking gapsections in data"))
  }
  if(!hasgaps){
    if(verbose){message("- No gapsections found, using supplied GAP and tol values")}
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


  # ------------------------------------------------------------------- #
  # 11. if drop = TRUE then drop OUT single gapsection fixes as cannot be assessed in the speed filter
  # ------------------------------------------------------------------- #
  if(drop_single_gap){

    if(verbose){
      message(centre_line("Single gapsection checker"))
      }

    if(annotate){

      data_dp <- data_dp %>%
        group_by(TagID, gapsec) %>%
        mutate(singlegap_rm = if_else(n() == 1, 1L, 0L)) %>%  # 1 if only one row in group
        ungroup()

      rm_cols_created <- c(rm_cols_created, "singlegap_rm")

      if(verbose){

        n_found <- sum(data_dp$singlegap_rm,na.rm=TRUE)
        pc <- round((n_found / nrow(data_dp))*100, 2)

        if(n_found > 0){
          message("- Annotating ", n_found, " rows with single fix gapsections (", ifelse(pc < 1, "< 1 %", paste0(pc, "%")), " data)")
        } else{
          message("- No rows with single gapsections")
        }

      }

    } else{

      n_as_is <- nrow(data_dp)

      data_dp = data_dp %>% group_by(TagID,gapsec) %>% mutate(nn = n()) %>%
        dplyr::filter(nn > 1) %>% # drop any single gapsecs
        dplyr::select(-nn) %>%
        ungroup()

      # # # # #
      data_s <- data_dp %>% ungroup() %>% group_by(TagID) %>% count(TagID) %>% rename(singlegap_n = n) %>% left_join(data_s, by = "TagID")
      #data_sr <- data_dp %>%  count(TagID)
      # # # # #

      if(verbose){

        n_found <- n_as_is - sum(data_s$singlegap_n)
        pc <- round((n_found / n_as_is)*100, 2)

        if(n_found > 0){
          message("- Removing ", n_found, " rows with single fix gapsections (", ifelse(pc < 1, "< 1 %", paste0(pc, "%")), " data)")
        } else{
          message("- No rows with single gapsections to drop")
        }
      }

    }

  }


  # ------------------------------------------------------------------- #
  # reproject or if internal X column not found, project data as UTM metres needed for dist cals ideally below in adehabitat
  if(reproject | !exists("X",data_dp)){

    if(verbose){
      message(centre_line("Reprojecting xy data to chosen p4s"))
    }

    data_sf <- sf::st_as_sf(data_dp, coords = c("longitude", "latitude"), crs = 4326)
    data_sf <- sf::st_transform(data_sf, p4s)
    coords = sf::st_coordinates(data_sf)
    data_dp$X <- coords[,1]
    data_dp$Y <- coords[,2]

    #data_dp <- data_dp[!duplicated(data_dp$DateTime),]

  }

  # ------------------------------------------------------------------- #
  # 12. Trajectory speed filter
  # ------------------------------------------------------------------- #
  if(speedFilt){

    if(verbose){
      message(centre_line("Custom trajectory speed assessment"))
      }

    dot_args <- list(...) # capture all dots
    speed_args <- names(formals(speed_filt)) # formal arguments of speed_filt
    speed_dot_args <- dot_args[names(dot_args) %in% speed_args]  # only keep those relevant to speed_filt
    args <- c("hybrid", "fall_back", "include_centre", "method", "fun", "fun_args", "points_each_side") # args we want to manage / populate defaults for
    defaults <- lapply(formals(speed_filt)[args], eval) # get evaluated defaults from speed_filt
    speed_dot_args <- modifyList(defaults, speed_dot_args) # merge user-supplied with defaults (user-supplied takes precedence)

    # now call speed_filt safely — all required args exist
    data_dp <- speed_filt(
      data = data_dp,
      sp_thres = sp_thres,
      verbose = verbose,
      annotate = annotate,
      hasgaps = TRUE,
      detailed_verbose = detailed_verbose,
      hybrid = speed_dot_args$hybrid,
      fall_back = speed_dot_args$fall_back,
      include_centre = speed_dot_args$include_centre,
      method = speed_dot_args$method,
      points_each_side = speed_dot_args$points_each_side,
      fun = speed_dot_args$fun,
      fun_args = speed_dot_args$fun_args
    )

    rm_cols_created <- c(rm_cols_created, "speed_rm") # speed_rm flag for grand annotation checking below

    if(!annotate){

      data_s <- data_dp %>% ungroup() %>% group_by(TagID) %>% count(TagID) %>% rename(speed_n = n) %>% left_join(data_s, by = "TagID")

      if(sum(data_s$speed_n) - nrow(data_dp) > 0){
        if(verbose){message("~@~@~@~@~@ Dropped ", sum(data_s$n) - nrow(data_dp), " fixes after all speed filt choices ~@~@~@~@~@")}
      } else{
        if(verbose){message("~@~@~@~@~@ No fixes dropped from speed filter :) ~@~@~@~@~@")}
      }

    }

  }

  # ------------------------------------------------------------------- #
  # 13. Turning angle filter
  # ------------------------------------------------------------------- #
  if(!is.null(turnFilt)){

    if(verbose){
      message(centre_line("Custom turning angle assessment"))
      }

    data_dp <- turn_filt(data_dp, turnFilt = turnFilt, turnFilt_dir = turnFilt_dir,
                         annotate = annotate, verbose = verbose, hasgaps = TRUE) # using pre-identified gaps from step #6

    data_dp <- data_dp %>% select(-any_of(c("init_row", "cc", "row_id")))

    rm_cols_created <- c(rm_cols_created, "angle_rm")

    if(!annotate) {

        data_s <- data_dp %>% ungroup() %>% group_by(TagID) %>% count(TagID) %>% rename(angle_n = n) %>% left_join(data_s, by = "TagID")

        if(sum(data_s$angle_n) - nrow(data_dp) > 0){
          if(verbose){message("~@~@~@~@~@ Dropped ", sum(data_s$n) - nrow(data_dp), " fixes after all turn filt choices ~@~@~@~@~@")}
        } else{
          if(verbose){message("~@~@~@~@~@ No fixes dropped from turn filter :) ~@~@~@~@~@")}
        }

    }
  }

  # ------------------------------------------------------------------- #
  # 14. Birds with too few fixes (before all other tests)
  # ------------------------------------------------------------------- #

  if(min_fix_when == "after_all"){

    if(verbose){
      message(centre_line("Min animal fix checker (after other tests)"))
      }

    if(min_fix > 0){

      data_mins = data_dp %>% group_by(TagID) %>%
        summarise(n = n()) %>%
        mutate(flag_ = ifelse(n < min_fix, 1L, 0L))

      # which animals have less than the min number
      tid_rm <- data_mins[data_mins$flag_ == 1L,]$TagID
      data_dp$tid_rm <- ifelse(data_dp$TagID %in% tid_rm, 1L, 0L)

      rm_cols_created <- c(rm_cols_created, "tid_rm")

      if(annotate){

        if(verbose){

          n_found <- sum(data_dp$tid_rm,na.rm=TRUE)
          pc <- round((n_found / nrow(data_dp))*100, 2)

          if(n_found > 0){
            message("- Annotating ", n_found, " rows for animals with too few fixes ",  "(", ifelse(pc < 1, "< 1 %", paste0(pc, "%")), " data; min thresh = " , min_fix, " fixes)")
          } else{
            message("- No animals flagged with too few fixes")
          }

        }

      } else{

        n_as_is <- nrow(data_dp)

        data_dp <- data_dp %>% dplyr::filter(tid_rm == 0L)

        # # # # #
        data_s <- data_dp %>% ungroup() %>% group_by(TagID) %>% count(TagID) %>% rename(tid_rm_n_after = n) %>% left_join(data_s, by = "TagID")
        # # # # #

        if(verbose){

          n_found <- n_as_is - sum(data_s$tid_rm_n_after)
          pc <- round((n_found / n_as_is)*100, 2)

          if(n_found > 0){
            message("- Removing ", n_found, " rows for animals with too few fixes ",  "(", ifelse(pc < 1, "< 1 %", paste0(pc, "%")), " data; min thresh = " , min_fix, " fixes)")
          } else{
            message("- No data removed, no animals had too few fixes")
          }
        }

        data_dp <- data_dp %>% select(-tid_rm)
      }

    } else{

      if(verbose){

        if(annotate){
          message("- No animals flagged with too few fixes")
        } else{
          message("- No data removed, no animals had too few fixes")
        }

      }
    }

  }


  ##########################################################
  # if selecting to annotate, search for annotate labelled columns and flag a combined version of all...

  if(annotate){

    if(verbose){
      message(centre_line("Building combined row flag column"))
    }
    # Detect all columns that end with "_rm"
    #cols <- grep("_rm$", names(data_dp), value = TRUE) # this is dangerous if the user already has such columns

    # safer:
    cols <- intersect(rm_cols_created, names(data_dp))

    #nms <- c("tid_rm", "NA_latlong_rm", "all_NA_rm", "sat_rm", "flt_rm", "gps_pdop_rm", "gps_hdop_rm", "singlegap_rm", "dup_rm", "speed_rm", "angle_rm")
    #cols <- intersect(nms, names(data_dp))

    setDT(data_dp)

    if(length(cols) == 0L){
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
  # this only modifies general attributes alongside clean_track
  if(is.null(attr(data_dp, "general") )){
    attr(data_dp, "general") <- "general"
  }
  if(is.null(attr(data, "clean_track") )){
    attr(data_dp, "clean_track") <- "clean_track"
  }

  # sub_attributes for arguments
  attr(attr(data_dp, "general"), "GAP") <- get("GAP")
  attr(attr(attr(data_dp, "general"), "GAP"),"function") <- "clean_track"

  attr(attr(data_dp, "general"), "unit") <- "secs"
  attr(attr(attr(data_dp, "general"), "unit"),"function") <- "clean_track"

  attr(attr(data_dp, "general"), "drop_single_gap") <- get("drop_single_gap")
  attr(attr(attr(data_dp, "general"), "drop_single_gap"),"function") <- "clean_track"

  attr(attr(data_dp, "general"), "p4s") <- get("p4s")
  attr(attr(attr(data_dp, "general"), "p4s"),"function") <- "clean_track"

  # this will never capture all that you could feed into the function for "..." in at least 4 functions.
  attr(attr(data_dp, "clean_track"), "drop_sats") <- get("drop_sats")
  attr(attr(data_dp, "clean_track"), "sp_thres") <- get("sp_thres")
  attr(attr(data_dp, "clean_track"), "sat_check") <- get("sat_check")
  attr(attr(data_dp, "clean_track"), "flt_switch") <- get("flt_switch")
  attr(attr(data_dp, "clean_track"), "min_fix") <- get("min_fix")
  attr(attr(data_dp, "clean_track"), "min_fix_when") <- get("min_fix_when")
  attr(attr(data_dp, "clean_track"), "NA_latlong") <- get("NA_latlong")
  attr(attr(data_dp, "clean_track"), "duplicates") <- get("duplicates")
  attr(attr(data_dp, "clean_track"), "dup_method") <- get("dup_method")
  attr(attr(data_dp, "clean_track"), "annotate") <- get("annotate")
  attr(attr(data_dp, "clean_track"), "turn_filt") <- get("turn_filt")
  attr(attr(data_dp, "clean_track"), "turn_filt_dir") <- get("turnFilt_dir")



  #class(data_dp) <- c("Track", "tbl_df", "tbl", "data.frame")
  data_dp <- structure(.Data = data_dp, class = c("Track", "grouped_df","tbl_df","tbl","data.frame"))

  if(verbose){
    message("Done.")
    message(strrep("*", getOption("width")))
  }
  return(data_dp)

}

#' @rdname clean_track
#' @export
clean_GPS <- clean_track

#' @rdname clean_track
#' @export
get_attributes <- function(data){

  assign_rates_attr <- attr(data, "assign_rates")
  general_attr <- attr(data, "general")
  clean_track_attr <- attr(data, "clean_track")
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
  attr_list[[3]] <- if(!is.null(clean_track_attr)){clean_track_attr}else{NA}
  attr_list[[4]] <- if(!is.null(define_trips_attr)){define_trips_attr}else{NA}
  attr_list[[5]] <- if(!is.null(sub_samp_attr)){sub_samp_attr}else{NA}
  attr_list[[6]] <- if(!is.null(rake_one_attr)){rake_one_attr}else{NA}
  attr_list[[7]] <- if(!is.null(speed_filt_attr)){speed_filt_attr}else{NA}
  attr_list[[8]] <- if(!is.null(turn_filt_attr)){turn_filt_attr}else{NA}
  attr_list[[9]] <- if(!is.null(trip_stats_attr)){trip_stats_attr}else{NA}

  names(attr_list) <- c("assign_rates_attr", "general_attr", "clean_track_attr",
                        "define_trips_attr", "sub_samp_attr", "rake_outlie_attr",
                        "speed_filt_attr", "turn_filt_attr",
                        "trip_stats_attr")

  return(attr_list)

}

#' @rdname clean_track
#' @export
give_attributes <- function(data, attr_list){

  # looks for attr_list from get_attributes()
  # add them to data if not NA

  if(!is.na(attr_list[[1]])){  attr(data, "assign_rates") <- attr_list[[1]]}
  if(!is.na(attr_list[[2]])){  attr(data, "general") <- attr_list[[2]]}
  if(!is.na(attr_list[[3]])){  attr(data, "clean_track") <- attr_list[[3]]}
  if(!is.na(attr_list[[4]])){  attr(data, "define_trips") <- attr_list[[4]]}
  if(!is.na(attr_list[[5]])){  attr(data, "track_subsamp") <- attr_list[[5]]}
  if(!is.na(attr_list[[6]])){  attr(data, "rake_outlie") <- attr_list[[6]]}
  if(!is.na(attr_list[[7]])){  attr(data, "speed_filt") <- attr_list[[7]]}
  if(!is.na(attr_list[[8]])){  attr(data, "turn_filt") <- attr_list[[7]]}

  return(data)

}

























