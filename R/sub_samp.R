#' Sub-sample telemetry data to a standardised sampling rate
#'
#' The function \code{sub_samp} filters the telemetry data to a given rate specified by the user
#' so that all positional fixes are at a near-enough standard rate, such as 2 mins, 5 mins, 30 mins.
#' This reduces sampling bias should tags sample more quickly or more slowly over their deployment,
#' and is needed for analyses such as some of those used to produce time-static utilisation distributions.
#'
#' @details
#'
#' Very often, tags deployed on animals will not always be scheduled to sample at a constant rate
#' or gaps may occur that render particular fixes unusable given potential for spatio-temporal
#' sampling biases in further analyses. \code{sub_samp} offers a way to reduce \code{Track},
#' \code{TrackStack} and \code{TrackMultiStack} class objects to a common temporal denominator
#' based on the start and end time of each Tag's record. Note, however, this method does not fully interpolate
#' the deployments to a fixed temporal value, as is done in linear and non-linear (e.g. correlated
#' random walk) interpolation offered through R packages adehabitat (Calenge 2006) and Crawl (Johnson et al. 2008)
#' \code{sub_samp} has its early origins from original code from Emiel van Loon at the University of Amsterdam.
#'
#' Four options are available in \code{sub_samp} for subsampling: "sequencer", "rounder", "boot_sequencer" or "boot_rate", see
#' the argument descriptions for more details. These allow subsampling to the nearest time unit based on
#' either the start time of the animal (sequencer) or the nearest whole time unit (rounder). These methods also handle
#' situations where two competing fixes may be available to be rounded to the same time, which can result in holes in the data if
#' only one valid fix is picked. The default of \code{sub_samp()} is to preserve a true downgraded sample, and so the "rounder"
#' and "sequencer" methods (as also with the boot methods) make an attempt to randomly fill in potentially missing fixes in such cases
#' via a final sweep of the data.
#'
#' The boot methods have been developed to produce simulations so as to
#' avoid reliance on always selecting the same fixes from the sample - useful for example if an animal traverses
#' a spatial unit a limited number of times and you want o make the most of the data from within the track.
#' The function \code{retrieve_boots} can be used to access the bootstrapped GPS data, that are stored as lookup indices
#' from \code{sub_samp} for memory saving purposes.
#'
#' Regarding the bootstrap methods, these are complex and tricky to arrive at a true random sample as it depends
#' ultimately on the structure of the data underlying. The aim is to preserve the linear temporal
#' structure of the original track as much as possible, just at a downgraded rate. In doing so, the \code{sub_samp()} function caters for as made of these
#' data quirks as possible. In particular, a key issue is when a sequence of fixes is 'locked in', for example when a switch of
#' rates of the tag occurs, or when there are just too few samples to choose from to meet a desired acceptable
#' lower and upper bound around the target downgrade rate. As in other \code{MoveRakeR} functions, tolerance values
#' are allowed to vary this bound around the rate required resulting in a lower 'rateL' and upper 'rateU' around the original 'dt'
#' sampling rate. Widening this gap will bring more GPS fixes into play within the samplers, but at expense of deviating from the targetted downgrade rate.
#' The samplers operate via random search windows where one fixe is randomly picked in a 'segment' window along the track where the segment length is rateU.
#' The ambition is that sampled fixes meet both rateL and rateU condition, but may not always possible for every segment and boot.
#' Some flexibility is therefore provided in \code{sub_samp()} to vary the method used. The primary "boot_sequencer" approach is similar to the
#' "sequencer" method in sampling within strings of points, with the first fixes' DateTime used to start the sampling off.
#' The string of points considered for segmentation is thus a nested hierarchy of: TagID/(optional by variable)/gapsection. Another level is introduced in the second
#' boot method "boot_rate" that still uses a sequence from the first DateTime in the valid ctring of points,
#' but using: TagID/(optional by variable)/rate/gapsection to begin the segment randomisation process. Strictness in how the samplers treats fixes that are too close
#' together after the sampling is finished, as well as a repairing process to insert potentially valid fixes back in if holes in the data arise.
#' Finally, the sampling can also be done 'blindly' without remembering any fixes in the sequence, but at a cost
#' of violating the rateL/rateU conditions between sampled fixes.
#'
#' @param data Input data object, with required columns: TagID, DateTime, longitude, latitude.
#' @param dt The new sampling rate at which we wish the data to be downgraded to. Numeric value.
#' @param Unit The unit of the new sampling rate, fed to function \code{base::difftime} specified as
#' e.g. 'secs' or 'mins'.
#' @param tol Lee-way proportion numeric value - ideally between >0 and <=1 giving a proportional amount to be
#' added or subtracted from the specified filtering rate \code{dt}. See below.
#' @param tol_u A bespoke upper tolerance if the user wants to specify a different value for upper and lower tolerance.
#' @param tol_l A bespoke lower tolerance if the user wants to specify a different value for upper and lower tolerance.
#' @param by A by variable also which to work with data at a lower level, e.g. year, migration phase (but currently only used by the boot sampler).
#' @param rateL A lower rate given automatically using \code{tol} as (dt-(dt*tol)).
#' For the methods 'sequencer' and 'rounder', occasionally the filter will not remove some rates <= dt due to it's matching nature
#' therefore after the subsampling, the numeric value given in \code{rateL} will drop rates lower than this value. For the 'boot_sequencer'
#' and 'boot_rate' methods, rateL is used as a minimum value to search for within segment windows of length rateU.
#'  By default, \code{rateL} and \code{rateU} are therefore both determined by the tolerance proportional value \code{tol}, to
#' give a window of lee-way error given that GPS rates rarely sample spot on the rate they are programmed to take measurements at
#' i.e. due to satellite communications, time to fix errors etc. However rateL and rateU can also be determined by
#' bespoke l_tol or u_tol parameters that override tol, which are in turn also overridden if the user specifies
#' a direct raeL or rateU. The hierarchy of rateL and rateU determination in order of
#' increasing priority are are thus via tol, u_tol or l_tol and finally via the rateL or rateU provision directly. By default
#' rateU and rateL are NULL, and so tol values are used.
#' @param rateU The upper rate (dt+(dt*tol)) at which to remove fixes as above using the \code{tol} parameter; this is the primary
#' value for which sub-sampling is carried out in all methods; see rateL for more details of calculation with tol arguments.
#' @param drop_coarser Logical to drop the coarser rates than the ones we sub-sample for? Default TRUE.
#' This is handy to set to FALSE if comparing trip statistics and effects of downgrading if multiple rates per trip are evident.
#' @param fun Logical TRUE or FALSE (default TRUE) to use the \code{\link{gap_section}} function to remove coarser rates, see also 'method' argument.
#' @param method A choice of four, "sequencer", "rounder", "boot_sequencer", or "boot_rate" as to how to do the downgrading:
#' (1) The sequencer is the default method that \code{sub_samp()} uses, with a
#' data.table used to 'roll' DateTimes to nearest on a sequences of dates at the desired sampling rate from the start of the animal per year.
#' (2) The second option is to round to the nearest whole DateTime at the desired sampling rate, both being valid options.
#' The function \code{sub_samp} also calls \code{\link{gap_section}} using the drop = TRUE option to remove isolated gappy sections
#' of data (single points) and remove all data beyond the desired rate, e.g. if 300 s was wanted, then any coarse rates
#' such as 1800 s would be dropped, that is if \code{fun} is set to TRUE.
#' (3) The boot sequencer uses the sequence approach as above
#' but selects random fixes within consecutive sections of data t0 to t+1 where (t+1)-t = dt.
#' The boot_sequencer samples with replacement, and searches within 'segments' of points within windows length rateU.
#' This window search operates across TagID/'VAR'/gapsec levels with 'VAR' being the potential by variable that can be included in the hierarchy.
#' This process of course slower for large datasets, e.g. 10 s to five minutes for data across many months per animal. The sequencer
#' also has a quirk if used with default settings (blind = TRUE, strict = FALSE), in that across windows of the data,
#' there will naturally be only certain GPS fixes that can be sampled from the segment windows, dependent on the rateL and rateU conditions used.
#' This will 'lock in' the same fixes sampled on the same bootstraps each time. Options are available to relax this condition
#' through setting the 'blind' argument to TRUE, which will forget the previous point selection in the sequence. If the 'strict'
#' argument is also set to TRUE, then any fixes too close to a previous segment selection will be dropped. With 'strict' as FALSE (default)
#' these points will not be dropped and any gaps of too lengthy time spans where rateU is violated will be checked for other points to insert.
#' A more conservative but still quite strict approach is to lower the lower tolerance 'l_tol' value, e.g. to 0.4 from 0.2 overall 'tol'
#' so that some variation within the sequence of fixes is allowed, but this is a trade-off as the lower l_tol specified will
#' increasingly deviate away from the intended overall 'dt' rate downgraded to.
#' Another option is to use the fourth method option:
#' (4) This introduces another level into the hierarchy so that sequences are performed within TagID/'VAR'/rate/gapsec.
#' This uses the \code{assign_rates()} function \strong{with defaults from that function}, to allow strings of fixes
#' along which the sequence of bootstrapping is performed to occur within rate changes of the tag, which is useful if there
#' are multiple rates used. This will begin the search of random points at each rate switch to break that 'locked in' pattern
#' of samples across bootstraps. However, this will also result in possible no valid fixes being found at those boundaries.
#' \strong{A warning here}: If your data is on the scale of one fix every day, then the 'boot_rate' approach will not work as
#' the defaults are used within \code{assign_rates()}, which guesses rates up to three hours.#'
#' The result of the two boot methods is a listed output of boots, that can be translated back into
#' \code{Track} data using the function \code{retrieve_boots}.
#' @param strict Logical, defaults to FALSE. Determines whether the function drops out the fixes that are too fast and violate
#' the rateL lower bound as determined by the tol or l_tol arguments. In the boot methods, frequently, fixes cannot be sampled to meet
#' both rateL and rateU, particularly at coarser rates from slightly less coarser rates, where fewer fixes are available fore resampling.
#' This of course depends on the tolerance parameters used too. That said a situation can also arise if for example
#' one sampling segment has an issue finding a fix skipping it, which forces the next segment along to randomly pick from any of the fixes
#' in that second segment, which may be further forward in time > rateU between remaining sampled fixes; this is because
#' essentially that second segment has to start the search off again considering all fixes.
#' This can give 'holes' in the data, but these on occasion can be back-filled (repaired) by going back to the original data to check for valid points, and if
#' strict = FALSE, this will happen with 1:n fixes randomly inserted, where n is the maximum possible on that slice of the data.
#' @param blind Logical, default FALSE. Following on from the strict argument, if blind is TRUE, all segments sample 'blindly'
#' where within segment t, any previous GPS random pick of a point in the segment t-1 is forgotten. This allows a full true randomisation
#' but will violate rateL and rateU conditions. Therefore strict arguments should be considered alongside. If strict is TRUE as well as blind being TRUE,
#' the result is removal of fixes violating rateL but keeping 'holes' in the data from rateU segment to segment quirks (see strict argument).
#' If blind is TRUE but strict is FALSE, then faster sampled points are retained, but attempts are made to put potentially missing fixes into data 'holes'.
#' @param nboots The number of bootstraps to take using the sequence sampler, relevant only for that method.
#' @param seed Seed to set for reproducibility using the boot sequencer.
#' @param verbose logical to return detailed messaging.
#' @param verbose_repair Logical defaulting to FALSE. This turns on detailed messaging for the routine that
#' attempts to 'repair' the randomly sub-sampled data after the "boot_sequencer" or "boot_rate" methods have been used.
#'
#' @return Returns the same format data as inputted but a reduced dataset (except for the boot_sequencer - see below)
#' removing rows of data for each animal faster than the desired rate. The boot sequencer returns a list of row numbers matching
#' the original dataset, to save memory, stored as attributes to the output under \code{attr(attr(data2, "sub_samp"), "boots")}; these are retrieved and converted
#' to \code{Track} data using the \code{retrieve_boots} function that adds an additional column of 'boot'
#' in a stacked dataset across all animals.
#'
#' @seealso [MoveRakeR::clean_GPS], [MoveRakeR::gap_section], [MoveRakeR::Track2move], [MoveRakeR::progress_estimated2]
#'
#' @examples
#'
#' # NOT RUN
#' ColLon = -3.185
#' ColLat = 54.045
#' p4 <- sf::st_crs((paste("+proj=laea +lon_0=", ColLon," +lat_0=", ColLat, " +units=m", sep="")))
#' data <- yourdata # GPS data with tagID, DateTime, longitude and latitude named columns
#'
#' indata <- data %>% clean_GPS(speedfilt = TRUE, drop = FALSE) %>%
#'   define_trips(method="rect", lls = c(-3.2, 54.0553,-3.1689, 54.0437), p4s = p4)
#'
#' # test of rounding and sequencing approaches
#' round_test = sub_samp(indata, dt=300, tol = 0.4, method = "rounder")
#' seq_test = sub_samp(indata, dt=300, tol = 0.4, method = "sequencer")
#'
#' # If wanting different upper and lower tolerances, e.g. more strict removal of fixes after the sub-sampling has been carried out
#' round_test = sub_samp(indata, dt=300, u_tol = 0.4, l_tol = 0.2, method = "rounder")
#'
#' plot_leaflet(round_test)
#' plot_leaflet(seq_test)
#'
#' # ------------------------------------------------------------------- #
#' # EXAMPLE ISSUES DURING THE PROCESSING
#' # The function presents two main methods for sub-sampling (also called thinning or downgrading) the data
#' # to coarser sampling rates: (1) Rounding DateTimes to the nearest rate (+ unit) and (2) A sequence of
#' # DateTimes from the start of the bird (default).
#' # Both are valid estimations, but will result in different answers for points they may select
#' #
#' # Of note, there is a quirk in the way this operates, with potential for overly strict removal of locations
#' # with either of the above methods snapping to regular time grids where data are close already to the desired
#' # downgrading rate. Although mathematically correct, without consideration may be overly reductionist.
#' #
#' # consider this example of five minute data, and the user wants to downgrade the data to five minutes,
#' # given that further data are below this rate in the sequence. But focusing here on the data already near 300 s:
#'
#' dates <- as.POSIXct(c("2014-05-21 05:37:44","2014-05-21 05:42:31","2014-05-21 05:47:20"), tz = "UTC")
#' data_test_df <- tibble(DateTime = dates, TagID = 1)
#' data_test_df <- data_test_df %>% mutate(difftime = as.vector(difftime(DateTime, lag(DateTime), units= 'secs')))
#'
#' # these points are all at slightly less than a 5 min rate but still very valid and near to them.
#'
#' # -------------------------------------------------------- #
#' # 1. Rounding to DateTime units
#'
#' df_subsampled_test <- data_test_df %>%
#'   group_by(TagID) %>%
#'   rename(DateTime_orig = "DateTime") %>%
#'   mutate(DateTime = round_date(DateTime_orig, unit = period(num = 5, units = 'mins')),
#'          diff = abs(as.vector(difftime(DateTime_orig, DateTime, units = "secs")))
#'   ) %>% group_by(TagID, DateTime) %>% slice_min(diff) %>%
#'   group_by(TagID) %>%
#'   mutate(dt = as.vector(difftime(lead(DateTime_orig), DateTime_orig, units = 'secs')) ) # recalculate dt
#'
#' # This results in the middle point being dropped because the 47 is the closer match to the nearest round
#' # but the three points seem like valid 5 minute samples, albeit below the intended 300s, but very close
#' #
#' # subsample           DateTime               TagID difftime
#' # <dttm>              <dttm>              <dbl>    <dbl>
#' #  1 2014-05-21 05:40:00 2014-05-21 05:37:44     1       NA
#' #  2 2014-05-21 05:45:00 2014-05-21 05:47:20     1      289
#' #
#' # -------------------------------------------------------- #
#' # 2. Sequential 'rolling' in data.table to a sequence
#'
#' #' # ordinarily this is the approach:
#' #date_start = min(data_test_df$DateTime,na.rm=TRUE)
#' #date_end = max(data_test_df$DateTime,na.rm=TRUE)
#' #date_seq = seq(from=date_start, to=date_end, by= paste(5, 'mins))
#' #if(date_seq[length(date_seq)] < date_end){  date_seq <- c(date_seq,date_seq[length(date_seq)]+dt) }
#'
#' # but if we ended up with the the start of the animal being at 05:45:03, this would give sequential 5 min sequences at:
#' date_seq <- as.POSIXct(c("2014-05-21 05:35:03", "2014-05-21 05:40:03", "2014-05-21 05:45:03"), tz = "UTC")
#' data_seq <- data.frame(TagID = 1, DateTime = date_seq)
#'
#' # then rolling to the nearest seq DateTime per TagID:
#' r1 <- data.table::data.table(data_test_df, key = c("TagID","DateTime"))  # raw data
#' r1$DateTime_orig <- r1$DateTime
#' r2 <- data.table::data.table(data_seq, key = c("TagID","DateTime"))  # interpolated data
#' new <- r1[r2 , list(TagID, DateTime, DateTime_orig) , roll = "nearest", allow.cartesian=TRUE ]
#'
#' new$diff <- as.vector(difftime(new$DateTime_orig,new$DateTime,units = 'secs'))
#' new$diff <- ifelse(new$diff < 0, new$diff *-1,new$diff)
#'
#' # which.min again, similar to above under (1)
#' select_cols <- c("TagID","DateTime","DateTime_orig","diff")
#' new2 <- data.table::setDT(new)[, .SD[which.min(diff)], by = .(DateTime_orig)][, ..select_cols]
#'
#' # and so forth
#' data.table::setkey(new2, TagID, DateTime_orig)
#' new2[,dt:= (difftime(DateTime_orig, data.table::shift(DateTime_orig,1,type="lag"),unit = "secs")), by = "TagID"]
#' new2$dt <- c(as.vector(new2$dt)[-1],NA)
#' new2$dt <- ifelse(is.na(new2$dt),0,new2$dt)
#'
#' #Key: <DateTime_orig>
#' #  DateTime       DateTime_orig  diff    dt
#' # <POSc>              <POSc> <num> <num>
#' #  1: 2014-05-21 05:40:03 2014-05-21 05:37:44   139   576
#' #  2: 2014-05-21 05:45:03 2014-05-21 05:47:20   137     0
#'
#' # -------------------------------------------------------- #
#' # To get around the rounder issue, in the sub_samp() function, we therefore revisit the original data
#' # and look for potential real points that could sit in between consecutive fixes that would meet a
#' # minimum threshold difference between the fixes informed by dt-(dt*tol)
#' # -------------------------------------------------------- #
#'
#' # Ultimately, interpolation methods may be more useful depending on further analytical uses.
#'
#' sub_samp(data = data_test_df, dt = 300, tol = 0.4, method = "rounder")
#'
#' #Using method: rounder
#' #Computed rateL from tolerance: 180
#' #Computed rateU from tolerance: 420
#' #---- rounding process ----
#' #  ---- overly strict point removal checker ----
#' #  TagID difftime            DateTime  dt rn
#' #1     1       NA 2014-05-21 05:37:44 287  1
#' #2     1      287 2014-05-21 05:42:31 289  2 <------ better
#' #3     1       NA 2014-05-21 05:47:20  NA  3
#'
#' ##########################################################################################
#  # rigorous function tests - in the below example we have a a mix of rates 3600, 300 and faster 20 s
#' dates <- c("2014-05-21 05:37:44","2014-05-21 05:42:31","2014-05-21 05:47:20",
#'            "2014-05-21 03:32:01", "2014-05-21 03:32:22","2014-05-21 05:36:21",
#'            "2014-05-21 05:36:40", "2014-05-21 05:37:01", "2014-05-21 05:37:19",
#'            "2014-05-21 06:37:44", "2014-05-21 07:38:34","2014-05-21 07:38:54")
#'
#' data_test_df <- data.frame(DateTime = as.POSIXct(c(dates), tz = "UTC"), TagID = 1, difftime = NA) %>%
#'   arrange(DateTime) %>%
#'   mutate(difftime = as.vector(difftime(DateTime, lag(DateTime), units= 'secs')))
#' data_test_df$test <- "blah" # addtional column in supply data
#'
#' # testing 3600 s for rounder and sequencer, with drop_coarser TRUE/FALSE
#' sub_samp(data = data_test_df, dt = 3600, tol = 0.2, method = "rounder", drop_coarser = FALSE)
#' sub_samp(data = data_test_df, dt = 3600, tol = 0.2, method = "sequencer", drop_coarser = FALSE)
#' sub_samp(data = data_test_df, dt = 3600, tol = 0.2, method = "rounder", drop_coarser = TRUE)
#' sub_samp(data = data_test_df, dt = 3600, tol = 0.2, method = "sequencer", drop_coarser = TRUE)
#'
#' # 300 s
#' sub_samp(data = data_test_df, dt = 300, tol = 0.4, method = "rounder", drop_coarser = FALSE)
#' sub_samp(data = data_test_df, dt = 300, tol = 0.4, method = "sequencer", drop_coarser = FALSE)
#' sub_samp(data = data_test_df, dt = 300, tol = 0.4, method = "rounder", drop_coarser = TRUE)
#' sub_samp(data = data_test_df, dt = 300, tol = 0.4, method = "sequencer", drop_coarser = TRUE)
#'
#' # 20 s
#' sub_samp(data = data_test_df, dt = 20, tol = 0.2, method = "rounder", drop_coarser = FALSE)
#' sub_samp(data = data_test_df, dt = 20, tol = 0.2, method = "sequencer", drop_coarser = FALSE)
#' sub_samp(data = data_test_df, dt = 20, tol = 0.2, method = "rounder", drop_coarser = TRUE)
#' sub_samp(data = data_test_df, dt = 20, tol = 0.2, method = "sequencer", drop_coarser = TRUE)
#'
#' # 1800 s: Behaviour when you are trying to round to a rate that isn't possible...??
#' sub_samp(data = data_test_df, dt = 1800, tol = 0.2, method = "rounder", drop_coarser = TRUE)
#' sub_samp(data = data_test_df, dt = 1800, tol = 0.2, method = "sequencer", drop_coarser = TRUE)
#'
#' # but....if not dropping coarser, and the case is true where we (after filtering)
#' # have no valid data for the rates BUT you want to keep all data COARSER than that
#' # we should...do so? This therefore acts as a filter taking out anything less than 1800+tol
#'
#' sub_samp(data = data_test_df, dt = 1800, tol = 0.2, method = "rounder", drop_coarser = FALSE)
#' sub_samp(data = data_test_df, dt = 1800, tol = 0.2, method = "sequencer", drop_coarser = FALSE)
#'
#' ## back to real data tests
#' data <- yourdata
#' x1800 <- sub_samp(data = data, dt = 1800, tol = 0.2, method = "sequencer", drop_coarser = TRUE)
#' x3600 <- sub_samp(data, dt = 3600, tol = 0.4, method = "sequencer", drop_coarser = TRUE)
#' x300 <- sub_samp(data = data, dt = 300, tol = 0.2, method = "sequencer", drop_coarser = TRUE)
#'
#' # -------------------------------------------------------- #
#' # 3. Boot sequencers
#'
#' # produce 'realisations' of the data randomly sampling
#' # This can be quite lengthy if say you have lots of animals, lots of 10 s data and wanting to go to e.g. 1800 s
#' # We have stuck to use of loops in the sub_samp() function as after much testing, these were actually
#' # some of the fastest approaches.
#'
#' # The two boot options:
#' # ------------------ #
#' # (a) "boot_sequencer" using TagID/(by variable)/gapsec/segment approach:
#'
#' x1800_boot <- sub_samp(data = data, dt = 1800, tol = 0.2, method = "boot_sequencer")
#' boots_x1800 <- retrieve_boots(x1800_boot) # use of retrieve-boots function to access boots (stored from the sub_samp function as an index lookup to save memory)
#'
#' RakeRvis::RakeRvis(data = boots_x1800) # check visualisation, plotting 'by' boot in the app or...
#' plot_leaflet(data = boots_x1800, plotby = "boot")
#'
#' # example using random points in all segments, and patching potential fixes in that violate rateL conditions after:
#' x1800_boot_blind <- sub_samp(data = data, dt = 1800, tol = 0.2, method = "boot_sequencer", blind = TRUE)
#'
#' #or being strict and dropping out any fixes that violate rateL without patching any fixes back in
#' x1800_boot_blind <- sub_samp(data = data, dt = 1800, tol = 0.2, method = "boot_sequencer", blind = TRUE, strict = TRUE)
#'
#' # example using a more leniant lower tolerance value, preserving the sequence of point randomisation
#' x1800_boot_ltol <- sub_samp(data = data, dt = 1800, tol = 0.2, l_tol = 0.4, method = "boot_sequencer", blind = FALSE, strict = FALSE)
#'
#' # ------------------ #
#' (b) "boot_rate" using TagID/(by variable)/rate/gapsec/segment approach:
#'
#' x1800_boot_rate <- sub_samp(data = data, dt = 1800, tol = 0.2, method = "boot_rate", drop_coarser = TRUE)
#' boots_x1800_rate <- retrieve_boots(x1800_boot)
#'
#' @references
#'
#' Calenge, C. (2006) The package adehabitat for the R software: tool for the analysis
#' of space and habitat use by animals. \emph{Ecological Modelling}, \strong{197}, 516-519
#'
#' Johnson, D.S., London, J.M., Lea, M.-A. & Durban, J.W. (2008) Continuous-time correlated random
#' walk model for animal telemetry data. \emph{Ecology}, \strong{89}, 1208-1215. doi:10.1890/07-1032.1.
#'
#' Johnson, D.S. & London, J.M. (2018) crawl: an R package for fitting continuous-time
#' correlated random walk models to animal movement data. Zenodo.
#' \url{https://doi.org/10.5281/zenodo.596464}
#'
#' @export
sub_samp = function(data, dt=300, Unit='secs', by = NULL, tol = 0.2, u_tol=NULL, l_tol = NULL,
                    rateU=NULL, rateL = NULL,
                    method = c("sequencer", "rounder", "boot_sequencer", "boot_rate"),
                    strict = FALSE, blind = FALSE,
                    n_boot = 5, seed = NULL,
                    drop_coarser = TRUE,
                    verbose = TRUE,
                    verbose_repair = FALSE){



  # --------------------------------------- #
  # assessment of relevant minimum namings in data
  attr_list <- get_attributes(data)
  # --------------------------------------- #

  method = method[1] # using sequence by default

  if(verbose){

    message("Using method: ", method[1])

    # Step 0: define base rate
    rate <- dt

    # Step 1: determine effective tolerances
    # If tol is NULL, use midpoint of provided l_tol/u_tol if any, else default 0.2
    if (is.null(tol)) {

      if(is.null(l_tol) & is.null(u_tol)){tol = 0.2} else{

        tol <- mean(c(l_tol, u_tol), na.rm = TRUE)
        #if (is.nan(tol)) tol <- 0.2  # default fallback
      }

    }

    l_tol_use <- if(!is.null(l_tol)) l_tol else tol
    u_tol_use <- if(!is.null(u_tol)) u_tol else tol

    # Step 2: rateL
    if (!is.null(rateL)) {
      final_rateL <- rateL
      message("Using rateL supplied: ", final_rateL)
    } else {
      final_rateL <- rate - (rate * l_tol_use)
      message("Computed rateL from tolerance: ", final_rateL)
    }

    # Step 3: rateU
    if (!is.null(rateU)) {
      final_rateU <- rateU
      message("Using rateU supplied: ", final_rateU)
    } else {
      final_rateU <- rate + (rate * u_tol_use)
      message("Computed rateU from tolerance: ", final_rateU)
    }

  }

  rateL = final_rateL
  rateU = final_rateU

  ##################################
  # Three options
  # (1) A rounding approach based on DateTime calendar, i.e. round_date()
  # (2) A sequential approach from the first fix per bird (or bird gapsec);
  # The latter method stems from Emiel van Loon, original code supplied as part of a joint collaboration with
  # the University of Amsterdam, from an R course at UvA. Our sincerest thanks go to Emiel for this initial approach
  # The general approach of using a sequence of points is followed but modified using a rolling to nearest datetime.
  # The updated code here accounts for potential issues with missed fixes (see help example)
  # (3) A resampling approach for sections of data at the level of thinning required using option (2) above
  # This is in beta development and could possibly be made more efficient

  final_samp <- coarser_fixes <- NULL # for below attribute output, a bit hacky

  ##########################################################
  data_tb <- tibble(data) %>% group_by(TagID) %>% arrange(TagID, DateTime)

  # ======================================================= #
  # 1. Rounding to the nearest datetime unit
  # ======================================================= #
  if(method == "rounder"){

    if(verbose){message("---- rounding process ----")}

    # quickly inserted this patch - may be a better way around
    #if(dt > 60){
    #  dt2 = dt / 60
    #  Unit2 = "mins"
    #} else{
    #  dt2 = dt
    #  Unit2 = Unit
    #}

    #new2 <- data_tb %>%
    #  group_by(TagID) %>%
    #  mutate(difftime = as.vector(difftime(DateTime, lag(DateTime), units = 'secs'))) %>% # retain this to check from original data if any data are above the rate we are downgrading too
    #  rename(DateTime_orig = "DateTime") %>%
    #  mutate(DateTime = round_date(DateTime_orig, unit = period(num = dt2, units = Unit2)),
    #         diff = abs(as.vector(difftime(DateTime_orig, DateTime, units = "secs")))
    #  ) %>% group_by(TagID, DateTime) %>% slice_min(diff) %>%
    #  group_by(TagID) %>%
    #  mutate(dt = as.vector(difftime(lead(DateTime_orig), DateTime_orig, units = 'secs')) ) # recalculate dt#

    dt_secs = dt

    if(Unit == "mins"){
      dt_secs <- dt * 60
    }
    if(Unit == "hours"){
      dt_secs <- dt * 60 * 60
    }
    if(Unit == "days"){
      dt_secs <- dt * 60 * 60 * 24
    }

    new2 <- data_tb %>%
      ungroup() %>%  # remove any prior grouping
      mutate(
        difftime = as.vector(difftime(DateTime, lag(DateTime), units = 'secs')), # retain this to check from original data if any data are above the rate we are downgrading too
        DateTime_orig = DateTime,
        RoundedTime = as.POSIXct(dt_sec * round(as.numeric(DateTime_orig) / dt_sec),
                                 origin = "1970-01-01",
                                 tz = tz(DateTime_orig)),
        diff = abs(as.numeric(DateTime_orig) - as.numeric(RoundedTime))
      ) %>%
      group_by(TagID, RoundedTime) %>%
      slice_min(diff, with_ties = FALSE) %>%
      ungroup() %>%     # optional, to return ungrouped
      dplyr::select(-RoundedTime) %>%
      group_by(TagID) %>%
      mutate(dt = as.vector(difftime(lead(DateTime_orig), DateTime_orig, units = 'secs')) ) # recalculate dt#

    # sorry this is complex
    new2 <- new2 %>% mutate(dt = as.vector(difftime(DateTime_orig, lag(DateTime_orig), units = 'secs')) )
    new2 <- new2[is.na(new2$dt) | new2$dt > rateL,]
    new2 <- new2 %>% mutate(dt = as.vector(difftime(lead(DateTime_orig), DateTime_orig, units = 'secs')) )

    # used below in the process for the sequence (just so code will flow)
    r1 <- data.table::data.table(data_tb, key = c("TagID","DateTime"))  # raw data
    r1$DateTime_orig <- r1$DateTime

  }

  # ======================================================= #
  # 2. Sequential 'rolling' in data.table
  # ======================================================= #
  if(method == "sequencer"){

    if(verbose){message("---- sequencer process ----")}
    # get sequential starts and end and grids to snap to per bird/gapsec whatever
    # this is probably going to be most efficient within the calendar year, i.e. have an additional YEAR
    # element to the sub-sample - this is because CPF data will be extrapolated for single birds over winter periods that may not be of interest?
    # Note here, we could use something specific in the dataset to group by e.g. if migrations, spanning across Jan 1st
    # as year is not necessarily ecologically appropriate always

    data_tb$year <- as.factor(lubridate::year(data_tb$DateTime))
    data_seq = data_tb %>% group_by(TagID, year) %>%
      summarise(date_start = min(DateTime), date_end = max(DateTime), .groups = "keep")  %>%
      #group_by(TagID, year) %>%
      do( data.frame(., DateTime= seq(.$date_start, # macro variables in the do loop
                                      .$date_end+dt, by =  paste(dt, Unit))) ) %>% ungroup() %>%
      dplyr::select(-c(date_start,date_end, year))

    # then rolling to the nearest seq DateTime per TagID:
    data_tb <- data_tb %>%  dplyr::select(-year)

    r1 <- data.table::data.table(data_tb, key = c("TagID","DateTime"))  # raw data
    r1$difftime = as.vector(difftime(r1$DateTime, lag(r1$DateTime), units = 'secs')) # retain this to check from original data if any data are above the rate we are downgrading too

    r1$DateTime_orig <- r1$DateTime
    r2 <- data.table::data.table(data_seq, key = c("TagID","DateTime"))  # interpolated data
    new <- r1[r2 , roll = "nearest", allow.cartesian=TRUE ] #list(TagID, DateTime, DateTime_orig),

    # get difference of DateTime to the original Datetime
    new$diff <- as.vector(difftime(new$DateTime_orig,new$DateTime,units = 'secs'))
    new$diff <- ifelse(new$diff < 0, new$diff *-1,new$diff)
    #new$rn <- 1:length(new$DateTime)

    # which min, taking the minimum of the difference for sub-sample
    new2 <- data.table::setDT(new)[, .SD[which.min(diff)], by = .(DateTime_orig)] #[, ..select_cols]

    # alignment with rounding output and recalculating dt
    data.table::setkey(new2, TagID, DateTime_orig)
    new2[,dt:= (difftime(DateTime_orig, data.table::shift(DateTime_orig,1,type="lag"),unit = "secs")), by = "TagID"]
    new2$dt <- c(as.vector(new2$dt)[-1],NA)
    new2$dt <- ifelse(is.na(new2$dt),0,new2$dt)

    new2 <- new2 %>% mutate(dt = as.vector(difftime(DateTime_orig, lag(DateTime_orig), units = 'secs')) ) # recalculate dt
    new2 <- new2[is.na(new2$dt) | new2$dt > rateL,] # was difftime

    # alignment with rounding output
    new2 <- new2 %>% mutate(difftime = dt) %>%
      mutate(dt = as.vector(difftime(lead(DateTime_orig), DateTime_orig, units = 'secs')) ) # recalculate dt

    new2 <- tibble(new2)

  }

  ##########################################
  if(!method %in% c("boot_sequencer", "boot_rate")){

    ###############################
    # first row difftime make same as second lag for rateU filter, also complexity for first valid dt!
    new2$difftime[1] <- new2$difftime[2]

    # end dt replace with previous (thisis for search windows below)
    new2 = new2 %>% group_by(TagID) %>%
      mutate(rn = row_number(),
             dt = ifelse(rn == max(rn),lag(dt),dt),
             difftime = ifelse(dt < difftime, dt, difftime) )

    #################################################################################################################
    # look for potentially missing fixes from the 'too harsh' sub-sampling
    # Base R approach used originally. This looked for any violations of the dt rule
    # working with the filtered dt values that are greater than dt, check for existence of other valid fixes
    # if there were situations as in the above where the re-estimated dt was greater than the
    # maximum threshold we are allowing, e.g. 300 seconds plus a tolerance (0.4 default) = 420 s
    # then if in the original fix there is a fix than is within the re-estimated dt gap, here being 576 s
    # check to see if it is OK to include the additional fix as part of the sequence
    # flipping between tidyverse and data.table here

    if(verbose){message("---- overly strict point removal checker ----")}

    new2. <- new2 %>% group_by(TagID) %>% arrange(TagID, DateTime_orig) %>%
      mutate(DateTime_orig2 = DateTime_orig + dt) %>%
      dplyr::select(-c(diff, dt, DateTime)) %>% #group_by(TagID, DateTime_orig)
      rename(st = "DateTime_orig", en = "DateTime_orig2") %>%
      dplyr::select(c(TagID, st, en))

    # to slim this down - look at this point for number of fixes between these dates
    # i.e. any fixes between these two values consider for adding back into the data
    # left_join in - this will be bigger as there will be starts and ends doubling up of rows
    st_en <- new2. %>%  tidyr::pivot_longer(cols = c(`st`,`en`), names_to = "st_en", values_to = "DateTime")

    # ------------------- #
    # extra step here as we do not want to reinsert the fixes rightly filtered out
    # so use rateL to drop fixes with faster dts?
    # the issue we are getting at here is for data already on the rate we are filtering too

    dt_ <- dt
    r1a <- r1 %>% mutate(dt = as.vector(difftime(DateTime, lag(DateTime), units = 'secs')) ) %>%
      relocate(dt, .after = DateTime) %>% group_by(TagID) %>%
      mutate(rn = row_number(),
             dt = ifelse(rn == min(rn),lead(dt),dt),
             dt = ifelse(is.na(dt), dt_, dt)) %>% # for nrows of 1 per animal the above lead will not work, i.e. no second row, so just take dt supplied to function (dt_ here)
      dplyr::select(-rn) %>% filter(dt >= rateL)

    # ------------------ #

    # drop any fixes > rateU as at the start and end it can include fixes - see if they are arphaned with gapsec process
    r1a = r1a %>% gap_section(GAP = rateU, tol = 0, drop = TRUE, verbose = FALSE) %>% tibble()

    if(!exists("gapsec", data)){r1a <- r1a %>%  dplyr::select(-c(gapsec))}
    if(!exists("gap", data)){r1a <- r1a %>%  dplyr::select(-c(gap))}

    # ------------------- #
    # are there any valid fixes to actually check to add in?

    if(nrow(r1a) > 0){

      # better to rbind as now r1a is slimmed down so may not have the start or end to match to in prev left_join
      # first remove the 'negative space' between the starts and ends of sections of data where dt was VALID i.e. < dt threshold

      part1 = r1a %>%  dplyr::select(TagID, DateTime) %>% mutate(st_en = NA) %>% relocate(st_en, .after = TagID) %>%
        rbind(.,st_en) %>% arrange(TagID, DateTime) %>%
        filter( !(is.na(st_en) & lead(DateTime) == DateTime)  ) %>%  # firstly, if there is a fix MATCHING the start or end, then this NA row we can drop first, i.e. no fixes in between the boundaries

        mutate(cond0 = if_else(st_en == "st" & is.na(lead(st_en)), row_number(),
                               ifelse(st_en == "en" & is.na(lag(st_en)), row_number(),
                                      ifelse(st_en == "st" & lead(st_en) == "en",row_number(),NA)))
        ) %>%
        tidyr::fill(cond0) %>%
        mutate(cond0 = ifelse(is.na(cond0) & row_number() == 1, lead(cond0), cond0),
               cond0 = ifelse(is.na(lag(st_en)), lag(cond0), cond0),
               Sec = consecutive_id(cond0)
        ) %>%
        dplyr::select(-cond0)


      # count the sections - if just a start and an end, then no extra data was between the st and end
      part2 = part1 %>% group_by(TagID, Sec) %>% summarise(n = n(), .groups="keep") %>% filter(n > 2) # part1 data LBBGU WA six birds, left 1350 rows (1538 for the rounded time version)

      # then only way I can think of doing this is to add back into the full dataset, recalculate dt, and if
      # any remain below the threshold then retain them! Most should just drop out again
      part3 = left_join(part2, part1, by = c("TagID", "Sec")) %>% ungroup() %>% dplyr::select(-c(n, Sec, st_en)) # keeping only the sections with in between points
      part3 <- part3 %>% group_by(TagID) %>% filter(!duplicated(DateTime)) %>% ungroup()

      # add this back to the sub-sampled data
      # need to switch the date_time orig and DateTime columns as relabelled for rolling above
      fin <- new2 %>% dplyr::select(-c(DateTime, diff)) %>% rename(DateTime = "DateTime_orig")

      # lookup those rows in data
      fin_extra <- right_join(data, part3, by = c("TagID", "DateTime")) %>%
        mutate(dt = as.vector(difftime(lead(DateTime), DateTime, units = 'secs')) ) %>%
        group_by(TagID)

      # recalculate dt, drop out extra rows from simple filter
      fin2 = rbind(fin, fin_extra) %>% group_by(TagID) %>% arrange(TagID, DateTime) %>%
        mutate(dt = as.vector(difftime(DateTime, lag(DateTime), units = 'secs')) ) %>%
        filter(!duplicated(DateTime)) %>%
        mutate(dt = as.vector(difftime(DateTime, lag(DateTime), units = 'secs')) )

      # ------------------ #
      # the LAST fix WILL ALWAYS be dropped if you don't apply this dirty work around
      dt_ = dt
      fin2 = fin2 %>% group_by(TagID) %>%
        mutate(rn = row_number(),
               dt = ifelse(rn == min(rn),lead(dt),dt),
               dt = ifelse(is.na(dt), dt_, dt))
      # ------------------ #

      fin2 <- fin2 %>% filter(dt > 0) # prob unneccessary

    } else{

      fin2 <- new2 %>% dplyr::select(-DateTime, diff) %>% rename(DateTime = "DateTime_orig")
    }

    fin2 <- fin2 %>% gap_section(GAP = rateL, tol = 0, drop = FALSE, verbose = FALSE)

    if(!exists("gapsec", data)){fin2 <- fin2 %>% dplyr::select(-c(gapsec))}
    if(!exists("gap", data)){fin2 <- fin2 %>% dplyr::select(-c(gap))}

    fin2 <- fin2 %>%
      mutate(dt = as.vector(difftime(DateTime, lag(DateTime), units = 'secs')) ) %>%
      arrange(TagID, DateTime) %>% tibble()

    dt_ = dt
    fin2 = fin2 %>% group_by(TagID) %>%
      mutate(rn = row_number(),
             dt = ifelse(rn == min(rn),lead(dt),dt),
             dt = ifelse(is.na(dt), dt_, dt))

    # STILL may be quicker fixes than rateL (e.g. dt = 300 tol = 0.2 from rounder process inserted BACK IN a fix in my tests)
    fin2 <- fin2[is.na(fin2$dt) | fin2$dt >= rateL,]

    if(drop_coarser){

      # drop any fixes > rateU
      fin2 = fin2  %>% gap_section(GAP = rateU, tol = 0, drop = TRUE, verbose = FALSE) %>% tibble()

      if(!exists("gapsec", data)){fin2 <- fin2 %>% dplyr::select(-c(gapsec))}
      if(!exists("gap", data)){fin2 <- fin2 %>% dplyr::select(-c(gap))}

    }

    data2 <- fin2

  }

  # -------------------------------------------------------------------------------------------------------- #
  # 3. Addtional approach for boot sequencing

  # Worker function for checking violations in rateL after sampling under strict = FALSE condition
  repair_rateL <- function(df_tv, picked_rn, rateL, Verbose = verbose) {
    verbose <- Verbose
    if(verbose){
      message("Checking for rateL violations...")
    }

    # Subset picked rows
    check <- df_tv[df_tv$rn %in% picked_rn, ]

    # Compute time differences
    check2 <- check %>%
      group_by(TagID, !!!syms(unique(by))) %>%
      mutate(dt = as.vector(difftime(lead(DateTime), DateTime, units = 'secs')),
             tooquick = ifelse(dt < rateL, 1, 0),
             tooquick = ifelse(is.na(tooquick), 0, tooquick))

    too_quick <- check2[check2$tooquick == 1, ]
    oks <- check2[check2$tooquick == 0, ]

    if(nrow(too_quick) > 0){
      if(verbose){message(nrow(too_quick), " rateL violations found, attempting repair...")}

      # Identify segments needing repair
      segs <- too_quick %>%
        mutate(dt = as.vector(difftime(lead(DateTime), DateTime, units = 'secs'))) %>%
        gap_section(., GAP = rateL, tol = 0, verbose = FALSE) %>%
        group_by(TagID, !!!syms(unique(by)), gapsec) %>%
        mutate(rn2 = row_number()) %>%
        filter(rn2 == 1)

      df2 <- data.table(oks)
      df1 <- data.table(segs)

      # Nearest previous
      df1[, prev_idx := df2[.SD, on = "DateTime", roll = Inf, which = TRUE]]
      df1[, prev_time := df2$DateTime[prev_idx]]

      # Nearest next
      df1[, next_idx := df2[.SD, on = "DateTime", roll = -Inf, which = TRUE]]
      df1[, next_time := df2$DateTime[next_idx]]

      df_raw <- as.data.table(df_tv)

      # Loop through each hole to insert as many valid points as possible
      reinsert_points <- lapply(1:nrow(df1), function(i){
        prev_time_i <- df1$prev_time[i]
        next_time_i <- df1$next_time[i]

        # Candidate points in the hole (chronological order)
        candidates <- df_raw[DateTime > prev_time_i & DateTime < next_time_i, ]
        if(nrow(candidates) == 0) return(NULL)

        inserted <- integer(0)
        last_time <- prev_time_i

        # Keep adding valid points sequentially until no more fit
        candidate_rn <- candidates$rn
        candidate_dt <- candidates$DateTime

        idx_try <- 1
        while(idx_try <= length(candidate_rn)){
          t_candidate <- candidate_dt[idx_try]
          dt_from_last <- as.numeric(difftime(t_candidate, last_time, units = "secs"))
          dt_to_next <- as.numeric(difftime(next_time_i, t_candidate, units = "secs"))

          if(dt_from_last >= rateL & dt_to_next >= rateL){
            inserted <- c(inserted, candidate_rn[idx_try])
            last_time <- t_candidate
          }
          idx_try <- idx_try + 1
        }

        df_raw[df_raw$rn %in% inserted, ]
      })

      reinserted <- bind_rows(reinsert_points)

      if(verbose){
        message(nrow(reinserted), " extra valid fixes inserted, meeting rateL condition (", rateL, " s)")
      }

      # Return combined picked rows with reinserted points
      return(sort(c(oks$rn, reinserted$rn)))

    } else {
      if(verbose){message("No rateL violations found")}
      return(sort(oks$rn))
    }
  }

  # ======================================================= #
  # 3a boot sequencer method, TagID/VAR/gapsec/segments
  # ======================================================= #
  if(method == "boot_sequencer"){

    message("***********************************************************************")
    message("************************** Boot sequencer *****************************")
    message("***********************************************************************")
    message("~~~~~~~~~~~ Running ", n_boot, " boots ~~~~~~~~~~~")

    if(!is.null(seed)) {
      set.seed(seed)
    }

    # local copy + rn
    df <- data %>% arrange(TagID, DateTime) %>% group_by(TagID) %>% mutate(rn = row_number())

    # ------------------------------------------- #
    # VAR HANDLING
    if(is.null(by)){
      use_var <- FALSE
    } else{
      if(!by %in% names(df)) stop("`by` column not found in data.")
      df$var <- as.factor(df[[by]])
      use_var <- TRUE
    }
    # ----------------------- #

    tag_ids <- unique(df$TagID)

    # Compute gapsecs once
    # always uses the dt and tol values for segmenting, but rateL and rateU can be specified separately for random controls within segments
    df <- gap_section(df, GAP = dt, tol = tol, verbose = FALSE)

    # Progress estimation
    if(verbose){
      if(use_var){
        # average var count per tag
        avg_vars <- max(1, length(unique(df$var)) / length(tag_ids))
      } else{
        avg_vars <- 1
      }
      total_steps <- length(tag_ids) * avg_vars * n_boot
      pb <- progress_estimated2(total_steps)
    }

    boots_all <- vector("list", length(tag_ids))
    names(boots_all) <- tag_ids

    # ------------------------------------------- #
    # TagID
    # ------------------------------------------- #
    #ti = 7
    for(ti in seq_along(tag_ids)){

      tag <- tag_ids[ti]
      df_tag <- df %>% filter(TagID == tag)

      # Determine var levels for this tag, if used
      if(use_var){
        vars_tag <- unique(df_tag$var)
      } else{
        vars_tag <- "NOVAR"
        df_tag$var <- "NOVAR"
      }

      var_list <- vector("list", length(vars_tag))
      names(var_list) <- as.character(vars_tag)

      # ------------------------------------------- #
      # VAR LOOP (or a single NOVAR)
      # ------------------------------------------- #
      # vi <- 1
      for(vi in seq_along(vars_tag)){

        v <- vars_tag[vi]
        df_tv <- df_tag %>% filter(var == v)

        #data[data$TagID %in% "5377_stack_2",]

        gapsecs <- sort(unique(df_tv$gapsec))
        boot_list <- vector("list", n_boot)

        #df_tv[df_tv$gapsec %in% 150,]

        # ------------------------------------------- #
        # BOOTS
        # ------------------------------------------- #
        for(b in seq_len(n_boot)){

          picked_rows <- integer(0)
          last_time <- NA

          # ------------------------------------------- #
          # GAP LOOP
          # ------------------------------------------- #
          #gap = 1
          for(gap in gapsecs){

            #df_tv[df_tv$gapsec %in% gap,]

            gap_rows <- which(df_tv$gapsec == gap)
            gapped <- data.frame(df_tv[df_tv$gapsec %in% gap,])

            if(length(gap_rows) == 0) next

            times <- df_tv$DateTime[gap_rows]

            # Define segment starts
            seg_starts <- seq(min(times), max(times), by = paste(dt, Unit))

            # ------------------------------------------- #
            # SEGMENT LOOP
            # ------------------------------------------- #
            #i = 1
            for(i in seq_along(seg_starts)){
              #message(seg_starts[i])
              # segment indices relative to gap_rows / times
              times_id <- which(times >= seg_starts[i] & times < seg_starts[i] + dt)
              if(length(times_id) == 0) next

              # option here to IGNORE the NA condition at the start of a run

              if(blind){
                last_time = NA
              } else{
                if(!strict){
                  if(i == 1){last_time = NA}
                }
              }


              # Filter by rateL using relative indices
              #if(!is.na(last_time)){
              #  time_check = as.numeric(times[times_id] - last_time, units = "secs")
              #  valid_idx_rel <- times_id[time_check >= rateL & time_check <= rateU]
              #  if(length(valid_idx_rel) == 0) next   # skip segment but DO NOT reset last_time
              #} else {
              #  valid_idx_rel <- times_id
              #}

              if(!is.na(last_time)){
                time_check = as.numeric(times[times_id] - last_time, units = "secs")
                valid_time_ids <- times_id[time_check >= rateL & time_check <= rateU] # note here we check for last point being at least rateL AND and fixes > rateU
              } else{
                time_check = as.numeric(times[times_id] - seg_starts[i], units = "secs") # here we use seg_times[i] as essentially the last_time; the rateU check is pointless really as it is tied to the segment
                valid_time_ids <- times_id[time_check <= rateU] # as above this is just for safety and rateU should always be satisfied for the segment
              }

              # here, if there is no valid fix, we have to forget the previous remembered time, because
              # otherwise the sampler will always find nothing valid within the UPPER rate.
              if(length(valid_time_ids) == 0) {
                #message("No times")
                last_time = NA
                next
              }

              #now we select these specific timeIDs as rowids in the full dataset
              samples <- gapped[valid_time_ids,]$rn
              pick <- if(length(samples) == 1) samples else sample(samples, 1)

              picked_rows <- c(picked_rows, pick)
              last_time <- df_tag$DateTime[pick] # this has to be from the full dataset as the rn's are relative to the entire tagID



              # Map relative indices to absolute row numbers
              #seg_idx <- gap_rows[valid_idx_rel]

              #if(length(seg_idx) == 0) next  # safety check

              # Randomly pick one fix from the valid ones
              #pick_rel <- if(length(valid_idx_rel) == 1) valid_idx_rel else sample(valid_idx_rel, 1)
              #pick <- if(length(seg_idx) == 1) seg_idx else sample(seg_idx, 1)

              #print(paste0("THIS IS MY PICK: ", pick, " from ", seg_starts[i]))

              #df_tv[pick,]

              # Map back to absolute row numbers in df_tv
              #pick <- gap_rows[pick_rel]
              #picked_rows <- c(picked_rows, pick)

              # Update last_time using absolute pick
              #last_time <- df_tv$DateTime[pick]
            }
          }

          # convert to df rn values
          #picked_rn <- df_tv$rn[picked_rows]

          # ------------------------ #
          # coarser fixes

          #df_tag2 = tibble(simplify(gap_section(df_tag, GAP = dt, tol = tol, verbose = FALSE), keep = c("gapsec","dt","rn")))
          #ogap = as.vector(which(table(df_tag2$gapsec) == 1))
          #
          ## get rns of these
          #coarse_rn = df_tag2[df_tag2$gapsec %in% ogap,]$rn
          #picked_pl_coarse <- c(picked_rn, coarse_rn)
          #picked_pl_coarse <- sort(unique(picked_pl_coarse))

          if(nrow(df_tv) > 1){
            df_all <- gap_section(df_tv, GAP = dt, tol = tol, verbose = FALSE)
          } else {
            df_all <- df_tv
            df_all$gapsec <- 1   # assign a default gapsec
          }

          singleton_rn <- df_all %>% group_by(gapsec) %>% filter(n() == 1) %>% pull(rn)
          picked_rn <- sort(unique(c(df_tv$rn[picked_rows], singleton_rn)))

          # ------------------------ #
          # rateL violations under strict = FALSE

          if(strict){
            check = df_tv[df_tv$rn %in% picked_rn,]
            check = check %>%
              group_by(TagID, !!!syms(unique(by))) %>%
              mutate(dt = as.vector(difftime(lead(DateTime), DateTime, units = 'secs')),
                     tooquick = ifelse(dt < rateL,1,0),
                     tooquick = ifelse(is.na(tooquick),0,tooquick))
            # drop the rows that are violating rateL
            picked_rn = check[check$tooquick == 0,]$rn
            if(verbose){message("Strict removal checks took out ", length(df_tv$rn) - length(picked_rn), " fixes violating rateL (", rateL, " s)")}
          } else{
            ### try an insert others back in.
            picked_rn <- repair_rateL(df_tv, picked_rn, rateL, Verbose = verbose_repair)
          }

          devcheck = TRUE
          if(devcheck){
            check = df_tv[df_tv$rn %in% picked_rn,]
            check2 = check %>%
              group_by(TagID, !!!syms(unique(by))) %>%
              mutate(dt = as.vector(difftime(lead(DateTime), DateTime, units = 'secs')),
                     tooquick = ifelse(dt < rateL,1,0))
            #RakeRvis(Track(check2))
            #check2[check2$tooquick == 1,] used for a check - no extra faster rates remaining after insertion
            if(!all(is.na(check2[check2$tooquick == 1,]))){
              warning("Faster dts remaining, dev check")
            }
          }

          boot_list[[b]] <- list(picked = picked_rn, coarser = singleton_rn)

          if (verbose) pb$tick()
        } # end boot

        var_list[[vi]] <- boot_list
      } # end var level

      boots_all[[ti]] <- var_list
    }
    data2 <- data
  }

  # ======================================================= #
  # 3a boot rate method, TagID/VAR/rate/gapsec/segments
  # ======================================================= #
  if(method == "boot_rate"){

    message("***********************************************************************")
    message("************************* Boot rate sequencer *************************")
    message("***********************************************************************")
    message("~~~~~~~~~~~ Running ", n_boot, " boots ~~~~~~~~~~~")

    if(!is.null(seed)) {
      set.seed(seed)
    }

    # local copy + rn
    df <- data %>% arrange(TagID, DateTime) %>% group_by(TagID) %>% mutate(rn = row_number())

    # ------------------------------------------- #
    # VAR HANDLING
    if(is.null(by)){
      use_var <- FALSE
    } else{
      if(!by %in% names(df)) stop("`by` column not found in data.")
      df$var <- as.factor(df[[by]])
      use_var <- TRUE
    }
    # ----------------------- #
    #dt = 300

    tag_ids <- unique(df$TagID)

    # Progress estimation
    if(verbose){
      if(use_var){
        # average var count per tag
        avg_vars <- max(1, length(unique(df$var)) / length(tag_ids))
      } else{
        avg_vars <- 1
      }
      total_steps <- length(tag_ids) * avg_vars * n_boot
      pb <- progress_estimated2(total_steps)
    }

    # Compute gapsecs once
    #df <- gap_section(df, GAP = dt, tol = tol, verbose = FALSE)

    boots_all <- vector("list", length(tag_ids))
    names(boots_all) <- tag_ids

    #tag = "4032_stack_1"

    for(tag in tag_ids){
      #message(tag)
      df_tag <- df %>% filter(TagID == tag)

      # assign rates to data.frame
      df_tag <- assign_rates(df_tag, out = NULL, verbose=FALSE)  # use defaults for now

      # get list of rates available:
      rates = as.vector(na.omit(unique(df_tag$rate)))

      # makes sure no "+"
      #rates = gsub("[[:punct:]]", "", rates)  # no libraries needed

      var_list <- list()

      #v = "NOVAR"
      for(v in if(is.null(by)) "NOVAR" else unique(df_tag[[by]])){
        #message(v)

        df_tv <- if(v == "NOVAR") df_tag else df_tag %>% filter(.data[[by]] == v)
        boot_list <- vector("list", n_boot)

        # b = 1
        for(b in seq_len(n_boot)){
          #message("boot: ",n_boot[b])

          picked_rows <- integer(0)
          last_time <- NA

          # Loop over rates
          # r = 1
          for(r in 1:length(rates)){
            #message("rate: ",rates[r])
            df_rate <- df_tv[df_tv$rate %in% rates[r], ]

            if(nrow(df_rate) == 1){
              # Only one row, pick it automatically (also covered though under coarser orphaned fixes)
              pick <- 1
              picked_rows <- c(picked_rows, df_rate$rn[pick])  # use absolute row number
              #last_time <- df_rate$DateTime[pick]
              last_time <- NA
              next  # go to next gapsec / segment
            }
            df_rate <- gap_section(df_rate, GAP = dt, tol = tol, verbose = FALSE)

            # compute gap sections
            gapsecs <- sort(unique(df_rate$gapsec))

            # Loop over gap sections
            # g = 1
            for(g in gapsecs){
              #message("gapsec: ",g)
              #gap_rows <- which(df_rate$gapsec == g)

              # gapsection numbers 1:n gaps for an ANIMAL - if there are multiple levels within an
              # animal, e.g. by variable or then rate splits as well (!) this eill be 1:n per sub-group
              # so yes if renaming the gapsec per slice ok, but we have to go back to the original 'rn' for indexing
              gapped <- data.frame(df_rate[df_rate$gapsec %in% g,])

              gap_rows <- which(df_rate$gapsec == g)
              times <- df_rate$DateTime[gap_rows]

              # Define segment starts
              seg_starts <- seq(min(times), max(times), by = dt)

              # Loop over segments
              #i = 1
              for(i in seq_along(seg_starts)){
                times_id <- which(times >= seg_starts[i] & times < seg_starts[i] + dt) # index related to times
                if(length(times_id) == 0) next

                # within the sequence within the rate we need to NOT consider any last time
                # from a previous rate block

                # But.....note if going from a finer rate to a coarser rate, we could still end up with the
                # rateL being violated. Needs a capture check at the end - going chronological again will
                # end up with the same fixed sequence we are trying to avoid in the rate_boot

                if(blind){
                  last_time = NA
                } else{
                  if(i == 1){last_time = NA}
                }

                # ------------------------------------------- #
                # Points VALID to pick within this segment
                # First check point is far enough into segment from the last time picked in this rate sequence
                # If this is the first point in the sequence, we allow a normal check without last_time]
                # Then check filter by rateL and rateU within the time segment for valid fixes

                if(!is.na(last_time)){
                  time_check = as.numeric(times[times_id] - last_time, units = "secs")
                  valid_time_ids <- times_id[time_check >= rateL & time_check <= rateU] # note here we check for last point being at least rateL AND and fixes > rateU
                } else{
                  time_check = as.numeric(times[times_id] - seg_starts[i], units = "secs") # here we use seg_times[i] as essentially the last_time; the rateU check is pointless really as it is tied to the segment
                  valid_time_ids <- times_id[time_check <= rateU] # as above this is just for safety and rateU should always be satisfied for the segment
                }

                if(length(valid_time_ids) == 0) {
                  #message("No times")
                  last_time = NA
                  next
                }

                #now we select these specific timeIDs as rowids in the full dataset
                samples <- gapped[valid_time_ids,]$rn
                pick <- if(length(samples) == 1) samples else sample(samples, 1)

                picked_rows <- c(picked_rows, pick)
                last_time <- df_tag$DateTime[pick] # this has to be from the full dataset as the rn's are relative to the entire tagID
              }
            }

          }

          # Coarser fixes: retain any singletons in the rate blocks

          if(nrow(df_tv) > 1){
            df_rate_all <- gap_section(df_tv, GAP = dt, tol = tol, verbose = FALSE)
          } else {
            df_rate_all <- df_tv
            df_rate_all$gapsec <- 1   # assign a default gapsec
          }

          singleton_rn <- df_rate_all %>% group_by(gapsec) %>% filter(n() == 1) %>% pull(rn)
          picked_rn <- sort(unique(c(df_tv$rn[picked_rows], singleton_rn)))

          # ---------------------------------------------------------- #
          # Check here for violations of rateL

          if(strict){
            check = df_tv[df_tv$rn %in% picked_rn,]
            check = check %>%
              group_by(TagID, !!!syms(unique(by))) %>%
              mutate(dt = as.vector(difftime(lead(DateTime), DateTime, units = 'secs')),
                     tooquick = ifelse(dt < rateL,1,0),
                     tooquick = ifelse(is.na(tooquick),0,tooquick))

            # drop the rows that are violating rateL
            picked_rn = check[check$tooquick == 0,]$rn

            if(verbose){message("Strict removal checks took out ", length(df_tv$rn) - length(picked_rn), " fixes violating rateL (", rateL, " s)")}

          } else{

            ### try an insert others back in.
            picked_rn <- repair_rateL(df_tv, picked_rn, rateL, Verbose = verbose_repair)

          }

          devcheck = TRUE
          if(devcheck){
            check = df_tv[df_tv$rn %in% picked_rn,]
            check2 = check %>%
              group_by(TagID, !!!syms(unique(by))) %>%
              mutate(dt = as.vector(difftime(lead(DateTime), DateTime, units = 'secs')),
                     tooquick = ifelse(dt < rateL,1,0))


            #RakeRvis(Track(check2))
            #check2[check2$tooquick == 1,] used for a check - no extra faster rates remaining after insertion

            if(!all(is.na(check2[check2$tooquick == 1,]))){
              warning("Faster dts remaining, dev check")
            }
          }
          # ---------------------------------------------------------- #

          boot_list[[b]] <- list(picked = picked_rn, coarser = singleton_rn)

          if (verbose) pb$tick()
        }

        var_list[[v]] <- boot_list
      }


      boots_all[[tag]] <- var_list
    } ###

    data2 <- data
  }

  # ======================================================= #
  #### Finalisation
  # ======================================================= #
  data2 <- data2 %>% group_by(TagID)

  # set gapsection back to default settings if exist
  attr_test = attr(attr(data2, "general"), "GAP") # these are not working at the moment - likely attributes are lost from tbl conversion!!

  if(nrow(data2) > 0){
    if(!is.null(attr_test)){
      data2 <- gap_section(data2, GAP = as.vector(attr_test), tol = 0, verbose = FALSE, drop = FALSE)
    } else{
      data2 <- gap_section(data2, GAP = 60*60*8, tol = 0, verbose = FALSE, drop = FALSE)
    }

    if(!exists("gap", data)){data2 <- data2 %>% dplyr::select(-gap) }
    if(!exists("gapsec", data)){data2 <- data2 %>% dplyr::select(-gapsec) }

  }

  if(nrow(data2) > 2){
    data2 <- structure(.Data = data2, class = c("Track", "grouped_df", "tbl_df","tbl","data.frame"))
  }

  # ======================================================= #
  # assign attributes that may have been present at the start
  data2 <- give_attributes(data2, attr_list)

  # ======================================================= #
  #### retain attributes of the choices made
  if(is.null(attr(data2, "sub_samp") )){
    attr(data2, "sub_samp") <- "sub_samp"
  }

  # sub_attributes for arguments
  attr(attr(data2, "sub_samp"), "method") <- method
  attr(attr(data2, "sub_samp"), "dt") <- dt
  attr(attr(data2, "sub_samp"), "Unit") <- Unit
  attr(attr(data2, "sub_samp"), "tol") <- tol
  attr(attr(data2, "sub_samp"), "u_tol") <- u_tol
  attr(attr(data2, "sub_samp"), "l_tol") <- l_tol
  attr(attr(data2, "sub_samp"), "by") <- by

  if(method %in% c("boot_sequencer", "boot_rate")){

    attr(data2, "sub_samp_boots") <- list(
      method = "boot_sequencer",
      boots = boots_all,
      dt = dt, tol = tol,
      rateL = rateL, rateU = rateU,
      by = by,
      use_var = use_var
    )
  }

  return(data2)

}

#' @rdname sub_samp
#' @export
retrieve_boots <- function(data, animal = NULL, include_coarser = FALSE, verbose = TRUE) {

  # Check for sub_samp_boots
  if (is.null(attr(data, "sub_samp_boots"))) {
    stop("Data does not have a 'sub_samp_boots' attribute. Run sub_samp() first.")
  }

  sub_samp <- attr(data, "sub_samp_boots")
  boots_attr <- sub_samp$boots
  by_var_name <- sub_samp$by

  # Animals to process
  anims <- unique(data$TagID)
  if (!is.null(animal)) {
    if (!(animal %in% anims)) stop("Animal '", animal, "' not found in TagID")
    anims <- animal
  }

  # Verbose
  if (!is.null(by_var_name) && by_var_name != "") {
    all_levels <- unique(unlist(lapply(boots_attr, names)))
    if (verbose) message("Boots generated by '", by_var_name, "' over levels: ", paste(all_levels, collapse = ", "))
  } else if (verbose) {
    message("No 'by' variable used; NOVAR sampling only.")
  }

  if (verbose) {
    message("Include coarser fixes below resampling rate? ", ifelse(include_coarser, "YES", "NO"))
  }

  # Ensure absolute row numbers exist
  data <- data %>% arrange(TagID, DateTime) %>% mutate(rn = row_number())

  final_list <- list()

  #tag = "506_stack_1"
  for (tag in anims) {
    tag_df <- data[data$TagID == tag, ]
    tag_boots <- boots_attr[[tag]]

    boot_list <- list()

    # Determine if by-variable used
    var_names <- names(tag_boots)
    by_used <- !(length(var_names) == 1 && var_names == "NOVAR")

    if (!by_used) {
      # NOVAR mode
      nboots <- length(tag_boots$NOVAR)
      for (b in seq_len(nboots)) {
        inds <- tag_boots$NOVAR[[b]]$picked
        if (!include_coarser) inds <- setdiff(inds, tag_boots$NOVAR[[b]]$coarser)

        # Use absolute row numbers directly
        boot_df <- tag_df[inds, , drop = FALSE]
        boot_df$boot <- b
        boot_list[[b]] <- as_tibble(boot_df)
      }
    } else {
      # BY-variable mode
      for (var_val in var_names) {
        var_boots <- tag_boots[[var_val]]
        nboots <- length(var_boots)

        for (b in seq_len(nboots)) {
          inds <- var_boots[[b]]$picked
          if (!include_coarser) inds <- setdiff(inds, var_boots[[b]]$coarser)

          # Pick exact rows
          boot_df <- tag_df[inds, , drop = FALSE]
          boot_df$boot <- b
          boot_list <- append(boot_list, list(as_tibble(boot_df)))
        }
      }
    }

    final_list[[tag]] <- bind_rows(boot_list)
  }

  result <- bind_rows(final_list)
  rownames(result) <- NULL
  return(result)
}












