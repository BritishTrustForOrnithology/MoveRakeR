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
#' \code{sub_samp} is based on original code from Emiel van Loon at the University of Amsterdam.
#'
#' Three options are available in \code{sub_samp} for subsampling: "sequencer", "rounder", or "boot_sequencer", see
#' the argument descriptions for more details. These allow subsampling to the nearest time unit based on
#' either the start time of the animal (sequencer) or the nearest whole time unit (rounder). The boot method
#' is a beta method that has been developed to produce simulations so as to
#' avoid reliance on always selecting the same fixes from the sample - useful for example if an animal traverses
#' a spatial unit a limited number of times and you want o make the most of the data from within the track.
#' The function \code{retrieve_boots} can be used to access the bootstrapped GPS data, that are stored as lookup indices
#' from \code{sub_samp} for memory saving purposes.
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
#' Occasionally the filter will not remove some rates <= dt due to it's matching nature
#' therefore after the subsampling, the numeric value given in \code{rateL} will drop rates lower than this value. By default,
#' \code{rateL} and \code{rateU} are therefore both determined by the tolerance proportional value \code{tol}, to
#' give a window of lee-way error given that GPS rates rarely sample spot on the rate they are programmed to take measurements at
#' i.e. due to satellite communications, time to fix errors etc.
#' @param rateU The upper rate (dt+(dt*tol)) at which to remove fixes as above using the \code{tol} parameter; this is the primary
#' value for which sub-sampling is carried out.
#' @param drop_coarser Logical to drop the coarser rates than the ones we sub-sample for? Default TRUE.
#' This is handy to set to FALSE if comparing trip statistics and effects of downgrading if multiple rates per trip are evident.
#' @param fun Logical TRUE or FALSE (default TRUE) to use the \code{\link{gap_section}} function to remove coarser rates, see also 'method' argument.
#' @param method A choice of three, "sequencer", "rounder", or "boot_sequencer" as to how to do the downgrading; sequencing (default) uses
#' data.table to 'roll' DateTimes to nearest on a sequences of dates at the desired sampling rate from the start of the animal per year.
#' The second option is to round to the nearest whole DateTime at the desired sampling rate, both being valid options.
#' The function \code{sub_samp} also calls \code{\link{gap_section}} using the drop = TRUE option to remove orphaned gappy sections
#' of data (single points) and remove all data beyond the desired rate, e.g. if 300 s was wanted, then any coarse rates
#' such as 1800 s would be dropped, that is if \code{fun} is set to TRUE. The boot sequencer uses the sequence approach
#' but selects random fixes within consecutive sections of data t0 to t+1 where (t+1)-t = dt; this
#' approach should be considered at the beta stage. The boot_sequencer samples with replacement. This
#' process is currently slow for large datasets, e.g. 10 s to five minutes for data across many months per animal;
#' that is even with use of \code{base::combn} and tidyverse approaches such as \code{slice_sample()}; here
#' we vectorised the process comparing pairs of points per section samples. The routine works by selecting points within sections of data at length dt, and a validity check
#' is needed to make sure fixes are not selecting at the boundaries of neighbouring sections < rateL; requiring an
#' iterative approach. The result is a listed output of boots, that can be translated back into
#' \code{Track} data using the function \code{retrieve_boots}. Initial tests for five birds for 100
#' boots downgrading to 300 s from GPS data with a mixture of 10 s, 300 s and 1800 s rates across five months were in the region of a 1.5 hours on an i5 8GB RAM PC, but
#' parallel processing has not yet been explored. We would welcome any feedback.
#' @param nboots The number of bootstraps to take using the sequence sampler, relevant only for that method.
#' @param split_ratio A ratio to split up pairs of sequences for the boot sequencer option, to speed up processing inthe
#' \code{combn} \code{data.table} process assessing validity of pairs. Defaults to 50. Toggling this may help runtime.
#' @param seed Seed to set for reproducibility using the boot sequencer.
#' @param verbose logical to return detailed messaging.
#'
#' @return Returns the same format data as inputted but a reduced dataset (except for the boot_sequencer - see below)
#' removing rows of data for each animal faster than the desired rate. The boot sequencer returns a list of row numbers matching
#' the original dataset, to save memory, stored as attributes to the output under \code{attr(attr(data2, "sub_samp"), "boots")}; these are retrieved and converted
#' to \code{Track} data using the \code{retrieve_boots} function that adds an additional column of 'boot'
#' in a stacked dataset across all animals.
#'
#' @seealso [MoveRakeR::clean_GPS], [MoveRakeR::gap_section], [MoveRakeR::Track2move]
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
#' # To get around this, in the sub_samp function, we therefore revisit the original data
#' # and look for potential real points that could sit in between consecutive fixes that would meet a
#' # minimum threshold difference between the fixes informed by dt-(dt*tol)
#' # -------------------------------------------------------- #
#'
#' # Ultimately, interpolation methods may be more useful depending on further analytical uses.
#'
#' sub_samp(data = data_test_df, dt = 300, tol = 0.4, method = "rounder")
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
#' # 3. Boot sequencer
#' # produce 'realisations' of the data randomly sampling
#' # This can be lengthy if say you have lots of 10 s data and wanting to go to e.g. 1800!
#' # So that's a lot of potential combinations of pairs of points to assess validity in the current approach.
#'
#' x1800_boot <- sub_samp(data = data, dt = 1800, tol = 0.2, method = "boot_sequencer", drop_coarser = TRUE)
#' x1800_actual_boots <- retrieve_boots(x1800_boot) # use of retrieve-boots function to access boots (stored from the sub_samp function as an index lookup to save memory)
#'
#' data_test_df <- data_test_df %>% mutate(difftime = as.vector(difftime(DateTime, lag(DateTime), units= 'secs')))
#'
#' new3 = sub_samp(data, dt=7200, tol = 0.2, method = "boot_sequencer", nboots = 10, seed = 1)
#' new = retrieve_boots(new3)
#' plot_leaflet(data = data, plotby = "boot") # check visualisation
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
sub_samp = function(data, dt=300, Unit='secs', by = NULL, tol = 0.4, u_tol=NULL, l_tol = NULL, rateU=dt+(dt*tol), rateL = dt-(dt*tol),
                    method = c("sequencer", "rounder", "boot_sequencer"),
                    nboots = 5, seed = NULL, split_ratio = 50,
                    drop_coarser = TRUE, verbose = TRUE){

  # --------------------------------------- #
  # assessment of relevant minimum namings in data
  attr_list <- get_attributes(data)
  # --------------------------------------- #

  method = method[1] # using sequence by default

  #l_tol = NULL
  ##u_tol = 0.4
  #u_tol = NULL
  #rateL <- rateU <- NULL

  if(!is.null(u_tol)){
      message("Bespoke upper tolerance: (", dt, "+(", dt,"*",u_tol, ") = ", dt+(dt*u_tol), " s)", " using ", method, " method")

      rateU <- dt+(dt*u_tol)

      if(is.null(l_tol)){
        l_tol = u_tol

        rateL <- dt-(dt*l_tol)
      }

  }

  if(!is.null(l_tol)){
      message("Bespoke lower tolerance: (", dt, "-(", dt,"*",l_tol, ") = ", dt-(dt*l_tol), " s)", " using ", method, " method")

      if(is.null(u_tol)){
        u_tol = l_tol
        message("Bespoke upper tolerance: (", dt, "+(", dt,"*",u_tol, ") = ", dt+(dt*u_tol), " s)", " using ", method, " method")
        rateU <- dt+(dt*u_tol)
      }

    rateL <- dt-(dt*l_tol)
  }

  if(is.null(l_tol) & is.null(u_tol)){
    if(verbose){
      message("Reducing data to common rate of ", dt, "s, with tolerance of ", tol, " using ", method, " method")
    }
  }


  #if(fun){
  #  if(verbose){
  #      if(method != "boot_sequencer" & drop_coarser & exists("gapsec", data)){
  #        message("calling gap_section function, dropping all points > ",rateU, Unit, " (including tol of ", tol,"*",dt,")")
  #      }
  #      if(!exists("gapsec", fin2) & drop_coarser){
  #        message("gapsec not found in data: WARNING result may contain data above rateU+tol")
  #      }
  #    }
  #}


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
  #
  # Tests
  #data <- LBBGWalB201416
  ##data = BTOTrackingTools::simplify(data, extra_vars = "dt")
  #data$rn <- 1:nrow(data)
  ##data <- data[249:432,] # some slower than 5 mins, some faster
  ##BTOTrackingTools::assign_rates(LBBGWalB201416)

  ##########################################################
  data_tb <- tibble(data) %>% group_by(TagID) %>% arrange(TagID, DateTime)

  ##########################################################
  # 1. Rounding to the nearest datetime unit

  if(method == "rounder"){

    if(verbose){message("---- rounding process ----")}

    # quickly inserted this patch - may be a better way around
    if(dt > 60){
      dt2 = dt / 60
      Unit2 = "mins"
    } else{
        dt2 = dt
        Unit2 = Unit
        }

    new2 <- data_tb %>%
      group_by(TagID) %>%
      mutate(difftime = as.vector(difftime(DateTime, lag(DateTime), units = 'secs'))) %>% # retain this to check from original data if any data are above the rate we are downgrading too
      rename(DateTime_orig = "DateTime") %>%
      mutate(DateTime = round_date(DateTime_orig, unit = period(num = dt2, units = Unit2)),
             diff = abs(as.vector(difftime(DateTime_orig, DateTime, units = "secs")))
      ) %>% group_by(TagID, DateTime) %>% slice_min(diff) %>%
      group_by(TagID) %>%
      mutate(dt = as.vector(difftime(lead(DateTime_orig), DateTime_orig, units = 'secs')) ) # recalculate dt

    # sorry this is complex
    new2 <- new2 %>% mutate(dt = as.vector(difftime(DateTime_orig, lag(DateTime_orig), units = 'secs')) )
    new2 <- new2[is.na(new2$dt) | new2$dt > rateL,]
    new2 <- new2 %>% mutate(dt = as.vector(difftime(lead(DateTime_orig), DateTime_orig, units = 'secs')) )

    # used below in the process for the sequence (just so code will flow)
    r1 <- data.table::data.table(data_tb, key = c("TagID","DateTime"))  # raw data
    r1$DateTime_orig <- r1$DateTime

  }

  ##########################################################
  # 2. Sequential 'rolling' in data.table, original methods used at BTO

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

    #r1[525:535,]
    #as.POSIXct("2016-06-05 10:17:07 UTC", tz = "UTC") %in% new$DateTime_orig
    #new[5080:5120,]

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


    # the sequencer can misbehave more than the rounder - this is because when looking
    # for nearest matches it will always find them nearest the sequence even if too close
    # to the next fix
    # we need to make sure therefore that our result at this point is valid and that no fixes
    # quicker than the rate we want to degrade to are still present
    new2 <- new2[is.na(new2$dt) | new2$dt > rateL,] # was difftime

    # alignment with rounding output
    new2 <- new2 %>% mutate(difftime = dt) %>%
      mutate(dt = as.vector(difftime(lead(DateTime_orig), DateTime_orig, units = 'secs')) ) # recalculate dt

    new2 <- tibble(new2)

  }

  ##########################################
  if(method != "boot_sequencer"){

    ###############################
    # first row difftime make same as second lag for rateU filter, also complexity for first valid dt!
    new2$difftime[1] <- new2$difftime[2]

    # end dt replace with previous (thisis for search windows below)
    new2 = new2 %>% group_by(TagID) %>%
      mutate(rn = row_number(),
             dt = ifelse(rn == max(rn),lag(dt),dt),
             difftime = ifelse(dt < difftime, dt, difftime) )

    #new2[525:535,]

    #tail(new2_test[new2_test$TagID == 4032,]$dt)
    #new2$dt[nrow(new2)] <- new2$difftime[(nrow(new2)-1)]
    #new2$difftime <- ifelse(new2$dt < new2$difftime, new2$dt, new2$difftime)

    #################################################################################################################
    # look for potentially missing fixes from the 'too harsh' sub-sampling
    # Base R approach used originally. This looked for any violations of the dt rule
    # working with the filtered dt values that are greater than dt, check for existence of other valid fixes
    # if there were situations as in the above where the re-estimated dt was greater than the
    # maximum threshold we are allowing, e.g. 300 seconds plus a tolerance (0.4 default) = 420 s
    # then if in the original fix there is a fix than is within the re-estimated dt gap, here being 576 s
    # check to see if it is OK to include the additional fix as part of the sequence
    # flipping between tidyverse and data.table here!
    if(verbose){message("---- overly strict point removal checker ----")}

    new2. <- new2 %>% group_by(TagID) %>% arrange(TagID, DateTime_orig) %>%
      mutate(DateTime_orig2 = DateTime_orig + dt) %>%
      dplyr::select(-c(diff, dt, DateTime)) %>% #group_by(TagID, DateTime_orig)
      rename(st = "DateTime_orig", en = "DateTime_orig2") %>%
      dplyr::select(c(TagID, st, en))

    #new2.[525:535,]

    # to slim this down - look at this point for number of fixes between these dates
    # i.e. any fixes between these two values consider for adding back into the data
    # left_join in - this will be bigger as there will be starts and ends doubling up of rows
    st_en <- new2. %>%  tidyr::pivot_longer(cols = c(`st`,`en`), names_to = "st_en", values_to = "DateTime")

    #st_en[st_en$DateTime %in% as.POSIXct("2016-05-18 18:50:36", tz = "UTC"),]
    #tail(st_en[st_en$TagID == 202,])

    #new2.[new2.$st %in% as.POSIXct("2016-05-31 07:28:28", tz = "UTC"),]
    #st_en[st_en$DateTime %in% as.POSIXct("2016-05-31 07:28:28", tz = "UTC"),]
    #st_en[st_en$DateTime %in% as.POSIXct("2016-05-31 08:27:41", tz = "UTC"),]
    #st_en$p <- sort(rep(1:(nrow(st_en)/2), 2))
    #st_en[st_en$p == 2418,]

    # ------------------- #
    # extra step here as we do not want to reinsert the fixes rightly filtered out
    # so use rateL to drop fixes with faster dts?
    # the issue we are getting at here is for data already on the rate we are filtering too
    #r1a = r1 %>% mutate(dt = as.vector(difftime(lead(DateTime), DateTime, units = 'secs')) ) %>%
    #  filter(dt >= rateL) %>% relocate(dt, .after = DateTime) %>% tibble()

    #### the above row in the example of 202, drops out "2016-06-05 10:17:07"
    #### which is a valid fix five mins after "2016-06-05 10:12:07"
    #### this is because the lead DT - DT is being used to filter rateLs
    #### whereas it should be I think DT - lag(DT)
    dt_ <- dt
    r1a <- r1 %>% mutate(dt = as.vector(difftime(DateTime, lag(DateTime), units = 'secs')) ) %>%
       relocate(dt, .after = DateTime) %>% group_by(TagID) %>%
        mutate(rn = row_number(),
             dt = ifelse(rn == min(rn),lead(dt),dt),
             dt = ifelse(is.na(dt), dt_, dt)) %>% # for nrows of 1 per animal the above lead will not work, i.e. no second row, so just take dt supplied to function (dt_ here)
      dplyr::select(-rn) %>% filter(dt >= rateL)

    #r1b[525:535,]
    #r1a[525:535,]
    #r1b[1,]$dt # NA
    # ------------------ #

    #if(!exists("longitude", data)){r1a$longitude <- 1}
    #if(!exists("latitude", data)){r1a$latitude <- 1}

    # drop any fixes > rateU as at the start and end it can include fixes - see if they are arphaned with gapsec process
    r1a = r1a %>% gap_section(GAP = rateU, tol = 0, drop = TRUE, verbose = FALSE) %>% tibble()

    #if(!exists("longitude", data)){r1a <- r1a %>%  dplyr::select(-c(longitude))}
    #if(!exists("latitude", data)){r1a <- r1a %>%  dplyr::select(-c(latitude))}
    if(!exists("gapsec", data)){r1a <- r1a %>%  dplyr::select(-c(gapsec))}
    if(!exists("gap", data)){r1a <- r1a %>%  dplyr::select(-c(gap))}

    #r1a$dt[nrow(r1a)] <- dt

    # ------------------- #

    # are there any valid fixes to actually check to add in?


    if(nrow(r1a) > 0){

      # better to rbind as now r1a is slimmed down so may not have the start or end to match to in prev left_join
      # first remove the 'negative space' between the starts and ends of sections of data where dt was VALID i.e. < dt threshold

      #st_en_test <- st_en[st_en$TagID %in% 4032 & st_en$DateTime >= as.POSIXct("2016-05-31 07:28:28", tz = "UTC") &
      #                      st_en$DateTime <= as.POSIXct("2016-05-31 08:27:41", tz = "UTC"),]
      #r1a_test <- r1a[r1a$TagID == 4032 & r1a$DateTime >= as.POSIXct("2016-05-31 07:28:28", tz = "UTC") &
      #                  r1a$DateTime <= as.POSIXct("2016-05-31 08:27:41", tz = "UTC"),]

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

      #View(part1)

      # old way broken
      #part1 = left_join(r1a, st_en, by = c("TagID", "DateTime")) %>% group_by(TagID) %>%
      #  mutate(cond0 = if_else(is.na(st_en) & lag(st_en) == "en", row_number(), 0), # complex => sure there must be a way of doing in one single if_else but on the day it got better of me, splitting up to two columns at no real overhead cost
      #         cond1 = if_else(is.na(st_en) & lead(st_en) == "st", row_number(), 0),
      #         cond2 = if_else(cond0 == 0 & cond1 == 0, 0,
      #                         if_else(cond0 > 0 & is.na(cond1), cond0,
      #                                 if_else(cond1 > 0 & is.na(cond0), cond1, NA)))
      #  ) %>%
      #   dplyr::select(-c(cond0,cond1)) %>%  tidyr::fill(cond2) %>%
      #  mutate(cond2 = if_else(cond2 > 0, 1, cond2)) %>%
      #  filter(cond2 == 0) %>% # remove the sections where we do not want to search, now have st and end for all data for > dt violation
      #  mutate(pnum = case_when(st_en == "st" ~ cur_group_rows())) %>% # add in row.number for st
      #  tidyr::fill(pnum) %>% # fill in between st
      #  mutate(Sec = consecutive_id(pnum)) %>% # consecutively number sections
      #   dplyr::select(-c(cond2,pnum))

      # count the sections - if just a start and an end, then no extra data was between the st and end
      part2 = part1 %>% group_by(TagID, Sec) %>% summarise(n = n(), .groups="keep") %>% filter(n > 2) # part1 data LBBGU WA six birds, left 1350 rows (1538 for the rounded time version)

      #part1[part1$Sec == 946,]

      #st_en
      #st_en[st_en$DateTime %in% as.POSIXct("2016-05-31 07:28:28", tz = "UTC"),]

      # then only way I can think of doing this is to add back into the full dataset, recalculate dt, and if
      # any remain below the threshold then retain them! Most should just drop out again
      part3 = left_join(part2, part1, by = c("TagID", "Sec")) %>% ungroup() %>% dplyr::select(-c(n, Sec, st_en)) # keeping only the sections with in between points

      part3 <- part3 %>% group_by(TagID) %>% filter(!duplicated(DateTime)) %>% ungroup()

      # add this back to the sub-sampled data
      # need to switch the date_time orig and DateTime columns as relabelled for rolling above
      fin <- new2 %>% dplyr::select(-c(DateTime, diff)) %>% rename(DateTime = "DateTime_orig")
      #fin_extra <- part3  #%>% dplyr::select(-DateTime_orig)

      # lookup those rows in data
      fin_extra <- right_join(data, part3, by = c("TagID", "DateTime")) %>%
        mutate(dt = as.vector(difftime(lead(DateTime), DateTime, units = 'secs')) ) %>%
        group_by(TagID)

      # fin columns switched around
      #fin = fin %>% relocate(TagID) #%>% relocate(Type, .after = TagID) #%>% dplyr::select(-rn)

      # recalculate dt, drop out extra rows from simple filter
      fin2 = rbind(fin, fin_extra) %>% group_by(TagID) %>% arrange(TagID, DateTime) %>%  #mutate(rn = 1:n())
        #mutate(dt = as.vector(difftime(lead(DateTime), DateTime, units = 'secs')) ) %>%
        mutate(dt = as.vector(difftime(DateTime, lag(DateTime), units = 'secs')) ) %>%
        filter(!duplicated(DateTime)) %>%
        #mutate(dt = as.vector(difftime(lead(DateTime), DateTime, units = 'secs')) )
        mutate(dt = as.vector(difftime(DateTime, lag(DateTime), units = 'secs')) )

      # ------------------ #
      # the LAST fix WILL ALWAYS be dropped if you don't apply this dirty work around
      #fin2$dt[length(fin2$dt)] <- dt
      #dt_ = dt
      #fin2 = fin2 %>% group_by(TagID) %>%
      #  mutate(rn = row_number(),
      #         dt = ifelse(rn == max(rn),dt_,dt)) %>% dplyr::select(-rn)

      # But... if using the dt - lag(dt) then the above does not apply, instead it is the
      # first in the list, so just make that equal to the next lag
      dt_ = dt
      fin2 = fin2 %>% group_by(TagID) %>%
        mutate(rn = row_number(),
             dt = ifelse(rn == min(rn),lead(dt),dt),
             dt = ifelse(is.na(dt), dt_, dt))

      #fin2[525:535,]

      # ------------------ #


      fin2 <- fin2 %>% filter(dt > 0) # prob unneccessary

    } else{

      fin2 <- new2 %>% dplyr::select(-DateTime, diff) %>% rename(DateTime = "DateTime_orig")
    }

    #fin2 %>% relocate(dt, .after = Type) %>% mutate(dt = as.vector(difftime(lead(DateTime), DateTime, units = 'secs')) )
    #t1 = as.POSIXct("2016-05-18 19:52:37", tz = "UTC")
    #t2 = as.POSIXct("2016-05-18 18:51:13", tz = "UTC")
    #difftime(t1,t2, unit = 'secs')

    # check again for those below the minimum rate (filter(dt >= rateL) was too strict),
    # and should be done with gapsection that retains strings of points
    # allowing also use of function if lat long missing
    #if(!exists("longitude", data)){fin2$longitude <- 1}
    #if(!exists("latitude", data)){fin2$latitude <- 1}
    fin2 <- fin2 %>% gap_section(GAP = rateL, tol = 0, drop = FALSE, verbose = FALSE)

    #if(!exists("longitude", data)){fin2 <- fin2 %>% select(-c(longitude))}
    #if(!exists("latitude", data)){fin2 <- fin2 %>% select(-c(latitude))}
    if(!exists("gapsec", data)){fin2 <- fin2 %>% dplyr::select(-c(gapsec))}
    if(!exists("gap", data)){fin2 <- fin2 %>% dplyr::select(-c(gap))}

    fin2 <- fin2 %>% # filter(dt >= rateL)
      #mutate(dt = as.vector(difftime(lead(DateTime), DateTime, units = 'secs')) ) %>%
      mutate(dt = as.vector(difftime(DateTime, lag(DateTime), units = 'secs')) ) %>%
      arrange(TagID, DateTime) %>% tibble()

    #lead(dt)-dt
    #dt_ = dt
    #fin2 = fin2 %>% group_by(TagID) %>%
    #  mutate(rn = row_number(),
    #         dt = ifelse(rn == max(rn),dt_,dt)) %>% dplyr::select(-rn)

    #dt-lag(dt)
    dt_ = dt
    fin2 = fin2 %>% group_by(TagID) %>%
      mutate(rn = row_number(),
             dt = ifelse(rn == min(rn),lead(dt),dt),
             dt = ifelse(is.na(dt), dt_, dt))

    # STILL may be quicker fixes than rateL (e.g. dt = 300 tol = 0.2 from rounder process inserted BACK IN a fix in my tests)
    fin2 <- fin2[is.na(fin2$dt) | fin2$dt >= rateL,]

    #any(is.na(fin2$dt))
    #fin2[525:535,]

    if(drop_coarser){

      #if(!exists("longitude", data)){fin2$longitude <- 1}
      #if(!exists("latitude", data)){fin2$latitude <- 1}

      # drop any fixes > rateU
      fin2 = fin2  %>% #%>% rename(DateTime_ = "DateTime", DateTime = "DateTime_orig")
        gap_section(GAP = rateU, tol = 0, drop = TRUE, verbose = FALSE) %>% tibble() #%>%
        #rename(DateTime_orig = "DateTime", DateTime = "DateTime_")

      #as.vector(as.POSIXct("2014-05-21 05:42:31") - as.POSIXct("2014-05-21 05:36:21") )*60

      #if(!exists("longitude", data)){fin2 <- fin2 %>% dplyr::select(-c(longitude))}
      #if(!exists("latitude", data)){fin2 <- fin2 %>% dplyr::select(-c(latitude))}
      if(!exists("gapsec", data)){fin2 <- fin2 %>% dplyr::select(-c(gapsec))}
      if(!exists("gap", data)){fin2 <- fin2 %>% dplyr::select(-c(gap))}

      #new2$difftime[nrow(new2)] <- ifelse(new2$difftime[nrow(new2)] < rateU & new2$dt[nrow(new2)] > rateU, new2$dt[nrow(new2)], new2$difftime[nrow(new2)])
      #new2 <- new2 %>% filter(difftime < rateU | is.na(difftime)) # default DROP any fixes that are > rateU
    }


    # last fix coarser remains
#    if(drop_coarser){
#
#      fin2$difftime[1] <- ifelse(fin2$difftime[1] > rateU & fin2$dt[1] <= rateU, fin2$dt[1], fin2$difftime[1])
#      fin2$difftime[1] <- ifelse(is.na(fin2$difftime[1]),fin2$dt[1],fin2$difftime[1])
#      fin2 <- fin2[fin2$difftime < rateU,]
#
#      #### Dropping fixes greater than rateU and fort this also using gapsections
#      # gapsection relabelling, dropping out orphan fixes, which just means this is a one line code rather than
#      # allowing use of function if lat long missing
#      #if(!exists("longitude", data)){fin2$longitude <- 1} # dealt with above
#      #if(!exists("latitude", data)){fin2$latitude <- 1}
#      # this time drop = TRUE, i.e. removing orphaned fixes from the rateU threshold breach
#      fin2 <- tibble(gap_section(fin2, GAP = rateU, tol = 0, drop = TRUE, verbose = FALSE))
#
#      # missing last row if the end is OK
#      #if(fin2$difftime[nrow(fin2)] < rateU){
#      #  dates_to_search <- data$DateTime
#      #  date_to_find <- fin2[nrow(fin2),]$DateTime + fin2[nrow(fin2),]$difftime
#      #  extra_end_row <- data[dates_to_search %in% date_to_find,] %>% relocate(TagID) %>% mutate(dt = difftime)
#      #  fin2 <- rbind(fin2,extra_end_row)
#      #}
#
#      # what a mess this (tbh not sure if really needed if the gapsection checks are already done above:
#      check_again <- lag(fin2$dt)
#      check_again[1] <- ifelse(is.na(check_again[1]), fin2$dt[1], check_again[1])
#      bool = check_again > rateU
#      bool2 = rep(FALSE, length(bool))
#      bool2[1] <- bool[1]
#      bool2[length(bool2)] <- bool[length(bool)]
#
#      fin2 <- fin2[!bool2,]
#    }
#
#    # return any variables inserted if were not in original data
#    if(!exists("longitude", data)){ fin2 <- subset(fin2, select = -longitude) }
#    if(!exists("latitude", data)){ fin2 <- subset(fin2, select = -latitude) }
#    if(!exists("gap", data)){ fin2 <- subset(fin2, select = -gap) }
#    if(!exists("gapsec", data)){ fin2 <- subset(fin2, select = -gapsec) }

    data2 <- fin2

    # ----------------------------- #
    # overall behaviour if dt rates are not possible! The function will return
    # matches in dates or sequences even if they are not appropriate for a silly rate that may have been picked
    # recheck validity of dt therefore needed post-hoc

    # this is true ONLY if you are dropping coarser:
    #if(drop_coarser){
    #  all_check <- fin2$dt[!is.na(fin2$dt)]
    #  w = which(all_check < rateU)
    #  if(length(w) <= 1){
    #    # i.e. if all are way greater than the rateU OR the last one fictitiously entered = orphan fix, then drop
    #    data2 <- as_tibble(matrix(nrow = 0, ncol = length( names(fin2))), .name_repair = ~  names(fin2))
    #  }
    #}

    #else{

      # NOT SURE ABOUT THIS - IT CAUSE PROBLEMS ELSEWHERE
      # SEEMS TO BE HANDLED ANYWAY ABOVE
#      # if not dropping coarser, then....filter the ORIGINAL data and keep ONLY rates greater than
#      # rateL - a bit of an odd situation
#
#      data_ <- data
#      data_$difftime = as.vector(difftime(data_$DateTime, lag(data_$DateTime), units = 'secs'))
#      data_$dt = as.vector(difftime(lead(data_$DateTime), data_$DateTime, units = 'secs'))
#      data_$difftime[1] <- data_$dt[1]
#      data_$dt[nrow(data_)] <- data_$difftime[nrow(data_)]
#
#      data_$difftime <- ifelse(data_$difftime < data_$dt,data_$dt,data_$difftime)
#      data_ <- data_[data_$difftime >= rateL,]
#      data2 <- data_
#      data2 <- data2[names(data2) %in% names(data)]

   # }

    #data2[data2$TagID == 202,]

    #if(fun){
    #  if(drop_coarser){
    #    if(exists("gapsec", fin2)){
    #      data2 <- gap_section(data = fin2, GAP = rateU, tol = 0, unit = Unit, drop = TRUE, verbose = FALSE)
    #    } else{
    #      data2 = fin2
    #    }
    #  } else{
    #    data2 = fin2
    #  }
    #} else{
    #  data2 = fin2
    #}

  }

  # ------------------------------------- #
  # 3. Addtional approach for boot sequencing, after much toil, most efficient was still
  # using lists and applies rather than a tidyverse approach but I could be wrong.

  if(method == "boot_sequencer"){

    data <- tibble(data) %>% arrange(TagID, DateTime)

    if(is.null(by)){
      data$var <- lubridate::year(data$DateTime)
    } else{
      data$var <- pull(data[,which(names(data) == by)])
    }
    data$var <- as.factor(data$var)

    #######################################
    # gapsections are needed to search within valid sections of data!
    # as a lower level nesting under the var(or year) variable within TagID
    # note this doesn't matter so much in the default methods of the function as fixes filter out across gaps whatever the case
    # but for the sampler we need to know gapsections first for the validity check of points between dt[i] top dt[i+1] in the sequences

    gapped <- gap_section(data, GAP=rateL, tol = 0.01, drop = FALSE, verbose = FALSE)

    message("---- Segmenting data to desired degradation time steps ")
    sections2sample = gapped %>% group_by(TagID, var, gapsec) %>%
      summarise(date_start = min(DateTime), date_end = max(DateTime), .groups = "keep")  %>%
      do( data.frame(., DateTime= seq(.$date_start, # macro variables in the do loop
                                      .$date_end+dt, by =  paste(dt, Unit))) ) %>% ungroup() %>%
      dplyr::select(-c(date_start,date_end))

    # get rows in data other than main three
    ign <- c("TagID","var","gapsec","DateTime")
    nms = names(gapped)[which(!names(gapped) %in% ign)]
    a = data.frame(matrix(NA,ncol = length(nms), nrow = nrow(sections2sample)))
    names(a) <- nms
    joiner = tibble(cbind(sections2sample,a))

    # quick re-arrage of data
    gapped = gapped %>% relocate(TagID) %>% relocate(var, .after = TagID) %>% relocate(gapsec, .after = var) %>% relocate(DateTime, .after = gapsec)

    # make sure we know which columns are fake segmenting data
    gapped$real <- 1
    joiner$real <- 0

    #### this is the issue!! We have no way of knowing if NO points were within the phases....
    #### e.g. first point is at the starting fake sequence, then there is another e.g. 30 min fix
    #### just after maybe the 9th, but no way should they be available for sampling!
    #### it does fall out below...

    gapped2 = tibble(rbind(gapped, joiner)) %>% arrange(TagID, var, gapsec, DateTime, real)

    # segment in normal way
    gapped2 = gapped2 %>% group_by(TagID, var) %>%
      mutate(pnum = case_when(real == "0" ~ cur_group_rows())) %>% # add in row.number for when the slice was (prob is some slice option that could be more efficient actually)
      tidyr::fill(pnum) %>% # fill the rows in between for segments
      filter(real == 1) %>% # filter out the real = 0's?
      mutate(Sec = consecutive_id(pnum)) %>% # consecutively number segments sensibly from start per bird/var
      dplyr::select(-c(pnum, real)) %>% ungroup() %>% mutate(rn = row_number()) %>% relocate(c(Sec, rn), .after = DateTime)

    # sorry this is complex - there may still be quicker fixes in there violating rateL condition
    #gapped2 <- gapped2 %>% mutate(dt = as.vector(difftime(DateTime, lag(DateTime), units = 'secs')) )
    #gapped2 <- gapped2[is.na(gapped2$dt) | gapped2$dt > rateL,]
    #gapped2 <- gapped2 %>% mutate(dt = as.vector(difftime(lead(DateTime), DateTime, units = 'secs')) )

    # MOST PROMISING APPROACH SO FAR
    # this could be the answer, 3D arrays/lists!
    # first pick secs 1 and 2 is a simple 2 d from a 2d array,
    # therafter the next slice of the 3d array is for the next pair, being secs 2 and 3
    # you will have picked e.g. sec 1_5 and 2_10; 2_10 then needs to be looked up in the ROWS of the next array Sec 2 vs 3
    # so you have to select 2_10 and look at the VALID fixes for Sec 3 and so forth

    # but cost of setting up these matrices....?
    #gapped2_ft_onebird <- gapped2[gapped2$TagID == 5026,]
    gapped2a <- gapped2
    gapped2a$Sec2 <- paste(gapped2a$gapsec, gapped2a$Sec, sep = "_")
    gapped2a = gapped2a %>% group_by(TagID, gapsec, var, Sec) %>% mutate(sec_id = row_number()) %>%
      relocate(sec_id, .after = Sec) #mutate(sec_id = paste(gapsec, Sec, rn, sep = "_"))

    #gapped2a[1960:2010,]

    if(!is.null(seed)){
      set.seed(seed)
    }

    message("***************************************************************************")
    message("********************* Boot approach for sequencer *********************")
    message("***********************************************************************")
    message("~~~~~~~~~~~ Running ", nboots, " boots")

    #data = gapped2a
    # run through all animals
    gapped2a$top_lev = paste(gapped2a$TagID, gapped2a$var, sep = "_")
    loop_thru <- unique(gapped2a$top_lev)
    final_samp <- list()

    #data_in =  data[data$TagID == 5377,]

    a = 1
    coarser_fixes <- list()
    for(a in 1:length(loop_thru)){

      message("Level ", loop_thru[a])

      gapped2_ft_onebird <- gapped2a[gapped2a$top_lev == loop_thru[a],]
      gapped2_ft_onebird = gap_section(data = gapped2_ft_onebird, GAP = rateU, tol = 0, drop = TRUE, verbose = FALSE) %>% tibble()

      # there is a weird complication here - if we drop out coarser fixes this can leave
      # odd sections I think - e.g. at the end... resulting in mess up of pairs

      # remember the rownames of those you dropped!
      orig = gapped2a[gapped2a$top_lev == loop_thru[a],]
      bool = !orig$rn %in% gapped2_ft_onebird$rn
      coarser_fixes[[a]] = orig[bool,]$rn
      sec_list <- unique(gapped2_ft_onebird$Sec2)

      #tail(gapped2_ft_onebird)

      ############################### UNDER DEV

      message("------ Assessing all valid paired combinations (dt > rateL) ------")

      ### mapply only works with > 1 pair! If there is only one valid pair
      ### then we have nothing to compare against so have to take that!

      if(length(sec_list) <= 1){

        message("One or no pairs found, no bootstrap possible")

        } else{


          paired = mapply(FUN = c, sec_list[-length(sec_list)], sec_list[-1], SIMPLIFY = FALSE)
          paired2 = data.frame(do.call('rbind', paired), pair = 1:length(paired))
          row.names(paired2) <- 1:nrow(paired2)
          # long format putting all pairs together - one column
          paired3 <- paired2 %>%  tidyr::pivot_longer(cols = c(`X1`,`X2`), names_to = "X1X2",
                                                      values_to = "pair", names_repair = "minimal")
          names(paired3) <- c("pair","X1X2","Sec2") # avoiding renaming warning returned in pivot_longer....probably a way to turn off but this just gets around it
          paired3 <- paired3 %>% dplyr::select(-X1X2)

          paired4 = tibble(merge(paired3, gapped2_ft_onebird, by = "Sec2", all.x = TRUE)) %>%
            arrange(TagID, var, pair, DateTime)
          paired4a <- paired4[paired4$pair %in% unique(paired4$pair),]

          # data.table seems fastest using C++
          #https://stackoverflow.com/questions/47276418/expanding-a-list-to-include-all-possible-pairwise-combinations-within-a-group
          # Code inspired by the above example, credit is here given here to that code
          #x <- paired4a[paired4a$pair %in% c(4998),]
          #x <- paired4a[paired4a$pair %in% 1:100,]

          ex <- subset(paired4a, select = c(DateTime, pair, Sec, sec_id, gapsec))
          ex$Sec <- paste(ex$gapsec, ex$Sec, sep = "_")
          # sec_id is the consecutive number of fixes WITHIN a section 'Sec'
          #unique(ex$sec_id) # 32
          #unique(ex$gapsec) # 6
          #data.frame(ex[ex$pair == 4999,])

          ################## THIS NEEDED FURTHER WORK ###################### TOO SLOW
          ################## BUT ON SECOND RUN IT SEEMS OK.... MAYBE MAKE SURE FRESH R SESSION?

          ex$cc = paste(ex$Sec, ex$sec_id, sep = "__")

          # the task is slow as data.table is having to do too much vectorisation in one go
          # if this job is split up, the same result occurs
          #ex_ <- ex[1:20000,]
          #ex_ <- ex[1:10000,]
          #nrow(test[pair == 535]) # 63 both times

          pairs2do <- unique(ex$pair)
          tail(ex,10)

          start = Sys.time()

          if(length(pairs2do) > 100){

            if(verbose){message("Subsetting combn process in data.table given number of pairs to do...")}
            if(verbose){message("(For large data this process can take a few minutes per animal)")}
            ## no as that splits up pairs - has to be done with pairs in mind then
            rr = round(length(pairs2do) / split_ratio)
            chs = c(seq(1,length(pairs2do),rr),length(pairs2do))

            # progress bar
            #i = 51
            test_ <- test2a_ <- list()
            if(verbose){cli::cli_progress_bar("Assessing pairs", total = length(chs))}
            for(i in 2:length(chs)){
              #if(verbose){message("pair group ", i-1, " / ", length(chs)-1)}
              #if(verbose){message(round(((i-1) / (length(chs)-1)) * 100,0), " %...")}
              if(verbose){cli::cli_progress_update()}

              if(i > 2){
                ex_ <- ex[ex$pair %in% ((chs[i-1]+1):chs[i]),]
              } else{
                ex_ <- ex[ex$pair %in% (chs[i-1]:chs[i]),]
              }
              test_[[i-1]] = data.table::setDT(ex_)[, {temp <- combn(cc, 2); temp2 <- combn(DateTime, 2); .(G1 = temp[1,], G2 = temp[2,], G3 = temp2[1,], G4 = temp2[2,])}, pair][!duplicated(data.table::data.table(pmax(G1, G2), pmin(G1, G2)))]
              test_[[i-1]]$G3 <- as.POSIXct(as.numeric(test_[[i-1]]$G3), origin='1970-01-01', tz = "UTC")
              test_[[i-1]]$G4 <- as.POSIXct(as.numeric(test_[[i-1]]$G4), origin='1970-01-01', tz = "UTC")
              names(test_[[i-1]])[c(4,5)] <- c("DateTime_1", "DateTime_2")

              # remove the WITHIN group pairs
              rb = do.call('rbind',strsplit(test_[[i-1]]$G1, "__"))
              rb2 = do.call('rbind',strsplit(test_[[i-1]]$G2, "__"))
              test_[[i-1]]$Sec_1 <- rb[,1]
              test_[[i-1]]$Sec_2 <- rb2[,1] # Sec_1 an Sec_2 below is the concatenation of the gapsection and the Sec 1 and 2's
              test_[[i-1]]$rn <- 1:nrow(test_[[i-1]])

              # Sec_no_1, Sec_no_2 = 1:n fixes per Sec i.e. sec_id
              test_[[i-1]]$Sec_no_1 <- rb[,2]
              test_[[i-1]]$Sec_no_2 <- rb2[,2]

              test_[[i-1]] <- test_[[i-1]][test_[[i-1]]$Sec_1 != test_[[i-1]]$Sec_2]
              #test[pair == 4999]

              # ------------------------- #
              ### datetime version (done above!)
              #test2a_[[i-1]]

              #test2a_ = data.table::setDT(ex_)[, {temp <- combn(DateTime, 2); .(G1 = temp[1,], G2 = temp[2,])}, pair][!duplicated(data.table::data.table(pmax(G1, G2), pmin(G1, G2)))]
              #test2a_$G1 <- as.POSIXct(as.numeric(test2a_$G1), origin='1970-01-01', tz = "UTC")
              #test2a_$G2 <- as.POSIXct(as.numeric(test2a_$G2), origin='1970-01-01', tz = "UTC")
              #test2a_$rn <- 1:nrow(test2a_)
              #names(test2a_)[c(2,3)] <- c("DateTime_1", "DateTime_2")
              # add in the DateTimes
              #test_[[i-1]] <- merge(test_[[i-1]], subset(test2a_, select = -pair), by = "rn")

              # calculate dt
              test_[[i-1]]$dt_ <- as.vector(difftime( test_[[i-1]]$DateTime_2,  test_[[i-1]]$DateTime_1, units = 'secs'))

              # gapsection
              test_[[i-1]]$gapsec = do.call('rbind',strsplit(test_[[i-1]]$Sec_1, "_"))[,1]

              # TagID and var
              test_[[i-1]]$TagID = unique(gapped2_ft_onebird$TagID)[1]
              test_[[i-1]]$var = unique(gapped2_ft_onebird$var)[1]

            }
            if(verbose){cli::cli_progress_done()}

            test = do.call('rbind',test_)
            #test2a = do.call('rbind',test2a_)
            #tail(test)
            #if(verbose){message("Done.")}

          } else{

            # combn is faster, but DateTimes lose formatting, so doing twice - may be a better way
            test = data.table::setDT(ex)[, {temp <- combn(cc, 2); temp2 <- combn(DateTime, 2); .(G1 = temp[1,], G2 = temp[2,], G3 = temp2[1,], G4 = temp2[2,])}, pair][!duplicated(data.table::data.table(pmax(G1, G2), pmin(G1, G2)))]

            #test2a = data.table::setDT(ex)[, {temp <- combn(DateTime, 2); .(G1 = temp[1,], G2 = temp[2,])}, pair][!duplicated(data.table(pmax(G1, G2), pmin(G1, G2)))]

            ######
            test$G3 <- as.POSIXct(as.numeric(test$G3), origin='1970-01-01', tz = "UTC")
            test$G4 <- as.POSIXct(as.numeric(test$G4), origin='1970-01-01', tz = "UTC")
            names(test)[c(4,5)] <- c("DateTime_1", "DateTime_2")

            # remove the WITHIN group pairs
            rb = do.call('rbind',strsplit(test$G1, "__"))
            rb2 = do.call('rbind',strsplit(test$G2, "__"))
            test$Sec_1 <- rb[,1]
            test$Sec_2 <- rb2[,1] # Sec_1 an Sec_2 below is the concatenation of the gapsection and the Sec 1 and 2's

            test$rn <- 1:nrow(test)
            test2a$rn <- 1:nrow(test2a)

            # Sec_no_1, Sec_no_2 = 1:n fixes per Sec i.e. sec_id
            test$Sec_no_1 <- rb[,2]
            test$Sec_no_2 <- rb2[,2]

            test <- test[test$Sec_1 != test$Sec_2]
            #test[pair == 4999]

            # add in the DateTimes
            #test <- merge(test, subset(test2a, select = -pair), by = "rn")

            # calculate dt
            test$dt_ <- as.vector(difftime(test$DateTime_2, test$DateTime_1, units = 'secs'))

            # gapsec
            test$gapsec = do.call('rbind',strsplit(test$Sec_1, "_"))[,1]

            # add in TagID and var
            test$TagID = unique(gapped2_ft_onebird$TagID)[1]
            test$var = unique(gapped2_ft_onebird$var)[1]
          }

          end = Sys.time()
          total_time <- round(as.vector(difftime(end,start, units = "mins")),2)
          if(verbose){message("Pair processing: ", total_time, " mins ellpased")}

          # 4032 tests (original before further process included)
          # split_ratio = 100, 3.52816 mins
          # split_ratio = 25, 3.914343 mins
          # split_ratio = 50, 4.004487 mins

          # 4.12 with full process (sr = 50)


          #### NOT THE POINTS HERE WITH dt's much greater than dt are the first fixes WITHIN
          # their gapsection so are valid as initial fixes to consider I think
          # we already filtered out dt > rateU above...potentially...
          # i.e. this code still works in pairs ACROSS gapsecs

          # back in the tidyverse from data.table
          # most importantly, REMOVE any invalid pairings that violate lower dt

          if(verbose){message("Finishing up pairs...")}

          test <- tibble(test) %>% dplyr::select(-c(rn, G1, G2)) %>% arrange(TagID, DateTime_1) %>%
            filter(!is.na(dt_) & dt_ >= rateL)

          # relable pairings just in case any drop out, prob shouldn't be any
          test <- test %>% mutate(pair = consecutive_id(pair)) #%>% mutate(pair = as.factor(pair))

          #################
          test <- unique(test) # don't understand this - end of the data duplicates, not tracked down why

          #tail(test)
          # now we ONLY have valid pairings

          #2016-08-17 17:13:56
      }

      #tail(test)
      # ------------------------------------------------------------- #
      # boot routine as a while condition, takes advantage of slice_sample() vectorisation
      # ------------------------------------------------------------- #
      message("------ Bootstrap routine ------")
      boot_list <- list()

      start = Sys.time()

      for(b in 1:nboots){

        message("Boot ", b)

        # as above if only one pairing then that is the answer!
        if(length(sec_list) <= 1){

          boot_list[[b]]  <- gapped2_ft_onebird$rn

          #tail(gapped2_ft_onebird)
          #gapped2_ft_onebird[nrow(gapped2_ft_onebird),]

        } else{

          # add in the last row as the section will be missed from the slice!
          last_row <- test[nrow(test),]
          last_row$DateTime_1 <- last_row$DateTime_2
          last_row$DateTime_2 <- last_row$DateTime_1 + last_row$dt_ # fake second DateTime
          last_row$pair <- last_row$pair+1
          last_row$Sec_no_2 <- 1
          # the last row sec 1 has to equal the sec2 from the row before
          last_row$Sec_no_1 <- test[nrow(test),]$Sec_no_2
          #last_row$rn <- test[nrow(test),]$rn + 1

          test <- rbind(test,last_row)

          #tail(test)
          #test[nrow(test)-1,]
          #test_use <- test

          # do an initial sample across all pairs
          test2 = test %>% group_by(pair) %>% slice_sample(n = 1)
          #test2[nrow(test2),]
          test2$match = test2$Sec_no_2 == c(test2$Sec_no_1[-1],NA)
          test2 <- test2 %>% relocate(match,.after = Sec_no_2)
          # last match should always be TRUE (nothing to compare against)
          test2$match <- ifelse(is.na(test2$match),TRUE,test2$match)

          #tail(test2)

          # also from test2 at this point if remembering COARSER fixes, the ones that are
          # invalid should they be added to the coarser fixes?? Not sure. The whole
          # point of this is to assess any TOO QUICK fix pairs from the random selection
          # so the removal of them is warranted also from the coarse group....I think...

          # while FALSES remain in the list...
          while(any(!test2$match)){

            #check = test2[test2$pair %in% c(869,870,871),]

            # go down the Secs and if Sec_no_2 != Sec_no_1
            # resample....

            # this will massively narrow it down

            #test2[test2$pair == 744,]
            #test2[test2$pair == 745,]

            # while they are all false, keep going!?
            dones = test2[test2$match,]
            notdones = test2[!test2$match,]

            #notdones[notdones$pair == 744,]

            #dones[dones$pair %in% c(36,37),]
            #test[test$pair %in% c(36,37),]

            # this is far better, but could still be lengthy
            # if say you did manage to have all 10 s data, then this would be a chance game
            # and may only reduce a bit, but here we have lots of five min and 10 s data
            # so pairs where there are only two possible points (i.e. already on that rate)
            # will be selected
            # However, the issue is that adjustment will impact already matching pairs!!

            # so this is confusing, we have those that no not match the CURRENT pair and the next
            # but the current pair IS what was sampled, so the NEXT pair is the one we want to
            # resample based on the section 2 of the previous.
            # So if pair 6 is "not matching", then we need to resample the 7th

            ##### here referring BACK to test i..e the full set of AVAILABLE VALID pairs
            test0 <- test[test$pair %in% (notdones$pair+1),]

            #test0[test0$pair == 745,]

            # KEY POINT we need to select ALL the Sec_no_2s from the pair+1s right? i.e. resample the next pair along?
            notdones$pair1 <- notdones$pair+1
            nd_sub <- notdones %>% ungroup() %>% dplyr::select(pair1,Sec_no_2) %>% # rename pair1 as pair for merge
              rename(pair = "pair1", Sec_no_2_pairbefore = "Sec_no_2")

            #nd_sub[nd_sub$pair == 745,]

            # then select the test0 pair+1s with Sec1 = Sec2
            #test <- test %>% ungroup()
            test_again <- tibble(merge(test0, nd_sub, by = "pair")) %>% relocate(Sec_no_2_pairbefore, .after = "Sec_no_2") %>%
              filter(Sec_no_1 == Sec_no_2_pairbefore) # SLIM DOWN TO THOSE MATCHING

            #### if NONE to test again, then the loop is complete! i.e. will be conditions where it is impossible

            if(nrow(test_again) == 0){

              test2$match = TRUE # this will end the loop! set everything to valid
            } else{

              #test_again[test_again$pair == 745,] # so no matching available pair...

              # and then resample from those
              test3 = test_again %>% group_by(pair) %>% slice_sample(n = 1) %>% dplyr::select(-Sec_no_2_pairbefore)

              #test3[test3$pair == 745,] # none

              # need to drop those with no possible matches OUT from .... where...? dones, not dones?
              # if NO matches are found, set the previous section match to TRUE in original sample,
              # so it doesn't try and find again!

              #######

              # put the other ones that were not matching but were available to be resampled (FALSE added but will be reassessed)
              test_a <- test3 %>% mutate(match = FALSE) %>% relocate(match, .after = Sec_no_2)

              # remove the original picks and replace with these
              test_b <- test2 # test2 is the first sample of all the pairs
              test_b <- test_b[!test_b$pair %in% test_a$pair,]

              test2a <- rbind(test_a, test_b) %>% ungroup() %>% arrange(pair,DateTime_1) %>% group_by(pair)

              ## REASSES THE matches between consecutive pairs
              test2a$match = test2a$Sec_no_2 == c(test2a$Sec_no_1[-1],NA)

              # BUT.....need to set the PREVIOUS pair of these back to TRUE as they were missing:
              pairs_to_adj <- nd_sub$pair[which(!nd_sub$pair %in% test3$pair)] -1
              test2a[test2a$pair %in% pairs_to_adj,]$match = TRUE

              # last match again always TRUE
              test2a$match <- ifelse(is.na(test2a$match),TRUE,test2a$match)

              #test2a[test2a$pair %in% c(744,745),]

              # THEN REMOVE the ones in the NEXT section that could not be matched from the sample....
              test2a <- test2a[!test2a$pair %in% (pairs_to_adj+1),]

              test2 <- test2a

            }

          }

          # merge in info from full data
          dc_sampled <- subset(test2, select = c(DateTime_1, TagID, var)) %>% rename(DateTime = "DateTime_1")
          data_ <- left_join(dc_sampled, gapped2a, by = c("TagID", "DateTime", "var")) %>% dplyr::select(-c(var, top_lev, Sec2)) %>% relocate(DateTime,.after = TagID)

          #tail(data_)

          data_ = gap_section(data = data_, GAP = rateU, tol = 0, drop = TRUE, verbose = FALSE) %>% tibble()

          boot_list[[b]]  <- data_$rn # storage of row.numbers
        }

        }
      names(boot_list) <- 1:nboots

      final_samp[[a]] <- boot_list

      end = Sys.time()
      total_time <- round(as.vector(difftime(end,start, units = "mins")),2)
      if(verbose){message("Bootstrapping took: ", total_time, " mins")}

    # end bird/var top level
    }
    names(final_samp) <- loop_thru
    names(coarser_fixes) <- loop_thru

    # and add back the data supplied by the function, with row.numbers you
    #boot_dat = run_all_bird_boots(data = gapped2a, n = nboots)

    gapped2a <- gapped2a %>% ungroup() %>% dplyr::select(-c(Sec2, top_lev, var, Sec, sec_id)) %>% group_by(TagID) # assuming this detailed info of the exact sections will not be needed

    data2 <- structure(.Data = gapped2a, class = c("Track", "grouped_df", "tbl_df","tbl","data.frame"))

    if(is.null(attr(data2, "sub_samp") )){
      attr(data2, "sub_samp") <- "sub_samp"
    }

    attr(attr(data2, "sub_samp"), "boots") <- final_samp
    attr(attr(data2, "sub_samp"), "coarser") <- coarser_fixes
  }

  # 44243 for rounding date version, 44237 for the sequence version, NOT BAD! given they will never match of course

  #data.frame(data2[1:100,])
  #tibble(data2[data2$TagID == 4032,])
  #BTOTrackingTools::assign_rates(BTOTrackingTools::Track(data2))
  #
  #fin2$dt_check <- as.vector(difftime(c(fin2$DateTime[-1],NA),fin2$DateTime,units=Unit))
  #subset(fin2, select = c(dt, dt_check))
  #
  # first idea was way too lengthy as a silly loop across all rows
  #i <<- 1
  #func <- function(x){
  #  ch <- r1[r1$TagID %in% x$TagID & r1$DateTime > x$DateTime_orig & r1$DateTime <  x$DateTime_orig2,]$DateTime
  #  ii <- i +1
  #  print(ii)
  #  i <<- ii
  #  if(length(ch) > 0){
  #    kp <- data.frame(
  #      TagID  = rep(x$TagID, length(ch)),
  #      DateTime = rep(x$DateTime, length(ch)), # the snap2grid time for this fix
  #      DateTime_orig = ch,
  #      diff = rep(x$diff,length(ch)),
  #      dt = rep(x$dt,length(ch))
  #    )
  #  }
  #}
  #test = new2. %>% group_split(TagID, DateTime_orig) %>%
  #  purrr::map_df(~.x %>% func) %>%
  #  ungroup() %>% rbind(.,new2) %>% arrange(TagID, DateTime_orig) %>% # append to the sub-sampled data above, sort by TagID DateTime_orig
  #  group_by(TagID) %>%
  #  mutate(dt = as.vector(difftime(DateTime_orig, lag(DateTime_orig), units= 'secs'))) %>% # recalculating dt with some rather basic nudge up adjustments
  #  ungroup() %>%
  #  mutate(dt = c(dt[-1],NA), dt = if_else(is.na(dt), 0, dt))
  # the above takes too long. This will of course have all the dt values retained from coarser rates also included in the dataset
  # even in purr this is just a loop across all 17K data.frame elements!

  # ------------------------------------------- #
  #### Finalisation
  data2 <- data2 %>% group_by(TagID)

  #### the gapsection function has been called a lot, therefore set back to default, or last used
  #### check data attributes on input
  ##### we need to re-run gap_section as it will have been set to a small value meaning trip
  ##### evaluation will be impossible
  ##### for now running the default? ideally need these darn attributes working properly

  # from potential use of clean_GPS
  attr_test = attr(attr(data2, "general"), "GAP") # these are not working at the moment - likely attributes are lost from tbl conversion!!

  if(nrow(data2) > 0){
    if(!is.null(attr_test)){
      data2 <- gap_section(data2, GAP = as.vector(attr_test), tol = 0, verbose = FALSE, drop = FALSE)
    } else{
      data2 <- gap_section(data2, GAP = 60*60*8, tol = 0, verbose = FALSE, drop = FALSE)
    }

    if(!exists("gap", data)){data2 <- data2 %>% dplyr::select(-gap) }
    if(!exists("gapsec", data)){data2 <- data2 %>% dplyr::select(-gapsec) }
    #data2 <- data2 %>% dplyr::select(-rn) # MUST RETAIN rns for boot_seq option!

  }

  #attributes(data)
  if(nrow(data2) > 2){
    data2 <- structure(.Data = data2, class = c("Track", "grouped_df", "tbl_df","tbl","data.frame"))
  }

  # --------------------------------------------------------- #
  # assign attributes that may have been present at the start
  data2 <- give_attributes(data2, attr_list)

  # --------------------------------------------------------- #
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

  return(data2)

}

#' @rdname sub_samp
#' @export
retrieve_boots <- function(data, drop_coarser = TRUE, verbose = TRUE){

  if(attr(attr(data, "sub_samp"), "method") != "boot_sequencer"){
    stop("This function is only relevant for the boot sampler from the sub_samp function.")
  }

  # extract the row numbers per animal (and var) and boot
  # names of the list under stored boots = $`5026_2014`$`2` e.g. animal_year_boot number
  # sorry, was a bit convoluted to keep var level in there as well separate

  boot_dat = attr(attr(data, "sub_samp"), "boots")

  if(verbose){
    message("Retrieiving boot sub_samp data for ", length(boot_dat), " animals, for ", length(boot_dat[[1]]), " boots")
  }

  i = 1
  anim_list <- list()

  for(i in 1:length(boot_dat)){

    b = 1
    boot_list <- list()
    if(verbose){cli::cli_progress_bar(paste0("Retrieving boots for animal ", i), total = length(boot_dat[[1]]))}
    for(b in 1:length(boot_dat[[1]])){
      #print(b)

      if(verbose){cli::cli_progress_update()}

      boot_list[[b]] = data[data$rn %in% boot_dat[[i]][[b]],]

      # if coarser = TRUE get the coarser fixes too and add in
      if(!drop_coarser){
        coarser_rn = attr(attr(data, "sub_samp"), "coarser")
        coarser = data[data$rn %in% coarser_rn[[i]],]
        boot_list[[b]] <- rbind(boot_list[[b]], coarser) %>% arrange(TagID, DateTime)
      }
      boot_list[[b]]$boot <- b

    }
    if(verbose){cli::cli_progress_done()}
    anim_list[[i]] <- do.call('rbind', boot_list)

  }


  anim_list <- do.call('rbind', anim_list)

  # issue with Track class not liking two rows of data on print (bug that ideally needs fixing)
  if(length(unique(anim_list$DateTime)) <= 2){
    anim_list <- anim_list %>% relocate(boot, .after = rn)
  } else{
    anim_list <- anim_list %>% relocate(boot, .after = rn) %>% structure(.Data = ., class = c("Track", "grouped_df", "tbl_df","tbl","data.frame"))
  }
  return(anim_list)

}












