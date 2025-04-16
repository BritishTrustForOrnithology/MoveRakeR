#' Cleaning the tracking data
#'
#' \code{clean_GPS} is provided to allow some basic cleaning of the raw tracking data to be
#' carried out, i.e. after having read in the raw data using
#' \code{\link{read_track_UvA}} and/or \code{\link{read_track_MB}}. \code{clean_GPS} is used
#' to take out less certain positional fixes, filter out fixes associated with unrealistic trajectory speeds, and further
#' define date-time sections (strings of points) where tags were deemed have valid recording in relation to the programmed GPS sampling rate.
#'
#' @details
#' The single function \code{clean_GPS} is used to clean the data. There are of course many
#' reasons why (here focusing on GPS) animal-borne telemetry loations may have error, such as
#' internal firmware settings of tags e.g. connection parameters with satellites, sampling rate
#' of tags, number of satellites, satellites position in the sky, along with external factors
#' such as temperature, humidity etc. Here, we assume the user is mainly interested in 2D space,
#' so we retain all vertical information in columns \code{altitude} and \code{altitude_agl} - that's a story for another time.
#' Instead here, we carry out very basic cleaning to remove fixes that may be deemed less reliable
#' based on a minimum number of acceptable satellites used for the fix, and for Movebank data, removing inaccurate \code{flt_switch} values.
#' Finally the function includes a trajectory (ground) speed filter based on distance traveled divided by time taken
#' between consecutive fixes to remove points that are more than an
#' unrealistic threshold for the species, that may not have been picked up in the other filters.
#' This also depends on \code{adehabitatLT::as.ltraj}
#' for efficiently computing distance for trajectory speed estimation
#' in the speed filter. Ground speeds will of course vary by species - so it may be best to view
#' them as a histogram - the default value is 50 m/s based on Lesser Black-backed Gulls,
#' which is actually still large. Ultimately this is a little subjective, and for analysis purposes, it could be argued that
#' all error should be retained, but this depends on the purpose of the analysis. This is up to the user's discretion. Also, using trajectory speed has drawbacks
#' when it comes to coarser GPS sampling rates sampling, making it harder to discern potentially erroneous
#' fixes in this way. Similarly at very fine spatio-temporal scales, i.e. with fast sampling
#' less than 60 seconds, a small change in GPS position has a much greater influence on trajectory speed
#' than for coarser sampling schedules. This can be solved by increasing the speed filter threshold value
#' but it may be better to investigate the error potential in traj_speed calculations at these finer scale
#' and the potential sensitivity of the data to such flat decisions. This relationship can be examined further
#' using the \code{tspeed_jit} function.
#'
#' Further very simple options for removing fixes under a certain value of DOP (e.g. pdop, hdop) are provided
#' in the function. Again it is worth investigating the distributions of these variables more fully
#' and the relationship they may have with error in the horizontal dimension. It is also worth noting that in the speed filter, the filter stages happen sequentially, i.e. removing
#' number of satellites and flt_switch values (for Movebank) and then applying the speed filter.
#' So the trajectory speed is recalculated.
#'
#' on a more detailed note, the speed filter was initially coded in a 'forwards' direction in DateTime
#' and so the fix after the current one was always 'blamed' for being incorrect. This may not always
#' be the case for data where there are many gapsections defined, i.e. where there is a greater chance the
#' first fix in a string could be the one incorrect. So in the current version, both forwards and backward checks are made to
#' pick up this issue and correctly identify the errant fix in a string - less of an issue for continuous non gappy data.
#'
#' @param data Input data object, with required columns: TagID, DateTime, longitude, latitude.
#' @param drop_sats Numeric integer value for the minimum number of GPS satellites to drop from the database.
#' Defaults to 3, meaning only 4 and above are retained in the data.
#' @param GAP Numeric integer in seconds. The function serves another use, that of labeling
#' sections of assumed continuous GPS recording for the individual animal.
#' The argument \code{GAP} here is the cut-off i.e. temporal hiatus between two
#' consecutive GPS fixes, which defaults to 28800 seconds (eight hours), although note, the tolerance parameter
#' will also be incorporated (see tol argument).
#' This is a subjective choice for the user, and will depend also on GPS sampling
#' resolution. \code{GAP} is also used to defined \strong{gapsections} in the data,
#' that are sequentially labelled and are used in many further function operations.
#' These are perhaps equivalent in name to \strong{bursts} as in other R packages.
#' Note, gap sections can also be defined after this function has been run using the seaprate
#' standalone function for this purpose: \code{\link{gap_section}}. Also note that in \code{clean_GPS}
#' gap sections (bursts) of fixes of length = 1 are by default EXCLUDED in the speed filter, as in \code{\link{gap_section}}
#' can be dropped to retain \emph{only} consecutive fixes via the \code{GAP} criteria. If a precise GAP is needed with no tolerance,
#' set tol to 0. Units are in seconds.
#' @param tol A tolerance is also applied to the GAP value, useful for example to give lee-way around
#' the gap criteria relating to imprecision of sampling rate in GPS deviating from the initial rate specified.
#' By default this is set as 0.2, i.e. to give a true value used in delineation as: GAP +/- (GAP*0.2) s
#' @param sat_check Logical whether to filter data for satellite count, defaults to TRUE, and if TRUE,
#' the data needs a column called "satellite_used".
#' @param flt_switch logical argument, whether to exclude
#' MoveBank data erroneous flt_switch values. Warning though this can remove fixes erroneously if not careful
#' so it is suggested if not sure to look at Movebank directly and do this separately. This is set to
#' FALSE by default i.e. no flt_switch filtering done.
#' @param pdop Default is numeric 0 indicating do nothing, but if greater than zero, the function will
#' look for a column named pdop, gps.pdop (or gps_pdop) or PDOP and keep only data less than that value.
#' @param hdop Default is numeric 0 indicating do nothing, but if greater than zero, the function will
#' look for a column named hdop, gps.hdop (or gps_hdop) or HDOP and keep only data less than that value.
#' @param speed_filt logical defaulting to TRUE as to whether to use the speed filter in the cleaning steps.
#' Due to potential differences in how users may want to apply this cleaning process, they may want
#' to e.g. to their own cleaning, such as using instantaneous speed instead. In which case, \code{clean_GPS}
#' can be used to ONLY remove satellites and do flt_switch filtering. Further cleaning such as
#' using accuracy of positions, altitude requires bespoke manipulation beyond the scope here.
#' @param drop logical defaults to FALSE, but user can specify TRUE to retain single gapsections
#' that by default get removed from the speed filter as isolated fixes that are at too coarser rate
#' as determined by the \code{GAP} argument
#' @param reproject As above, a logical value whether or not to reproject lat-long WGS84
#' coordinates to the new desired projection as specified via, \code{p4s}.
#' @param sp_thres Numeric value for the minimum threshold (m/s) to be used in the speed filter, which requires
#' inspection and will vary by species. This will depend on the sampling resolution
#' of your data. The function then uses a while loop to remove all points not
#' meeting the threshold level until all offending fixes are removed.
#' @param attrib Logical set to FALSE if detailed attributes should be attached to each
#' data.frame per bird for numbers of fixes removed in various stages from \code{clean_GPS}.
#' These can be accessed afterwards using: attr(data, "data_summary"), where data for Track-family
#' objects is the individual "Track" data.frame per animal.
#' @param p4s The coordinate reference system projection, should it be needed, defaults to NW Europe epsg:3035.
#' @param verbose Logical argument if runtime messages are wanted
#' @param detailed_verbose Logical argument if the very detailed process of the speed filtering
#' part of the function is required. Defaults to TRUE.
#'
#' @seealso [MoveRakeR::clean_GPS], [MoveRakeR::gap_section], [MoveRakeR::sub_samp]
#'
#' @return The function returns a tibble format with an additional \code{Track} class.
#' Additional columns in the objects are also returned:
#' including: \strong{dt}, time difference (secs) between consecutive GPS fixes,
#' \strong{gap}, a binary (1,0) for fixes where a gap was identified and \strong{gapsec}
#' as a numeric integer of sequentially-labelled gapsection strings of telemetry fixes.
#'
#' @examples
#'
#' ## We can use the dplyr %>% notation if we wanted to directly with Track objects
#' indata <- yourdata # data.frame with a minimum of columns named TagID, DateTime, longitude, latitude
#' data <- Track(indata) # optional to convert to in-house class (most functions will still run on non track data but generic S3 methods will not be available for plot and summary)
#' data <- data %>% clean_GPS() # accepting the defaults of the clean_GPS function
#'
#' # this also gives warnings that some satellite count data are found to be NA
#' # and so these data are retained in the filter of number of satellites.
#'
#' # dropping data with 3 satellites, a speed threshold of 50 m/s and labelling gaps in data at 28800 s
#' # with no consideration of hdop or pdop set to 0 for no filtering
#' data = clean_GPS(data, drop_sats = 3, sp_thres = 50, GAP = 28800)
#'
#' # check out where fixes where dropped:
#' attr(data,"data_summary")
#'
#' ## summarise the data, using a further function summary
#' #summary(data)
#'
#' @seealso [MoveRakeR::read_track_MB], [MoveRakeR::tspeed_jit], [MoveRakeR::read_track_UvA], [MoveRakeR::gap_section], [MoveRakeR::summary]
#'
#' @import dplyr
#' @import tibble
#' @import tidyr
#' @import geosphere
#' @import sf
#' @export
clean_GPS <- function(data, drop_sats=3, sp_thres = 50, pdop = 0, hdop = 0, GAP = 28800, tol = 0.2,
                       attrib = TRUE, sat_check = TRUE, flt_switch = FALSE,
                       speed_filt = TRUE, drop = FALSE, reproject = FALSE,
                       verbose = TRUE, detailed_verbose = FALSE, p4s = 3035){

  data_dp <- tibble(data)

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
    if(speed_filt){
      if(verbose){message("Applying trajectory speed filter")}
         }
    if(!speed_filt){
      if(verbose){message("Skipping trajectory speed filter")}
    }

    if(drop){
      if(verbose){message("Removing single gapsection fixes")}
    }
    if(!drop){
      if(verbose){message("Keeping single gapsection fixes")}
    }
  }


  if(verbose){message("Object of ", length(unique(data$TagID)), " animals, ", nrow(data), " rows of data")}


  if(verbose){
    message("tol value of ", tol, "; GAP = ", GAP, " " ,"secs", "; Real GAP = ",GAP," + ", GAP,"*",tol, " = ", GAP+(GAP*tol), " ", "secs")
  }

  # the real gao being used
  GAP = GAP+(GAP*tol)

  data_dp <- tibble(data)
  #data_dp <- tibble(BTOTTdata::LBBGWalB201416[[1]][[1]])

  # # # # #
  #0. Initial tally of data length
  data_s <- data_dp %>% count(TagID)


  # # # # #


  ###########
  #1. Remove <x satellites, check for NAs

  # first check if there are numeric data in the "satellites_used" column to work with
  if(sat_check){
    if(!exists("satellites_used",data)){
      stop("For filtering satellite count, make sure a column called: 'satellites_used' is present, or if no sat data, then add in blank column with this name OR set sat_check argument to FALSE")
    }

    data_sc <- data_dp %>% filter(is.na(satellites_used)) %>% count(TagID)

    if(nrow(data_sc) > 0){

      pc = round((sum(data_sc$n) / nrow(data_dp))*100,2)
      warning("NA satellite data for ", nrow(data_sc)," TagIDs", " (total of ", sum(data_sc$n), " fixes, ", ifelse(pc < 1, "< 1 %", paste0(pc, "%")), " data): NA data will be retained")

    }

    # drop satellites less than min threshold
    #150527-150515

    data_dp <- data_dp %>% filter(satellites_used > drop_sats | is.na(satellites_used))

  }

  # # # # #
  #data_s$sat_n <- nrow(data)
  data_s$sat_n <- data_dp %>% count(TagID) %>% .$n
  # # # # #

  data_dp = data_dp %>% filter(if_any(everything(), ~ !is.na(.)))

  # # # # #
  data_s$DT_NA <- data_dp %>% count(TagID) %>% .$n
  # # # # #

  #############
  ###2. Flt_switch - switch != 0 removed if Move data

  if(flt_switch){

    if(!exists("Type", data)){
      warning("No Type column found but 'flt_switch' is TRUE; if you have MoveBank data and want values flt_switch != 0 removed, add in a column called 'Type' with 'MoveBank' for all values: this allows assessment of the 'flt_switch' column (here an underscore not dot)")
    } else{
      data_dp <- data_dp %>% group_by(TagID) %>% arrange(TagID, DateTime) %>%
        mutate(flt_switch = ifelse(is.na(flt_switch), -99, flt_switch)) %>% # if Movebank, check for flt_switch column being NA, then set to -99 for dropping
        mutate(flt_switch = ifelse(flt_switch != 0, -99, flt_switch)) %>%  # also if anything isn't zero then treat as a flagged bad data
        subset(Type != "MoveBank" | flt_switch != -99) # DROP THE -99s if the column is called Movebank

    }

  }

  #x11()
  #plot(data_dp[data_dp$flt_switch != -99,]$latitude ~ data_dp[data_dp$flt_switch != -99,]$longitude)
  #points(data_dp[data_dp$flt_switch == -99,]$latitude ~ data_dp[data_dp$flt_switch == -99,]$longitude, col = "red")
  #lines(data_dp$latitude ~ data_dp$longitude, col = "grey")

  # # # # #
  data_s$flt_n <- data_dp %>% count(TagID) %>% .$n
  # # # # #

  ###########
  # 3a. pdop
  # check for existence of pdop named column or variants

  if(pdop > 0){

    if(!exists("pdop", data_dp)){
      if(!exists("gps.pdop", data_dp)){
        if(!exists("PDOP", data_dp)){
          if(!exists("gps_pdop", data_dp)){
            warning("No column found matching pdop, gps.pdop, gps_pdop or PDOP, name to one of those.")
          } else{
            data_dp <- data_dp %>% group_by(TagID) %>% arrange(TagID, DateTime) %>%
              filter(gps_pdop < pdop)
          }
        } else{
          data_dp <- data_dp %>% group_by(TagID) %>% arrange(TagID, DateTime) %>%
            filter(PDOP < pdop)
        }
      } else{
        data_dp <- data_dp %>% group_by(TagID) %>% arrange(TagID, DateTime) %>%
          filter(gps.pdop < pdop)
      }
    } else{
      data_dp <- data_dp %>% group_by(TagID) %>% arrange(TagID, DateTime) %>%
        filter(pdop < pdop)
    }

  }

  # # # # #
  data_s$pdop_n <- data_dp %>% count(TagID) %>% .$n
  # # # # #

  ##########
  # 3b. hdop
  # check for existence of pdop named column or variants

  if(hdop > 0){

    if(!exists("hdop", data_dp)){
      if(!exists("gps.hdop", data_dp)){
        if(!exists("HDOP", data_dp)){
          if(!exists("gps_hdop", data_dp)){
            warning("No column found matching hdop, gps.hdop, gps_hdop or HDOP, name to one of those.")
          } else{
            data_dp <- data_dp %>% group_by(TagID) %>% arrange(TagID, DateTime) %>%
              filter(gps_hdop < hdop)
          }
        } else{
          data_dp <- data_dp %>% group_by(TagID) %>% arrange(TagID, DateTime) %>%
            filter(HDOP < hdop)
        }
      } else{
        data_dp <- data_dp %>% group_by(TagID) %>% arrange(TagID, DateTime) %>%
          filter(gps.hdop < hdop)
      }
    } else{
      data_dp <- data_dp %>% group_by(TagID) %>% arrange(TagID, DateTime) %>%
        filter(hdop < hdop)
    }

  }

  # # # # #
  data_s$hdop_n <- data_dp %>% count(TagID) %>% .$n
  # # # # #


  ###########
  #4. Assign gapsections (via 'gap' identification)

  data_dp = data_dp %>% mutate(dt = as.vector(difftime(lead(DateTime), DateTime, units = 'secs')) ) %>% #.$dt
    mutate(gap = if_else(dt > GAP,1,0), rn = row_number()) %>%
    mutate(gap = if_else(rn == max(rn), 1, gap))  %>%
    ungroup() %>%
    mutate(gap0 = gap, gap = lag(gap)) %>% # this is needed across IDs as needing to shunt all rows (lag) down; this enables a 1 in the gap column to start numbering by, and ensures the gap is labelled in the next row to start correct gapsec labelling from (prob could be done in steps above too)
    group_by(TagID) %>%
    mutate(gap_num = case_when(gap == 1  ~ row_number() ),
           gap_num = if_else(min(row_number()) == row_number(), row_number(), gap_num)) %>% # make the first row or each group (TagID) the ro number to label from
    tidyr::fill(gap_num) %>%
    mutate(gapsec = consecutive_id(gap_num), gap = gap0) %>% # set gap back to what it was - it's used in other programs
    #filter(!is.na(gap))
    #group_split() %>%
    #  purrr::map_df(~.x %>% group_by(gap_num) %>% mutate(gapsec = cur_group_id())) %>% # assign sequential gapsections
    #  ungroup() %>%
    dplyr::select(-c(gap_num,rn,gap0)) %>%
    group_by(TagID)

  # the first row always comes out with gap == 1 in new code from above process
  data_dp = data_dp %>%  group_by(TagID) %>% mutate(gap = if_else(row_number() == min(row_number()), 0, gap))

  #table(data_dp[data_dp$TagID %in% tid[5],]$gapsec)


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

  # not sure if needed but this would drop out any added extra tripnumbering rows!
  data_dp = data_dp %>% filter(!duplicated(DateTime))

  ############
  # 5. if drop = TRUE then drop OUT single gapsection fixes as cannot be assessed in the speed filter
  if(drop){

    data_dp = data_dp %>% group_by(TagID,gapsec) %>% mutate(nn = n()) %>%
      filter(nn > 1) %>% # drop any single gapsecs
      dplyr::select(-nn) %>%
      ungroup()

  }

  ############
  # 6. for all sections of continuous data, filter out any data that is apparently very fast, too fast for reality. Default i 30 m/s which is stil VERY fast
  # this also accounts for displacement from a central place in that it is done sequentially via speeds until everything is below the threshold
  # here using a custom function mapped by TagID and gapsection

  if(speed_filt){

    if(verbose){message("----- Custom speed filter -----")}

    #data_dp <- data_dp %>% group_by(TagID, gapsec) %>% group_split()
    #
    #custom_test(data_dp[[2]])
    #x = data_dp[[1]]

    #x = subdat
    custom_test <- function(x){

      thres <- sp_thres+20 # initialise while loop with value higher than selected as sp_thres
      thres_to_meet <- sp_thres

      dat <- x # note really needed but was for code testing, replace dat with x ideally in cleaner version
      #dat$rn <- 0

      if(length(dat$DateTime) > 1){ # only works on >1 gapsecs, i.e. can't calculate a dt value between DateTimes for row number == 1

        while(thres > thres_to_meet){
          #print("foobar")
          if(detailed_verbose){message("====== Starting threshold: ",thres, " ======")}

          # remove any issues with latitude being zero - this is problematic if a species is tracked at the equator!! Hmmmm, but were instances in some cases where this happened for European projects where the species was obviously not there. Maybe needs a toggle.
          dat <- dat %>% filter(latitude > 0) %>% filter(!is.na(latitude)) %>%
            mutate(rn = row_number())  # assign rownames for look up

          # switch to adhabitat here which couldn't devetail properly with dplyr here
          # But..... this is slow! it is doing more computations than we ever really needed
          # all we need is the distance, and the time for trajectory speed.
          #data2a <- tibble(adehabitatLT::as.ltraj(as.data.frame(subset(dat,select=c(X,Y))),dat$DateTime,id=1)[[1]])

          #data.frame(subset(data2a, select = c(dist,dt)))

          # Quicker to use disHaversine, geosphere::distVincentySphere seems the same
          data2A = dat %>%
            #group_by(id) %>%
            mutate(dist = c(geosphere::distHaversine(cbind(longitude, latitude)), NA),
                   dt = as.vector(difftime(lead(DateTime), DateTime, units = 'secs'))
                   ) %>%
            ungroup()

          # ISSUE IDENTIFIED #1 - we have a fix for a Movetech bird on the Isle of May
          # where in a specific gapsection, the FIRST point is the one that is wacky
          # IT NEEDS BOTH FORWARD AND BACKWARD

          # ASSESS FORWARDS DIRECTION
          data2a <- data2A %>% mutate(rn = row_number(),
                                      traj_speed = dist / dt,
                                      traj_speed = lag(traj_speed, default= 1)) # pad with '1' at the start, removing last row

          data2a$cond1 <- ifelse(data2a$traj_speed > thres,1,0)
          data2a$cond1[1] <- 1 # always make the first condition 1 at start and end

          #data2a$traj.speed > thres # 701

          # ASSESS REVERSE DIRECTION
          data2B <- data2A[rev(order(data2A$DateTime)),]
          data2b <- data2B %>%
                    mutate(
                      dist = c(geosphere::distHaversine(cbind(longitude, latitude)), NA),
                      dt = as.vector(difftime(DateTime, lead(DateTime), units = 'secs')),
                      traj_speed = dist / dt,
                      traj_speed = lag(traj_speed, default= 1)
                    )
          data2b$cond2 <- ifelse(data2b$traj_speed > thres,1,0)
          data2b$cond2[1] <- 1
          data2b <- data2b[order(data2b$DateTime),]
          data2a$cond2 <- data2b$cond2


          #subset(data2a, select = c(DateTime,dist,dt,traj_speed,rn, cond1, cond2))

          # OK this is more complicated than expected. Frequently for tiny sampling rates
          # we can get the situation where in both forwards and backwards there are no
          # double 1s identified, May 5894:
          #subset(data2a[698:705,], select = c(DateTime,dist,dt,traj_speed,rn, cond1, cond2))

          # could this suggest the fix is actually OK and the harshness of the filter is to blame due to scale??

          #plot(data2a[690:720,]$latitude ~ data2a[690:720,]$longitude)
          #lines(data2a[690:720,]$latitude ~ data2a[690:720,]$longitude)

          # so if there are no obvious 'double 1s' but there are still erroneous fixes, suggest
          # reverting to the forwards methods as this indicates two points IN A ROW
          # are at fault so one needs removing before other. This is rather tricky!
          # Or we bump up the threshold at these fine scales...??

          ############################
          ## 1. assess if any values are above the threshold and remove ONLY ONE (to allow reassessment in the while loop)
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

          #if(nrow(double_ones) == 0 & any(data2a$traj_speed > thres_to_meet)){
          #  # revert to previous not so stable approach in forwards only direction...
          #  above_thresh = data2a[data2a$traj_speed >= thres_to_meet,]
          #  if(detailed_verbose){message(" ---> no double-X's, reversion to forward point deletion <--- ")}
          #} else{
          #  if(detailed_verbose){message("---> double-X point removal <---")}
          #}

          rns_keep <- data2a$rn

          #subset(data2aa[698:705,], select = c(DateTime,dist,dt,traj_speed,rn))

          # ISSUE IDENTIFIED is that at very small speeds, a small change in GPS can lead to a big
          # perceived difference in trajectory ground speed
          # this can drop out fixes in an overly harsh way
          # ideally we would model the error around GPS for a known error amount and look at
          # the variation in speeds estimated.

          if(nrow(above_thresh) > 0){
            if(detailed_verbose){message("~~~ point(s) identified to drop. Dropping point no ",above_thresh$rn[1],": ", above_thresh$DateTime[1], " ~~~")}
            # then take out the FIRST ONLY ONTHIS LOOP
            rns_keep <- rns_keep[rns_keep != above_thresh$rn[1]] # take out first in the datetime order - could be two points far away in datetime, in which case new ltraj assignment should still find the second etc on next while loop
          }

          ## now select rownames as a subset i.e. matching those that meet above thres.to.meet condition
          dat <- dat[dat$rn %in% rns_keep,]

          ## reassign thresh value based on new ltraj of ttrips2 after matching condition
          #data2aa <- tibble(adehabitatLT::as.ltraj(as.data.frame(subset(dat,select=c(X,Y))),dat$DateTime,id=1)[[1]])

          data2Aa = dat %>%
            #group_by(id) %>%
            mutate(dist = c(geosphere::distHaversine(cbind(longitude, latitude)), NA),
                   dt = as.vector(difftime(lead(DateTime), DateTime, units = 'secs'))
            ) %>%
            ungroup()

          data2aa <- data2Aa %>% mutate(rn = row_number(),
                                      traj_speed = dist / dt,
                                      traj_speed = lag(traj_speed, default= 1)) # pad with '1' at the start, removing last row


          ## check if there is still a point beyond thres_to_meet via max of REMAINING dt/dists
          if(!is.na(data2aa$dist[1])){
            thres <- max((data2aa$dist / data2aa$dt),na.rm=TRUE)
            if(detailed_verbose){message("====== thres after: ", thres, " ======")}
            #dat <- data2aa # I think we probably assign this back to dat right??
          }
          if(is.na(data2aa$dist[1])){
            #print("break")
            break
          }

          #XX<<- "TEST"
          #print(thres)
          #thres = 29
          #print(thres)

        }

      }

      return(dat)
    }

    # why is mapping it so much slower??
    #data_dp <- data_dp %>% group_by(TagID, gapsec) %>% group_split() %>%
    #  purrr::map_df(~custom_test(.x)) # end map
    #
    # simple loop? seems faster
    #data_dp_spl <- data_dp %>% group_by(TagID, gapsec) %>% group_split()
    #q = 1
    #data_dp_ls <- list() # Would be better not to have to split and recombine as it slows down in rbind
    #for(q in 1:length(data_dp_spl)){
    #  data_dp_ls[[q]] <- custom_test(data_dp_spl[[q]])
    #}
    #if(verbose){message("rbinding after group_split()")}
    #data_dp = do.call('rbind',data_dp_ls)

    ##############################
    # replace in position? better than rbinding
    #q = 1; g = 17
    data_dp$rn = 0
    data_dp$rn2 = 1:nrow(data_dp)

    # remember fixes before removal of points from speed filter
    vv = data.frame(table(data_dp$TagID))

    # Note for dev: it would be more efficient to identify where there are traj speed violations!
    # rather than running through everything...but leaving as it is for now

    ##############################
    # LOOP
    tid = unique(data_dp$TagID)
    for(q in 1:length(tid)){
      if(verbose){message("TagID: ", tid[q])}
      gs = unique(data_dp[data_dp$TagID %in% tid[q],]$gapsec)
      for(g in 1:length(gs)){
        if(detailed_verbose){message("G A P   S E C T I O N   ", g, ", TagID: ", tid[q])}
        subdat = data_dp[data_dp$TagID %in% tid[q] & data_dp$gapsec %in% gs[g],]
        subdat0 = custom_test(subdat)

        # remove non-matching rows from original
        rdrop = subdat$rn2[which(!subdat$rn2 %in% subdat0$rn2)] # rows removed

        if(length(rdrop) > 0){
          if(detailed_verbose){message("DROPPING FIXES")}
          data_dp = data_dp[!data_dp$rn2 %in% rdrop,]
        }
      }

      if(verbose){

        dd = nrow(data_dp[data_dp$TagID %in% tid[q],] )
        orig_n = vv[vv$Var1 %in% tid[q],]$Freq

        if(dd < orig_n){
          message("TagID: ", tid[q], ": Dropped: ", orig_n - dd, " fixes due to speed filter")
        }

      }

    }

    # # # # #
    data_s$spe_n <- data_dp %>% count(TagID) %>% .$n
    # # # # #

    if(sum(data_s$n) - nrow(data_dp) > 0){
      if(verbose){message("~@~@~@~@~@ Dropped ", sum(data_s$n) - nrow(data_dp), " fixes after all choices ~@~@~@~@~@")}
    } else{
      if(verbose){message("~@~@~@~@~@ No fixes dropped :) ~@~@~@~@~@")}
    }

  }

  # previous code (removed) was siphoning out the gapsecs of length 1 and rbinding them back in again to avoid considering in the while loop
  # but from code above the dt values are NA already at the end of a gap (rightly or wrongly!) - should dt not be as is? within TagID?

  data_dp <- data_dp %>% group_by(TagID) %>%
    mutate(dt = as.vector(difftime(lead(DateTime), DateTime, units = 'secs')))

  if(exists("rn", data_dp)){ data_dp <- data_dp %>% dplyr::select(-c(rn)) }
  if(exists("rn2", data_dp)){ data_dp <- data_dp %>% dplyr::select(-c(rn2)) }

  if(!is.null(data_dp)){

    if(attrib){
      attributes(data_dp)$data_summary <- data_s
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

  attr(attr(data_dp, "general"), "drop") <- get("drop")
  attr(attr(attr(data_dp, "general"), "drop"),"function") <- "clean_gps"

  attr(attr(data_dp, "general"), "p4s") <- get("p4s")
  attr(attr(attr(data_dp, "general"), "p4s"),"function") <- "clean_gps"

  attr(attr(data_dp, "clean_gps"), "drop_sats") <- get("drop_sats")
  attr(attr(data_dp, "clean_gps"), "sp_thres") <- get("sp_thres")
  attr(attr(data_dp, "clean_gps"), "sat_check") <- get("sat_check")
  attr(attr(data_dp, "clean_gps"), "flt_switch") <- get("flt_switch")


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
  trip_stats_attr <- attr(data, "trip_stats")

  attr_list <- list()
  attr_list[[1]] <- if(!is.null(assign_rates_attr)){assign_rates_attr } else{NA}
  attr_list[[2]] <- if(!is.null(general_attr)){general_attr}else{NA}
  attr_list[[3]] <- if(!is.null(clean_GPS_attr)){clean_GPS_attr}else{NA}
  attr_list[[4]] <- if(!is.null(define_trips_attr)){define_trips_attr}else{NA}
  attr_list[[5]] <- if(!is.null(sub_samp_attr)){sub_samp_attr}else{NA}
  attr_list[[6]] <- if(!is.null(trip_stats_attr)){trip_stats_attr}else{NA}

  names(attr_list) <- c("assign_rates_attr", "general_attr", "clean_GPS_attr", "define_trips_attr", "sub_samp_attr", "trip_stats_attr")

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

  return(data)

}

























