#' Assess trajectory speed error over sampling rate
#'
#' The function \code{tspeed_jit} can be used to understand the variance in trajectory speed over
#' varying GPS sampling rate to inform the speed trajectory threshold used in \code{clean_GPS}.
#'
#' @details
#' Within the \code{clean_GPS} function, a trajectory speed filter is used to
#' estimate likely erroneous GPS fixes consecutively in DateTime strings of points. Very large
#' speeds beyond that of the upper limit of travel speed of an animal would be more likely to be flagged
#' as inccorect. However, this filter is applied in a blanket fashion across all sampling rates that
#' may have been programmed for the tag used. With very small sampling rates, small deviation in the
#' GPS xy horizontal position could have a bigger impact on the the estimated trajectory speed
#' with it therefore being more likely erroneous fixes may be flagged even if the chosen speed filter
#' is beyond the maximum speed of an animal. This can be easily assessed through quantifying the error spread
#' of the trajectory speed around fixes set to different sampling rates. This is provided in the
#' \code{tspeed_jit} function, where a 'jitter' of random noise as inserted around the GPS fixes and then
#' the data bootstrapped to assess the effect on the trajectory speeds. A non-linear function can then be fitted
#' to assess the relationship in variance from the true trajectory speed, and can inform the trajectory speed
#' filter used in \code{clean_GPS}.
#'
#' Of course it is quite simple just to increase the overall trajectory speed threshold used in \code{clean_GPS()}
#' to circumvent these issues, i.e. an animal travelling to the equator and back from the UK in five minutes or 10 seconds will
#' be flagged as erroneous if the speed threshold is much higher than the maximum speed of the animal. The
#' \code{tspeed_jit} function simply serves as a tool to understand the nature of the data.
#'
#' The \code{tspeed_jit} makes use of \code{assign_rates} to bin rates into appropriate segments or cuts of the data.
#' These can be specified with a tolerance value (see assign_rates() for more details). Here a range of likely rates
#' are selected up to 10800 s. The \code{tspeed_jit} also recalculates 'gapsections' in the data, although these
#' are not strictly used at present so consider them deprecated for \code{tspeed_jit}.
#'
#' Notes:
#'
#' The variations we see in traj_speed over sampling rate will depend on the
#' behaviour of the bird, i.e. resting vs travelling, and if sampling rate is biased to a particular behaviour, this
#' should also be borne in mind.
#'
#' The approach here is based on geographic coordinates, with a jitter the same in lat-long dimensions; of course
#' that will not hold across much of the globe so further improvements can be made using \code{sf}. Tests done at mid
#' latitudes (e.g. 52 deg N) suggest results are similar to UTMs. For very large datasets this code will run slowly. Consider this code therefore as in development
#' that will be look to be improved in the future.
#'
#' @param data Input data object, of class \code{data.frame}, \code{tibble-family} or \code{data.table}, with required columns: TagID, DateTime, longitude, latitude
#' @param nsim The number of jitter simulations to run for the data, default is 50.
#' @param j This is the jitter value, which is simply a random selection made between -j and j,
#' via \code{runif(n = n(), min = -j, max = j)}. Larger j valules will thus sample from a wider normally-distributed spread of error.
#' @param GAP Currently specified as a default of 28000 s but as noted, this is a deprecated element and should not be worried bout in this function.
#' Larger dt values between fixes out to the upper bounds of the gaps in the data are discarded in this assessment.
#' @param breaks The threshold for binning the time between successive fixes ('dt') into easy-to-interpret segments; see the
#' \code{assign_rates} function for details.
#' @param tol The tolerance value around the spread of data to bin time between successive fixes ('dt') into; see
#' the \code{assign_rates} function for details.
#' @param verbose Logical should detailed trip counting be displayed, defaults to TRUE.
#'
#' @return
#' The function outputs a simple data.frame of TagID, DateTime, dt, rate, sim, (sum number) sim_ (sim name, not needed really)
#' and value. Value is the difference in speed from the actual GPS trajectory speed to the jitter, for each point, having been
#' jittered randomly (normal distribution) bounded by the j argument.
#'
#' The output also contains an additional summary of the mean, standard deviation and sample size per rate
#' as an additional attribute called 'summary', i.e. attr(data, "summary").
#'
#' @examples
#'
#' # testing a 0.0005 jitter ca 50 m around fixes, across a range of sampling rates
#' # this test was made on a dataset of Lesser Black-backed Gulls in southern England.
#'
#' test = tspeed_jit(data_in, nsim = 50) # sample dataset used was 56 animals (2.5 million fixes), for 50 simulations
#'
#' install.packages('drc')
#' library(drc)
#'
#' # Fit an exponential decay model
#' model <- drm(value ~ rate, data = test, fct = EXD.3()) # Warning: obviously not accounting for pseudoreplication
#' summary(model)
#' plot(model)
#'
#' @import dplyr
#' @import tidyr
#' @import tibble
#' @rawNamespace import(data.table, except = c(month, hour, quarter, week, year, wday, second, minute, mday, yday, isoweek))
#'
#' @export
tspeed_jit <- function(data, nsim = 50, j = 0.0005, GAP = 28800,
                       breaks = c(1, 5, 10, 20, 60, 100, 120, 300, 600, 900, 1200, 1800, 3600, 7200, 10800),
                       tol = c(0,0.5,0.5,0.5,0.5,rep(0.4,3),rep(0.3,5),rep(0.2,2)),
                       verbose = TRUE){
  # d = 10,

  if(verbose){message("Running traj speed assessment for Haversine jitter for ", length(unique(data$TagID)), " animals")}
  if(verbose){message("Using j = ", j , " (approx. ", j*(111/0.001), " m)")}
  if(verbose){message("Gapsection defined = ", GAP, " s (larger gaps omitted for this experiment)")}

  if(nrow(data) > 500000){message("Large data, lower expectations of run time! ...")}

  # Assign gapsections (via 'gap' identification) - same code at the moment as in clean_GPS()
  data_dp <- tibble(data)
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

  #######################################
  # on all the data with gapsection
  #data_dp2 <- data_dp %>% group_by(TagID, gapsec) %>% group_split() # not liking the speed of mapped functions in hindsight
  #x = data_dp2[[1]]

  #gap_nm <- unique(data_dp$gapsec)
  #x = data_dp[data_dp$gapsec %in% gap_nm[1],]
  x = data_dp
  jitter_coords <- function(x, nsim = 50){

    #df <- list()
    df <- subset(x, select = c("TagID", "DateTime", "longitude", "latitude")) %>%
      mutate(
        dt = as.vector(difftime(lead(DateTime), DateTime, units = 'secs')),

        dist = c(geosphere::distHaversine(cbind(longitude, latitude)), NA),
        traj_speed = dist / dt,
        traj_speed = lag(traj_speed, default= 1)
      )

    # sf option but similar results obtained through haversine, and slows down run time
    #df_sf <- sf::st_as_sf(df, coords = c("longitude", "latitude")) %>%
    #  sf::st_set_crs(4326) %>%
    #  sf::st_transform(3035)
    #
    ## estimate distance between consecutive fixes
    #dists <- sf::st_distance(df_sf[1:nrow(df_sf)-1,], df_sf[2:nrow(df_sf),], by_element = TRUE)
    #dists <- c(as.vector(dists),NA)
    #
    #df_sf$dist_sf <- dists
    #head(df_sf)
    #df_sf$dist[1:10]
    #df_sf$dist_sf[1:10]

    #df_sf = df_sf %>% mutate(
    #  traj_speed_sf = dist_sf / dt,
    #  traj_speed_sf = lag(traj_speed_sf, default= 1)
    #)
    #
    #proj_coords = data.frame(sf::st_coordinates(df_sf))

    #df <- sf::st_drop_geometry(df_sf)
    #df$longitude <- x$longitude
    #df$latitude <- x$latitude
    #df = cbind(df, proj_coords)

    # ------------------- #
    #i = 1
    for(i in 1:nsim){
      print(i)
      df <- df %>%
        mutate(
          # jitter straight on the wgs84 coords
          long_jit = longitude + runif(n = n(), min = -j, max = j),
          lat_jit  = latitude  + runif(n = n(), min = -j, max = j)
          #,
          # jitter by x m on the projected UTMs
          #sf_long_jit = X + runif(n=n(), min = -d, max = d),
          #sf_lat_jit = Y + runif(n=n(), min = -d, max = d)

        ) %>%
        ### wgs84
        mutate(
          dist = c(geosphere::distHaversine(cbind(long_jit, lat_jit)), NA),
          traj_speed_jit = dist / dt,
          traj_speed_jit = lag(traj_speed_jit, default= 1),
          ## work out the difference in trajectory speed delta
          traj_speed_diff = traj_speed_jit - traj_speed,
          traj_speed_diff = ifelse(traj_speed_diff < 0, traj_speed_diff*-1, traj_speed_diff),
          sim = i
        ) %>%
        ### UTM - a nice to have but slows things down - of course 0.001 lat long is not constant!
        #mutate(
        #  # project the sf_long_jit and sf_lat_jit UTMs? Or just use as is
        #  xx = sf::st_as_sf(.,coords = c("sf_long_jit", "sf_lat_jit")) %>%  sf::st_set_crs(3035)
        #  ,
        #  ###xx = sf::st_as_sf(., coords = c("long_jit", "lat_jit")) %>%  sf::st_set_crs(4326) %>%  sf::st_transform(3035),
        #  dist_sf_jit = c(as.vector(sf::st_distance(xx[1:nrow(xx)-1,], xx[2:nrow(xx),], by_element = TRUE)),NA),
        #
        #  # as above get the traj_speed and difference
        #  traj_speed_sf_jit = dist_sf_jit / dt,
        #  traj_speed_sf_jit = lag(traj_speed_sf_jit, default= 1)
        #  ,
        #  # work out the difference in trajectory speed delta
        #  traj_speed_sf_diff = traj_speed_sf_jit - traj_speed_sf,
        #  traj_speed_sf_diff = ifelse(traj_speed_sf_diff < 0, traj_speed_sf_diff*-1, traj_speed_sf_diff),
        #
        #) %>%

        dplyr::select(TagID, DateTime, traj_speed_diff) %>%  #traj_speed_sf_diff ##### traj_speed_jit, traj_speed_sf_jit
        left_join(df, by = c("TagID", "DateTime")) %>%
        #rename(!!quo_name(paste0("sim_",i, "_tsd")) := "traj_speed_diff")  %>%
        #rename(!!quo_name(paste0("sim_",i, "_tsd_sf")) := "traj_speed_sf_diff")

        rename(!!quo_name(paste0("sim_",i, "_ts")) := "traj_speed_diff")  #%>%
      #rename(!!quo_name(paste0("sim_",i, "_ts_sf")) := "traj_speed_sf_diff")

      df$sim <- i
    }

    # for each sim, then just take mean of the traj diffs
    #library(data.table)

    #return(df)

    if(verbose){message("Binning rates using assign_rates function ... ")}
    # use the assign rates function to bin the rates to something sensible
    df = assign_rates(data = df, breaks = breaks,  tol = tol, out = NULL)
    if(verbose){message("Done. ")}
    df <- data.frame(df)

    df00 <- subset(df, select = c(-latitude,-longitude,-traj_speed, -dist)) #-X,-Y, -traj_speed_sf, -dist_sf
    df1 = df00[,names(df00) %in% names(df00)[!grepl("_sf", names(df00))]]

    # sf version separate from latlong version (they are very similar - for function run time taking out the sf for now)
    #df0 = df1[,!names(df1) %in% names(df1)[grepl("_ts", names(df1))]]
    #df2 = cbind(df0, df[,names(df) %in% names(df)[grepl("_sf", names(df))]])

    # or long form after all
    # long form across all sims?
    #library(data.table)
    df2_long = data.table::melt(setDT(df1), id.vars = c("TagID","DateTime", "dt", "rate", "sim"), variable.name = "sim_")
    df2_long = df2_long[!grepl("<",df2_long$rate),] # bigger gaps removed
    df2_long$rate <- as.numeric(df2_long$rate)

    return(df2_long)

  } # end jitter_coords

  # ------------------------- #
  #store <- list()
  #for(g in 1:length(gap_nm)){
  #  print(g)
  #  x = data_dp[data_dp$gapsec %in% gap_nm[1],]
  df = jitter_coords(data_dp, nsim = nsim)

  df <- data.table::data.table(df)
  ##### this should be done across all at the end!
  sds = data.frame(df[,lapply(.SD,sd), by = c("TagID", "rate"), .SDcols = "value"])
  mns = data.frame(df[,lapply(.SD,mean), by = c("TagID", "rate"), .SDcols = "value"])
  ns = data.frame(df[,.N, by = c("TagID", "rate")])

  names(sds)[names(sds) %in% "value"] <- "sd"
  names(mns)[names(mns) %in% "value"] <- "mn"
  names(ns)[names(ns) %in% "value"] <- "N"

  a = merge(sds,mns, by = c("TagID", "rate"))
  b = merge(a,ns, by = c("TagID", "rate"))

  b = b[order(b$TagID,b$rate),]
  b = b[!is.na(b$rate),]

  #mns = df2_long[,lapply(.SD,mean), by = c("TagID", "rate"), .SDcols = "value"]
  #
  # take coefficient of variation = sd / xbar?
  # or surely this is more just a quantification of the variance?
  # sds$value / mns$value
  #sds = sds[!grepl("<",sds$rate),]
  #sds$rate <- as.numeric(sds$rate)
  #boxplot(df2$value ~ df2$rate)
  #
  #boxplot(df2_long$value ~ df2_long$rate)

  #out <- list()
  #out[[1]] <- df
  #out[[2]] <- b # summary

  attr(df,"summary") <- b

  return(df)

}













