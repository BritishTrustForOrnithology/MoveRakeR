#' Tabulated summary of trip statistics for report presentation
#'
#' @description
#' \code{tabulate_trips} is a simple summary function to tabulate information on 'trips' of
#' animals from a central place. This table follows on from initial annotating of telemetry
#' data as trips through function \code{define_trips} and subsequent running of function
#' \code{trip_stats} to produce trip-level information per animal.
#'
#' @param data A \code{Trip} object, from function \code{\link{trip_stats}}.
#' @param roundval Numeric integer for the round value of decimal places for summary metrics, set to 2 as default.
#' @param rm_inc Logical defaulting to TRUE, for whether incomplete trips should be removed
#' from statistics.
#' @param TD_unit One of three options for the units of trip duration; defaults to 'days'
#' but the user can specify 'mins' or 'hours'.
#' @param by Additional variables to summarise data over, such as year perhaps, or stage of season,
#' which need to be present in the \code{Trip} data supplied. Means for 'all' animals are also estimated
#' for all animals, as well as for the 'by' grouping (defaults to NULL).
#' @param option a character vector of either \code{"means"} or \code{"boxplots"}; these options are for how
#' the user wants the metrics summarised, either as means+/-SD and (max) value, for the "means" option,
#' or, probably more appropriately, presenting as median values of the distribution (upper and lower quartile presented in parentheses),
#' and max value presented in square brackets, for the "boxplots" option. Obviously see \code{graphics::boxplot} on which these
#' parameters are extracted.
#'
#' @return
#' A \code{data.frame} of several variables, depending on those available and elected by the user,
#' from earlier choices made in the \code{trip_stats} function.
#' The function will automatically sumarise core variables of
#' - \code{Distmax}, representing the maximum distance of distal point
#' - \code{DistTotal}, the total distance traveled on the trip, and
#' - \code{TripDur}, and the time elapsed from start to finish, respectively.
#' All units of distance are in km and time in hours.
#' Also outputted in all instances of use of the function are:
#' - \code{TagID}, for the animal identifier,
#' - \code{n}, number of trips for number of trips the animal made,
#' - \code{incompl} and the number of "incomplete" trips identified via gapsections present.
#' If offshore and inshore distinctions have been used in the' \code{trip_stats} function, then
#' additional columns are processed to give:
#' - \code{DistOff} representing animal-level summaries of maximum distances traveled per trip (distal point)
#' within offshore areas (as delineated by function \code{offshore}).
#' - \code{DistOn}, the same as above for inshore areas.
#' - \code{n_off} Number of trips offshore.
#' - \code{n_on} Number of trips onshore.
#' - \code{incompl_off} Number of incomplete trips offshore.
#' - \code{n_compl_off} Number of complete trips offshore.
#' - \code{incompl_on} Number of incomplete trips onshore.
#' - \code{n_compl_on} Number of complete trips onshore.
#'  Further, should distance-to-coast information be present within input Trip classes fed to
#'  \code{tabulate_trips},' then the function will summarise additional variables in the returned output:
#' - \code{DistCoastOff}, the maximum distance from the coast for offshore strata, and
#' - \code{DistCoastOn},  the same as above for inshore areas.
#'
#' @details
#' The sole purpose of this function is to make reporting trip statistics easier,
#' and repeatable. The outputs from this function are contingent on other choices
#' made throughout the data processing. Also output in the function output is a line representing "All" birds,
#' including for additional selection of other variables in the "by" argument.
#' Note, these 'all' columns are arithmetic means across birds thus, birds with more trips will contribute more to this value.
#' Options for offshore/onshore delineation and distances reached inland and offshore from the
#' coast (stemming from choices in \code{trip_stats}) are also provided.
#'
#' @seealso [MoveRakeR::trip_stats], [MoveRakeR::define_trips]
#'
#' @examples
#'
#' ColLon = -3.185
#' ColLat = 54.045
#' p4 <- sf::st_crs((paste("+proj=laea +lon_0=", ColLon," +lat_0=", ColLat, " +units=m", sep="")))
#'
#' indata <- yourdata # data.frame with a minimum of columns named TagID, DateTime, longitude, latitude
#' indata <- Track(indata) # optional to convert to in-house class (most functions will still run on non track data but generic S3 methods will not be available for plot and summary)
#'
#' trip_test <- indata %>% clean_GPS() %>% # 8 hour gapsections by default
#'   define_trips(method="rect", lls = c(-3.2, 54.0553,-3.1689, 54.0437), p4s = p4) %>%
#'   trip_stats()
#'
#' # tabulate trips by year
#' tabulate_trips2(trip_test, by = "year")
#'
#' # or just overall
#' tabulate_trips2(trip_test)
#'
#' # This function could easily be better generalised in future if more variables from trip stats were added, e.g. tortuosity etc
#'
#' @export
tabulate_trips <- function(data, roundval = 2, TD_unit = 'days', by = NULL,
                           rm_inc = TRUE, option = "means"){

  if(TD_unit == 'days'){
    data$TripDur <- data$TripDur/24
  }
  if(TD_unit == 'hours'){
    # do nothing - already in hours
    #data$TripDur <- x$TripDur*24
  }
  if(TD_unit == 'mins'){
    data$TripDur <- data$TripDur*60
  }

  #by = NULL

  if(!is.null(by)){
    if("year" %in% by){
      if(!exists("year",data)){
        data$year <- lubridate::year(data$DateTime)
      }
    }
  }

  data <- data %>% ungroup()
  # note for future dev would be better if the time units were time formatted not <dbl> bypassing need for TD_unit

  # take means of key variables, excluding bearing here - individual animal level

  # assess how many trips are incomplete
  n_incompl = data %>% group_by(TagID, !!!syms(unique(by))) %>% summarise(incompl = sum(incompl), .groups = "keep")
  n_total = data %>% group_by(TagID, !!!syms(unique(by))) %>% summarise(n = length(incompl), .groups = "keep")
  n_ <- left_join(n_incompl, n_total, by = c("TagID", by))
  n_$n_compl <- n_$n - n_$incompl

  # also needs doing for the 'all' data row
  n_incompl_all = data %>% ungroup() %>% summarise(incompl = sum(incompl), .groups = "keep") %>% mutate(TagID = "all")
  n_total_all = data %>% ungroup() %>% summarise(n = length(incompl), .groups = "keep") %>% mutate(TagID = "all")
  n_all <- left_join(n_incompl_all, n_total_all, by = "TagID")
  n_all$n_compl <- n_all$n - n_all$incompl

  # do we also want to do the total using the by variable??
  n_incompl_all_BY = data %>% group_by(!!!syms(unique(by))) %>% summarise(incompl = sum(incompl), .groups = "keep") %>% mutate(TagID = "all")
  n_total_all_BY = data %>% group_by(!!!syms(unique(by))) %>% summarise(n = length(incompl), .groups = "keep") %>% mutate(TagID = "all")
  n_all_BY <- left_join(n_incompl_all_BY, n_total_all_BY, by = c("TagID", by))
  n_all_BY$n_compl <- n_all_BY$n - n_all_BY$incompl

  n_all <- full_join(n_all, n_all_BY, by = c("TagID", "n", "incompl", "n_compl"))

  # exclude the incomplete trips?
  if(rm_inc){
    data <- data %>% filter(incompl == 0) %>% group_by(TagID, !!!syms(unique(by)))
  }

  # ------------------------------------------------------- #
  # Main process, mean DistMax, TotalDist and TripDur
  # ------------------------------------------------------- #

  if(option == "means"){
    mns = data %>% group_by(TagID, !!!syms(unique(by))) %>% summarise(
      DistMax = paste0(round(mean(DistMax,na.rm=TRUE),roundval), "±", round(sd(DistMax,na.rm=TRUE),roundval)," (",round(max(DistMax,na.rm=TRUE),roundval),")"),
      TotalDist = paste0(round(mean(DistTotal,na.rm=TRUE),roundval), "±", round(sd(DistTotal,na.rm=TRUE),roundval)," (",round(max(DistTotal,na.rm=TRUE),roundval),")"),
      TripDur = paste0(round(mean(TripDur,na.rm=TRUE),roundval), "±", round(sd(TripDur,na.rm=TRUE),roundval)," (",round(max(TripDur,na.rm=TRUE),roundval),")"),
      .groups = "keep")

    # all
    mns_all <-  data %>% ungroup() %>% summarise(
      DistMax = paste0(round(mean(DistMax,na.rm=TRUE),roundval), "±", round(sd(DistMax,na.rm=TRUE),roundval)," (",round(max(DistMax,na.rm=TRUE),roundval),")"),
      TotalDist = paste0(round(mean(DistTotal,na.rm=TRUE),roundval), "±", round(sd(DistTotal,na.rm=TRUE),roundval)," (",round(max(DistTotal,na.rm=TRUE),roundval),")"),
      TripDur = paste0(round(mean(TripDur,na.rm=TRUE),roundval), "±", round(sd(TripDur,na.rm=TRUE),roundval)," (",round(max(TripDur,na.rm=TRUE),roundval),")"),
      .groups = "keep") %>% mutate(TagID = "all")

    ### and using the by variable
    mns_all_BY <-  data %>% group_by(!!!syms(unique(by))) %>% summarise(
      DistMax = paste0(round(mean(DistMax,na.rm=TRUE),roundval), "±", round(sd(DistMax,na.rm=TRUE),roundval)," (",round(max(DistMax,na.rm=TRUE),roundval),")"),
      TotalDist = paste0(round(mean(DistTotal,na.rm=TRUE),roundval), "±", round(sd(DistTotal,na.rm=TRUE),roundval)," (",round(max(DistTotal,na.rm=TRUE),roundval),")"),
      TripDur = paste0(round(mean(TripDur,na.rm=TRUE),roundval), "±", round(sd(TripDur,na.rm=TRUE),roundval)," (",round(max(TripDur,na.rm=TRUE),roundval),")"),
      .groups = "keep") %>% mutate(TagID = "all")

  }
  if(option == "boxplot"){
    mns = data %>% group_by(TagID, !!!syms(unique(by))) %>% summarise(
      DistMax = paste0(round(boxplot(DistMax,plot=FALSE)$stats[3,1],roundval)," (", round(boxplot(DistMax,plot=FALSE)$stats[2,1],roundval),",",round(boxplot(DistMax,plot=FALSE)$stats[4,1],roundval),") ","[",round(range(DistMax,na.rm=TRUE)[2],roundval),"]"),
      TotalDist = paste0(round(boxplot(DistTotal,plot=FALSE)$stats[3,1],roundval)," (", round(boxplot(DistTotal,plot=FALSE)$stats[2,1],roundval),",",round(boxplot(DistTotal,plot=FALSE)$stats[4,1],roundval),") ","[",round(range(DistTotal,na.rm=TRUE)[2],roundval),"]"),
      TripDur = paste0(round(boxplot(TripDur,plot=FALSE)$stats[3,1],roundval)," (", round(boxplot(TripDur,plot=FALSE)$stats[2,1],roundval),",",round(boxplot(TripDur,plot=FALSE)$stats[4,1],roundval),") ","[",round(range(TripDur,na.rm=TRUE)[2],roundval),"]"),
      .groups = "keep")

    mns_all = data %>% ungroup() %>% summarise(
      DistMax = paste0(round(boxplot(DistMax,plot=FALSE)$stats[3,1],roundval)," (", round(boxplot(DistMax,plot=FALSE)$stats[2,1],roundval),",",round(boxplot(DistMax,plot=FALSE)$stats[4,1],roundval),") ","[",round(range(DistMax,na.rm=TRUE)[2],roundval),"]"),
      TotalDist = paste0(round(boxplot(DistTotal,plot=FALSE)$stats[3,1],roundval)," (", round(boxplot(DistTotal,plot=FALSE)$stats[2,1],roundval),",",round(boxplot(DistTotal,plot=FALSE)$stats[4,1],roundval),") ","[",round(range(DistTotal,na.rm=TRUE)[2],roundval),"]"),
      TripDur = paste0(round(boxplot(TripDur,plot=FALSE)$stats[3,1],roundval)," (", round(boxplot(TripDur,plot=FALSE)$stats[2,1],roundval),",",round(boxplot(TripDur,plot=FALSE)$stats[4,1],roundval),") ","[",round(range(TripDur,na.rm=TRUE)[2],roundval),"]"),
      .groups = "keep") %>% mutate(TagID = "all")

    ### and using the by variable
    mns_all_BY = data %>% group_by(!!!syms(unique(by))) %>% summarise(
      DistMax = paste0(round(boxplot(DistMax,plot=FALSE)$stats[3,1],roundval)," (", round(boxplot(DistMax,plot=FALSE)$stats[2,1],roundval),",",round(boxplot(DistMax,plot=FALSE)$stats[4,1],roundval),") ","[",round(range(DistMax,na.rm=TRUE)[2],roundval),"]"),
      TotalDist = paste0(round(boxplot(DistTotal,plot=FALSE)$stats[3,1],roundval)," (", round(boxplot(DistTotal,plot=FALSE)$stats[2,1],roundval),",",round(boxplot(DistTotal,plot=FALSE)$stats[4,1],roundval),") ","[",round(range(DistTotal,na.rm=TRUE)[2],roundval),"]"),
      TripDur = paste0(round(boxplot(TripDur,plot=FALSE)$stats[3,1],roundval)," (", round(boxplot(TripDur,plot=FALSE)$stats[2,1],roundval),",",round(boxplot(TripDur,plot=FALSE)$stats[4,1],roundval),") ","[",round(range(TripDur,na.rm=TRUE)[2],roundval),"]"),
      .groups = "keep") %>% mutate(TagID = "all")

  }

  mns_all <- full_join(mns_all,mns_all_BY, by = c("DistMax", "TotalDist", "TripDur", "TagID"))

  core = left_join(n_, mns, by = c("TagID", by))
  all = full_join(n_all, mns_all, by = c("TagID", by))

  fin = full_join(core, all, by = c("TagID", by, "incompl","n","n_compl", "DistMax","TotalDist","TripDur"))

  #data$DistMax_off <- runif(nrow(data), 0.1, 52.8)
  #data$DistMax_on <- runif(nrow(data), 0.1, 28.8)
  #data$offshore <- sample(0:1,size=nrow(data),replace=TRUE)
  #data$DistCoastMax_off <- runif(nrow(data), 0.1, 38.8)
  #data$DistCoastMax_on <- runif(nrow(data), 0.1, 52.8)

  # ------------------------------------------------------- #
  # if dist2coast and offshore used
  # ------------------------------------------------------- #
  if(exists("DistMax_off", data)){

    # then we have offshore and onshore delineated

    if(option == "means"){
      mns2 = data %>% group_by(TagID, !!!syms(unique(by))) %>% summarise(
        DistMax_off = paste0(round(mean(DistMax_off,na.rm=TRUE),roundval), "±", round(sd(DistMax_off,na.rm=TRUE),roundval)," (",round(max(DistMax_off,na.rm=TRUE),roundval),")"),
        DistMax_on = paste0(round(mean(DistMax_on,na.rm=TRUE),roundval), "±", round(sd(DistMax_on,na.rm=TRUE),roundval)," (",round(max(DistMax_on,na.rm=TRUE),roundval),")"),
        .groups = "keep")

      mns2_all = data %>% ungroup() %>% summarise(
        DistMax_off = paste0(round(mean(DistMax_off,na.rm=TRUE),roundval), "±", round(sd(DistMax_off,na.rm=TRUE),roundval)," (",round(max(DistMax_off,na.rm=TRUE),roundval),")"),
        DistMax_on = paste0(round(mean(DistMax_on,na.rm=TRUE),roundval), "±", round(sd(DistMax_on,na.rm=TRUE),roundval)," (",round(max(DistMax_on,na.rm=TRUE),roundval),")"),
        .groups = "keep") %>% mutate(TagID = "all")

      mns2_all_BY = data %>% group_by(!!!syms(unique(by))) %>% summarise(
        DistMax_off = paste0(round(mean(DistMax_off,na.rm=TRUE),roundval), "±", round(sd(DistMax_off,na.rm=TRUE),roundval)," (",round(max(DistMax_off,na.rm=TRUE),roundval),")"),
        DistMax_on = paste0(round(mean(DistMax_on,na.rm=TRUE),roundval), "±", round(sd(DistMax_on,na.rm=TRUE),roundval)," (",round(max(DistMax_on,na.rm=TRUE),roundval),")"),
        .groups = "keep") %>% mutate(TagID = "all")

    }
    if(option == "boxplot"){
      mns2 = data %>% group_by(TagID, !!!syms(unique(by))) %>% summarise(
        DistMax_off = paste0(round(boxplot(DistMax_off,plot=FALSE)$stats[3,1],roundval)," (", round(boxplot(DistMax_off,plot=FALSE)$stats[2,1],roundval),",",round(boxplot(DistMax_off,plot=FALSE)$stats[4,1],roundval),") ","[",round(range(DistMax_off,na.rm=TRUE)[2],roundval),"]"),
        DistMax_on = paste0(round(boxplot(DistMax_on,plot=FALSE)$stats[3,1],roundval)," (", round(boxplot(DistMax_on,plot=FALSE)$stats[2,1],roundval),",",round(boxplot(DistMax_on,plot=FALSE)$stats[4,1],roundval),") ","[",round(range(DistMax_on,na.rm=TRUE)[2],roundval),"]"),
        .groups = "keep")

      mns2_all = data %>% ungroup() %>% summarise(
        DistMax_off = paste0(round(boxplot(DistMax_off,plot=FALSE)$stats[3,1],roundval)," (", round(boxplot(DistMax_off,plot=FALSE)$stats[2,1],roundval),",",round(boxplot(DistMax_off,plot=FALSE)$stats[4,1],roundval),") ","[",round(range(DistMax_off,na.rm=TRUE)[2],roundval),"]"),
        DistMax_on = paste0(round(boxplot(DistMax_on,plot=FALSE)$stats[3,1],roundval)," (", round(boxplot(DistMax_on,plot=FALSE)$stats[2,1],roundval),",",round(boxplot(DistMax_on,plot=FALSE)$stats[4,1],roundval),") ","[",round(range(DistMax_on,na.rm=TRUE)[2],roundval),"]"),
        .groups = "keep") %>% mutate(TagID = "all")

      mns2_all_BY = data %>% group_by(!!!syms(unique(by))) %>% summarise(
        DistMax_off = paste0(round(boxplot(DistMax_off,plot=FALSE)$stats[3,1],roundval)," (", round(boxplot(DistMax_off,plot=FALSE)$stats[2,1],roundval),",",round(boxplot(DistMax_off,plot=FALSE)$stats[4,1],roundval),") ","[",round(range(DistMax_off,na.rm=TRUE)[2],roundval),"]"),
        DistMax_on = paste0(round(boxplot(DistMax_on,plot=FALSE)$stats[3,1],roundval)," (", round(boxplot(DistMax_on,plot=FALSE)$stats[2,1],roundval),",",round(boxplot(DistMax_on,plot=FALSE)$stats[4,1],roundval),") ","[",round(range(DistMax_on,na.rm=TRUE)[2],roundval),"]"),
        .groups = "keep") %>% mutate(TagID = "all")

    }

    mns2_all <- full_join(mns2_all, mns2_all_BY, by = c("DistMax_off", "DistMax_on", "TagID"))

#    # no. of offshore and onshore trips
#    n_incompl_2 = data %>% group_by(TagID, !!!syms(unique(by)), offshore) %>% summarise(incompl = sum(incompl), .groups = "keep")
#    n_total_2 = data %>% group_by(TagID, !!!syms(unique(by)), offshore) %>% summarise(n = length(incompl), .groups = "keep")
#    n_2 <- left_join(n_incompl_2, n_total_2, by = c("TagID", by, "offshore"))
#    n_2$n_compl <- n_2$n - n_2$incompl
#
#    # two separate merges, could've just re-arranged this as wide form!
#    n_2_off <- n_2 %>% filter(offshore == 1) %>% ungroup() %>% rename(incompl_off = "incompl", n_off = "n", n_compl_off = "n_compl") %>% select(-offshore)
#    n_2_on <- n_2 %>% filter(offshore == 0) %>% ungroup() %>% rename(incompl_on = "incompl", n_on = "n", n_compl_on = "n_compl") %>% select(-offshore)
#    n_2 <- left_join(n_2_off, n_2_on, by = c("TagID", by))
#
#    # add total rows across birds for offshore/onshore trips
#    n_2_off_total <- n_2_off %>% ungroup() %>% summarise(incompl_off = sum(incompl_off), n_off = sum(n_off), n_compl_off = sum(n_compl_off), .groups = "keep") %>% mutate(TagID = "all")
#    n_2_on_total <- n_2_on %>% ungroup() %>% summarise(incompl_on = sum(incompl_on), n_on = sum(n_on), n_compl_on = sum(n_compl_on), .groups = "keep") %>% mutate(TagID = "all")
#    n_2_total <- left_join(n_2_off_total, n_2_on_total, by = c("TagID"))
#
#    # and for the by variable
#    n_2_off_total_BY <- n_2_off %>% group_by(!!!syms(unique(by))) %>% summarise(incompl_off = sum(incompl_off), n_off = sum(n_off), n_compl_off = sum(n_compl_off), .groups = "keep") %>% mutate(TagID = "all")
#    n_2_on_total_BY <- n_2_on %>%  group_by(!!!syms(unique(by))) %>% summarise(incompl_on = sum(incompl_on), n_on = sum(n_on), n_compl_on = sum(n_compl_on), .groups = "keep") %>% mutate(TagID = "all")
#    n_2_total_BY <- left_join(n_2_off_total_BY, n_2_on_total_BY, by = c("TagID", by))
#
#    n_2 <- full_join(n_2, n_2_total, by = c("TagID", "incompl_off", "n_off", "n_compl_off", "incompl_on", "n_on", "n_compl_on"))
#    n_2 <- full_join(n_2, n_2_total_BY, by = c("TagID", by, "incompl_off", "n_off", "n_compl_off", "incompl_on", "n_on", "n_compl_on"))

    #######
    mns2 <- full_join(mns2, mns2_all, by = c("TagID", by, "DistMax_off","DistMax_on"))
#    mns2 <- left_join(mns2, n_2, by = c("TagID", by))

    # add into the data
    fin = left_join(fin, mns2, by = c("TagID", by))

  }

  # ------------------------------------------------------- #
  # Dist2coast if used
  # ------------------------------------------------------- #
  if(exists("DistCoastMax_off", data)){

    if(option == "means"){
      mns3 = data %>% group_by(TagID, !!!syms(unique(by))) %>% summarise(
        DistCoastMax_off = paste0(round(mean(DistCoastMax_off,na.rm=TRUE),roundval), "±", round(sd(DistCoastMax_off,na.rm=TRUE),roundval)," (",round(max(DistCoastMax_off,na.rm=TRUE),roundval),")"),
        DistCoastMax_on = paste0(round(mean(DistCoastMax_on,na.rm=TRUE),roundval), "±", round(sd(DistCoastMax_on,na.rm=TRUE),roundval)," (",round(max(DistCoastMax_on,na.rm=TRUE),roundval),")"),
        .groups = "keep")

      mns3_all = data %>% ungroup() %>% summarise(
        DistCoastMax_off = paste0(round(mean(DistCoastMax_off,na.rm=TRUE),roundval), "±", round(sd(DistCoastMax_off,na.rm=TRUE),roundval)," (",round(max(DistCoastMax_off,na.rm=TRUE),roundval),")"),
        DistCoastMax_on = paste0(round(mean(DistCoastMax_on,na.rm=TRUE),roundval), "±", round(sd(DistCoastMax_on,na.rm=TRUE),roundval)," (",round(max(DistCoastMax_on,na.rm=TRUE),roundval),")"),
        .groups = "keep") %>% mutate(TagID = "all")

      mns3_all_BY = data %>% group_by(!!!syms(unique(by))) %>% summarise(
        DistCoastMax_off = paste0(round(mean(DistCoastMax_off,na.rm=TRUE),roundval), "±", round(sd(DistCoastMax_off,na.rm=TRUE),roundval)," (",round(max(DistCoastMax_off,na.rm=TRUE),roundval),")"),
        DistCoastMax_on = paste0(round(mean(DistCoastMax_on,na.rm=TRUE),roundval), "±", round(sd(DistCoastMax_on,na.rm=TRUE),roundval)," (",round(max(DistCoastMax_on,na.rm=TRUE),roundval),")"),
        .groups = "keep") %>% mutate(TagID = "all")

    }
    if(option == "boxplot"){
      mns3 = data %>% group_by(TagID, !!!syms(unique(by))) %>% summarise(
        DistCoastMax_off = paste0(round(boxplot(DistCoastMax_off,plot=FALSE)$stats[3,1],roundval)," (", round(boxplot(DistCoastMax_off,plot=FALSE)$stats[2,1],roundval),",",round(boxplot(DistCoastMax_off,plot=FALSE)$stats[4,1],roundval),") ","[",round(range(DistCoastMax_off,na.rm=TRUE)[2],roundval),"]"),
        DistCoastMax_on = paste0(round(boxplot(DistCoastMax_on,plot=FALSE)$stats[3,1],roundval)," (", round(boxplot(DistCoastMax_on,plot=FALSE)$stats[2,1],roundval),",",round(boxplot(DistCoastMax_on,plot=FALSE)$stats[4,1],roundval),") ","[",round(range(DistCoastMax_on,na.rm=TRUE)[2],roundval),"]"),
        .groups = "keep")

      mns3_all = data %>% ungroup() %>% summarise(
        DistCoastMax_off = paste0(round(boxplot(DistCoastMax_off,plot=FALSE)$stats[3,1],roundval)," (", round(boxplot(DistCoastMax_off,plot=FALSE)$stats[2,1],roundval),",",round(boxplot(DistCoastMax_off,plot=FALSE)$stats[4,1],roundval),") ","[",round(range(DistCoastMax_off,na.rm=TRUE)[2],roundval),"]"),
        DistCoastMax_on = paste0(round(boxplot(DistCoastMax_on,plot=FALSE)$stats[3,1],roundval)," (", round(boxplot(DistCoastMax_on,plot=FALSE)$stats[2,1],roundval),",",round(boxplot(DistCoastMax_on,plot=FALSE)$stats[4,1],roundval),") ","[",round(range(DistCoastMax_on,na.rm=TRUE)[2],roundval),"]"),
        .groups = "keep") %>% mutate(TagID = "all")

      mns3_all = data %>% group_by(TagID, !!!syms(unique(by))) %>% summarise(
        DistCoastMax_off = paste0(round(boxplot(DistCoastMax_off,plot=FALSE)$stats[3,1],roundval)," (", round(boxplot(DistCoastMax_off,plot=FALSE)$stats[2,1],roundval),",",round(boxplot(DistCoastMax_off,plot=FALSE)$stats[4,1],roundval),") ","[",round(range(DistCoastMax_off,na.rm=TRUE)[2],roundval),"]"),
        DistCoastMax_on = paste0(round(boxplot(DistCoastMax_on,plot=FALSE)$stats[3,1],roundval)," (", round(boxplot(DistCoastMax_on,plot=FALSE)$stats[2,1],roundval),",",round(boxplot(DistCoastMax_on,plot=FALSE)$stats[4,1],roundval),") ","[",round(range(DistCoastMax_on,na.rm=TRUE)[2],roundval),"]"),
        .groups = "keep") %>% mutate(TagID = "all")

    }

    mns3_all <- full_join(mns3_all, mns3_all_BY, by = c("DistCoastMax_off", "DistCoastMax_on", "TagID"))

    mns3 <- full_join(mns3, mns3_all, by = c("TagID", by, "DistCoastMax_off","DistCoastMax_on"))

    # add into the data
    fin = left_join(fin, mns3, by = c("TagID", by))

  }

  # ------------------------------------------------------- #
  if(is.null(attr(fin, "tabulate_trips") )){
    attr(fin, "tabulate_trips") <- "tabulate_trips"
  }

  # sub_attributes for arguments
  attr(attr(fin, "tabulate_trips"), "by") <- by
  attr(attr(fin, "tabulate_trips"), "option ") <- option
  attr(attr(fin, "tabulate_trips"), "TD_unit ") <- TD_unit
  attr(attr(fin, "tabulate_trips"), "rm_inc ") <- rm_inc
  attr(attr(fin, "tabulate_trips"), "roundval ") <- roundval

  return(fin)

}





