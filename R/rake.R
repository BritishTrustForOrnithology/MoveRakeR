#' Overarching assessment of data quality
#'
#' A general 'rake' of the data to assess quality of data using several queries of data structure
#'
#' @details
#' This overarching \code{rake} function is an initial assessment of the overall quality of the data structure
#' made by using broad queries relating to: (1) the data quality in space and time extents ('spans'), (2) whether
#' any missing (NA) data are present in latitude or longitude columns ('NAlatlong'), (3) whether any duplicate DateTime records exist ('dupDT'),
#' (4) an assessment of animal tracking durations and density over overlaps among the data over days (timeline'), (5) the
#' likely rates of GPS sampling schedules and potentially missing data ('rates'), and (6) a quick appraisal of number of satellites
#' and missing (NA) satellite data ('nsats').
#'
#' While the \code{rake_outlie} function is concerned with identifying outlying for integer and numeric covariates,
#' for example relating to measurement error, the \code{rake} function provides a further summary output relating
#' to potential sampling biases inherent in the data for individual animals.
#' The \code{rake} function uses the function \code{tag_timeline} to return the information about the temporal schedule
#' of the tags and overlaps of the data, \code{tag_spans} to determine spatio-temporal extents and
#' potential fixes errant outside user-provided bounds, and \code{assign_rates2} to query the likely sampling rates present.
#' However, the \code{rake} function also considers some measurement
#' error aspects such as duplicate DateTimes, missing latitude/longitude information, and number of satellites.
#' Each of the above functions used by \code{rake} can also be used on their own. As noted, the xra and yra arguments
#' are only a guide of what you may expect to be incorrect and is a simplistic first pass of the data.
#'
#' The \code{ShinyRakeR} interactive Shiny app can be used to flag potential outlying data points, and also uses some process
#' within \code{Rake} such as \code{tag_timeline} for a more interactive experience.
#'
#' @param data Input data object, with required columns: TagID, DateTime, longitude, latitude.
#' @param check_for One of six options as noted in details for "spans", "NAlatlong", "dupDT", "timeline", "rates", "nsats",
#' @param xra From \code{tag_spans}: The expected limits of acceptable data in the x longitude dimension (WGS84),
#' supplied as a two-length vector c(min,max), c(-10,5).
#' @param yra From \code{tag_spans}: The expected limits of acceptable data in the y latitude dimension (WGS84),
#' supplied as a two-length vector c(min,max), defaulting to c(25,65).
#' @param dates From \code{tag_spans}: The expected valid range of dates over which the dataset was collected, supplied as a
#' two-length vector c(min,max) as character YYYY-MM-DD HH:MM:SS format, defaulting to c("2014-01-01 00:00:01", "2021-12-31 23:59:59").
#' @param buffer_days From \code{tag_spans}: A buffer in days for flagging dates as a temporal flag either side of the 'dates' time window.
#' @param max_show The maximum number of individual animals to show in the messaging print out.
#' @param plot From \code{tag_timeline}: A TRUE/FALSE (defaulting to FALSE) whether to plot a ggplot visualisation of daily tracking
#' intensity upon function completion.
#' @param return_plot From \code{tag_timeline}: A TRUE/FALSE (defaulting to TRUE) as to whether to return the ggplot visualisation of daily tracking
#' intensity, regardless of whether the user wanted it plotted to the R session on function completion. If TRUE,
#' the plot is given in the returned object: attr(, "plot").
#' @param overlaps From \code{tag_timeline}: A TRUE/FALSE whether to compute overlap data between individual animals, defaults to TRUE.
#' @param active_thresh From \code{tag_timeline}: a continuous numeric value between 0 and 1 used to quantify which animals were present when
#' less than (active_thresh*100)% of other animals were active.
#' @param alone_thresh From \code{tag_timeline}: a continuous numeric value between 0 1nd 1 used quantify which animals had more
#' than (alone_thresh*100)% time tracked alone.
#' @param miss_thresh From \code{tag_timeline}: a continuous numeric value between 0 and 1 to determine which animals had more
#' than (miss_thresh*100)% days with missing data.
#' @param breaks As in \code{assign_rates2}: A vector of binned choices of potential rates: the default here is for 11 categories:
#' 10, 120, 300, 600, 900, 1200, 1800, 3600, 7200 and 10800s, and the upper value beyond,
#' labelled as <max dt. This function operates independent of and gapsecs that may have been
#' chosen in \code{gap_section}, i.e. dts are assessed consecutively regardless of gaps, unless
#' supplied specifically within the by argument.
#' @param tol As in \code{assign_rates2}: A vector of tolerances that are added to the breaks argument to account for
#' imperfect GPS sampling rates in data obtained; this is set to: c(0,0.5,rep(0.4,2),rep(0.3,5),rep(0.2,2)).
#' The tol argument can also be a single vector that is then applied equally to all break categories.
#' @param pctn_rate_thresh From \code{assign_rates2}:The percentage value threshold above which assessment for data collection at coarser
#' or finer sampling rates is presented in the messaged outputs, e.g. For animal(s) X and Y: Z% or more data was collected at coarser/finer rates of 'S' s.
#' @param max_show The maximum number of individual animals to show in the messaging print out.
#' @param sat_col The column for which to use for assessing satellite data quality, defaulting to "satellites_used".
#' @param drop_sats The minimum number of satellites present in the data, defaulting to 3.
#' @param messages Whether printed console messages are to be returned to the user, a Boolean defaulting to TRUE.
#' @param msg_col A concatenated vector for colours wanted in the messaged output per
#' data query, defaulting to:  c(32,33,34,35,36,37).
#'
#' @return
#' The \code{rake} function returns a listed object of maximum length = 6, depending on the \emph{check_for} argument.
#' Each element relates to the \emph{check_for} options for each separate data query. Outputs of the functions
#' \code{tag_timeline}, \code{tag_spans} and \code{assign_rates2} are directly given in their respective list slots,
#' with their options in \code{rake} reflected in their structured outputs - see each function for more details.
#' For the 'NAlatlong' list element, this is simply a tibble summary of TagID, whether any missing data was detected ('any_missing'),
#' the number of missing data ('n_missing') and the respective DateTimes for those missing data ('missing_times'). Similarly,
#' for the 'dupDT' check_for, the output is a separate tibble, with, TagID, any_dupDT (TRUE/FALSE), dupDT (the actual DateTimes), and n_dupDT (how many duplicates per TagID).
#' For 'nsats' the output is similar again, with columns denoting any TagIDs that had missing satellite information,
#' and respective DateTimes, and further if any cells were found with satellites less than then minimum as input into the function, agai with the DateTimes for those records.
#'
#' @seealso [MoveRakeR::rake_outlie], [MoveRakeR::ShinyRakeR], [MoveRakeR::tag_timeline], [MoveRakeR::tag_spans], [MoveRakeR::assign_rates2]
#'
#' @examples
#'
#' indata = yourdata # data.frame with a minimum of columns named TagID, DateTime, longitude, latitude
#' data = Track(indata) # optional to convert to in-house class (most functions will still run on non track data but generic S3 methods will not be available for plot and summary)
#'
#' # add in some random fake NA longs and NA lats if you have none just to see what happens in messaging:
#' data_fake = data
#' data_fake$latitude[sample(seq(1,nrow(data_fake),1),100, replace = FALSE)] <- NA
#' data_fake$longitude[sample(seq(1,nrow(data_fake),1),50, replace = FALSE)] <- NA
#'
#' # duplicate some rows
#' a = data_fake[sample(seq(1,nrow(data_fake),1),25, replace = FALSE),]
#' data_fake = rbind(data_fake,a) %>% arrange(TagID,DateTime)
#'
#' # all with defaults
#' rake_all = rake(data_fake, check_for = c("spans", "NAlatlong", "dupDT", "timeline", "rates", "nsats"),
#'            msg_col = c(32,33,34,35,36,37))
#'
#' # spans only
#' rake_spans = rake(data_fake, check_for = c("spans"),
#'            xra = c(-10,5), # the expected likely longitudinal distribution range
#'            yra = c(25,65), # the expected likely latitudinal distribution range
#'            dates = c("2014-01-01 00:00:01", "2021-12-31 23:59:59"),
#'            buffer_days = 2, max_show = 3,
#'            msg_col =32)
#'
#' # or just using the tag_spans() function
#' dat <- tag_spans(data_fake, measure = c("spatial", "temporal"),
#'     msg_col = 33,
#'     xra = c(-10,5), yra = c(25,65), max_show = 3,
#'     dates = c("2014-01-01 00:00:01", "2021-12-31 23:59:59"), buffer_days = 2,
#'     messages = FALSE)
#'
#'
#' # NAlatlongs and "dupDT"
#' rake_ll_dt = rake(data_fake, check_for = c("NAlatlong", "dupDT"),
#'                  max_show = 3,
#'                  msg_col = c(33,34))
#'
#' # timeline
#' rake_time_line = rake(data_fake, check_for = c("timeline"), msg_col = 35,
#'                      plot = FALSE, # tag_timeline() plot option
#'                      return_plot = TRUE,
#'                      overlaps = TRUE,
#'                      active_thresh = 0.55,
#'                      max_show = 3,
#'                      alone_thresh = 0.05,
#'                      miss_thresh = 0.55)
#'
#' # or just call tag_timeline()
#' rake_time_line = tag_timeline(data_fake,
#'     plot = FALSE,
#'     return_plot = TRUE,
#'     overlaps = TRUE,
#'     active_thresh = 0.5,
#'     alone_thresh = 0.5,
#'     miss_thresh = 0.25,
#'     msg_col = "35")
#'
#' # rates
#' rake_rates = rake(data_fake, check_for = c("rates"),
#'                   breaks = c(1, 60, 100, 300, 600, 900, 1200, 1800, 3600, 7200, 10800, 18000),
#'                   tol = c(0,0.5,rep(0.4,2),rep(0.3,5),rep(0.2,3)),
#'                   pctn_rate_thresh = 20,
#'                   msg_col = 36)
#'
#' @export
rake <- function(data,
                check_for = c("spans", "NAlatlong", "dupDT", "timeline", "rates", "nsats"),

                # arguments from tag_spans()
                xra = c(-10,5), # the expected likely longitudinal distribution range
                yra = c(25,65), # the expected likely latitudinal distribution range
                dates = c("2014-01-01 00:00:01", "2021-12-31 23:59:59"),
                buffer_days = 2,
                max_show = 3,

                # arguments from tag_timeline()
                plot = FALSE, # this triggers assign_rates2() plotting option and tag_timeline() plot option
                return_plot = TRUE,
                overlaps = TRUE,
                active_thresh = 0.5,
                alone_thresh = 0.5,
                miss_thresh = 0.25,

                # arguments for assign_rates()
                breaks = c(1, 60, 100, 300, 600, 900, 1200, 1800, 3600, 7200, 10800),
                tol = c(0,0.5,rep(0.4,2),rep(0.3,5),rep(0.2,2)),
                pctn_rate_thresh = 20,

                # sats used
                sat_col = "satellites_used",
                drop_sats = 3,

                messages = TRUE,
                msg_col = c(32,33,34,35,36,37)

                ){

  width <- getOption("width")   # current terminal width

  if(length(msg_col) != length(check_for)){
    msg_col <- seq(32,32+(length(check_for)-1),1)
  }

  # get number of things we are checking for:
  msg_cols = data.frame(check_for = check_for, col_w = 1:length(check_for), msg_col)

  # -------------------------------------- #
  # Spans of the data for a 'soft' check of the data (spatial and temporal)
  if("spans"  %in% check_for){

    #tagspan <- tag_spans(data, measure = c("general","spatial", "temporal"),
    #          msg_col = msg_col[1],
    #          xra = c(-10,5), yra = c(25,65), max_show = 3,
    #          dates = c("2014-01-01 00:00:01", "2021-12-31 23:59:59"), buffer_days = 2,
    #          messages = TRUE)

    cval = msg_cols[msg_cols$check_for == "spans",]$msg_col

    tagspan <- tag_spans(data, measure = c("general","spatial", "temporal"),
                         msg_col = cval,
                         xra = xra, yra = yra, max_show = max_show,
                         dates = dates, buffer_days = buffer_days,
                         messages = messages)


  }

  # -------------------------------------- #
  # NAlatlong data (spatial)

  if("NAlatlong" %in% check_for){
    cval_2 = msg_cols[msg_cols$check_for == "NAlatlong",]$msg_col

    cat(rep(" ",  getOption("width")), "\n", sep = "")
    cat("\033[", cval_2, "m", strrep("*", getOption("width")), "\033[0m\n", sep = "")
    rule_centre("NA lat-long checks", col = cval_2, char = "*")
    cat("\033[", cval_2, "m", strrep("*", getOption("width")), "\033[0m\n", sep = "")

    cat(rep(" ",  getOption("width")), "\n", sep = "")

    data_NAlatlong <- data %>% group_by(TagID) %>%
      mutate(
        NA_latlong = if_else(is.na(longitude) | is.na(latitude), 1L, 0L)
      ) %>% group_by(TagID) %>%
      summarise(
        any_missing = any(NA_latlong == 1),
        n_missing   = sum(NA_latlong, na.rm = TRUE),
        missing_times = list(DateTime[NA_latlong == 1]),
        .groups = "drop"
      ) %>% ungroup()

    if(all(!data_NAlatlong$any_missing)){
      msg1 = paste0("- No missing xy data found")
      cat(paste0("\033[",cval_2,"m"),msg1,"\033[0m\n")

    } else{
      w = which(data_NAlatlong$any_missing)
      if(length(w) == nrow(data_NAlatlong)){

        msg2 = paste0("- All animals have some missing xy data \n TagID (n rows):")
        cat(paste0("\033[",cval_2,"m"),msg2,"\033[0m\n")

      } else{

        msg3 = paste0("- ", length(w), "/", nrow(data_NAlatlong), " animals have missing xy data \n - TagID (n rows):")
        cat(paste0("\033[",cval_2,"m"),msg3,"\033[0m\n")

      }
      msg4 = paste0(paste0(data_NAlatlong$TagID, " (", data_NAlatlong$n_missing, ")", collapse = ", "))
      cat(paste0("\033[",cval_2,"m"),msg4,"\033[0m\n")

    }


  }

  # -------------------------------------- #
  # Duplicate DateTimes (temporal)

  if("dupDT" %in% check_for){

    cval_3 = msg_cols[msg_cols$check_for == "dupDT",]$msg_col

    cat(rep(" ",  getOption("width")), "\n", sep = "")
    cat("\033[", cval_3, "m", strrep("*", getOption("width")), "\033[0m\n", sep = "")
    rule_centre("Duplicate DateTime checks", col = cval_3, char = "*")
    cat("\033[", cval_3, "m", strrep("*", getOption("width")), "\033[0m\n", sep = "")

    cat(rep(" ",  getOption("width")), "\n", sep = "")

    data_dupDT <- data %>%
      group_by(TagID) %>%
      mutate(dupDT_flag = duplicated(DateTime) | duplicated(DateTime, fromLast = TRUE)) %>%
      reframe(
        any_dupDT = any(dupDT_flag),
        dupDT = list(unique(DateTime[dupDT_flag])),
        n_dupDT = length(unique(DateTime[dupDT_flag]))
      )

    #test = data_dupDT[data_dupDT$TagID %in% 4032,]$dupDT[[1]][[1]] # apparent DateTimes for TagID 4032, first one
    #nrow(data[data$TagID == 4032 & data$DateTime %in% test,]) #1

    if(all(!data_dupDT$any_dupDT)){
      #message("----- No duplicate DateTimes found ----- ")
      msg1 = paste0("- No duplicate DateTimes found")
      cat(paste0("\033[",cval_3,"m"),msg1,"\033[0m\n")

    } else{
      w = which(data_dupDT$any_dupDT)
      if(length(w) == nrow(data_dupDT)){
        #message("----- All animals have some duplicate DateTimes -----")

        msg2 = paste0("- All animals have some duplicate DateTimes \n TagID (n rows):")
        cat(paste0("\033[",cval_3,"m"),msg2,"\033[0m\n")

      } else{
        #message("----- ", length(w), "/", nrow(data_dupDT), " animals have some duplicate DateTimes -----")
        #message("-> Tags: ", paste(data_NAlatlong$TagID[w], collapse = ", "))

        msg3 = paste0("- ", length(w), "/", nrow(data_NAlatlong), " animals have duplicate DateTimes \n - TagID (n rows):")
        cat(paste0("\033[",cval_3,"m"),msg3,"\033[0m\n")

      }
      #message("-> Tags: ", paste0(data_dupDT$TagID, " (", data_dupDT$n_dupDT, ")", collapse = ", "))

      msg4 = paste0(data_dupDT$TagID, " (", data_dupDT$n_dupDT, ")", collapse = ", ")
      cat(paste0("\033[",cval_3,"m"),msg4,"\033[0m\n")

    }

  }

  # -------------------------------------- #
  # Timeline Schedule of the data, i.e. TagID overlaps (temporal)

  if("timeline"  %in% check_for){

    # call the tag_timeline function
    #time_summary = tag_timeline(data,
    #                   plot = FALSE,
    #                   return_plot = TRUE,
    #                   overlaps = TRUE,
    #                   active_thresh = 0.5,
    #                   alone_thresh = 0.5,
    #                   miss_thresh = 0.25,
    #                   msg_col = msg_col[4],
    #                   messages = TRUE)

    cval_4 = msg_cols[msg_cols$check_for == "timeline",]$msg_col

    time_summary = tag_timeline(data,
                                plot = plot,
                                return_plot = return_plot,
                                overlaps = overlaps,
                                active_thresh = active_thresh,
                                alone_thresh = alone_thresh,
                                miss_thresh = miss_thresh,
                                msg_col = cval_4,
                                max_show = max_show,
                                messages = messages)

    # attr(res,"plot") # this has the graphic

  }


  # -------------------------------------- #
  if("rates"  %in% check_for){

    cval_5 = msg_cols[msg_cols$check_for == "rates",]$msg_col

    ####### Run the assign_rates2() function
    rates = assign_rates2(data,

                         by = NULL,
                         split = FALSE,
                         group_after_gap = FALSE,
                         breaks = breaks,
                         tol = tol,
                         out = "summary",

                         # extra assign_rates2 things
                         pctn_rate_thresh = pctn_rate_thresh,
                         max_show = max_show,
                         version = "extended",
                         plot = plot,
                         messages = messages,
                         msg_col = cval_5,
                         verbose=FALSE)

  }


  # -------------------------------------- #
  # Number of satellites

  if("nsats"  %in% check_for){

    cval_6 = msg_cols[msg_cols$check_for == "nsats",]$msg_col

    if(!any(names(data) %in% sat_col)){
      warning("No satellite data column found matching that supplied under dat_col")
    } else{

      # summarise the number of sats that are less than the threshold provided

      # check for any data that have less than 'drop_sats'

      cat(rep(" ",  getOption("width")), "\n", sep = "")
      cat("\033[", cval_6, "m", strrep("*", getOption("width")), "\033[0m\n", sep = "")
      rule_centre("Checking number of satellites", col = cval_6, char = "*")
      cat("\033[", cval_6, "m", strrep("*", getOption("width")), "\033[0m\n", sep = "")

      cat(rep(" ",  getOption("width")), "\n", sep = "")

      #data <- data %>% rename(satellites_used = !!sym(sat_col)) # temporary renaming....
      #data_sc <- data %>% filter(is.na(!!sym(sat_col))) %>% count(TagID)

      data_sc <- data %>% group_by(TagID) %>%
        mutate(
          NA_sats = if_else(is.na(!!sym(sat_col)), 1L, 0L),
          sats_lt_n = if_else(!!sym(sat_col) <= drop_sats, 1L, 0L)
        ) %>% group_by(TagID) %>%
        summarise(
          any_sats_missing = any(NA_sats == 1),
          n_sats_missing   = sum(NA_sats, na.rm = TRUE),
          missing_sat_times = list(DateTime[NA_sats == 1]),

          any_sats_lt_n = any(sats_lt_n == 1),
          n_sats_lt_n   = sum(sats_lt_n, na.rm = TRUE),
          sat_times_lt_n = list(na.omit(DateTime[sats_lt_n == 1 & !is.na(DateTime)])),

          .groups = "drop"
        ) %>% ungroup()

      # -------------------------- #
      # NA sats

      data_sc_NA <- data_sc[data_sc$any_sats_missing,]

      if(nrow(data_sc_NA) > 0){

          pc = round((sum(data_sc_NA$n_sats_missing) / nrow(data))*100,2)

          if(length(data_sc_NA$TagID) > max_show){
            tids = paste0(paste(data_sc_NA$TagID, collapse = ", "), ", ...")
          } else{
            tids = paste(data_sc_NA$TagID, collapse = ", ")
          }

          msg = paste0("NA satellite data for ", nrow(data_sc_NA)," TagIDs: ", tids," (a total of ", sum(data_sc_NA$n_sats_missing), " fixes, ", ifelse(pc < 1, "< 1 %", paste0(pc, "%")), " data)")
          cat(paste0("\033[",cval_6,"m",msg,"\033[0m\n"))

      }

      # -------------------------- #
      # sats < thresh

      data_sc_lt <- data_sc[data_sc$any_sats_lt_n,]

      if(nrow(data_sc_lt) > 0){

        pc = round((sum(data_sc_lt$n_sats_lt_n) / nrow(data))*100,2)

        if(length(data_sc_lt$TagID) > max_show){
          tids = paste0(paste(data_sc_lt$TagID, collapse = ", "), ", ...")
        } else{
          tids = paste(data_sc_lt$TagID, collapse = ", ")
        }

        msg = paste0("GPS data found with <= ", drop_sats, " satellites for ", nrow(data_sc_lt)," TagIDs: ", tids," (a total of ", sum(data_sc_lt$n_sats_lt_n), " fixes, ", ifelse(pc < 1, "< 1 %", paste0(pc, "%")), " data)")
        cat(paste0("\033[",cval_6,"m",msg,"\033[0m\n"))

      }


    }


  }


  # -------------------------------------- #
  # OUTPUT

  out <- vector("list", 6)
  names(out) <- c("spans","NAlatlong","dupDT","timeline","rates","nsats")

  if("spans"  %in% check_for){
    out[[1]] <- tagspan
  }
  if("NAlatlong" %in% check_for){
    out[[2]] <- data_NAlatlong
  }
  if("dupDT" %in% check_for){
    out[[3]] <- data_dupDT
  }
  if("timeline"  %in% check_for){
    out[[4]] <- tibble(time_summary)
  }
  if("rates"  %in% check_for){
    out[[5]] <- rates
  }
  if("nsats"  %in% check_for){
    out[[6]] <- data_sc
  }

  return(out)

}

#' @rdname rake
#' @export
rule_centre <- function(text = "", col = "35", char = "-") {
  width <- getOption("width")
  text_len <- nchar(text) + 2  # add spaces around text
  side_len <- (width - text_len) %/% 2

  line <- paste0(
    strrep(char, side_len),
    " ", text, " ",
    strrep(char, width - side_len - text_len)
  )

  cat("\033[", col, "m", line, "\033[0m\n", sep = "")
}





