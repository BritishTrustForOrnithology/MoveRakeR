#' General summary of data quality extent in space and time
#'
#' Summarises the spatial and temporal range of the data and perceived outliers, building on that of summary.Track
#'
#' @details
#' The \code{tag_spans} function highlights potentially errant spatial and/or temporal data falling outside of
#' user provided bounds of data, and is therefore a 'soft' rake of the data, i.e. not formally identifying outliers.
#' Messaging is provided back to the user to indicate the spatio-temporal range and which animals in the data
#' have potentially outlying GPS fixes. This is useful if the user knows the overall extent of the area used,
#' or has an approximation of it, and therefore sudden jumps in the GPS can be identified easily. Note also,
#' these jumps are the very same fixes that would be identified under the \code{speed_filt} function.
#'
#' Note that the xra and yra arguments are only a guide of what you may expect to be incorrect.
#' This is an intentionally simplistic first pass of the data.
#'
#' @param data Input data object, with required columns: TagID, DateTime, longitude, latitude.
#' @param measure One of three options, "general", "spatial", and/or "temporal", with any combination allowed. This
#' directs the function to assess the (1) general data quality using the \code{summary} function in \code{moveRakeR},
#' being the overall bounding xy box, within which animals had most and least fixes, which animals were
#' at the extremes of the bounding box, which animals were
#' tracked longest and for the least amount of time; (2) the spatial quality, i.e. any fixes outside
#' and which birds had such fixes occuring outside the supplied xra and yra, and (3) temporal quality being either an errant DateTime in the future beyond the current date or outside
#' bounds of supplied DateTimes.
#' a data in the past.
#' @param xra The expected limits of acceptable data in the x longitude dimension (WGS84),
#' supplied as a two-length vector c(min,max), c(-10,5).
#' @param yra The expected limits of acceptable data in the y latitude dimension (WGS84),
#' supplied as a two-length vector c(min,max), defaulting to c(25,65).
#' @param dates The expected valid range of dates over which the dataset was collected, supplied as a
#' two-length vector c(min,max) as character YYYY-MM-DD HH:MM:SS format, defaulting to c("2014-01-01 00:00:01", "2021-12-31 23:59:59").
#' @param buffer_days A buffer in days for flagging dates as a temporal flag either side of the 'dates' time window.
#' @param max_show The maximum number of individual animals to show in the messaging print out.
#' @param messages Whether printed console messages are to be returned to the user, a Boolean defaulting to TRUE.
#' @param msg_col A numeric value for the colour to be used in console graphics.
#' The colour of the print out in the console, depending on your taste and whether black or white backgrounds
#' are currently being used within R Studio. Defaults to 33 (yellow).
#'
#' @return
#' An object as a list, length = 4, depending on the inputs to the function for the measure, xra, yra, and dates
#' arguments. A full spatio-temporal output would be obj[[1]], for the general data summary, using the
#' \code{summary} function, obj[[2]] for the xra summary, obj[[3]] for th yra summary and obj[[4]] for the date range summary.
#' Each summary is returned as a \code{tibble} with TagID, and then specific columns for each summary. The first object
#' is the same output as returned through the \code{summary} function - see that documentation.
#'
#' For xra obj[[2]],
#' columns are:
#' - any_xmin_bad: a Boolean of whether TagIDs had any fixes below the minimum longitude;
#' - any_xmax_bad: a Boolean of whether TagIDs had any fixes above the maximum longitude;
#' - xmin_bad: a list of which values were below the minimum longitude;
#' - xmax_bad: a list of which vallues were above the maximum longitude;
#' - xmin_bad_times: a list of DateTimes in order for which values were below the minimum longitude;
#' - xmax_bad_times: a list of DateTimes in order for which values were above the maximum longitude.
#'
#' The same output is returned for latitude obj[[3]].
#'
#' For the fourth obj[[4]] for the temporal summary, columns are given for:
#' - any_future: a Boolean for whether TagIDs had any fixes found to be in the future (past the DateTime clock from when the function wss run);
#' - future_dates: a list of future DateTimes if any were outside the current DateTime;
#' - n_future: the number of future DateTimes found;
#' - any_temporal: a Boolean for whether TagIDs had any DateTimes before or after the supplied 'date' range;
#' - temporal_dates: a list of DateTimes that were outside the date range;
#' - n_temporal: the number of fixes outside the date range.
#'
#' @seealso [MoveRakeR::rake], [MoveRakeR::rake_outlie], [MoveRakeR::tag_timeline]
#'
#' @examples
#'
#' dat <- tag_spans(data, measure = c("general","spatial", "temporal"),
#'     msg_col = 33,
#'     xra = c(-10,5), yra = c(25,65), max_show = 3,
#'     dates = c("2014-01-01 00:00:01", "2021-12-31 23:59:59"), buffer_days = 2,
#'     messages = TRUE)
#'
#' length(dat) # 4
#' dat[[2]] # returned summary of assessment for xra criterion
#'
#' @export
tag_spans <- function(data, measure = c("general", "spatial", "temporal"),
                      xra = c(-10,5),
                      yra = c(25,65),
                      dates = c("2014-01-01 00:00:01", "2021-12-31 23:59:59"), buffer_days = 2,
                      tz = "UTC",max_show = 3,
                      messages = TRUE,  msg_col = 33){

  cat(rep(" ",  getOption("width")), "\n", sep = "")

  if(messages){
    cat("\033[", msg_col, "m", strrep("*", getOption("width")), "\033[0m\n", sep = "")
    rule_centre("Data span checks", col = msg_col, char = "*")
    cat("\033[", msg_col, "m", strrep("*", getOption("width")), "\033[0m\n", sep = "")
    cat(rep(" ",  getOption("width")), "\n", sep = "")
  }

  # instead of using the MoveRakeR summary, here we provide the user further information for which
  # specific fixes were flagged and for the xy dimension, which DateTimes, those were

  # ------------------------------------------------------------------------ #
  # Overall initial summary of numbers of birds, fixes, starts/ends

  if("general" %in% measure){

    if(messages){
      rule_centre("Overall data summary", col = msg_col)
    }
    cat(rep(" ",  getOption("width")), "\n", sep = "")

    overall <- summary(Track(data), verbose = FALSE)

    if(messages){

      tot_fix = sum(overall$n_fixes)
      tot_bird = nrow(overall)

      mean_fix = mean(overall$n_fixes)
      sd_fix = sd(overall$n_fixes)

      # most/least fixes
      max_fix = overall[overall$n_fixes == max(overall$n_fixes),]
      min_fix = overall[overall$n_fixes == min(overall$n_fixes),]

      # tracking durations most/least
      max_dur = overall[overall$dur == max(overall$dur),]
      min_dur = overall[overall$dur == min(overall$dur),]

      # start and end times overall
      start = overall[overall$start == min(overall$start),]$start
      end = overall[overall$end == max(overall$end),]$end

      xmin = sprintf("%.2f",overall[overall$xmin == min(overall$xmin),]$xmin)
      xmax = sprintf("%.2f",overall[overall$xmax == max(overall$xmax),]$xmax)
      ymin = sprintf("%.2f",overall[overall$ymin == min(overall$ymin),]$ymin)
      ymax = sprintf("%.2f",overall[overall$ymax == max(overall$ymax),]$ymax)

      max_north = paste(overall[overall$ymax == min(overall$ymax),]$TagID, collapse = ", ")
      max_south = paste(overall[overall$ymin == min(overall$ymin),]$TagID, collapse = ", ")
      max_east = paste(overall[overall$xmax == max(overall$xmax),]$TagID, collapse = ", ")
      max_west = paste(overall[overall$xmin == min(overall$xmin),]$TagID, collapse = ", ")


      cat(paste0("\033[",msg_col,"m", "--> n = ",tot_bird, " animals, ", tot_fix, " fixes (mean/animal: ", paste(sprintf("%.0f",mean_fix), "±", sprintf("%.0f",sd_fix)),")", "\033[0m\n"))
      cat(paste0("\033[",msg_col,"m", "Animal(s): ",paste(max_fix$TagID,collapse = ", "), " has the most fixes (", max_fix$n_fixes[1], ")" ,"\033[0m\n"))
      cat(paste0("\033[",msg_col,"m", "Animal(s): ",paste(min_fix$TagID,collapse = ", "), " has the least fixes (", min_fix$n_fixes[1], ")" ,"\033[0m\n"))

      cat(rep(" ",  getOption("width")), "\n", sep = "")

      cat(paste0("\033[",msg_col,"m", "--> Date range: ",start, " to ", end,"\033[0m\n"))
      cat(paste0("\033[",msg_col,"m", "Animal(s): ", paste(max_dur$TagID, collapse = ", "), " tracked for the longest time (", sprintf("%.1f",max_dur$dur[1]), " ", attr(max_dur$dur, "units"),")" ,"\033[0m\n"))
      cat(paste0("\033[",msg_col,"m", "Animal(s): ", paste(min_dur$TagID, collapse = ", "), " tracked for the least time (", sprintf("%.1f",min_dur$dur[1]), " ", attr(min_dur$dur, "units"),")" ,"\033[0m\n"))

      #cat(paste0("\033[",msg_col,"m", "--> longitude range: ",xmin, " to ", xmax,"\033[0m\n"))
      #cat(paste0("\033[",msg_col,"m", "--> longitude range: ",ymin, " to ", ymax,"\033[0m\n"))

      #cat(paste0("\033[",msg_col,"m", "--> Spatial extent: \033[0m\n"))
      cat(rep(" ",  getOption("width")), "\n", sep = "")
      draw_bbox(width = 40, height = 15, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, col = msg_col)

      cat(rep(" ",  getOption("width")), "\n", sep = "")
      cat(paste0("\033[",msg_col,"m", "Bird max ranges --> north: ", max_north, ", east: ", max_east, ", south: ", max_south, ", west: ", max_west, "" ,"\033[0m\n"))

    }
  }


  # ------------------------------------------------------------------------ #
  if("spatial" %in% measure){

    if(messages){
      cat(rep(" ",  getOption("width")), "\n", sep = "")
      rule_centre("Spatial extent checks", col = msg_col)
    }
    #data$latitude < yra[1]
    #data[data$TagID == 4032 & data$DateTime %in% sum_dat$xmin_bad_times[[3]],]$longitude
    #which(sum_dat$ymax_bad_times == 1)

    # e.g. using summary
    #sum_dat0 = summary(data, round_out=FALSE)

    if(!is.null(xra)){
      if(length(xra) != 2){stop("Needs a vector of length 2 for longitude")}
    }
    if(!is.null(yra)){
      if(length(yra) != 2){stop("Needs a vector of length 2 for latitude")}
    }

    # ----------------------- #
    # LONGITUDE CHECK
    # ----------------------- #

    if(!is.null(xra)){

      sum_dat_x <- data %>%
        group_by(TagID) %>%
        mutate(
          minx_bad = if (!is.null(xra[1])) if_else(longitude < xra[1], 1L, 0L) else NA_integer_,
          maxx_bad = if (!is.null(xra[2])) if_else(longitude > xra[2], 1L, 0L) else NA_integer_
        ) %>%
        summarise(
          any_xmin_bad = any(minx_bad == 1, na.rm = TRUE),
          any_xmax_bad = any(maxx_bad == 1, na.rm = TRUE),

          xmin_bad = list(unique(longitude[which(minx_bad == 1L)])),
          xmax_bad = list(unique(longitude[which(maxx_bad == 1L)])),

          xmin_bad_times = list(unique(DateTime[which(minx_bad == 1L)])),
          xmax_bad_times = list(unique(DateTime[which(maxx_bad == 1L)])),

          .groups = "drop"
        )
      names(sum_dat_x$xmin_bad) <- names(sum_dat_x$xmax_bad) <- names(sum_dat_x$xmin_bad_times) <- names(sum_dat_x$xmax_bad_times) <- sum_dat_x$TagID


      if(messages){

        cat(paste0("\033[",msg_col,"m", "Min expected longitude: ",  xra[1], "", "\033[0m\n"))

        ######## min long
        if(any(sum_dat_x$any_xmin_bad)){

          wrongun = sum_dat_x[which(sum_dat_x$any_xmin_bad),]$xmin_bad
          for(tag in names(wrongun)) {
            vals = sprintf("%.2f",wrongun[[tag]])
            n_f <- length(vals)
            if(length(vals) > max_show) {
              vals <- c(vals[1:max_show], "...")
            }
            msg <- paste0("- Animal: ", tag, " -> n = ", n_f, " bad min longitude(s): ", paste(vals, collapse = ", "))
            cat(paste0("\033[",msg_col,"m"),msg,"\033[0m\n")
          }

        } else{
          cat(paste0("\033[",msg_col,"m"),"- All data within bounds","\033[0m\n")
        }

        ######## max long
        cat(paste0("\033[",msg_col,"m", "Max expected longitude: ",  xra[2], "", "\033[0m\n"))
        if(any(sum_dat_x$any_xmax_bad)){

          wrongun = sum_dat_x[which(sum_dat_x$any_xmax_bad),]$xmax_bad
          for(tag in names(wrongun)) {
            vals = sprintf("%.2f",wrongun[[tag]])
            n_f <- length(vals)
            if(length(vals) > max_show) {
              vals <- c(vals[1:max_show], "...")
            }
            msg <- paste0("- Animal: ", tag, " -> n = ", n_f, " bad max longitude(s): ", paste(vals, collapse = ", "))
            cat(paste0("\033[",msg_col,"m"),msg,"\033[0m\n")
          }

        } else{
          cat(paste0("\033[",msg_col,"m"),"- All data within bounds","\033[0m\n")
        }
      }


    }

    # ----------------------- #
    # LATITUDE CHECK
    # ----------------------- #

    if(!is.null(yra)){

      sum_dat_y <- data %>%
        group_by(TagID) %>%
        mutate(
          miny_bad = if (!is.null(yra[1])) if_else(latitude < yra[1], 1L, 0L) else NA_integer_,
          maxy_bad = if (!is.null(yra[2])) if_else(latitude > yra[2], 1L, 0L) else NA_integer_
        ) %>%
        summarise(
          any_ymin_bad = any(miny_bad == 1, na.rm = TRUE),
          any_ymax_bad = any(maxy_bad == 1, na.rm = TRUE),

          ymin_bad = list(unique(latitude[which(miny_bad == 1L)])),
          ymax_bad = list(unique(latitude[which(maxy_bad == 1L)])),

          ymin_bad_times = list(unique(DateTime[which(miny_bad == 1L)])),
          ymax_bad_times = list(unique(DateTime[which(maxy_bad == 1L)])),

          .groups = "drop"
        )
      names(sum_dat_y$ymin_bad) <- names(sum_dat_y$ymax_bad) <- names(sum_dat_y$ymin_bad_times) <- names(sum_dat_y$ymax_bad_times) <- sum_dat_y$TagID


      if(messages){

        ######## min lat
        cat(paste0("\033[",msg_col,"m", "Min expected latitude: ",  yra[1], "", "\033[0m\n"))
        if(any(sum_dat_y$any_ymin_bad)){

          wrongun = sum_dat_y[which(sum_dat_y$any_ymin_bad),]$ymin_bad
          for(tag in names(wrongun)) {
            vals = sprintf("%.2f",wrongun[[tag]])
            n_f <- length(vals)
            if(length(vals) > max_show) {
              vals <- c(vals[1:max_show], "...")
            }
            msg <- paste0("- Animal: ", tag, " -> n = ", n_f, " bad min latitude(s): ", paste(vals, collapse = ", "))
            cat(paste0("\033[",msg_col,"m"),msg,"\033[0m\n")
          }

        } else{
          cat(paste0("\033[",msg_col,"m"),"- All data within bounds","\033[0m\n")
        }

        ######## max lat
        cat(paste0("\033[",msg_col,"m", "Max expected latitude: ",  yra[2], "", "\033[0m\n"))
        if(any(sum_dat_y$any_ymax_bad)){

          wrongun = sum_dat_y[which(sum_dat_y$any_ymax_bad),]$ymax_bad
          for(tag in names(wrongun)) {
            vals = sprintf("%.2f",wrongun[[tag]])
            n_f <- length(vals)
            if(length(vals) > max_show) {
              vals <- c(vals[1:max_show], "...")
            }
            msg <- paste0("- Animal: ", tag, " -> n = ", n_f, " bad max latitude(s): ", paste(vals, collapse = ", "))
            cat(paste0("\033[",msg_col,"m"),msg,"\033[0m\n")
          }

        } else{
          cat(paste0("\033[",msg_col,"m"),"- All data within bounds","\033[0m\n")
        }
      }

    }

  }

  # ------------------------------------------------------------------------ #
  if("temporal" %in% measure){

    if(messages){
      cat(rep(" ",  getOption("width")), "\n", sep = "")
      rule_centre("Temporal extent checks", col = msg_col)
    }
    # easy to highlight fixes in future as a tag clocking error

    if(!is.null(dates)){
      if(length(dates) != 2){stop("Needs a vector of length 2 for dates")}

      dates = as.POSIXct(dates, tz = tz)

      time_summary <- data %>%
        group_by(TagID) %>%
        mutate(future_flag = if_else(DateTime > Sys.time(), 1L, 0L),
               temporal_flag = if_else(DateTime < (dates[1] - buffer_days) | DateTime > (dates[2] + buffer_days), 1L,0L)
        ) %>%
        summarise(
          any_future = any(future_flag == 1),
          future_dates = list(DateTime[future_flag == 1]),
          n_future = sum(future_flag == 1),

          any_temporal = any(temporal_flag == 1),
          temporal_dates = list(DateTime[temporal_flag == 1]),
          n_temporal = sum(temporal_flag == 1),

          .groups = "drop"
        )
      names(time_summary$future_dates) <- names(time_summary$temporal_dates) <- time_summary$TagID

      if(messages){

        if(any(time_summary$any_future)){

          cat(paste0("\033[",msg_col,"m", "", sum(time_summary$n_future), " time stamp(s) are in data are in the future!", "\033[0m\n"))

          wrongun = time_summary[which(time_summary$any_future),]$future_dates

          for(tag in names(wrongun)) {
            dates_print <- wrongun[[tag]]
            dates_print <- format(dates_print, "%Y-%m-%d %H:%M:%S", tz = tz)
            if(length(dates_print) > max_show) {
              dates_print <- c(dates_print[1:max_show], "...")
            }
            n_f <- time_summary[time_summary$TagID == tag,]$n_temporal
            msg <- paste0("- Animal: ", tag, " -> n = ", n_f, ": ", paste(dates_print, collapse = ", "))
            cat(paste0("\033[",msg_col,"m"),msg,"\033[0m\n")
          }
        } else{
          cat(paste0("\033[",msg_col,"m"),"- No errant future data","\033[0m\n")
        }

        cat(paste0("\033[",msg_col, "m", "Date bounds: ",  dates[1], " to ", dates[2], "", "\033[0m\n"))

        if(any(time_summary$any_temporal)){

          cat(paste0("\033[",msg_col,"m", " --> ", sum(time_summary$n_temporal), " fix time stamp(s) are outside the date bounds", "\033[0m\n"))

          wrongun = time_summary[which(time_summary$any_temporal),]$temporal_dates

          for(tag in names(wrongun)) {
            dates_print <- wrongun[[tag]]
            dates_print <- format(dates_print, "%Y-%m-%d %H:%M:%S", tz = tz)
            if(length(dates_print) > max_show) {
              dates_print <- c(dates_print[1:max_show], "...")
            }
            n_f <- time_summary[time_summary$TagID == tag,]$n_temporal
            msg <- paste0("- Animal: ", tag, " -> n = ", n_f, ": ", paste(dates_print, collapse = ", "))
            cat(paste0("\033[",msg_col,"m"),msg,"\033[0m\n")
          }
        } else{
          cat(paste0("\033[",msg_col,"m"),"- All data within date bounds","\033[0m\n")
        }

      }
    } else{
      if(messages){
        cat("\033[35m", "No date range provided", "\033[0m\n")
      }

    }

  }

  out <- vector("list", 4)
  names(out) <- c("summary","xra","yra","dates")

  if(!is.null(xra)){
    out[[1]] <- overall
  }
  if(!is.null(xra)){
    out[[2]] <- sum_dat_x
  }
  if(!is.null(yra)){
    out[[3]] <- sum_dat_y
  }
  if(!is.null(dates)){
    out[[4]] <- time_summary
  }

  return(out)
}


#' @rdname tag_spans
#' @export
draw_bbox <- function(width = 40, height = 15, col = "35",
                      xmin = -10, xmax = 10, ymin = -5, ymax = 5,
                      title = "Spatial extent (long, lat)") {
  # ANSI colour codes
  start_col <- paste0("\033[", col, "m")
  end_col   <- "\033[0m"

  # top border with label (xmin,ymax)
  label_tl <- paste0("(", xmin, ",", ymax, ")")
  top <- paste0(label_tl,
                paste0(rep("-", max(0, width - nchar(label_tl))), collapse = ""))

  # bottom border with label (xmax,ymin)
  label_br <- paste0("(", xmax, ",", ymin, ")")
  bottom <- paste0(paste0(rep("-", max(0, width - nchar(label_br))), collapse = ""),
                   label_br)

  # middle row template
  middle <- function(text = "") {
    text_len <- nchar(text)
    if (text_len == 0) {
      paste0("|", paste0(rep(" ", width - 2), collapse = ""), "|")
    } else {
      pad_left <- floor((width - 2 - text_len) / 2)
      pad_right <- (width - 2) - text_len - pad_left
      paste0("|",
             paste0(rep(" ", pad_left), collapse = ""),
             text,
             paste0(rep(" ", pad_right), collapse = ""),
             "|")
    }
  }

  # print
  cat(start_col, top, end_col, "\n", sep = "")
  mid_rows <- height - 2
  for (i in seq_len(mid_rows)) {
    if (i == ceiling(mid_rows / 2)) {
      cat(start_col, middle(title), end_col, "\n", sep = "")
    } else {
      cat(start_col, middle(), end_col, "\n", sep = "")
    }
  }
  cat(start_col, bottom, end_col, "\n", sep = "")

  invisible(NULL)
}
