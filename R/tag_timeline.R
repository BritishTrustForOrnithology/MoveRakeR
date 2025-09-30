#' Determine the general temporal tracking window of animals
#'
#' A summary of temporal overlaps and intensity of tracking across animals within data object
#'
#' @details
#' The function \code{tag_timeline} assesses the time span of individual animals and the overlaps with others
#' in the dataset. The messaging output gives an indication to the user as to the extent of tracking
#' intensity through time across animals and any animals that have the least or most overlap with others.
#' The function also flags missing data through days without GPS data. The \code{tag_timeline} works by assessing
#' the proportion of days at different proportional thresholds for reporting.
#' These are:
#' - (1) The active threshold, \emph{active_thresh}, to quantify animals present when less than (active_thresh*100)% of other animals are active;
#' - (2) The alone threshold, \emph{alone_thresh}, for animals that had more than (alone_thresh*100)% time tracked alone; and
#' - (3) The missing threshold, \emph{miss_thresh}, for animals that had more than (miss_thresh*100)% days with missing data.
#'
#' Note however this is a coarse visual and broad assessment and so will not pick up on finer-scale intermittent data.
#' For example if one single fix was collected on three consecutive days, but the tag was meant to be at
#' five minutes, the function would still treat that as having presence data on those days.
#' Therefore this function should be used with caution should such intermittent issues occur. This function is therefore
#' of more use for longer tracking durations for GPS data collected as intended for bursts but that could have had
#' gaps spanning more than a day.
#'
#' Further, \code{tag_timeline} works on the scale of days and assumes that GPS sampling rates
#' are less than one day; this is to enable a unit over which to count and compute temporal density. This
#' could be updated in the future but for now any data that are collected at rates such as once every two+ days
#' will not be suitable for this function.
#'
#' @param data Input data object, with required columns: TagID, DateTime, longitude, latitude.
#' @param plot A TRUE/FALSE (defaulting to FALSE) whether to plot a ggplot visualisation of daily tracking
#' intensity upon function completion.
#' @param return_plot A TRUE/FALSE (defaulting to TRUE) as to whether to return the ggplot visualisation of daily tracking
#' intensity, regardless of whether the user wanted it plotted to the R session on function completion. If TRUE,
#' the plot is given in the returned object: attr(, "plot").
#' @param overlaps A TRUE/FALSE whether to compute overlap data between individual animals, defaults to TRUE.
#' @param active_thresh a continuous numeric value between 0 and 1 used to quantify which animals were present when
#' less than (active_thresh*100)% of other animals were active.
#' @param alone_thresh a continuous numeric vallue between 0 1nd 1 used quantify which animals had more
#' than (alone_thresh*100)% time tracked alone.
#' @param miss_thresh a continuous numeric value between 0 and 1 to determine which animals had more
#' than (miss_thresh*100)% days with missing data.
#' @param max_show The maximum number of individual animals to show in the messaging print out.
#' @param messages Whether printed console messages are to be returned to the user, a Boolean defaulting to TRUE.
#' @param msg_col A numeric value for the colour to be used in console graphics.
#' The colour of the print out in the console, depending on your taste and whether black or white backgrounds
#' are currently being used within R Studio. Defaults to 34 (blue).
#' @param date_breaks The x value DateTime tick mark breaks to be used in the ggplot for \emph{plot}, defaulting to "6 month".
#'
#' @seealso [MoveRakeR::rake], [MoveRakeR::rake_outlie], [MoveRakeR::tag_spans]
#'
#' @return
#' The returned data, other than the messaging to the console, is as a single data.frame
#' that contains the following information:
#' - mean_overlap = for each bird, the average number of other birds that were active on the days that bird had GPS data;
#' - sd_overlap = the sd of the number of other birds present on the days that the bird had GPS data;
#' - min_overlap = minimum number of other birds active on any given day (e.g. if any time alone, will be zero);
#' - max_overlap = maximum number of other birds active on any given day;
#' - mean_prop_birds_active = mean (across all days where the animal had active GPS data) of the the proportion of other birds active on a given day / total number of other birds that could have been active (i.e. total number fed into dataset - 1);
#' - sd_prop_birds_active = SD of above;
#' - sum_alone = total number of days where the animal was alone with no other birds contributing data;
#' - mean_prop_alone = the mean proportion of days where the animal was active but no others were (the fraction of days that a bird was active but had no other birds active at the same time);
#' - sd_prop_alone = the sd proportion of days where the animal was active but no others were;
#' - sum_n_days_active = the total number of days of the animal that had data;
#' - sum_n_tot = the total tracking duration of the animal;
#' - sum_prop_missing_days = the proportion of sum_n_days_active / sum_n_tot.
#'
#' If a bird never overlapped with any other birds on any of its active days, then it would be a mean_overlap
#' of zero and mean_prop_birds_active score of 1.
#'
#' @examples
#'
#' indata <- yourdata # data.frame with a minimum of columns named TagID, DateTime, longitude, latitude
#' data <- Track(indata)
#' res = tag_timeline(data,
#'     plot = FALSE,
#'     return_plot = TRUE,
#'     overlaps = TRUE,
#'     active_thresh = 0.5,
#'     alone_thresh = 0.5,
#'     miss_thresh = 0.25,
#'     msg_col = 34)
#'
#' # grab the plot output
#' # example replacing plot defaults for x axis
#' attr(res, "plot") + scale_x_date(
#'   date_breaks = "1 year",
#'   date_labels = "%Y"
#' )  + theme(
#'   axis.text.x = element_text(
#'     angle = 45,
#'     hjust = 1,
#'     vjust = 1
#'   )
#' )
#'
#' ## Also see
#' # library(ctmm)
#' # Plot the sampling intervals, e.g. of the 'gazelle' data
#' # dt.plot() # notrun
#'
#' @export
tag_timeline <- function(data, plot = FALSE, return_plot = TRUE, overlaps = TRUE,
                     active_thresh = 0.5, alone_thresh = 0.5, miss_thresh = 0.25, msg_col = 34,  max_show = 3,
                     date_breaks = "6 month", messages = TRUE){

  # function for setting width of hyphens to user window and colour
  #cat(rep("-", getOption("width")), sep = "")
  width <- getOption("width")   # current terminal width

  # Get the full date range
  full_dates <- seq.Date(min(as.Date(data$DateTime)),
                         max(as.Date(data$DateTime)), by = "day")

  # ---- 1. Daily presence per TagID ----
  # Expand TagID × Date
  daily_presence <- data %>%
    mutate(date = as.Date(DateTime)
           #,
           #TagID = factor(TagID, levels = unique(TagID))
    ) %>%
    group_by(TagID, date) %>%
    summarise(present = n() > 0, .groups = "drop") %>%  # TRUE if there is any GPS record
    complete(TagID, date = full_dates, fill = list(present = FALSE))  %>% # fill missing days
    arrange(TagID) %>%
    mutate(TagID_num = as.numeric(factor(TagID, levels = unique(TagID))))  # numeric y-axis

  # ---- 2. Daily intensity (all animals) ----
  daily_intensity <- daily_presence %>%
    group_by(date) %>%
    summarise(n_active = sum(present), .groups = "drop")

  # ---- 3. plot ----
  #g1 <- NULL

  # Determine y-value above all TagIDs for intensity row
  y_max <- length(unique(daily_presence$TagID)) + 1

  g1 <- ggplot() +
    # Intensity row
    geom_tile(data = daily_intensity,
              aes(x = date, y = y_max, fill = n_active),
              width = 1, height = 0.8) +
    scale_fill_gradient(low = "white", high = "steelblue", name = "Active animals") +
    # Individual presence tiles
    geom_tile(data = daily_presence %>% filter(present),
              aes(x = date, y = TagID_num),
              width = 1, height = 0.8, fill = "skyblue") +

    # Outlier days (red outline only)
    #geom_tile(data = daily_outliers,
    #          aes(x = date, y = TagID_num),
    #          width = 1, height = 0.8, fill = NA, colour = "red", linewidth = 0.7) +


    # Label y-axis with TagID
    scale_y_continuous(breaks = daily_presence$TagID_num,
                       labels = daily_presence$TagID,
                       expand = expansion(mult = c(0.05, 0.1))) +
    scale_x_date(
      date_breaks = date_breaks,
      date_labels = "%b %Y"
    ) +
    labs(x = "Date", y = "TagID",
         title = "Daily GPS Presence per TagID (top row = intensity of active animals)") +
    theme_minimal() + theme(
      axis.text.x = element_text(
        angle = 90,
        hjust = 1,
        vjust = 1
      )
    )

  if(plot){
    # Determine y-value above all TagIDs for intensity row
    g1
  }

  # ---- 4. Overlaps among animals and messaging ----
  if(overlaps){

    # individual dates
    #individ_dates <- summary(data) # MoveRakeR code here is slightly slower dev.
    individ_dates <- data %>%
      group_by(TagID) %>%
      summarise(
        start = min(DateTime),
        end   = max(DateTime),
        #duration_days = round(as.numeric(difftime(max(DateTime), min(DateTime), units = "days")),0),
        duration_days = as.integer(as.Date(max(DateTime)) - as.Date(min(DateTime)) + 1),
        .groups = "drop"
      )

    total_n <- nrow(individ_dates)
    daily_overlap <- daily_presence %>%
      left_join(daily_intensity, by = "date") %>%
      mutate(overlap_others = ifelse(present, n_active - 1, NA),
             prop_birds_active = n_active / total_n,
             prop_other_birds_active = overlap_others / (total_n-1)
      )

    #daily_outliers <- daily_overlap %>%
    #  filter(!is.na(overlap_others)) %>%
    #  mutate(outlier_day = overlap_others <= 1) %>%
    #  filter(outlier_day)   # keep only those problematic days

    # ---- Overlap stats at bird level ----
    all_dates <- seq.Date(
      from = min(daily_presence$date),
      to   = max(daily_presence$date),
      by   = "day"
    )
    total_days <- length(all_dates)

    # overlaps
    bird_overlap <- daily_overlap %>%
      group_by(TagID) %>%
      summarise(
        mean_overlap       = mean(overlap_others, na.rm = TRUE),
        sd_overlap         = sd(overlap_others, na.rm = TRUE),
        min_overlap        = min(overlap_others, na.rm = TRUE),
        max_overlap        = max(overlap_others, na.rm = TRUE),
        mean_prop_birds_active  = mean(prop_other_birds_active, na.rm = TRUE),
        sd_prop_birds_active  = sd(prop_other_birds_active, na.rm = TRUE),
        sum_alone     = sum(overlap_others == 0, na.rm = TRUE),
        mean_prop_alone     = mean(overlap_others == 0, na.rm = TRUE),
        sd_prop_alone  = sd(overlap_others == 0, na.rm = TRUE),

        sum_n_days_active          = sum(present),
        sum_n_tot = individ_dates[individ_dates$TagID %in% cur_group(),]$duration_days,
        sum_prop_missing_days   = 1- (sum(daily_overlap[daily_overlap$TagID %in% cur_group(),]$present) / individ_dates[individ_dates$TagID %in% cur_group(),]$duration_days),
        #sum_prop_missing_tot   = sum(!present) / total_days,                    # across all birds (makles no sense as this mixes missing data with tag not deployed)
        .groups            = "drop"
      ) %>% setDT()

    # ----------------------------- #
    # “outliers” as birds with low mean overlap, or a high proportion of days where they were giving data alone:
    #outliers <- bird_overlap %>%
    #  filter(mean_prop_birds_active < 0.5 | mean_prop_alone > 0.5) %>%
    #  pull(TagID)

    #### some sort of summary, e.g. birds that on average were tracked when less than half of other
    #### birds were tracked BUT NOTE THAT IS BIASED BY DEPLOYMENT LENGTH!

    #####################################################
    # MESSAGED OUTPUTS
    #####################################################

    if(messages){
      cat(rep(" ",  getOption("width")), "\n", sep = "")
      cat("\033[", msg_col, "m", strrep("*", getOption("width")), "\033[0m\n", sep = "")
      rule_centre("Overlap summary", col = msg_col, char = "*")
      cat("\033[", msg_col, "m", strrep("*", getOption("width")), "\033[0m\n", sep = "")

      cat(rep(" ",  getOption("width")), "\n", sep = "")

      rule_centre("Overlaps with other animals", col = msg_col)

      bd1 <- bird_overlap[bird_overlap$mean_prop_birds_active < active_thresh,]$TagID; bd1_ <- bd1
      if(length(bd1) > 0){
        if(length(bd1) > max_show){
          bd1 <- c(bd1[1:max_show], "...")
        }
        msg1 <- paste0("Animal(s) present when less than ", active_thresh*100, "% of other animals (n = ", total_n ," max), were active: \n- ", paste(bd1, collapse = ", "), " [n = ", length(bd1_), "]" )
      } else{
        msg1 <- paste0("Animal(s) present when less than ", active_thresh*100, "% of other animals (n = ", total_n ," max), were active: \n- None", collapse = ", ")
      }
      cat(paste0("\033[",msg_col,"m",msg1,"\033[0m\n"))

      # ---------- #
      # magnitudes
      least_1 = bird_overlap[bird_overlap$mean_overlap %in% min(bird_overlap$mean_overlap),]$TagID
      least_1 = paste(least_1, collapse = ", ")

      most_1 = bird_overlap[bird_overlap$mean_overlap %in% max(bird_overlap$mean_overlap),]$TagID
      most_1 = paste(most_1, collapse = ", ")

      msg2 <- paste0("- ",  least_1, " had the least overlap with other animals (Mean: ",
                     round(bird_overlap[bird_overlap$mean_overlap %in% min(bird_overlap$mean_overlap),]$mean_overlap[1],1), " ± ",
                     round(bird_overlap[bird_overlap$mean_overlap %in% min(bird_overlap$mean_overlap),]$sd_overlap[1],1),
                     " animals)" )
      cat(paste0("\033[",msg_col,"m",msg2,"\033[0m\n"))

      msg3 <- paste0("- ",  most_1, " had the most overlap with other animals (Mean: ",
                     round(bird_overlap[bird_overlap$mean_overlap %in% max(bird_overlap$mean_overlap),]$mean_overlap[1],1), " ± ",
                     round(bird_overlap[bird_overlap$mean_overlap %in% max(bird_overlap$mean_overlap),]$sd_overlap[1],1),
                     " animals)" )
      cat(paste0("\033[",msg_col,"m",msg3,"\033[0m\n"))

      cat(rep(" ",  getOption("width")), "\n", sep = "")


      # --------------------------- #
      # then proportion alone
      rule_centre("Animals with data alone", col = msg_col)

      bd2 <- bird_overlap[bird_overlap$mean_prop_alone > alone_thresh,]$TagID; bd2_ <- bd2
      if(length(bd2) > 0){

        if(length(bd2) > max_show){
          bd2 <- c(bd2[1:max_show], "...")
        }
        msg4 <- paste0("Animal(s) with more than ", alone_thresh*100, "% of their time alone: \n- ", paste(bd2, collapse = ", "), " [n = ", length(bd2_), "]" )
      } else{
        msg4 <- paste0("- Animals with more than ", alone_thresh*100, "% of their time alone: \n- None", collapse = ", ")
      }
      cat(paste0("\033[",msg_col,"m",msg4,"\033[0m\n"))

      # ---------- #
      # magnitudes
      most = bird_overlap[bird_overlap$mean_prop_alone %in% max(bird_overlap$mean_prop_alone),]$TagID
      most = paste(most, collapse = ", ")

      least = bird_overlap[bird_overlap$mean_prop_alone %in% min(bird_overlap$mean_prop_alone),]$TagID
      least = paste(least, collapse = ", ")

      msg5 <- paste0("- ", most  ," had the most days tracked alone (Mean: ",
                     round(bird_overlap[bird_overlap$mean_prop_alone %in% max(bird_overlap$mean_prop_alone),]$mean_prop_alone[1]*100,1), "% ± ",
                     round(bird_overlap[bird_overlap$mean_prop_alone %in% max(bird_overlap$mean_prop_alone),]$sd_prop_alone[1]*100,1), ")"
      )
      #cat("\033[35m",msg5,"\033[0m\n")
      cat(paste0("\033[",msg_col,"m",msg5,"\033[0m\n"))
      msg6 <- paste0("- ",  least, " had the least days tracked alone (Mean: ",
                     round(bird_overlap[bird_overlap$mean_prop_alone %in% min(bird_overlap$mean_prop_alone),]$mean_prop_alone[1]*100,1), "% ± ",
                     round(bird_overlap[bird_overlap$mean_prop_alone %in% min(bird_overlap$mean_prop_alone),]$sd_prop_alone[1]*100,1), ")"
      )
      cat(paste0("\033[",msg_col,"m",msg6,"\033[0m\n"))

      cat(rep(" ",  getOption("width")), "\n", sep = "")

      # --------------------------- #
      rule_centre("Missing data", col = msg_col)

      # Pass of missing days (note this will not work if sampling intervals are more than a day)
      bd3 <- bird_overlap[bird_overlap$sum_prop_missing_days > miss_thresh,]$TagID; bd3_ <- bd3

      if(length(bd3) > 0){
        if(length(bd3) > max_show){
          bd3 <- c(bd3[1:max_show], "...")
        }
        msg7 <- paste0("Animal(s) with more than ", miss_thresh*100, "% days missing data: \n- ", paste0(bd3, collapse = ", "), " [n = ", length(bd3_), "]")
      } else{
        msg7 <- paste0("Animal(s) with more than ", miss_thresh*100, "% days missing data: \n- None", collapse = ", ")
      }
      cat(paste0("\033[",msg_col,"m",msg7,"\033[0m\n"))

      # ---------- #
      # magnitudes
      most_2 = bird_overlap[bird_overlap$sum_prop_missing_days %in% max(bird_overlap$sum_prop_missing_days),]$TagID
      most_2 = paste(most_2, collapse = ", ")

      least_2 = bird_overlap[bird_overlap$sum_prop_missing_days %in% min(bird_overlap$sum_prop_missing_days),]$TagID
      least_2 = paste(least_2, collapse = ", ")

      msg8 <- paste0("- ", most_2, " has the greatest number of days with missing data (",
                     round(bird_overlap[bird_overlap$sum_prop_missing_day %in% max(bird_overlap$sum_prop_missing_day),]$sum_prop_missing_day[1]*100,1), "%)"
      )
      cat(paste0("\033[",msg_col,"m",msg8,"\033[0m\n"))

      msg9 <- paste0("- ",  least_2, " has the least number of days with missing data (",
                     round(bird_overlap[bird_overlap$sum_prop_missing_days %in% min(bird_overlap$sum_prop_missing_days),]$sum_prop_missing_days[1]*100,1), "%)"
      )
      cat(paste0("\033[",msg_col,"m",msg9,"\033[0m\n"))

      cat(rep(" ",  getOption("width")), "\n", sep = "")
      cat(paste0("\033[", msg_col, "m", strrep("*", getOption("width")), "\033[0m\n", sep = ""))

      # outliers? perhaps bring in rake_outlie(), but that doesn't handle datetimes...
      #median_start <- median(deploy_summary$start)
      #median_end   <- median(deploy_summary$end)
      #deploy_summary <- deploy_summary %>%
      #  mutate(
      #    starts_late = start > median_start + 30,    # example: more than 30 days late
      #    ends_early  = end   < median_end - 30,     # example: more than 30 days early
      #    duration_outlier = duration_days < quantile(duration_days, 0.1) |
      #      duration_days > quantile(duration_days, 0.9)
      #  )
    }


  }

  # ---- 5. Return outputs ----
  if(return_plot){

    attr(bird_overlap, "plot") <- g1

  }
  return(bird_overlap)


}


