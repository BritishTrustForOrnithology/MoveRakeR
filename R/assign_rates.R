#' Estimate tag sampling rates of a Track-family object
#'
#' @description
#' The function \code{assign_rates} assigns an approximate likely sampling rates of tags based on supplied
#' breaks in the data.
#'
#' @details
#' This function simply cuts the data via binned lower and upper bounds surrounding
#' the potential sampling rate (all rates given in seconds), with tolerance error
#' specified around the rates per break.
#'
#' @param data Input data object, with required columns: TagID, DateTime, longitude, latitude.
#' @param breaks A vector of binned choices of potential rates: the default here is for 11 categories:
#' 10, 120, 300, 600, 900, 1200, 1800, 3600, 7200 and 10800s, and the upper value beyond,
#' labelled as <max dt. This function operates independent of and gapsecs that may have been
#' chosen in \code{gap_section}, i.e. dts are assessed consecutively regardless of gaps, unless
#' supplied specifically within the by argument.
#' @param tol A vector of tolerances that are added to the breaks argument to account for
#' imperfect GPS sampling rates in data obtained; this is set to: c(0,0.5,rep(0.4,2),rep(0.3,5),rep(0.2,2)).
#' The tol argument can also be a single vector that is then applied equally to all break categories.
#' @param by An optional by argument (default to NULL i.e. only assessing by TagID); can be entered as c(x,y,z)
#' for levels to be assessed in the data, e.g. by year (if year is selected and is not present in the data it
#' is automatically added); out="summary" reflects the by variable choice.
#' @param split logical defaulting to FALSE for whether a split tibble is returned by TagID and the other by variables.
#' @param group_after_gap logical defaulting to FALSE for whether to summarise over gapsections if supplied as a by variable.
#' @param out By default the function outputs a \code{summary} count of fixes per TagID binned to the the
#' breaks chosen, and will result in a split tibble df per animal with start and end being the
#' breaks between subsequent categories and likely_rate = breaks. However, if out is \code{NULL}, then
#' the function will return the tibble with 'rate' added as a column to the data supplied.
#' The maximum category split will always be the maximum dt in the data with likely rate <max per animal.
#'
#' @return
#' Depending on the selection of the out argument, either a tabulation of TagID, likely_rate, start, end (of the category range),
#' and N (the summed number of fixes per bin), or a tibble as supplied with an additional rate column for the breaks.
#'
#' Note that if the max break selected is bigger than the max dt in the data, the behaviour
#' for category splits is currently to assess up to the max dts per bird within that upper category.
#'
#' @seealso [MoveRakeR::gap_section]
#' @examples
#'
#' # simple assignment of rates using the default option under breaks argument
#' assign_rates(data, out = "summary") # list of tibbles per animal
#' assign_rates(data, out = NULL) # returns original data with rate column added
#'
#' assign_rates(data, breaks = c(1,60,300,900,1200), tol = 0.2, out = "summary")
#'
#' assign_rates(data2, by = "year", split = FALSE)
#'
#' # Note the totals per bird will always be n-1 total number of rows as cannot estimate dt from the last rows in the data by levels assessed
#'
#' # Further consideration using gapsections
#' data <- data %>% gap_section2(GAP = 18000, tol = 0.1, drop = FALSE) # e.g. 5 hours instead of 8 default
#'
#' assign_rates(data, split = FALSE) # no use of any groupings, dts assessed across any section be animal
#' assign_rates(data, by = c("year"), split = FALSE) # grouping by year, assessment still made across gapsections
#' assign_rates(data, by = c("year","gapsec"), split = FALSE) # use of gapsection, dts between gapsections become NAs and dropped
#' assign_rates(data, by = c("gapsec"), split = FALSE) # same as bove without year
#' assign_rates(data, by = c("year","gapsec"), split = FALSE, group_after_gap = TRUE) # grouping by year after first assessing by gapsection (i.e. removing large dts between gaps)
#'
#'
#
#' \dontrun{
#' ### testing
#' data1 = data_in[data_in$TagID == "5377",]
#' data1 = data_in[data_in$TagID == "202",]
#' data2 <- data_in
#'
#' data = data_in
#' breaks = c(1, 60, 100, 300, 600, 900, 1200, 1800, 3600, 7200, 10800)
#' tol = c(0,0.5,rep(0.4,2),rep(0.3,5),rep(0.2,2))
#'
#' assign_rates(data2, by = "year", split = FALSE)
#'
#' data <- data %>% gap_section2(GAP = 18000, tol = 0.1, drop = FALSE)
#' test = assign_rates(data, split = FALSE) # no use of any groupings, dts assessed across any section be animal
#' test2 = assign_rates(data, by = c("year"), split = FALSE) # grouping by year
#' assign_rates(data, by = c("year","gapsec"), split = FALSE) # use of gapsection, dts between gapsections become NAs and dropped
#' assign_rates(data, by = c("gapsec"), split = FALSE)
#' assign_rates(data, by = c("year","gapsec"), split = FALSE, group_after_gap = TRUE) # grouping by year after first assessing by gapsection (i.e. removing large dts between gaps)
#'
#' by = "year"
#' by = "gapsec"
#'
#' data = data_in
#' by = c("year", "gapsec")
#'}
#' @import dplyr
#' @import tibble
#' @import lubridate
#' @rawNamespace import(data.table, except = c(month, hour, quarter, week, year, wday, second, minute, mday, yday, isoweek))
#' @export
assign_rates <- function(data, by = NULL, split = TRUE, group_after_gap = FALSE,
                          breaks = c(1, 60, 100, 300, 600, 900, 1200, 1800, 3600, 7200, 10800),
                          tol = c(0,0.5,rep(0.4,2),rep(0.3,5),rep(0.2,2)), out = "summary"){


  # --------------------------------------- #
  # assessment of relevant minimum namings in data
  cond <- ifelse(all(c("TagID","DateTime") %in% names(data)), TRUE, FALSE) # required columns
  if(!cond){
    stop("Missing column names! Check input data for named columns: 'TagID','DateTime'")
  }

  # get attributes from previous function runs
  attr_list <- get_attributes(data)

  # --------------------------------------- #
  if(length(tol) == 1){
    tol = rep(tol,length(breaks))
  }


  if(length(breaks)!= length(tol)){
    stop("length of breaks != tol!")
  }

  breaks2 = mapply(function(b, t) b+(b*t), breaks, tol)

  data <- tibble(data)

  # note that as you may have assigned gapsections, there will be no valid dt between two gaps,
  # so we need to work around this, as it is best to simply summarise the WHOLE data even across gaps!
  # if dt exists, go and recalculate dt? Call it something else temporarily?
  # call dt_ and ONLY group by TagID (unless by argument given!)

  if(!is.null(by)){
    if("year" %in% by & !exists("year", data)){
      data$year <- lubridate::year(data$DateTime)
    }
  }

  #if(!exists("dt", data)){
  data <- data %>% group_by(TagID, !!!syms(unique(by))) %>% arrange(TagID, !!!syms(unique(by)), DateTime) %>%
    mutate(dt_ = as.vector(difftime(lead(DateTime), DateTime, units = 'secs')))
  #}

  # for upper catgeory bound - assess max dt in data
  if( breaks[length(breaks)] < max(data$dt_, na.rm=TRUE)){

    #cond = 1
    breaks <- c(breaks, max(data$dt_, na.rm=TRUE))
    breaks2 <- c(breaks2, (max(data$dt_, na.rm=TRUE) + (max(data$dt_, na.rm=TRUE)*0.2)))
    if(length(tol) > 1){  tol = c(tol,tol[length(tol)]) }
  } else{cond = 0}

  if(exists("rate", data)){
    data <- data %>% dplyr::select(-rate) # removing the rate variable if it already exists given merging below
  }

  data = tibble(data) %>% ungroup() %>% mutate(rate_ = cut(dt_, breaks = breaks2, dig.lab = max(nchar(breaks2))))

  # issue perhaps with breaks not being in the data you define...so labels in cut argument would have to match whatever you have?
  # Easier as a posthoc ifelse?
  data$rate_ = as.numeric(do.call('rbind',strsplit(as.character(data$rate_), '[(),]'))[,2])

  # we cut based on the tolerances but naming the rates as that supplied originally in breaks
  df = tibble(rate = breaks, rate_ = breaks2) %>% mutate(rate = lead(rate)) %>% filter(!is.na(rate))

  #if(cond == 1){
  #  # adjusting the maximum upper including a "<"
  #  df$rate[length(df$rate)] <- paste0("<",df$rate[length(df$rate)] )
  #}

  # remember max dt_ per animal before you drop the newly estimated dts (across all gapsections)
  #data_ = data.table::data.table(data)

  ### need a special case for gapsection?
  data_dtmax <- data.table::setDT(data)[,lapply(.SD,max, na.rm=TRUE), .SDcols = "dt_", by = c("TagID", by)]

  # final data
  data = left_join(data, df, by = "rate_") %>% dplyr::select(-rate_) %>% group_by(TagID, !!!syms(unique(by))) %>% dplyr::select(-dt_)


  # --------------------------------------------------------------------------------- #
  # within the data, per animal, replace the maximum category with the upper limit
  # for the specific animal, i.e. upper dt will be that across all animals

  # could do as a split ---> apply f(), or a which.max, siphon off, edit, recombine?
  # so quick either way, doesn't really matter

  #data$rate <- as.character(data$rate)
  #test = data[data$TagID == "253" & data$gapsec == 3,]

  # note that if any groups are < 2 or all NAs for some reason for dt_ this will fall over
  # needed a patch work around, e.g. orphaned fixes from gapsection delineation

  # any ones....drop....
  dd = data %>% group_by(TagID, !!!syms(unique(by))) %>% tally
  dd = dd[dd$n > 1,]
  dd$cc <- dd[,which(names(dd) %in% c("TagID",by))] %>% mutate(cc = paste(TagID, !!!syms(unique(by)), sep = "_")) %>% .$cc

  #head(dd, 20)

  # not keen on too many big merges!
  # get all the by variables, cc and subset, but need to ensure lines of data are not dropped from full output!
  data$cc <- data[,which(names(data) %in% c("TagID",by))] %>% mutate(cc = paste(TagID, !!!syms(unique(by)), sep = "_")) %>% .$cc
  data_ <- data[!data$cc %in% dd$cc,] %>% dplyr::select(-cc)
  data <- data[data$cc %in% dd$cc,]

  maxs = data %>% group_by(TagID, !!!syms(unique(by))) %>% filter(rate == max(rate,na.rm=TRUE)) %>% dplyr::select(-cc)
  non_maxs = data %>% group_by(TagID, !!!syms(unique(by))) %>% filter(rate != max(rate,na.rm=TRUE)) %>% dplyr::select(-cc)
  nas = data %>% group_by(TagID, !!!syms(unique(by))) %>% filter(is.na(rate)) %>% dplyr::select(-cc)

  names(data_dtmax)[which(names(data_dtmax) == "dt_")] <- "real_max_rate"

  maxs = left_join(maxs, data_dtmax, by = c("TagID", by))
  maxs$rate = ifelse(maxs$rate != maxs$real_max_rate & maxs$rate > breaks2[length(breaks2)-1], maxs$real_max_rate, maxs$rate)
  maxs$rate = ifelse(maxs$rate > breaks2[length(breaks2)-1], paste0("<",maxs$rate), maxs$rate)
  maxs <- subset(maxs, select = -real_max_rate)

  maxs$rate <- as.character(maxs$rate)
  non_maxs$rate <- as.character(non_maxs$rate)
  nas$rate <- as.character(nas$rate)
  data_$rate <- as.character(data_$rate)

  data = rbind(maxs, non_maxs, nas, data_) %>% arrange(TagID, !!!syms(unique(by)), DateTime) %>% group_by(TagID, !!!syms(unique(by)))
  data <- structure(.Data = data, class = c("Track", "grouped_df", "tbl_df","tbl","data.frame"))

  #unique(data$rate)

  # -------------------------- #
  # for the summary option
  # -------------------------- #

  if(!is.null(out)){

    if(group_after_gap){

      # option for aggregating back over gapsections if that was specified in the by variable

      if("gapsec" %in% by){
        # get by variables not including gapsec
        by = by[by != "gapsec"]
        #df5 <- df5 %>% group_by(TagID, !!!syms(unique(by2)), likely_rate, start, end, rn) %>% summarise(N = sum(N), .groups = "keep") %>% ungroup() %>% arrange(TagID, !!!syms(unique(by2)), rn)

        # and then again have to combine the "<" columns having been split by gapsec!

      }

    }

    # better to just use the data rate assigned to tally up the count
    df2 = data %>% group_by(TagID, !!!syms(unique(by)), rate, .drop=FALSE) %>% mutate(rate = as.factor(rate)) %>%
      tally() %>%
      rename(likely_rate = "rate") %>%
      filter(!grepl("<",likely_rate) | n > 0) %>%
      filter(!is.na(likely_rate))

    #View(df2[df2$TagID == 253,])

    # merge in rates for categories not "<"
    # merge in the starts and ends
    df_again = df %>% dplyr::select(-rate_) %>% mutate(rn = row_number()) %>% rename(likely_rate = "rate")
    df4 = tibble(likely_rate = breaks, end = breaks2) %>% mutate(start = lag(end)) %>% filter(end != 1) %>%
      relocate(end,.after=start) %>% left_join(., df_again, by = "likely_rate")
    #df4$end[length(df4$end)]
    df4$end = ifelse(df4$likely_rate > breaks2[length(breaks2)-1], df4$likely_rate, df4$end)
    df4$likely_rate <- as.character(df4$likely_rate)

    df3 = df2 %>% ungroup() %>% filter(!grepl("<",likely_rate)) %>%
     left_join(df4, df3, by = "likely_rate") %>% arrange(TagID,rn)

    df5 = df2 %>% ungroup() %>% filter(grepl("<",likely_rate))

    if(group_after_gap){
      # combine the likely rates to higher level

      if(nrow(df5) > 0){

        df5 <- df5 %>% mutate(lr_ = as.numeric(gsub("<","",likely_rate))) %>%
          group_by(TagID, !!!syms(unique(by))) %>%
          summarise(likely_rate = max(lr_), n = sum(n), .groups = "keep") %>%
          mutate(likely_rate = paste0("<", likely_rate))

      }
    }

    #df2[df2$TagID == 4032 & df2$year == 2016,]
    #View(df5[df5$TagID == 253,])

    if(nrow(df5) > 0){
      df5$start <- breaks2[length(breaks2)-1]
      df5$end <- gsub("<", "", df5$likely_rate)
      df5$rn <- length(breaks2) +1
      df5 = rbind(df3, df5) %>% arrange(TagID, rn)
    } else{
      df5 = df3
    }

    df5 <- df5 %>% rename(N = "n") %>% relocate(N, .after = end) %>% ungroup() %>% dplyr::select(-rn) # drop rn last of all

    #View(df5)

    #df5 = tibble(likely_rate = paste0(max(df4$likely_rate),"+"),start = max(df4$end), end = "?")
    ##df4 = rbind(df2, df3)
    #data %>% filter(is.na(data$rate))
    #data_ = data.table::data.table(data)
    #rate_dist = data_[,.N,by=c("TagID","rate"),]
    #names(rate_dist)[2] <- "likely_rate"
    #rate_dist <- rate_dist[order(TagID,likely_rate)]
    #rate_dist <- rate_dist[!is.na(rate_dist$likely_rate),] # NAs are the dt's at end of animal with nothing to assess against
    ##rate_dist$likely_rate = ifelse(is.na(rate_dist$likely_rate), "10800+",rate_dist$likely_rate)
    ##df5 = expand_grid(TagID = unique(data$TagID), df4)
    #df5 = expand_grid(TagID = unique(data$TagID), df2)
    #data = left_join(df5, rate_dist, by = c("TagID","likely_rate"))
    ## drop NA start
    #data <- data[!is.na(data$start),]

    # hacky update the max end for each animal in the summaries (not needed now - done above in main data)
    #df3 = df2 %>% group_by(TagID) %>% filter(rn == max(rn))
    #df2 = df2 %>% group_by(TagID) %>% filter(rn != max(rn))
    ## IF the TagID max dt is greater than the max breaks2[length(breaks2)-1], i.e. the last you selected in the agument, then do nothing otherwise add "<"
    #data_dtmax = data_dtmax %>% mutate(dt = if_else(dt > breaks2[length(breaks2)-1], paste0("<",dt), as.character(dt)))
    ## put those into the df3 above regardless
    #df3 = left_join(data_dtmax, df3, by = "TagID") %>% rename(rate_ = "rate", rate = "dt") %>% select(-c(rate_))
    #df2 = rbind(df2,df3) %>% arrange(TagID,rn) %>% select(-rn)

    if(split){
      data = df5 %>% group_split(TagID, !!!syms(unique(by)))
      if(is.null(by)){
        names(data) <- unique(df5$TagID)
      }
    } else{
      data = df5
    }

  }

  # --------------------------------------------------------- #
  # assign attributes that may have been present at the start
  data <- give_attributes(data, attr_list)

  # --------------------------------------------------------- #
  # add new attr from current function run
  if(is.null(attr(data, "assign_rates") )){
    attr(data, "assign_rates") <- "assign_rates"
  }
  attr(attr(data, "assign_rates"), "breaks") <- breaks
  attr(attr(data, "assign_rates"), "tol") <- tol
  attr(attr(data, "assign_rates"), "out") <- out


  return(data)

}



