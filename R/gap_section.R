#' Defining gapsections for tracking data
#'
#' Function \code{gap_section} inserts a label for each date-time-stamped positional fix as to whether there was a
#' \code{GAP} recorded based on the interval between \code{DateTime} stamp of locations, with the
#' value of the gap threshold (\code{"GAP"}) specified by the user. \code{gap_section} then numbers
#' strings of points deemed without a gap, as "gapsections", sequentially in the dataset.
#'
#' The issue of gaps in the telemetry record is a well known issue for tracking datasets.
#' Other packages such as \code{move} and \code{momentuHMM} may consider these as \emph{bursts}, but
#' in MoveRakeR, to keep them separate, these are named \emph{gapsections} and labelled as
#' an additional column \code{gapsec}. This allows further manipulation such as use with downstream
#' analyses, for example making sure that periods when tags may not have been recording, e.g. due to
#' low battery are excluded from consideration, e.g. speeds, plotting etc.
#'
#' \code{gap_section} code is carried out directly within the \code{\link{clean_GPS}} function,
#' so this duplication as a standalone function allows the user to redefine gaps should they so
#' wish.
#'
#' @param data Input data object, with required columns: TagID, DateTime, longitude, latitude.
#' @param GAP Numeric integer in seconds for labeling sections of assumed continuous GPS recording for the individual animal.
#' The argument \code{GAP} here is the cut-off i.e. temporal hiatus between two
#' consecutive GPS fixes, which defaults to 28800 seconds, although note the tolerance parameter will be incorporated if greater than zero.
#' This is a subjective choice for the user, and will depend also on GPS sampling
#' resolution. \code{GAP} is also used to defined \strong{gapsections} in the data,
#' that are sequentially labelled. Also note that in \code{\link{clean_GPS}}
#' gap sections (bursts) of fixes of length = 1 are by default EXCLUDED, by the \code{drop_single_gap} argument,
#' see below, but this can be overridden with \code{drop_single_gap = FALSE}. If a precise GAP is needed with no tolerance,
#' set tol to 0. Units are in seconds.
#' @param tol A tolerance is also applied to the GAP vaue, useful for example to give lee-way around
#' the gap criteria relating to imprecision of sampling rate in GPS deviating from the initial rate specified.
#' By default this is set as 0.2, i.e. to give a true value used in delineation as: GAP +/- (GAP*0.2).
#' @param drop_single_gap As above a logical argument as to whether or not to drop gapsections of length 1,
#' and thus retain strings of consecutive strings of points >= 2; i.e. this serving also as a partial
#' temporal filter. This replaces the previous 'drop' argument in MoveRakeR <1.0.2.
#' @param attr If TRUE gives attributes to the data output for arguments used; this is set to FALSE when used inside clean_GPS().
#' @param verbose logical argument as to whether detailed information processing of the function
#' should be displayed, defaults to TRUE.
#'
#' @return The function returns a tibble format with an additional \code{Track} class.
#' Additional columns in the objects are also returned:
#' including: \strong{dt}, time difference (secs) between consecutive GPS fixes,
#' \strong{gap}, a binary (1,0) for fixes where a gap was identified and \strong{gapsec}
#' as a numeric integer of sequentially-labelled gapsection strings of telemetry fixes. Note these
#' columns are overwritten if gapsections have already been defined in \code{\link{clean_GPS}} or
#' \code{gap_section}.
#'
#' @seealso [MoveRakeR::clean_GPS], [MoveRakeR::sub_samp]
#'
#' @examples
#'
#' ## Toy example reading data from one a bird from the UvA-BiTS repository
#' login <- move::movebankLogin("username","password")
#' db <- RODBC::odbcConnect("GPS") # ODBC
#'
#' ## One bird
#' dataUvA_one <- read_track_UvA('1', "2016-06-01 13:53:50","2016-07-15 09:17:14") %>% # replace '1' with TagID real name
#'   clean_GPS(GAP = 28800, tol = 0) %>% gap_section(GAP = 300, tol = 0.4, drop_single_gap = TRUE)
#'
#' TagIDUvA = c('1', '2', '3')
#' start = c("2016-06-01 13:53:50", "2016-06-15 12:22:13", "2016-06-05 08:07:23")
#' end = c("2016-07-15 09:17:14", "2016-07-20 01:08:58", "2016-07-18 14:22:45")
#' dataUvA_many <- read_track_UvA(TagIDUvA, start=start, end=end)
#'
#' dataUvA_2 <- clean_GPS(dataUvA, GAP = 450, tol = 0, Thres = 30, drop_single_gap = TRUE)
#' dataUvA_3 <- gap_section(dataUvA, GAP = 450, tol = 0, drop_single_gap = TRUE)
#'
#' @export
gap_section <- function(data, GAP=28800, tol = 0.2, drop_single_gap = FALSE, verbose = TRUE, attr = TRUE, ...){

  data_dp <- tibble(data)

  if(exists("drop", inherits = FALSE)){
    drop_single_gap = drop # legacy code of drop from previous funcion version retained
    message("Newer version of gap_section() in MoveRakeR v 1.02+ uses 'drop_single_gap' instead of 'drop'")
    cat(rep(" ",  getOption("width")), "\n", sep = "")
  }

  # --------------------------------------- #
  # assessment of relevant minimum namings in data
  #cond <- ifelse(all(c("TagID","DateTime", "longitude", "latitude") %in% names(data_dp)), TRUE, FALSE) # required columns
  #if(!cond){
  #  stop("Missing column names! Check input data for named columns: 'TagID','DateTime','longitude','latitude'")
  #}

  # get attributes from previous function runs
  attr_list <- get_attributes(data)
  # --------------------------------------- #

  if(verbose){

    message("Object of ", length(unique(data$TagID)), " animals, ", nrow(data), " rows of data")

    message("tol value of ", tol, "; GAP = ", GAP, " " ,"secs", "; Real GAP = ",GAP," + ", GAP,"*",tol, " = ", GAP+(GAP*tol), " ", "secs")
  }

  # the real gao being used
  GAP = GAP+(GAP*tol)

  data_dp = data_dp %>%
    #mutate(year = lubridate::year(DateTime)) %>%
    group_by(TagID) %>%
    mutate(dt = as.vector(difftime(lead(DateTime), DateTime, units = 'secs')) ) %>% #.$dt
    mutate(gap = if_else(dt > GAP,1,0), rn_ = row_number()) %>%
    mutate(gap = if_else(rn_ == max(rn_), 1, gap))  %>%
    ungroup() %>%
    mutate(gap0 = gap, gap = lag(gap)) %>% # this is needed across IDs as needing to shunt all rows (lag) down; this enables a 1 in the gap column to start numbering by, and ensures the gap is labelled in the next row to start correct gapsec labelling from (prob could be done in steps above too)
    group_by(TagID) %>%
    mutate(gap_num = case_when(gap == 1  ~ row_number() ),
           gap_num = if_else(min(row_number()) == row_number(), row_number(), gap_num)) %>% # make the first row or each group (TagID) the ro number to label from
    tidyr::fill(gap_num) %>%
    mutate(gapsec = consecutive_id(gap_num), gap = gap0) %>% # set gap back to what it was - it's used in other programs
    #filter(!is.na(gap))
    #group_split() %>%
    #purrr::map_df(~.x %>% group_by(gap_num) %>% mutate(gapsec = cur_group_id())) %>% # assign sequential gapsections
    #ungroup() %>%
    dplyr::select(-c(gap_num, rn_,gap0)) %>%
    group_by(TagID)

  # the first row always comes out with gap == 1 in new code from above process
  data_dp = data_dp %>%  group_by(TagID) %>%
    mutate(gap = if_else(row_number() == min(row_number()), 0, gap),
           gapsec = if_else(row_number() == min(row_number()) & (lead(gapsec)-1) != gapsec, lead(gapsec), gapsec)
           )
  if(data_dp$gapsec[1] > 1){
    data_dp$gapsec = data_dp$gapsec+1
    } else{
      data_dp$gapsec = data_dp$gapsec
  }

  ############
  # 5. if drop_single_gap = TRUE then drop OUT single gapsection fixes as cannot be assessed in the speed filter
  if(drop_single_gap){

    if(verbose){message("Keeping only consecutive fixes within gap sections")}

    data_dp = data_dp %>% group_by(TagID,gapsec) %>% mutate(nn = n()) %>%
      filter(nn > 1) %>% # drop any single gapsecs
      dplyr::select(-nn) %>%
      group_by(TagID) %>%
      mutate(dt = as.vector(difftime(lead(DateTime), DateTime, units = 'secs'))) %>%
      ungroup()

  }

  # --------------------------------------------------------- #
  # assign attributes that may have been present at the start

  if(attr){
    data_dp <- give_attributes(data_dp, attr_list)

    # --------------------------------------------------------- #
    #### retain attributes of the choices made
    # main attribute entry for the function
    # this only modifies general attributes alongside clean_gps
    if(is.null(attr(data_dp, "general") )){
      attr(data_dp, "general") <- "general"
    }
    # sub_attributes for arguments
    attr(attr(data_dp, "general"), "GAP") <- get("GAP")
    attr(attr(attr(data_dp, "general"), "GAP"),"function") <- "gap_section"

    attr(attr(data_dp, "general"), "unit") <- "secs"
    attr(attr(attr(data_dp, "general"), "unit"),"function") <- "gap_section"

    attr(attr(data_dp, "general"), "drop_single_gap") <- get("drop_single_gap")
    attr(attr(attr(data_dp, "general"), "drop_single_gap"),"function") <- "gap_section"


  }


  data_dp <- structure(.Data = data_dp, class = c("Track", "tbl_df","tbl","data.frame"))
  return(data_dp)

}






































