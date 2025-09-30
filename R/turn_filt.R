#' Trajectory turning angle filter for GPS data
#'
#' This function filters or annotates data for fixes that are greater than or less than a user-provided
#' turning angle
#'
#' @details
#' It may be necessary to flag/annotate data for a given value of turning speed. This can be achieved with the function
#' \code{turn_filt}, which can fliter angles (radians) below or above a value provided by the user. The option
#' exists to filter (annotate = FALSE) or flag up rows (annotate = TRUE). The function \code{adehabitatLT::as.ltraj}
#' is used to compute the turning angle information. \code{turn_filt} also determines if gapsections already exist
#' in the data and if they do, compute assessment over valid strings of points using existing gaps, or if not,
#' using the \code{gap_section()} defaults of GAP = 28800 s and a tol of 0.2.
#'
#' Note, this function should be used with caution as behavioural characteristics of tracks may naturally be of interest ecologically,
#' and stripping out movements that are too straight or too variable could lead to unexpected results.
#'
#'
#' @param data Input data object, with required columns: TagID, DateTime, longitude, latitude.
#' @param turnFilt Numeric value in radians below or above which to filter or annotate data, defaults to  pi/4.
#' @param turnFilt_dir The direction above or below the turnFilt value for which to annotate or filter; one of "less_than" or "greater_than", defaults to former.
#' @param annotate TRUE or FALSE (default) for whether to annotate (TRUE) or filter (FALSE) the data by the turnFilt condition.
#' @param hasgaps TRUE (default) or FALSE whether to check for existing gaps having been defined already in the data,
#' and if found use those gaps, if not reverting to \code{gap_section} defaults; FALSE also takes the \code{gap_section} defaults.
#' @param verbose TRUE (default) or FALSE whether to provide within-function messaging.
#'
#' @return
#' Either an annotated dataset with an additional column of 'angle_rm' (1,0) for whether data were 'flagged', or a filtered
#' dataset with such rows having been removed.
#'
#' @seealso [MoveRakeR::speed_filt]
#'
#' @examples
#'
#' indata <- yourdata # data.frame with a minimum of columns named TagID, DateTime, longitude, latitude
#' data <- Track(indata)
#' data_turn_test <- turn_filt(data, turnFilt = pi/4, turnFilt_dir = "less_than", annotate = TRUE)
#'
#' # flagged angle condition
#' table(data_turn_test$angle_rm, useNA = "ifany") # Note, NA's flagged for start/end of a gapsection (adehabitatLT::as.ltraj needs >2) AND if any gapsection have just a single row
#'
#' @export
turn_filt <- function(data, turnFilt = pi/4, turnFilt_dir = c("less_than","greater_than"), annotate = FALSE,  hasgaps = TRUE, verbose = TRUE){

  # get attributes from previous function runs
  attr_list <- get_attributes(data)

  turnFilt_dir <- turnFilt_dir[1]
  # Prepare dataset
  if(verbose) message("Preparing the dataset for adehabitatLT::as.ltraj")
  if(verbose) message("Warning, this process for stationary animals can represent GPS noise error, use with caution!")

  # use existing gaps if found
  if(hasgaps){

    # if hasgaps = TRUE
    # if no gapsection found, call gapsection for defaults, drop = FALSE, else use existing gapsec
    if(!exists("gapsec",data)){
      if(verbose){
        message("----- Adding gap sections as none found -----\n",
                "-- Using defaults: \n",
                "-> GAP = 28800 s\n",
                "-> tol = 0.2")
      }
      data <- gap_section(data, verbose = FALSE, attr = FALSE)
    } else{
      if(verbose){message("----- Using pre-existing gapsec column found in data -----")}
    }

  } else{

    # Even if there are previous gapsections, override with new choices
    data <- gap_section(data, verbose = FALSE, attr = FALSE)

  }
  data <- data %>%
    mutate(init_row = row_number())

  dat <- data %>%
    group_by(TagID, gapsec) %>%
    mutate(burst_size = n()) %>%
    ungroup() %>%
    filter(burst_size > 1) %>%
    select(-burst_size) %>%
    mutate(
      cc = paste(TagID, gapsec, sep = "_"),
      TagID = as.character(TagID),
      cc = as.character(cc),
      longitude = as.numeric(longitude),
      latitude = as.numeric(latitude),
    )

  xy <- as.matrix(dat[, c("longitude", "latitude")])

  if(verbose) message("Running adehabitatLT::as.ltraj to calculate turning angles")
  ltraj_obj <- adehabitatLT::as.ltraj(
    xy = xy,
    date = dat$DateTime,
    id = dat$TagID,
    burst = dat$cc
  )

  # Convert to data.table and pre-allocate
  dat_dt <- as.data.table(dat)
  dat_dt[, `:=`(
    row_id = .I,         # original row numbers (although done above! Dev to streamline)
    angle_rm = NA_integer_,
    rel_angle = NA_real_,
    abs_angle = NA_real_
  )]

  # Vectorized assignment of turning angles and flag
  for(x in ltraj_obj) {
    rows <- as.integer(rownames(x))
    valid_idx <- !is.na(x$rel.angle)

    dat_dt[rows[valid_idx], `:=`(
      rel_angle = x$rel.angle[valid_idx],
      abs_angle = abs(x$rel.angle[valid_idx])
    )]

    if(turnFilt_dir == "less_than"){
      dat_dt[rows[valid_idx], angle_rm := fifelse(abs(x$rel.angle[valid_idx]) <= turnFilt, 1L, 0L)]
    } else if(turnFilt_dir == "greater_than"){
      dat_dt[rows[valid_idx], angle_rm := fifelse(abs(x$rel.angle[valid_idx]) > turnFilt, 1L, 0L)]
    }
  }

  # Output
  if(!annotate) {
    # Keep only rows flagged by turning angle filter
    ltraj_df <- dat_dt[angle_rm == 1 | is.na(angle_rm)]

  } else {
    # Annotated dataset with all rows, flags, and angles
    ltraj_df <- dat_dt

    # add on singles
    singles <- data %>%
      group_by(TagID, gapsec) %>%
      filter(n() == 1) %>%
      ungroup() %>%
      mutate(
        cc = paste(TagID, gapsec, sep = "_"),
        TagID = as.character(TagID),
        cc = as.character(cc),
        longitude = as.numeric(longitude),
        latitude = as.numeric(latitude),
        row_id = NA,
        angle_rm = NA,
        rel_angle = NA,
        abs_angle = NA
      )
    ltraj_df <- rbind(singles, ltraj_df) %>% arrange(TagID, DateTime)

  }

  # ------------------------------------------------------ #
  data_out <- Track(tibble(ltraj_df))

  # --------------------------------------------------------- #
  # assign attributes that may have been present at the start
  data_out <- give_attributes(data_out, attr_list)

  # ------------------------------------------------------ #
  # new attributes on current function run

  if(is.null(attr(data_out, "turn_filt") )){
    attr(data_out, "turn_filt") <- "turn_filt"
  }

  attr(attr(data_out, "turn_filt"), "turnFilt") <- turnFilt
  attr(attr(data_out, "turn_filt"), "turnFilt_dir") <- turnFilt_dir
  attr(attr(data_out, "turn_filt"), "annotate") <- annotate
  attr(attr(data_out, "turn_filt"), "hasgaps") <- hasgaps

  data_out <- data_out %>% group_by(TagID)
  data_out <- structure(.Data = data_out, class = c("Track","grouped_df", "tbl_df","tbl","data.frame"))

  return(data_out)

}


