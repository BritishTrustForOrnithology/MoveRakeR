#' Detect, annotate or resolve duplicate spatio-temporal records
#'
#' @description
#' \code{duplicate_track()} detects, annotates, and optionally resolves multiple classes of duplicate or conflicting
#' spatio-temporal records in animal tracking data. The function is designed for movement data
#' where fixes may conflict in time, space, or both.
#'
#' Duplicate resolution is performed in a fixed sequence of steps:
#' \enumerate{
#'   \item Parallel temporal streams (overlapping time segments within TagID)
#'   \item Same DateTime with different coordinates
#'   \item Same coordinates with different DateTime, although this is not used by default
#'   \item Exact duplicates (DateTime + latitude + longitude)
#'   \item Fully duplicated rows across all columns
#' }
#'
#' Each step may be resolved using a global \code{dup_method} or step-specific dup_method overrides supplied via \code{...}
#' (\code{dup_method_parallel}, \code{dup_method_same_time}, \code{dup_method_same_loc}, \code{dup_method_exact}).
#'
#' When \code{annotate = TRUE}, no rows are removed; instead, issue flags are added to the data indicating which
#' records participate in each conflict type.
#'
#' @param data Input data object, with required columns: TagID, DateTime, longitude, latitude.
#' @param dup_method Global conflict–resolution dup_method used for all steps unless overridden. One of
#' \code{"best_quality"}, \code{"first"}, \code{"last"}, \code{"random"}, or step-specific dup_methods
#' where applicable. Defaults to "random" if nothing provided.
#' @param quality_vars Character vector of column names used to score fix quality when \code{dup_method = "best_quality"}.
#' @param quality_directions Numeric vector of the same length as \code{quality_vars}, indicating
#' whether higher (\code{+1}) or lower (\code{-1}) values are better.
#' @param centroid_fun Function used to compute centroid coordinates when resolving same-time/different-location
#' conflicts (default: \code{median}).
#' @param quality_fun Function used to aggregate row-level quality scores at the segment or group level (default: \code{mean}).
#' @param time_fun Function used to aggregate \code{DateTime} values when resolving same-location/different-time conflicts with
#'   \code{dup_method_same_loc = "avg_time"} (default: \code{mean}).
#' @param use_dup_coords Logical defaulting to FALSE as to whether to use the search for duplicate coordinates across
#' different DateTimes This may be contentious given animals sat for long periods may have a high likelihood of
#' hitting upon the same GPS coordinates even with GPS error, for different datetimes. FALSE bypasses this check entirely.
#' @param annotate Logical. Legacy argument retained for backward compatibility. If \code{TRUE} and \code{mode}
#' is not supplied, equivalent to \code{mode = "annotate"}.
#' @param mode Character string specifying the operation mode:
#'   \itemize{
#'     \item \code{"resolve"}: resolve duplication issues (default);
#'     \item \code{"annotate"}: annotate duplication issues without resolving;
#'     \item \code{"detect"}: detect and summarise duplication issues only.
#'   }
#' @param step Only relevant for the function \code{is_duplicated_track()} defaulting to NULL to search for all duplication issues,
#' but "step" can accept one of c("parallel", "same_time", "same_loc", "exact", "all") to perform only specific
#' checks for these duplication steps.
#' @param verbose Logical. If \code{TRUE}, progress messages are printed for each resolution step.
#' @param ... Optional step-specific dup_method overrides (see details for dup_methods available for some duplication step):
#'   \describe{
#'     \item{\code{dup_method_parallel}}{Resolution dup_method for parallel temporal streams}
#'     \item{\code{dup_method_same_time}}{Resolution dup_method for same-time/different-location fixes}
#'     \item{\code{dup_method_same_loc}}{Resolution dup_method for same-location/different-time fixes}
#'   }
#'
#' @details
#'
#' The function can also operate in three modes:
#' \itemize{
#'   \item \code{"resolve"}: detect and resolve duplication issues (default);
#'   \item \code{"annotate"}: annotate rows involved in duplication issues without removing or modifying them;
#'   \item \code{"detect"}: detect and summarise duplication issues without modifying or annotating the data.
#' }
#'
#' Duplication issues are handled sequentially in the following order:
#'
#' \enumerate{
#'   \item \strong{Parallel time streams}: overlapping temporal segments within a \code{TagID}, typically caused
#'   by temporal jumps or multiple concurrent streams. Only the selected stream is retained when resolving.
#'
#'   \item \strong{Same DateTime, different coordinates}: multiple fixes recorded at the same timestamp but with
#'   differing spatial locations.#'
#'   \item \strong{Same coordinates, different DateTimes}: identical spatial locations recorded at multiple timestamps but that are by default not used.
#'   \item \strong{Exact duplicates}: duplicated rows sharing \code{DateTime}, \code{latitude}, and \code{longitude}.
#'   \item \strong{Fully duplicated rows}: rows duplicated across all columns.
#' }
#'
#' This sequential action of events ensures that earlier steps do not interfere with later, stricter duplicate definitions.
#' Also, note that exact duplicate \code{DateTime} values do not trigger parallel segmentation.
#'
#' As these issues are dealt with hierarchically, the first check is for parallel time segments, within which
#' technically such segments could include further duplication issues of DateTimes or locations. The first step therefore
#' is a \emph{segment-level} check, and all subsequent duplication checks operate at the \emph{fix} level.
#'
#' Each step can be resolved using different strategies, controlled either by the global \code{dup_method} argument
#' or by step-specific overrides (e.g., \code{dup_method_parallel}, \code{dup_method_same_time}). Three "step-specific"
#' dup_methods area available via the \code{...} argument. These are as follows:
#'
#' \enumerate{
#'  \item \strong{dup_method_parallel = "merge"}: assumes that both time streams are valid and combines them both into one DateTime sequence per TagID;
#'  \item \strong{dup_method_same_time = "centroid"}: takes the \code{centroid_fun} to apply (e.g. mean, median) of the lat-longs where there are duplicate locations for the same DateTime;
#'  \item \strong{dup_method_same_loc = "avg_time"}: applies the \code{time_fun} function, e.g. mean, median, where there are duplicate DateTimes for the same location.
#' }
#'
#' Parallel temporal streams are identified by detecting negative time differences within \code{TagID} sequences.
#' Overlapping temporal segments are resolved by selecting a single segment per overlap block
#' unless \code{dup_method_parallel = "merge"}.
#'
#' Annotation flags added when \code{annotate = TRUE} include:
#' \itemize{
#'   \item \code{parallel_issue}
#'   \item \code{dup_time_issue}
#'   \item \code{dup_coords_issue}
#'   \item \code{dup_time_coords_issue}
#'   \item \code{all_dup_issue}
#' }
#'
#' A further function \code{is_duplicated_track()} can be used to return a Boolean TRUE/FALSE for whether there are any
#' duplication issues, which calls \code{duplicate_track()} using the detect mode. \code{is_duplicated_track()} can also accept
#' a \code{step} argument to search for specific duplication issues as considered by \code{duplicate_track()}.
#'
#' As a word of caution, the \code{duplicated_track()} function identifies duplicates within animals. Should you
#' wish to identify duplicates across the dataset, this would require separate coding, or tricking the functon to
#' thinking you have only one animal.
#'
#' @return
#' A tibble or data frame with duplicate conflicts resolved or annotated according to the specified options.
#'
#' Depending on \code{mode}:
#' \itemize{
#'   \item \code{"resolve"}: a cleaned data frame with duplication issues resolved;
#'   \item \code{"annotate"}: the original data frame with additional logical or;
#'     integer flag columns indicating duplication issues
#'   \item \code{"detect"}: a named list summarising detected duplication issues.
#' }
#'
#' For \code{is_duplicated_track()}, the output is a Boolean with further attr(,"details) for Boolean results from
#' duplication steps wanted.
#'
#' @examples
#' library(dplyr)
#' library(purrr)
#'
#' set.seed(1)
#'
#' # generate some fake data
#'
#' # Base time
#' sttime <- as.POSIXct("2001-01-01 12:00:00", tz = "UTC")
#'
#' ################################################################################
#' # 1. Parallel time-streams
#' ################################################################################
#' # Parallel time streams exist giving alterative truths of a track trajectory
#'
#' t1 <- sttime + seq(0, 420, 60)   # track A
#' t2 <- sttime + seq(30, 450, 60)  # track B (offset by 30s but overlapping same time-frame)
#'
#' # Animal 1
#' dat1 <- data.frame(
#'   TagID = "505",
#'   DateTime = c(t1, t2),
#'   latitude  = c(runif(8, 54.2, 55.0), runif(8, 54.2, 55.0)),
#'   longitude = c(runif(8, 0.2, 1.2),  runif(8, 0.2, 1.2)),
#'   n_sats    = sample(4:12, 16, replace = TRUE),
#'   accuracy  = runif(16, 5, 40),
#'   pdop      = runif(16, 1, 3),
#'   hdop      = runif(16, 0.5, 2) )
#'
#' # Animal 2
#' dat2 <- data.frame(
#'   TagID = "212",
#'   DateTime = c(t1, t2),
#'   latitude  = c(runif(8, 55.0, 55.7), runif(8, 55.0, 55.7)),
#'   longitude = c(runif(8, 1.0, 2.0),  runif(8, 1.0, 2.0)),
#'   n_sats    = sample(4:12, 16, replace = TRUE),
#'   accuracy  = runif(16, 5, 40),
#'   pdop      = runif(16, 1, 3),
#'   hdop      = runif(16, 0.5, 2) )
#'
#' #more 'ok' data for animal 2
#' t3 <- sttime + seq(420+60, 1000, 60)   # track A
#' dat3 <- data.frame(
#'   TagID = "212",
#'   DateTime = t3,
#'   latitude  = runif(9, 55.0, 55.7),
#'   longitude = runif(9, 1.0, 2.0),
#'   n_sats    = sample(4:12, 9, replace = TRUE),
#'   accuracy  = runif(9, 5, 40),
#'   pdop      = runif(9, 1, 3),
#'   hdop      = runif(9, 0.5, 2) )
#'
#' # Combine
#' data <- rbind(dat1, dat2, dat3)
#'
#' # --------------------------------------- #
#' # detection mode VS annotation or resolve
#' # --------------------------------------- #
#'
#' # Should you want to run a "first-pass" over the data, then this is possible with mode = "detect".
#' # This can be called as a simple separate function: is_duplicated_track(), which calls
#' # duplicate_track(data, mode = "detect") but returns only a Boolean TRUE/FALSE:
#'
#' is_duplicated_track(data)
#'
#' #[1] TRUE
#' #attr(,"details")
#' #parallel same_time  same_loc     exact       all
#' #    TRUE     FALSE     FALSE     FALSE     FALSE
#'
#' #or for the specific parallel issue?
#' is_duplicated_track(data, step = "parallel")
#'
#' #[1] TRUE
#' #attr(,"details")
#' #parallel
#' #    TRUE
#'
#' # For more detail, call duplicate_track() directly with the mode = "detect" which will return exactly which
#' # parts of the data are problematic:
#'
#' duplicate_track(data, mode = "detect")
#'
#' # This returns:
#'
#' # A tibble: 2 × 6
#' # TagID step     issue_detected n_segments affected_data      n_affected_rows
#' # <chr> <chr>    <lgl>               <dbl> <list>                       <int>
#' # 1 212 parallel TRUE                    2 <tibble [14 × 15]>              14
#' # 2 505 parallel TRUE                    2 <tibble [14 × 15]>              14
#'
#' # which includes a summary of xx$affected_data for the rows with segments added indicating parallel streams
#' xx = duplicate_track(data, mode = "detect")
#' xx$affected_data # here "brksec" is the segment section with competing timestreams, 1 and 2 (ordered here by TagID, DateTime)
#'
#' # --------------------------------------- #
#' # Further annotation or resolve in the data:
#' # --------------------------------------- #
#' # you may then seek how to handle the conflicts, for example for parallel time stream "segments":
#'
#' # example randomly picking a segment
#' test1 = duplicate_track(data, dup_method = "random", annotate = FALSE) # legacy code using annotate TRUE/FALSE
#'
#' # alternatively using mode argument (same result)
#' test1 = duplicate_track(data, dup_method = "random", mode = "resolve")
#'
#' # annotating the data rather than filtering it, this time picking the first segment
#' test1_1 = duplicate_track(data, dup_method = "first", mode = "annotate") # equivalent to "annotate = TRUE"
#'
#' data.frame(test1_1) # noting parallel_issue column flag
#'
#' # using a "step-specific" merge only possible for the parallel duplication issue
#' # here if dup_method is specified (i.e. the global dup_method throughout the function), it is over-ridden
#' # for the parallel duplication step via dup_method_parallel:#'
#' test1_2 = duplicate_track(data, dup_method = "random", dup_method_parallel = "merge", mode = "resolve")
#'
#' # note in the above, the other duplication issues would still be handled via the global dup_method
#'
#' # best quality example
#' test1_3 = duplicate_track(data, dup_method = "best_quality",
#'              quality_vars = c("n_sats", "accuracy", "pdop", "hdop"), # variables in data to be used for quality assessment
#'              quality_directions = c(1,1,-1,-1), # directional gradient of increasing quality, 1 = better increasing, -1 = better decreasing
#'              annotate = FALSE)
#'
#' # following consideration of parallel time stream segments, the function looks to handle potential
#' # row-specific issues for duplicate DateTime and/or locations
#'
#' ################################################################################
#' # 2. Same-time/different-location
#' ################################################################################
#' # Duplicate locations are present for DateTimes
#' # Needing a resolve of how to reduce to one xy location per DateTime
#'
#' set.seed(123)
#'
#' # Base timestamps
#' sttime <- as.POSIXct("01/01/2001 12:00:30", format = "%d/%m/%Y %H:%M:%S", tz = "UTC")
#' DateTime <- sttime + seq(0, 420, 60)  # 8 fixes, 1 min apart
#'
#' # Animal 1
#' lat1 <- runif(8, 54.1, 55.5)
#' lon1 <- runif(8, 0, 2)
#' DateTime1 <- DateTime
#' DateTime1[2] <- DateTime1[3]          # duplicate timestamp
#' lat1[2] <- lat1[3] + 0.3              # slight duplicate location
#' lon1[2] <- lon1[3] + 0.3
#' n_sats1   <- sample(3:12, 8, replace = TRUE)
#' accuracy1 <- runif(8, 5, 50)
#' pdop1     <- runif(8, 1, 3)
#' hdop1     <- runif(8, 0.5, 2)
#'
#' # Animal 2
#' lat2 <- runif(8, 54.5, 55.8)
#' lon2 <- runif(8, 0.5, 2.5)
#' DateTime2 <- DateTime
#' DateTime2[5] <- DateTime2[4]          # duplicate timestamp
#' lat2[5] <- lat2[4] + 0.2              # slight duplicate location
#' lon2[5] <- lon2[4] + 0.2
#' n_sats2   <- sample(3:12, 8, replace = TRUE)
#' accuracy2 <- runif(8, 5, 50)
#' pdop2     <- runif(8, 1, 3)
#' hdop2     <- runif(8, 0.5, 2)
#'
#' # Combine into one dataframe
#' data <- data.frame(
#'   TagID = rep(c("505", "212"), each = 8),
#'   DateTime = c(DateTime1, DateTime2),
#'   latitude = c(lat1, lat2),
#'   longitude = c(lon1, lon2),
#'   n_sats = c(n_sats1, n_sats2),
#'   accuracy = c(accuracy1, accuracy2),
#'   pdop = c(pdop1, pdop2),
#'   hdop = c(hdop1, hdop2)
#' )
#'
#' # --------------------------------------- #
#' # detection mode VS annotation or resolve
#' # --------------------------------------- #
#'
#' # Should you want to run a "first-pass" over the data, then this is possible with mode = "detect"
#' duplicate_track(data, mode = "detect")
#'
#' # This returns:
#'
#' duplicate_track(data, mode = "detect")
#' # A tibble: 2 × 6
#' TagID step      issue_detected n_segments affected_data    n_affected_rows
#' <chr> <chr>     <lgl>               <dbl> <list>                     <int>
#' 1 212 same_time TRUE                   NA <tibble [2 × 10]>             2
#' 2 505 same_time TRUE                   NA <tibble [2 × 10]>             2
#'
#' # --------------------------------------- #
#' # Further annotation or resolve in the data:
#' # --------------------------------------- #
#'
#' # dup_method = random would randomly pick a location for the given time stamp to be used:
#' test2_1 = duplicate_track(data, dup_method = "random", mode = "resolve")
#'
#' # using a step-specific dup_method of "centroid", using centroid_fun
#'
#' # NOT RUN (would not work):
#' # test2_0 = duplicate_track(data, dup_method = "centroid", mode = "resolve") # cannot work as a global dup_method for other dup steps
#'
#' # instead use:
#' test2_2 = duplicate_track(data, dup_method = "random", dup_method_same_time = "centroid", centroid_fun = median, mode = "resolve")
#'
#' # or best quality as above:
#' test2_3 = duplicate_track(data, dup_method = "best_quality",
#'               quality_vars = c("n_sats", "accuracy", "pdop", "hdop"),
#'               quality_directions = c(1,1,-1,-1),
#'               mode = "resolve"
#' )
#'
#' ################################################################################
#' # 3. Same-location/different-time
#' ################################################################################
#' # This is not carried out by default as it is entirely possible to get valid GPS fixes
#' # at different times. But if you are sure the precision of the GPS and the animal under consideration would
#' # never have this case, then by all means use it.
#' #
#' # Duplicate DateTimes present for locations
#' # Needing a resolve of how to reduce to one DateTime per location
#'
#' set.seed(1234)
#'
#' # Base timestamps
#' sttime <- as.POSIXct("01/01/2001 12:00:30", format = "%d/%m/%Y %H:%M:%S", tz = "UTC")
#' DateTime <- sttime + seq(0, 420, 60)  # 8 fixes, 1 min apart
#'
#' # Animal 1
#' lat1 <- runif(8, 54.1, 55.5)
#' lon1 <- runif(8, 0, 2)
#' DateTime1 <- DateTime
#' lat1[2] <- lat1[3]
#' lon1[2] <- lon1[3]
#'
#' n_sats1   <- sample(3:12, 8, replace = TRUE)
#' accuracy1 <- runif(8, 5, 50)
#' pdop1     <- runif(8, 1, 3)
#' hdop1     <- runif(8, 0.5, 2)
#'
#' # Animal 2
#' lat2 <- runif(8, 54.5, 55.8)
#' lon2 <- runif(8, 0.5, 2.5)
#' DateTime2 <- DateTime
#' lat2[5] <- lat2[4]
#' lon2[5] <- lon2[4]
#'
#' n_sats2   <- sample(3:12, 8, replace = TRUE)
#' accuracy2 <- runif(8, 5, 50)
#' pdop2     <- runif(8, 1, 3)
#' hdop2     <- runif(8, 0.5, 2)
#'
#' # Combine into one dataframe
#' data <- data.frame(
#'   TagID = rep(c("505", "212"), each = 8),
#'   DateTime = c(DateTime1, DateTime2),
#'   latitude = c(lat1, lat2),
#'   longitude = c(lon1, lon2),
#'   n_sats = c(n_sats1, n_sats2),
#'   accuracy = c(accuracy1, accuracy2),
#'   pdop = c(pdop1, pdop2),
#'   hdop = c(hdop1, hdop2)
#' )
#'
#' # --------------------------------------- #
#' # detection mode VS annotation or resolve
#' # --------------------------------------- #
#'
#' # Should you want to run a "first-pass" over the data, then this is possible with mode = "detect"
#' duplicate_track(data, mode = "detect", use_dup_coord = TRUE)
#'
#' # This returns:
#' # A tibble: 2 × 6
#' step     TagID issue_detected n_segments affected_data     n_affected_rows
#' <chr>    <chr> <lgl>               <int> <list>                      <int>
#' 1 same_loc 212 TRUE                   NA <tibble [2 × 10]>               2
#' 2 same_loc 505 TRUE                   NA <tibble [2 × 10]>               2
#'
#' # --------------------------------------- #
#' # Further annotation or resolve in the data:
#' # --------------------------------------- #
#'
#' # dup_method = random would randomly pick a DateTime for the given location to be used:
#' test3_1 = duplicate_track(data, dup_method = "random", mode = "resolve", use_dup_coord = TRUE)
#'
#' # using a step-specific dup_method of "avg_time", using time_fun
#' # note this is termed "avg_time" but you can apply any function you want really
#'
#' # NOT RUN (would not work):
#' # test3_0 = duplicate_track(data, dup_method = "avg_time", mode = "resolve", use_dup_coord = TRUE) # as above for step 2, cannot work as a global dup_method for other dup steps
#'
#' # instead use:
#' test3_2 = duplicate_track(data, dup_method = "random", dup_method_same_loc = "avg_time", time_fun = mean, mode = "resolve", use_dup_coord = TRUE)
#'
#' # or best quality as above, same code:
#' test3_3 = duplicate_track(data, dup_method = "best_quality",
#'               quality_vars = c("n_sats", "accuracy", "pdop", "hdop"),
#'               quality_directions = c(1,1,-1,-1),
#'               mode = "resolve", use_dup_coord = TRUE
#' )
#'
#' ################################################################################
#' # 4. Exact duplicates
#' ################################################################################
#' # Exact duplicates DateTime + latitude + longitude
#' # Needing a resolve of taking one location and DateTime
#'
#' set.seed(456)
#'
#' # Base timestamps
#' sttime <- as.POSIXct("01/01/2001 12:00:30", format = "%d/%m/%Y %H:%M:%S", tz = "UTC")
#' DateTime <- sttime + seq(0, 420, 60)  # 8 fixes, 1 min apart
#'
#' # Animal 1: create duplicates at same time & location
#' lat1 <- runif(8, 54.1, 55.5)
#' lon1 <- runif(8, 0, 2)
#' DateTime1 <- DateTime
#' lat1[2] <- lat1[3]        # duplicate location
#' lon1[2] <- lon1[3]        # duplicate location
#' DateTime1[2] <- DateTime1[3]  # same timestamp
#'
#' # Additional quality columns
#' n_sats1   <- sample(3:12, 8, replace = TRUE)
#' accuracy1 <- runif(8, 5, 50)
#' pdop1     <- runif(8, 1, 3)
#' hdop1     <- runif(8, 0.5, 2)
#'
#' #Animal 2
#' lat2 <- runif(8, 54.5, 55.8)
#' lon2 <- runif(8, 0.5, 2.5)
#' DateTime2 <- DateTime
#' lat2[5] <- lat2[4]       # duplicate location
#' lon2[5] <- lon2[4]
#' DateTime2[5] <- DateTime2[4]
#'
#' n_sats2   <- sample(3:12, 8, replace = TRUE)
#' accuracy2 <- runif(8, 5, 50)
#' pdop2     <- runif(8, 1, 3)
#' hdop2     <- runif(8, 0.5, 2)
#'
#' # Combine into one dataframe
#' data <- data.frame(
#'   TagID = rep(c("505", "212"), each = 8),
#'    DateTime = c(DateTime1, DateTime2),
#'   latitude = c(lat1, lat2),
#'   longitude = c(lon1, lon2),
#'   n_sats = c(n_sats1, n_sats2),
#'   accuracy = c(accuracy1, accuracy2),
#'   pdop = c(pdop1, pdop2),
#'   hdop = c(hdop1, hdop2)
#' )
#'
#' # --------------------------------------- #
#' # detection mode VS annotation or resolve
#' # --------------------------------------- #
#'
#' # Should you want to run a "first-pass" over the data, then this is possible with mode = "detect"
#' duplicate_track(data, mode = "detect")
#'
#' # This returns:
#'
#' duplicate_track(data, mode = "detect")
#' # A tibble: 2 × 6
#' step     TagID issue_detected n_segments affected_data     n_affected_rows
#' <chr>    <chr> <lgl>               <int> <list>                      <int>
#' 1 exact  212   TRUE                   NA <tibble [2 × 10]>               2
#' 2 exact  505   TRUE                   NA <tibble [2 × 10]>               2
#'
#' # --------------------------------------- #
#' # Further annotation or resolve in the data:
#' # --------------------------------------- #
#'
#' # dup_method = random would randomly pick a DateTime for the given location to be used:
#' test4_1 = duplicate_track(data, dup_method = "random", mode = "resolve")
#'
#' # there are no step-specific dup_methods for this duplication issue
#'
#' # best quality as above, annotating instead of filtering:
#' test4_3 = duplicate_track(data, dup_method = "best_quality",
#'               quality_vars = c("n_sats", "accuracy", "pdop", "hdop"),
#'               quality_directions = c(1,1,-1,-1),
#'               mode = "annotate"
#' )
#'
#' @export
duplicate_track <- function(data,
                  dup_method = c("random", "best_quality", "first", "last", "mean"),
                  quality_vars = NULL,
                  quality_directions = NULL,
                  centroid_fun = median,
                  quality_fun = mean,
                  time_fun = mean,
                  use_dup_coords = FALSE,
                  mode = c("resolve", "annotate", "detect"),
                  annotate = FALSE, verbose = TRUE, ...){

  # -------------------------------------------- #
  # Backward-compatible mode handling vs annotate
  # -------------------------------------------- #

  mode_provided <- !missing(mode)
  annotate_provided <- !missing(annotate)

  #mode <- match.arg(mode)

  if(mode_provided){
    mode <- match.arg(mode)

    if(annotate_provided){
      warning(
        "`mode` and `annotate` were both supplied; `mode` takes precedence",
        call. = FALSE
      )
    }

  } else{
    # Legacy behaviour
    mode <- if(isTRUE(annotate)) "annotate" else "resolve"
  }

  # ---------------------------------------------- #
  #if(!annotate){
  if(mode == "resolve"){
    dup_method <- match.arg(dup_method)
    dots   <- list(...)

    allowed_dup_methods <- list(
      parallel  = c("random", "best_quality", "first", "last", "merge"),
      same_time = c("random", "best_quality", "first", "last", "centroid"),
      same_loc  = c("random", "best_quality", "first", "last", "avg_time"),
      exact = c("random", "best_quality", "first", "last")
    )

    resolve_dup_method <- function(step, default = "random") {

      step_arg <- paste0("dup_method_", step)

      # Step-specific override or global
      #m <- if (step_arg %in% names(dots)) dots[[step_arg]] else dup_method
      m <- if (step_arg %in% names(dots) && !is.null(dots[[step_arg]])) {
        dots[[step_arg]]
      } else {
        dup_method
      }

      m <- match.arg(m, unique(c(allowed_dup_methods[[step]], default)))

      if (!m %in% allowed_dup_methods[[step]]) {

        warning(
          sprintf(
            "dup_method '%s' not applicable to %s step; using '%s'",
            m, step, default
          ),
          call. = FALSE
        )

        m <- default
      }

      m
    }

    dup_method_parallel  <- resolve_dup_method("parallel",  default = "random")
    dup_method_same_time <- resolve_dup_method("same_time", default = "random")
    dup_method_same_loc  <- resolve_dup_method("same_loc",  default = "random")
    dup_method_exact     <- resolve_dup_method("exact",     default = "random")

    if(dup_method == "best_quality"){
      if(is.null(quality_vars) || length(quality_vars) == 0){
        stop("For 'best_quality', you must provide quality_vars (column names).")
      }
      if(is.null(quality_directions)){
        stop("For 'best_quality', you must provide quality_directions (+1/-1) for each quality_var.")
      }
      if(length(quality_vars) != length(quality_directions)){
        stop("quality_vars and quality_directions must be the same length.")
      }

      # Ensure quality_fun is a function
      if(!is.function(quality_fun)) stop("quality_fun must be a function")

    }

  }

  if(mode != "detect"){
    if(!is.function(time_fun)){
      stop("time_fun must be a function (e.g. mean, median)")
    }
    if(!is.function(centroid_fun)){
      stop("centroid_fun must be a function (e.g. mean, median)")
    }
  }

  # -------------------------------------------------------------- #
  # Ensure data is sorted by TagID + DateTime
  data <- data %>% dplyr::arrange(TagID)

  # overall detection yes/nos for further steps

  # parallel time streams
  has_parallel <- data %>%
    group_by(TagID) %>%
    mutate(dt = as.numeric(difftime(lead(DateTime), DateTime, units = "secs"))) %>%
    summarise(has_jump = any(dt < 0, na.rm = TRUE)) %>%
    pull(has_jump)
  has_parallel <- any(has_parallel)

  # Same (duplicate) Datetime, different coordinates
  #has_dup_times <- data %>%
  #  group_by(TagID, DateTime) %>%
  #  summarise(n_coords = n_distinct(latitude, longitude), .groups = "drop") %>%
  #  summarise(any_dup = any(n_coords > 1)) %>%
  #  pull(any_dup)

  dt <- as.data.table(data)
  has_dup_times <- dt[, any(duplicated(.SD)), .SDcols = c("TagID","DateTime","latitude","longitude")]

  if(use_dup_coords){

    # same (duplicate) coords, different times
    #has_dup_coords <- data %>%
    #  group_by(TagID, latitude, longitude) %>%
    #  summarise(n_times = n_distinct(DateTime), .groups = "drop") %>%
    #  summarise(any_dup = any(n_times > 1)) %>%
    #  pull(any_dup)

    keys <- paste(data$TagID, data$latitude, data$longitude, data$DateTime, sep = "_")
    coords_keys <- paste(data$TagID, data$latitude, data$longitude, sep = "_")
    dup_counts <- table(coords_keys)
    has_dup_coords <- any(dup_counts > 1)

  } else{
    has_dup_coords <- FALSE
  }

  # duplicate location and times
  #has_exact_dup <- data %>%
  #  group_by(TagID, DateTime, latitude, longitude) %>%
  #  summarise(n_rows = n(), .groups = "drop") %>%
  #  summarise(any_dup = any(n_rows > 1)) %>%
  #  pull(any_dup)

  has_exact_dup <- any(duplicated(data[c("TagID", "DateTime", "latitude", "longitude")]))

  any_dup <- any(duplicated(data))

  # ------------------------------------------------------------------------------------- #
  # ------------------------------------------------------------------------------------- #
  # If mode = "detect"
  # ------------------------------------------------------------------------------------- #
  # ------------------------------------------------------------------------------------- #

  # helper function for the parallel data stream (as it needs to be called in the reporting
  # detect = TRUE route, put also used in the wider annotation / resolving pipeline)
  get_parallel_overlaps <- function(data){

    data_dp <- data %>%
      group_by(TagID) %>%
      mutate(dt = as.numeric(difftime(lead(DateTime), DateTime, units = "secs")),
             brk = if_else(dt < 0, 1L, 0L),
             rn_ = row_number(),
             brk = if_else(rn_ == max(rn_), 1L, brk)) %>%
      ungroup() %>%
      mutate(brk0 = brk,
             brk = lag(brk)) %>%
      group_by(TagID) %>%
      mutate(
        brk_num = case_when(brk == 1 ~ row_number()),
        brk_num = if_else(min(row_number()) == row_number(), row_number(), brk_num)
      ) %>%
      tidyr::fill(brk_num) %>%
      mutate(
        brksec = consecutive_id(brk_num),
        brk = brk0
      ) %>%
      select(-c(brk_num, rn_, brk0)) %>%
      group_by(TagID)

    # Create segments
    segments <- data_dp %>%
      group_by(TagID, brksec) %>%
      summarise(start = min(DateTime), end = max(DateTime), .groups = "drop")

    dt_segments <- data.table::as.data.table(segments)
    dt_data     <- data.table::as.data.table(data_dp)
    dt_data[, DateTime2 := DateTime]

    dt_segs1 <- data.table::copy(dt_segments)
    dt_segs2 <- data.table::copy(dt_segments)
    setnames(dt_segs1, c("start", "end", "brksec"), c("start", "end", "brksec_i"))
    setnames(dt_segs2, c("start", "end", "brksec"), c("start", "end", "brksec_j"))

    setkey(dt_segs1, TagID, start, end)
    setkey(dt_segs2, TagID, start, end)

    overlap_segments <- foverlaps(dt_segs1, dt_segs2, type = "any", nomatch = 0L)[brksec_i != brksec_j]
    overlap_segments[, `:=`(
      overlap_start = pmax(start, i.start),
      overlap_end   = pmin(end, i.end)
    )]

    overlaps_dt <- unique(overlap_segments[, .(TagID, overlap_start, overlap_end)])

    setkey(dt_data, TagID, DateTime, DateTime2)
    setkey(overlaps_dt, TagID, overlap_start, overlap_end)

    dt_data[, parallel_issue := 0L]

    # # # # # # #
    # Safer parallel flagging
    dt_data[, tmp_id := .I]  # temporary row ID
    matches <- foverlaps(
      dt_data, overlaps_dt,
      by.x = c("TagID", "DateTime", "DateTime2"),
      by.y = c("TagID", "overlap_start", "overlap_end"),
      type = "within", nomatch = 0L
    )
    dt_data[tmp_id %in% matches$tmp_id, parallel_issue := 1L]
    dt_data[, tmp_id := NULL]
    dt_data[, DateTime2 := NULL]

    #matches <- foverlaps(dt_data, overlaps_dt, by.x = c("TagID", "DateTime", "DateTime2"),
    #                     by.y = c("TagID", "overlap_start", "overlap_end"),
    #                     type = "within", nomatch = NA)
    #dt_data[matches[!is.na(overlap_start), which = TRUE], parallel_issue := 1L]
    #dt_data[, DateTime2 := NULL]

    # # # # # # #

    data_dp <- as_tibble(dt_data)

    data_dp <- data_dp %>%
      arrange(TagID, DateTime) %>%
      group_by(TagID) %>%
      mutate(
        overlap_block = cumsum(
          parallel_issue == 1 &
            (lag(parallel_issue, default = 0) == 0)
        )
      ) %>%
      ungroup()

    return(data_dp)
  }

  if(mode == "detect") {

    # Initialize a list to store summaries for each step
    detected <- list()

    # ------------------------------------------------------ #
    # Step 1: Parallel time streams (temporal jumps)
    # ------------------------------------------------------ #

    if(any(has_parallel)){

      # Call helper function to flag exact rows
      data_flagged <- get_parallel_overlaps(data)

      detected$parallel <- data_flagged %>%
        filter(parallel_issue == 1) %>%
        group_by(TagID) %>%
        summarise(
          step = "parallel",
          issue_detected = TRUE,
          n_segments = length(unique(brksec)),
          affected_data = list(pick(everything())),
          n_affected_rows = n(),
          .groups = "drop"
        )

    } else {
      detected$parallel <- tibble(TagID=character(0),
                                  step=character(0),
                                  issue_detected=logical(0),
                                  n_segments=integer(0),
                                  affected_data=list(),
                                  n_affected_rows=integer(0))
      }


    # ------------------------------------------------------ #
    # Step 2: Same (dup) DateTime, different coordinates
    # ------------------------------------------------------ #
    detected$same_time <- data %>%
      group_by(TagID, DateTime) %>%
      filter(
        n() > 1 &
          n_distinct(latitude, longitude) > 1
      ) %>%
      ungroup() %>%
      group_by(TagID) %>%
      summarise(
        step = "same_time",
        issue_detected = TRUE,
        n_segments = NA_integer_,
        affected_data = list(cur_data()),
        n_affected_rows = n(),
        .groups = "drop"
      )

    # ------------------------------------------------------ #
    # Step 3: Same (dup) coordinates, different DateTimes
    # ------------------------------------------------------ #

    if(use_dup_coords){
      detected$same_loc <- data %>%
        group_by(TagID, latitude, longitude) %>%
        filter(
          n() > 1 &
            n_distinct(DateTime) > 1
        ) %>%
        ungroup() %>%
        group_by(TagID) %>%
        summarise(
          step = "same_loc",
          issue_detected = TRUE,
          n_segments = NA_integer_,
          affected_data = list(cur_data()),
          n_affected_rows = n(),
          .groups = "drop"
        )


    } else {
      detected$same_loc <- tibble(TagID=character(0),
                                  step=character(0),
                                  issue_detected=logical(0),
                                  n_segments=integer(0),
                                  affected_data=list(),
                                  n_affected_rows=integer(0))
    }



    # ------------------------------------------------------ #
    # Step 4: Exact duplicates (DateTime + coordinates)
    # ------------------------------------------------------ #
    detected$exact <- data %>%
      group_by(TagID, DateTime, latitude, longitude) %>%
      filter(n() > 1) %>%
      ungroup() %>%
      group_by(TagID) %>%
      summarise(
        step = "exact",
        issue_detected = TRUE,
        n_segments = NA_integer_,
        affected_data = list(cur_data()),
        n_affected_rows = n(),
        .groups = "drop"
      )

    # ------------------------------------------------------ #
    # Step 5: All duplicate rows (across all columns)
    # ------------------------------------------------------ #

    detected$all <- data %>%
      ungroup() %>%
      filter(duplicated(.) | duplicated(., fromLast = TRUE)) %>%
      group_by(TagID) %>%
      summarise(
        step = "all",
        issue_detected = TRUE,
        n_segments = NA_integer_,
        affected_data = list(cur_data()),
        n_affected_rows = n(),
        .groups = "drop"
      )

    # ------------------------------------------------------ #
    # Combine all steps into one tibble
    # ------------------------------------------------------ #
    detected_summary <- bind_rows(detected)

    return(detected_summary)
  }

  # ------------------------------------------------------------------------------------- #
  # ------------------------------------------------------------------------------------- #
  # Otherwise if mode = "annotate" or "resolve"
  # ------------------------------------------------------------------------------------- #
  # ------------------------------------------------------------------------------------- #

  # ------------------------------------------------------ #
  # Step 1: Detect parallel streams / temporal jumps
  # ------------------------------------------------------ #

  # unclean, but if no issues for parallel time phases, add this in first, otherwise skipped:
  if(mode == "annotate"){
    data$parallel_issue <- 0
  }

  if(any(has_parallel)){

    if(verbose){
      if(mode == "resolve"){
        message("Parallel time segments detected: applying segment resolution...")
        message("Using dup_method: ", dup_method_parallel)
      } else{
        message("Parallel time segments detected")
      }

    }

    ### annotation part (always done as needed for the filtering part)

    data_dp <- get_parallel_overlaps(data)

    #data.frame(data_dp)
    ###################################################
    # Filtering part
    ###################################################

    if(mode == "resolve"){

      # Resolution for parallel streams
      if(dup_method_parallel == "first"){

        winners <- data_dp %>%
          filter(parallel_issue == 1) %>%
          group_by(TagID, overlap_block) %>%
          summarise(win_brksec = min(brksec), .groups = "drop")


      } else if(dup_method_parallel == "last"){

        winners <- data_dp %>%
          filter(parallel_issue == 1) %>%
          group_by(TagID, overlap_block) %>%
          summarise(win_brksec = max(brksec), .groups = "drop")


        #data_dp <- data_dp %>% group_by(TagID) %>%
        #  filter(brksec == max(brksec)) %>%
        #  ungroup()

      } else if(dup_method_parallel == "random"){

        winners <- data_dp %>%
          filter(parallel_issue == 1) %>%
          distinct(TagID, overlap_block, brksec) %>%
          group_by(TagID, overlap_block) %>%
          summarise(
            win_brksec = brksec[sample.int(n(), 1)],
            .groups = "drop"
          )

        #data_dp <- data_dp %>%
        #  group_by(TagID) %>%
        #  summarise(brksec_list = list(unique(brksec)), .groups = "drop") %>%
        #  mutate(selected_brk = purrr::map_int(brksec_list, ~ sample(.x, 1))) %>% # pick one random segment per TagID
        #  select(TagID, selected_brk) %>%
        #  inner_join(data_dp, by = c("TagID", "selected_brk" = "brksec")) %>% # join back to keep all rows in that segment
        #  select(-selected_brk)

      } else if(dup_method_parallel == "best_quality"){

        #data_dp <- data_dp %>%
        #  group_by(TagID, brksec) %>%
        #  # row-level score for each fix in the segment, using quality_vars and quality_directions
        #  mutate(segment_score = rowSums(
        #    sweep(
        #      as.matrix(across(all_of(quality_vars))),
        #      2, quality_directions, `*`
        #    ),
        #    na.rm = TRUE
        #  )) %>%
        #  ungroup() %>%
        #  group_by(TagID) %>%
        #  # Compute the segment-level score using the chosen function
        #  mutate(segment_score_agg = tapply(segment_score, brksec, quality_fun)[as.character(brksec)]) %>%
        #  filter(brksec == brksec[which.max(segment_score_agg)]) %>%
        #  ungroup() %>%
        #  select(-c(segment_score, segment_score_agg))

        data_scored <- data_dp %>%
          filter(parallel_issue == 1) %>%
          mutate(
            row_score = rowSums(
              sweep(
                as.matrix(across(all_of(quality_vars))),
                2, quality_directions, `*`
              ),
              na.rm = TRUE
            )
          )

        segment_scores <- data_scored %>%
          group_by(TagID, overlap_block, brksec) %>%
          summarise(
            seg_score = quality_fun(row_score, na.rm = TRUE),
            .groups = "drop"
          )

        winners <- segment_scores %>%
          group_by(TagID, overlap_block) %>%
          slice_max(seg_score, n = 1, with_ties = FALSE) %>%
          ungroup() %>%
          select(TagID, overlap_block, win_brksec = brksec)

      }

      if(dup_method_parallel != "merge"){
        data_dp <- data_dp %>%
          left_join(winners, by = c("TagID", "overlap_block")) %>%
          filter(
            parallel_issue == 0 |
              (parallel_issue == 1 & brksec == win_brksec)
          ) %>%
          select(-c(win_brksec)) %>%
          arrange(TagID, DateTime)

      } else{

        data_dp <- data_dp %>% arrange(TagID, DateTime)
      }

      # remove remaining added internal columns
      data_dp <- data_dp %>%
        select(-c(brk, dt, brksec, parallel_issue, overlap_block))

      data <- data_dp

    } else{
      data <- data_dp %>% select(c(-dt, -brk, -brksec, -overlap_block))
    }

  } #else{
  #if(verbose){
  #  message("No temporal jumps detected: skipping segment resolution")
  #}
  #}

  # ------------------------------------------------------ #
  # Step 2: Same DateTime, different coordinates
  # ------------------------------------------------------ #

  #if(annotate){
  if(mode == "annotate"){
    data <- data %>%
      group_by(TagID, DateTime) %>%
      mutate(
        dup_time_issue = as.integer(n_distinct(latitude, longitude) > 1)
      ) %>%
      ungroup()

    if(any(has_dup_times)){
      if(verbose){
        message("Duplicate DateTime with different coordinates detected")
      }
    }

  } else{

    if(any(has_dup_times)){
      if(verbose){
        message("Duplicate DateTime with different coordinates detected: resolving...")
        message("Using dup_method: ", dup_method_same_time)
      }

      # identify where there are duplicates
      data_dp0 <- data %>% group_by(TagID, DateTime) %>%
        filter(n() > 1 & n_distinct(latitude, longitude) > 1) ### and where the lat and longs are NOT the same

      if(dup_method_same_time == "first"){

        data_dp0 <- data_dp0 %>%
          group_by(TagID, DateTime) %>%
          slice(1) %>%
          ungroup()

      } else if(dup_method_same_time == "last"){

        data_dp0 <- data_dp0 %>%
          group_by(TagID, DateTime) %>%
          slice(n()) %>%
          ungroup()

      } else if(dup_method_same_time == "random"){

        data_dp0 <- data_dp0 %>%
          group_by(TagID, DateTime) %>%
          slice_sample(n = 1) %>%
          ungroup()

      } else if(dup_method_same_time == "centroid"){

        other_cols <- setdiff(names(data_dp0), c("TagID", "DateTime", "latitude", "longitude"))

        data_dp0 <- data_dp0 %>%
          group_by(TagID, DateTime) %>%
          summarise(
            latitude  = centroid_fun(latitude, fun = mean),
            longitude = centroid_fun(longitude, fun = mean),
            dplyr::across(all_of(other_cols), ~ first(.x)),
            .groups = "drop"
          )

      } else if(dup_method_same_time == "best_quality"){

        data_dp0 <- data_dp0 %>%
          group_by(TagID, DateTime) %>%
          mutate(
            fix_score = rowSums(
              sweep(
                as.matrix(dplyr::across(all_of(quality_vars))),
                2, quality_directions, `*`
              ),
              na.rm = TRUE
            )
            #,
            #fix_score_agg = quality_fun(fix_score, na.rm = TRUE)
          ) %>%
          slice(which.max(fix_score)) %>%
          ungroup() %>%
          select(-fix_score)
      }

      # add to where there are no duplicates
      data_dp <- data %>% group_by(TagID, DateTime) %>%
        filter(n() <= 1 | n_distinct(latitude, longitude) == 1) %>%  ### and where the lat and longs ARE distinct and not duplicated
        bind_rows(.,data_dp0) %>%
        arrange(TagID,DateTime)

      data <- data_dp

    }
    #else{

    #if(verbose){
    #  message("No duplicate DateTime conflicts detected: skipping resolution")
    #}
    #}
  }


  # ------------------------------------------------------ #
  # Step 3: Same coordinates, different DateTimes
  # ------------------------------------------------------ #
  # this one is suspect and potentially NOT biologically reasonable
  # an animal could stay in one place and the tag could record the exact same valild coordinate
  #if(annotate){

  if(mode == "annotate"){

    if(use_dup_coords){
      data <- data %>%
        group_by(TagID, latitude, longitude) %>%
        mutate(
          dup_coords_issue = as.integer(n() > 1)
        ) %>%
        ungroup()
    } else{
      data$dup_coords_issue <- 0
    }

    if(any(has_dup_coords)){
      if(verbose){
        message("Same coordinates with different DateTimes detected")
      }
    }
  } else{

    if(any(has_dup_coords)){
      if(verbose){
        message("Same coordinates with different DateTimes detected: resolving...")
        message("Using dup_method: ", dup_method_same_loc)
      }

      # identify where there are duplicates
      data_dp0 <- data %>% group_by(TagID, latitude, longitude) %>%
        filter(n() > 1 & n_distinct(DateTime) > 1) ### and where the lat and longs are the same but DateTimes are different

      if(dup_method_same_loc == "first"){

        data_dp0 <- data_dp0 %>%
          group_by(TagID, latitude, longitude) %>%
          slice(1) %>%
          ungroup()

      } else if(dup_method_same_loc == "last"){

        data_dp0 <- data_dp0 %>%
          group_by(TagID, latitude, longitude) %>%
          slice() %>%
          ungroup()

      } else if(dup_method_same_loc == "random"){

        data_dp0 <- data_dp0 %>%
          group_by(TagID, latitude, longitude) %>%
          slice_sample(n = 1) %>%
          ungroup()

      } else if(dup_method_same_loc == "avg_time") {

        data_dp0 <- data_dp0 %>%
          group_by(TagID, latitude, longitude) %>%
          summarise(
            DateTime = {
              tz <- unique(attr(DateTime, "tzone")) %||% ""
              as.POSIXct(
                time_fun(as.numeric(DateTime), na.rm = TRUE),
                origin = "1970-01-01",
                tz = tz
              )
            },
            dplyr::across(-DateTime, first),
            .groups = "drop"
          )

      } else if(dup_method_same_loc == "best_quality"){

        data_dp0 <- data_dp0 %>%
          group_by(TagID, latitude, longitude) %>%
          mutate(
            fix_score = rowSums(
              sweep(as.matrix(across(all_of(quality_vars))), 2, quality_directions, `*`),
              na.rm = TRUE
            )
          ) %>%
          slice(which.max(fix_score)) %>%
          ungroup() %>%
          select(-fix_score)

      }

      # add to where there are no duplicates
      data_dp <- data %>%
        group_by(TagID, latitude, longitude) %>%
        filter(n() == 1 | n_distinct(DateTime) == 1) %>%  ### where there were no duplicates
        bind_rows(.,data_dp0) %>% # add to duplicated filtered data above
        arrange(TagID,DateTime)

      data <- data_dp
    } #else{
    #if(verbose){
    #  message("No duplicate location conflicts detected: skipping resolution")
    #}
    #}

  }



  # ------------------------------------------------------ #
  # Step 4: Exact duplicates (DateTime + coordinates)
  # ------------------------------------------------------ #

  #if(annotate){
  if(mode == "annotate"){
    data <- data %>%
      group_by(TagID, DateTime, latitude, longitude) %>%
      mutate(dup_time_coords_issue = as.integer(n() > 1)) %>%
      ungroup()

    if(any(has_exact_dup)){
      if(verbose){
        message("Exact DateTime + coord duplicates detected")
      }
    }
  } else{

    if(any(has_exact_dup)){

      if(verbose){
        message("Exact DateTime + coord duplicates detected: resolving...")
        message("Using dup_method: ", dup_method_exact)
      }

      # isolate exact duplicates
      data_dp0 <- data %>%
        group_by(TagID, DateTime, latitude, longitude) %>%
        filter(n() > 1)  # more than one row with identical DateTime and location

      if(dup_method_exact == "first"){

        data_dp0 <- data_dp0 %>%
          slice(1) %>%
          ungroup()

      } else if(dup_method_exact == "last"){

        data_dp0 <- data_dp0 %>%
          group_by(TagID, latitude, longitude) %>%
          slice(n()) %>%
          ungroup()

      } else if(dup_method_exact == "random"){
        data_dp0 <- data_dp0 %>%
          slice_sample(n = 1) %>%
          ungroup()

      } else if(dup_method_exact == "best_quality"){
        data_dp0 <- data_dp0 %>%
          mutate(
            fix_score = rowSums(
              sweep(
                as.matrix(across(all_of(quality_vars))),
                2, quality_directions, `*`
              ),
              na.rm = TRUE
            )
          ) %>%
          slice(which.max(fix_score)) %>%
          ungroup() %>%
          select(-fix_score)
      }

      # combine with "good" data
      data_dp <- data %>%
        group_by(TagID, DateTime, latitude, longitude) %>%
        filter(n() == 1) %>%  # rows without exact duplicates
        bind_rows(data_dp0) %>%
        arrange(TagID, DateTime) %>%
        ungroup()

      data <- data_dp

    } #else{
    #if(verbose){
    #  message("No full duplicates detected: skipping resolution")
    #}
    #}

  }

  # ------------------------------------------------------ #
  # Step 5: All duplicate rows
  # ------------------------------------------------------ #

  #if(annotate){
  if(mode == "annotate"){
    data <- data %>%
      mutate(all_dup_issue = as.integer(duplicated(.) | duplicated(., fromLast = TRUE)))

    if(any(any_dup)){
      if(verbose){
        message("Full duplicate rows detected")
      }
    }
  } else{

    if(any_dup){

      if(verbose){
        message("Full duplicate rows detected, resolving...")
      }
      data <- dplyr::distinct(data)
    }
  }
  if(verbose){
    message("Done.")
  }

  return(data)
}

#' @rdname duplicate_track
#' @export
is_duplicated_track <- function(data, step = NULL){

  steps_allowed <- c("parallel", "same_time", "same_loc", "exact", "all")

  if (is.null(step)) {
    step <- setdiff(steps_allowed, "full")
  }

  step <- match.arg(step, steps_allowed, several.ok = TRUE)

  details <- setNames(
    rep(FALSE, length(steps_allowed)),
    steps_allowed
  )

  # ----------------------------------
  # Full-row duplicates
  # ----------------------------------
  if ("full" %in% step) {
    details["full"] <- any(duplicated(data))
    step <- setdiff(step, "full")
  }

  # ----------------------------------
  # GPS-specific duplication checks
  # ----------------------------------
  if (length(step) > 0) {

    det <- duplicate_track(data, mode = "detect")

    if (nrow(det) > 0) {

      step_hits <- det |>
        dplyr::filter(step %in% step) |>
        dplyr::group_by(step) |>
        dplyr::summarise(hit = any(issue_detected), .groups = "drop")

      details[step_hits$step] <- step_hits$hit
    }
  }

  result <- any(details[step])

  attr(result, "details") <- details[step]

  result
}














