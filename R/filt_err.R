#' Generic filter function
#'
#' Filter or Annotate Data Based on Numeric, Character, or Function Criteria
#'
#' The `filt_err()` function can easily be coded openly in R. However, `filt_err()` is a simple convenience
#' function to flag or remove rows from a data.frame based on numeric bounds, character exact matches, or
#' custom function criteria. It creates per-column `_rm` indicator columns (1 = flagged, 0 = OK, NA preserved),
#' as well as a combined `all_rm` column indicating if any column flagged the row.
#'
#' @param data Input data object, with required columns: TagID, DateTime, longitude, latitude.
#' @param nms A character vector of column names in \code{data} to evaluate.
#' @param vals_l Optional list of lower bounds or custom functions per column.
#'   - For numeric/integer columns: numeric lower bounds.
#'   - For character columns: must be NULL.
#'   - For function-based flagging: provide a function of one argument returning logical or integer flags.
#' @param vals_u Optional list of upper bounds, exact character matches, or custom functions per column.
#'   - For numeric/integer columns: numeric upper bounds.
#'   - For character columns: a single character string to match for flagging.
#'   - For function-based flagging: provide a function of one argument returning logical or integer flags.
#' @param annotate Logical; if TRUE (default), returns the original data with `_rm` and `all_rm` columns.
#'   If FALSE, rows flagged as 1 in `all_rm` are removed. Only works if `all_flag` is set to TRUE.
#' @param inclusive Logical; if TRUE (default), numeric bounds are inclusive (`[lower, upper]` kept, outside flagged).
#'   If FALSE, numeric bounds are exclusive (`(lower, upper)` kept, bounds themselves flagged).
#' @param all_flag Logical; If TRUE (default) a single column 'all_rm' is added to the output to flag all
#' cases that have at least a '1' in one of the columns.
#' @param verbose Logical; If TRUE (default) for messaged outputs.
#'
#' @return A \code{data.frame} with additional `_rm` columns for each variable in \code{nms} and
#'   a combined `all_rm` column indicating if any row was flagged.
#'
#' @details
#' - Numeric columns are flagged based on provided lower and/or upper bounds.
#'   Values within bounds are marked 0 (OK); outside bounds are 1 (flagged); NAs preserved.
#' - Character columns are flagged if the value exactly matches the provided `vals_u` or `vals_l` string.
#' - Function-based criteria allow arbitrary custom flagging logic. The function should take a single argument (the column vector)
#'   and return a logical or integer vector of the same length. NAs can be handled inside the function.
#' - `all_rm` flags 1 if **any** column `_rm` is 1; 0 if all are 0; NA only if all columns are NA.
#'
#' @examples
#' data <- data.frame(
#'   speed_accuracy = c(9, 10, 11, 12, NA),
#'   flag = c("ok", "wrong", "ok", "ok", NA),
#'   satellites_used = c(2, 3, 4, 5, NA),
#'   flt_switch = c(NA, 0, 77, 0, NA)
#' )
#'
#' # Simple numeric bounds
#' test1 <- filt_err(
#'   data,
#'   nms = c("speed_accuracy", "satellites_used"),
#'   vals_l = list(0, 4),
#'   vals_u = list(11, NULL)
#' )
#'
#' # Or not needing lists if just one variable
#' test2 <- filt_err(
#'   data,
#'   nms = "satellites_used",
#'   vals_l = 4,
#'   vals_u = NULL
#' )
#'
#' # Mixed numeric and character
#' test3 <- filt_err(
#'   data,
#'   nms = c("speed_accuracy", "flag"),
#'   vals_l = list(NULL, NULL),
#'   vals_u = list(11, "wrong")
#' )
#'
#' # Function-based: treat NA and 0 as OK, everything else flagged
#' test4 <- filt_err(
#'   data,
#'   nms = c("flt_switch"),
#'   vals_l = list(NULL),
#'   vals_u = list(function(x) as.integer(!(is.na(x) | x == 0)))
#' )
#'
#' # or with other variables:
#' flag_flt_switch <- function(x) {
#'   flag <- as.integer(!(is.na(x) | x == 0))
#'   return(flag)
#'  }
#'
#' # speed accuracy <= 11 = '1', wrong values labelled '1', less than three sats, and flt_switch NA and 0 as OK
#' test5 = filt_err(data,
#'   nms = c("speed_accuracy", "flag", "satellites_used", "flt_switch"),
#'   vals_l = list(NULL,NULL,4,NULL),
#'   vals_u = list(11,"wrong",NULL,flag_flt_switch)
#' )
#'
#' # drop all rows where any one of the conditions met:
#' test_dt2 = filt_err(data, nms = c("speed_accuracy", "flag", "satellites_used", "flt_switch"),
#'   vals_l = list(NULL,NULL,4,NULL),
#'   vals_u = list(11,"wrong",NULL,flag_flt_switch),
#'   annotate = FALSE
#' )
#'
#'
#' @export
filt_err <- function(data, nms = NULL, vals_l = NULL, vals_u = NULL,  annotate = TRUE, inclusive = TRUE, all_flag = TRUE, verbose = TRUE){

  # ------------------------------------------------------- #
  # Basic checking
  # ------------------------------------------------------- #

  if(!is.data.frame(data)){stop("data must be a data.frame")}
  if(!all(nms %in% names(data))){stop("Some names in nms do not exist in data")}
  if(is.null(vals_l) & is.null(vals_u)){stop("No vals_l or vals_u supplied")}
  if(!is.null(vals_l) && length(vals_l) != length(nms)){stop("Length of vals_l must match length of nms")}
  if(!is.null(vals_u) && length(vals_u) != length(nms)){stop("Length of vals_u must match length of nms")}

  # convert to data.table
  dt <- as.data.table(data)
  rm_cols <- character()

  for(i in seq_along(nms)){
    var <- nms[i]
    x <- dt[[var]]

    lower <- if (!is.null(vals_l)) vals_l[[i]] else NULL
    upper <- if (!is.null(vals_u)) vals_u[[i]] else NULL
    rm_col <- paste0(var, "_rm")

    flag <- rep(0L, length(x)) # initialize
    flag[is.na(x)] <- NA       # preserve NAs by default

    # If the column is all NA, make the flag column all NA
    if(all(is.na(x))){
      if(verbose) message("- Column '", var, "' is all NA — setting ", rm_col, " to all NA")
      dt[[rm_col]] <- rep(NA_integer_, length(x))
      rm_cols <- c(rm_cols, rm_col)
      next
    }

    # ------------------------------------------------------- #
    # Function-based flagging
    # ------------------------------------------------------- #
    if(is.function(lower)){
      temp_flag <- as.integer(lower(x))
      if(length(temp_flag) != length(x)){stop(paste0("Function for ", var, " must return same length as column"))}
      flag <- temp_flag
      dt[[rm_col]] <- flag
      rm_cols <- c(rm_cols, rm_col)
      next
    }
    if(is.function(upper)){
      temp_flag <- as.integer(upper(x))
      if(length(temp_flag) != length(x)){stop(paste0("Function for ", var, " must return same length as column"))}
      flag <- temp_flag
      dt[[rm_col]] <- flag
      rm_cols <- c(rm_cols, rm_col)
      next
    }

    # ------------------------------------------------------- #
    # Numeric / integer logic
    # ------------------------------------------------------- #
    if(is.numeric(x) || is.integer(x)){
      if(!is.null(lower) && !is.numeric(lower)){stop(paste0("Lower bound for ", var, " must be numeric"))}
      if(!is.null(upper) && !is.numeric(upper)){stop(paste0("Upper bound for ", var, " must be numeric"))}

      if(!is.null(lower) & !is.null(upper)){
        if(!inclusive){
          flag[!is.na(x) & (x < lower | x > upper)] <- 1L
        } else{
          flag[!is.na(x) & (x <= lower | x >= upper)] <- 1L
        }
      } else if(!is.null(lower)){
        if(!inclusive){
          flag[!is.na(x) & (x < lower)] <- 1L
        } else{
          flag[!is.na(x) & (x <= lower)] <- 1L
        }
      } else if(!is.null(upper)){
        if(!inclusive){
          flag[!is.na(x) & (x > upper)] <- 1L
        } else{
          flag[!is.na(x) & (x >= upper)] <- 1L
        }
      }

      # ------------------------------------------------------- #
      # Character logic
      # ------------------------------------------------------- #
    } else if(is.character(x)){
      provided <- c(!is.null(lower), !is.null(upper))
      if(sum(provided) != 1){stop(paste0("Character variable ", var,
                                          " must have exactly ONE of vals_l or vals_u provided"))}
      tgt <- if(!is.null(lower)) lower else upper
      if(!is.character(tgt)){stop(paste0("Value for character variable ", var,
                                          " must be a character string"))}

      flag[x == tgt] <- 1L
      flag[is.na(x)] <- NA

    } else{
        stop(paste0("Variable ", var, " must be numeric, integer, character, or a function"))
    }

    dt[[rm_col]] <- flag
    rm_cols <- c(rm_cols, rm_col)
  }
  # ------------------------------------------------------- #
  # Single flagged row
  # ------------------------------------------------------- #
  if(all_flag){
    dt$all_rm <- do.call(pmax, c(dt[, ..rm_cols], na.rm = TRUE))
    all_na_rows <- rowSums(!is.na(dt[, ..rm_cols])) == 0
    dt$all_rm[all_na_rows] <- NA

    # ------------------------------------------------------- #
    # Drop or annotate?
    # ------------------------------------------------------- #

    if(!annotate){
      dt <- dt[all_rm != 1 | is.na(all_rm)]
    }

  }

  return(Track(as.data.frame(dt)))
}

