#' Dealing with 'come-and-go' trips directly in Track data
#'
#' Functions to extract, add and remove extra information for come-and-go trips after the \code{define_trips}
#' function and prior to use of \code{trips_stats}.
#'
#' @details
#' An issue with assigning trips to data is that issues can arise where the GPS sampling interval is not of
#' sufficient temporal resolution to register a unique start and end point for each trip. By default the \code{define_trips}
#' function assesses such conditions of trips, here defined as trips of an animal that appear 'come-and-go' (i.e. arrive and then depart again)
#' from a central place. Such trips will share a start or end with a neighbouring trip.
#' For the subsequent processing of trips in function \code{trip_stats} require each trip to have a unique
#' start and end DateTime. The way this is handled in \code{MoveRakeR} is to include duplicate data to correctly assess trip metrics.
#' These extra rows are by default not included directly in the return \code{Track} data output to avoid potential issues
#' in further downstream analyses, but instead are stored in an attribute of the data:
#' \code{attr(attr(data, 'define_trips'), 'extra_rows')}.
#'
#' Should you proceed directly from \code{define_trips} to
#' \code{trip_stats} there should be no issue, and such come-and-go conditions will be deal with automatically. However, if you
#' want to subset your data, or convert the data to and from other data object classes, then this can
#' result in either incorrect storage of such duplicate data or even lose the attribute for duplicates all together,
#' particularly using base R operation such as \code{data.frame()}, \code{cbind()} and so forth, meaning
#' you may see an error in \code{trip_stats} where extra_rows were not found. To get around this issue, some additional functions
#' are available to help.
#'
#' Should you be converting between data classes or sub-setting your data between the running of
#' \code{define_trips} and \code{trip_stats}, it is safer to add the (potential) extra_rows for come-and-go trips
#' that you may have back into the Track object directly, then do your subset operations, and then run \code{trip_stats}.
#'
#' The function \code{add_cag_trips} looks for the presence of extra duplicate data rows in the Track object attribute,
#' and adds them directly to the Track object, in turn removing them as an attribute from the Track object. The reverse
#' operation is performed by \code{reset_cag_trips} that looks for the presence of an 'extra_rows' column directly in the data
#' with a 1 for duplicate data rows and 0 for not, and adds the duplicates back as the data attribute, and removing the
#' duplicates from the main Track object. The final function \code{get_cag_trips} simply looks for such added rows
#' in the Track data attribute if available, and returns them as a direct object, or if log = TRUE, returns a logical
#' detecting the presence or absence of such duplicate data.
#'
#' Apologies if this is inconvenient; other options such as R6 classes may be better suited to this task and may be updated in the future.
#'
#' @param data The Track data object that should contain an attribute \code{attr(attr(data, 'define_trips'), 'extra_rows')}
#' @param log A boolean defaulting to FALSE for use in \code{get_cag_trips} to return (if TRUE) a logical TRUE if extra_rows
#' were found in the data attributes, otherwise, FALSE is returned. If log is FALSE then the actual duplicate data are returned if found.
#'
#' @seealso [MoveRakeR::define_trips], [MoveRakeR::trip_stats]
#'
#' @examples
#'
#' indata <- data_in %>% clean_GPS(speed_filt = TRUE, drop = FALSE) %>%
#'  define_trips(method="rect", lls = c(-3.2, 54.0553,-3.1689, 54.0437))
#'
#' # additional data may have been added to the Track object
#' indata$test1 <- 1
#' indata$test2 <- 2
#'
#' # this base R subset preserves the Track data attribute for 'extra_rows'
#' data1 = subset(indata, select = c(TagID, longitude,latitude, test1, DateTime, speed_3d, altitude, test2))
#'
#' # but you may also have inadvertently names another column the same called extra_row, that here is
#' # expected to be used to indicate 1 = duplicate and 0 = non duplicate rows
#' data1$extra_row <- 1:nrow(data1)
#'
#' ## 1. You can add the extra_rows back in the Track data by:
#' data_ <- add_cag_trips(data1)
#'
#' # A message will indicate extra_rows are detected, however warnings will be given in this example
#' # where non-zero extra_row information was present in the extra_row column in the Track data
#' # your existing column extra_rows would be preserved as '__extra_rows__' but then removed using reset_cag_trips()
#'
#' # the attributes from te Track object will have been NULLed out as the duplicate data are now in
#' # your Track data. This makes it safe to do further sub-setting and then run trip_stats()
#' attr(attr(data_, "define_trips"), "extra_rows") # NULL
#'
#' nrow(data_) # should be larger than as nrow(indata)
#'
#' ## 2. You can if you like though, go back to the original (preferred) layout of
#' ## having duplicate row data stored in the attribute not in the data:
#'
#' data_2 <- reset_cag_trips(data_)
#' attr(attr(data_2, "define_trips"), "extra_rows") # extra rows now present again
#'
#' nrow(data_2) # should be the same as nrow(indata)
#'
#' ## 3. Test for presence of or return (default) the duplicate row_data after define_trips() run:
#'
#' get_cag_trips(indata, log = FALSE)
#' get_cag_trips(indata, log = TRUE) # returns TRUE
#'
#' # or for above example after insertion of duplicate rows back in the Track data
#' get_cag_trips(data_2, log = FALSE)
#' get_cag_trips(data_2, log = TRUE)
#'
#' # Also note that duplicate rows ONLY relevant information for DateTime, longitude, latitude and TagID;
#' # all other variables from duplicate fixes are NA for duplicate rows.
#'
#' @import dplyr
#' @import tibble
#'
#' @export
add_cag_trips <- function(data){

  # This function looks for the presence of 'extra_rows' and gets those attributes if present from
  # the data (after define_trips has been run), and then NULLs them at the attribute position

  data <- tibble::tibble(data)

  # remove pre-existing internal named column __extra_rows__
  if(any(names(data) == "__extra_rows__")){
    data <- dplyr::select(data, select = -'__extra_rows__')
  }

  if(exists("extra_row",data)){

    message("-o-o- Column 'extra_row' detected -o-o-
      -- This should be a column of '0' for data not inserted for cag trips from define_trips()")

    # if extra_row has 1 and 0, then perhaps stop the function as it may be the user has already added
    vals = unique(data$extra_row)

    if(any(vals != 0)){
      warning("-o-o- Non zero data detected in 'extra_rows' in supplied Track data -o-o-
        -- Preserving your original column as '__extra_rows__'
        -- NOTE: if function 'reset_cag_trips()' is then run, '__extra_rows__' will be deleted
        -- NOTE: if function 'add_cag_trips()' was already run, then you may see duplicate rows!")

      data$'__extra_rows__' <- data$extra_row
      data$extra_row <- 0

    }

  }

  # if column "extra_row" is removed from data, add back in
  if(!exists("extra_row",data)){
    data$extra_row <- 0
  }


  # check for attribute of extra_rows
  er = attr(attr(data, "define_trips"), "extra_rows")

  if(!is.null(er)){
    # get common names of extra rows and data (as columns could have been altered)
    er = er[,which(names(er) %in% names(data))]

    # pretty sure that there can't be a case where all data in the Track data will not be in er
    # but get any missing columns from the er subset from those not in er

    nm_add <- names(data)[!names(data) %in% names(er)]

    na_add = tibble::tibble(data.frame(matrix(NA,nrow = nrow(er), ncol = length(nm_add))  ))
    names(na_add) <- nm_add
    er = cbind(er, na_add)
    er <- relocate(er, names(data))

    data <- data %>% bind_rows(.,er) %>% arrange(TagID,DateTime)

    # also DROP the attr(attr(data_, "define_trips"), "extra_rows") data as there could be a risk of adding back in!?

    attr(attr(data, "define_trips"), "extra_rows") <- NULL

  } else{
    warning("No extra_rows available for thie dataset, nothing added, returning original dataset")
  }

  return(Track(data))
}

#' @rdname add_cag_trips
#' @export
get_cag_trips <- function(data, log = FALSE){

  # this function simply gets the attributes of extra rows if available
  # or tests for the presence of extra_rows with log = TRUE

  er = attr(attr(data, "define_trips"), "extra_rows")

  if(!is.null(er)){

    data <- attr(attr(data, "define_trips"), "extra_rows")

    if(log){
      data <- TRUE
    }

  } else{

    data <- NULL

    if(log){
      data <- FALSE
    }
    warning("No extra_rows available for thie dataset, nothing added, returning original dataset")
  }

  return(data)
}

#' @rdname add_cag_trips
#' @export
reset_cag_trips <- function(data){

  # This function looks for presence of extra_rows 0,1 and adds those as 1 back as a data attribute
  # and removes the 1's from the Track object

  data <- tibble::tibble(data)

  # remove pre-existing internal named column __extra_rows__
  if(any(names(data) == "__extra_rows__")){
    data <- dplyr::select(data, select = -'__extra_rows__')

    warning("-o-o- Column '__extra_row__' inserted from 'add_cag_trips()' was deleted -o-o-"

    )
  }

  if(exists("extra_row",data)){

    message("-o-o- Column 'extra_row' detected -o-o-
      -- This should be a column distinguishing rows inserted for cag trips
      -- If these are 1,0 here, it is assumed the 1's are extra_rows
      -- Zeros will be retained as the Track data, 1's will be assigned to:
            'attr(attr(data, 'define_trips'), 'extra_rows')'"
    )

    vals = unique(data$extra_row)

    if(!all(vals %in% c(1,0))){

      stop("Non binary data detected for extra_rows, improper use of function")

    } else{

      # retain the duplicates for attribute
      data_extra <- tibble::tibble(data[data$extra_row == 1,])

      # Track should be only non dupliate data
      data_Track <- Track(data[data$extra_row == 0,])

      # check prior existence of attribute and warn that it will be overwritten (not sure if this condition would occur, just in case)
      er = attr(attr(data, "define_trips"), "extra_rows")
      if(!is.null(er)){ warning("attr extra_rows already present in Track data and was over-written") }

      # make the attribute from scratch as it may not exist at all
      data <- data_Track
      if(is.null(attr(data, "define_trips") )){
        attr(data, "define_trips") <- "define_trips"
      }
      attr(attr(data, "define_trips"), "extra_rows") <- data_extra
    }

  } else{
    warning("No extra_rows available for thie dataset, nothing added, returning original dataset")
  }

  return(data)

}

