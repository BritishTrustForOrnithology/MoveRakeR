#' Reading in pressure data from the University of Amsterdam Bird Tracking (UvA-BiTS) database.
#'
#' Connect and download pressure data directly from the UvA-BiTS phpPGAdmin SQL database.
#'
#' As with the function \code{\link{read_track_UvA}}, and \code{\link{read_accn_UvA}}, \code{read_pressure_UvA}  allows the user to extract sections
#' of data across multiple individuals and UTC date-times. This function liaises with the
#' table view in the UvA database called "ee_pressure_burst_limited" that contains
#' pressure and temperature data together with date-time stamps. Hopefully the user will have
#' specified a synched GPS sensor measurement with the pressure so a simple merge of the date_times will then be
#' needed to align the positional and the pressure sensor data.
#'
#' The user will have specified a number of pressure measurements to be taken in the birdtracking
#' groundstation software. A standard of ten measurements per GPS fix are typically recommended. The UvA wizards
#' then process the data by ranking the samples taken, and removing the highest and lowest and then taking a mean
#' of the remaining (e.g. eight) points, to give what is seen in the database as an averaged pressure measurement.
#' The same thing is also done for temperature.
#'
#' As with the read.track functions, \code{read_pressure_UvA} is designed to do work with a control file of
#' start and end date_times per animal for TagIDs can be used as a lookup table.
#' WARNING: \code{read_pressure_UvA} requires access to the UvA-BiTS database in the first place, and further, it is
#' possible within these read in functions to extract large amounts of data. It is therefore advisable
#' to make as few direct calls to the databases and extract the data needed to be stored
#' and then read in again for subsequent analytical steps.
#'
#' \code{read_pressure_UvA} uses an ODBC connection to establish a link with the UvA-BiTS
#' database. WARNING: THIS MUST BE SET UP FIRST BEFORE USING THIS FUNCTION
#' and of course you must have prior access to the UvA-BiTS system with a username and password. Steps to do
#' this are provided here: \url{https://wiki.e-ecology.nl/index.php/How_to_access_the_e-Ecology_database}.
#' Extraction of GPS positional data from the database is made using R library \code{RODBC}
#' and the user must also first establish the database connection in their global environment
#' named as object \strong{'db'}, specifying whet the connection was named as in the help file steps above:
#' in this case called "GPS", using \code{RODBC::odbcConnect("GPS")}. See examples.
#'
#' \code{read_pressure_UvA} then uses \code{RODBC} to contruct SQL statements.
#' The resultant \code{base::data.frames} objects to \code{Pr}, \code{PrStack} and \code{PrMultiStack} classes.
#' The user can also specify \emph{start} and and \emph{end} datetimes (see below)
#' to specify subsections from the entire dataset available in repositories for individual
#' animals.
#'
#' @param TagID The individual tag identifier from the UvA-BiTS database.
#' @param start The start time to delineate subsets of the total data for the individual in UTC.
#' @param end The end time to delineate subsets of the total data for the individual in UTC.
#' The start and end times must be give in the format "2014-06-25 15:20:23".
#' Arguments \emph{start} and \emph{end} default to NULL, reading in the entire track
#' of the animal and must be given in' \emph{character()} format, which are then converted to
#' \code{base::POSIXct} format within the \code{read.track.MB} function. Functionality is also
#' available for situations where only one or other of start and end need to be specified,
#' with the the other defaulting to NULL.
#' @param form The form of the pressure to return, being three options as a character variable of: (1) "mean"
#' for the mean pressure values averaged across the n-2 measurements (top and bottom removed) from UvA database "pressure_burst_limited"; (2) "raw" for
#' the raw pressure and temperature (p) and (t), i.e. I think from absolute measures prior to conversion to MB and degC,
#' for all measurements (1-10) from database pressure_point_limited; and (3) "calibration", which I think
#' are tag-specific metrics used in tag sensor calibration, but I don't know how. The option defaults to "mean".
#'
#' @return If the option "mean" is chosen in argument "form", then the final data outputted for individual animals
#' is a \code{Pr and data.frame} object of six variables of Type, TagID, pressure_avg (MB) and temperature_av (deg C). A further id column from the database is also retained.
#' If the option "raw" is selected in "form" then the output is a data.frame of seven variables being:
#' TagID, DateTime, burst_id, flag, point_seq, p and t and if option "calibration" is chosen in "form"
#' then the data are returned as a data.frame with columns of: TagID, c1, c2, c3, c4, c5, c6 and tracker_id.
#' It is assumed here the user will be familiar with these expected layouts.
#' For data returned from the "mean" option, data at the individual level are always provided as a \code{Pr} class for multiple
#' TagIDs, a \code{PrStack} object is returned. For all other data options, no class is assigned.
#'
#' @examples
#'
#' db.file <- "GPS" # what you named the ODBC link on set up
#' db <- RODBC::odbcConnect(db.file) # currently required "db" to be named globally
#'
#' ## Example using direct calls to UvA-BiTS database using SQL coding for date-time start and ends per animal
#' # Note, you can extract multiple birds with the same start-ends this way too
#' TagIDs <- "('1','2','3')" # replace with real TagIDs
#' sttime <- "'2020-05-30 17:00:00'"
#' entime <- "'2020-06-02 17:00:00'"
#'
#  # Pressure points - I think this is raw from the tag - NOT EXTRACTED BY THE FUNCTION
#' # But you can see the 10 pressure values from the tag (unprocessed)
#' data <- RODBC::sqlQuery(db, query =
#'                          paste(
#'                            "select", "device_info_serial, date_time, burst_id, flag, point_seq, p, t",
#'                            "from", "gps.ee_pressure_point_limited", "where",
#'                            "device_info_serial", "in", TagIDs,
#'                            "and",
#'                            "date_time >=", sttime,
#'                            "and",
#'                            "date_time <=", entime)
#' )
#'
#' # Pressure bursts - EXTRACTED FROM THE FUNCTION read_pressure_UvA
#' data <- RODBC::sqlQuery(db, query =
#'                          paste(
#'                            "select", "device_info_serial, date_time, id, pressure_avg, temp_avg", #clock_source, fp, gps_sync_status, prs_iv, prs_mode, prs_sa,
#'                            "from", "gps.ee_pressure_burst_limited", "where",
#'                            "device_info_serial", "in", TagIDs,
#'                            "and",
#'                            "date_time >=", sttime,
#'                            "and",
#'                            "date_time <=", entime)
#' )
#'
#' ## Using read_pressure_UvA - read in three birds with different start/end times in 2020
#' TagIDs = c('1', '2', '3')
#' start = c("2020-06-01 13:53:50", "2020-06-15 12:22:13", "2020-06-05 08:07:23")
#' end = c("2020-07-15 09:17:14", "2020-07-20 01:08:58", "2020-07-18 14:22:45")
#' dataPr <- read_pressure_UvA(TagIDs, start = start, end = end)
#'
#' class(dataPr)
#'
#' @seealso [MoveRakeR::read_track_UvA], [MoveRakeR::read_accn_UvA]
#'
#' @export
read_pressure_UvA <- function(TagID,start=NULL,end=NULL, form = "mean"){

  print(paste0("Processing UvA tags ", paste(TagID, collapse = " ")))

  # error capture
  if(!is.null(start) & length(TagID) > 1 & length(start) > 1 & length(TagID) != length(start)){
    stop("Multiple animals detected with multiple starts, but length(start) != length(TagID)")
  }
  if(!is.null(end) & length(TagID) > 1 & length(end) > 1 & length(TagID) != length(end)){
    stop("Multiple animals detected with multiple ends, but length(end) != length(TagID)")
  }
  if(length(end) > 1 & length(start) > 1 & length(end) != length(start)){
    stop("Multiple animals but start and end lengths do not match") # probably can't get to this error from above but just in case
  }

  ####### internal function to apply across input values
  .pressure.prep <- function(TagID, start, end){
    # .track.prep will expect a character now of NULLs to convert to R NULLs for mapply to work! Not that elegant

    if(any(start == "NULL")){start = NULL}
    if(any(end == "NULL")){end = NULL}

    # convert start and end times to posixCT format
    if(!is.null(start)){start <- as.POSIXct(start,format="%Y-%m-%d %H:%M:%S",tz="UTC")}
    if(!is.null(end)){end <- as.POSIXct(end,format="%Y-%m-%d %H:%M:%S",tz="UTC")}

    blist = paste0("('",TagID,"')")

    # if start and end are supplied, then use them, if not, get ALL the data for the bird!

    if(!is.null(start)){sttime = paste0("'",start,"'")}
    if(!is.null(end)){entime = paste0("'",end,"'")}

    # set up variables to extract and name of DB
    if(form == "mean"){

      vars = c("device_info_serial, date_time, id, pressure_avg, temp_avg")
      DB <- "gps.ee_pressure_burst_limited"

    } else if(form == "raw"){

      vars = c("device_info_serial, date_time, burst_id, flag, point_seq, p, t")
      DB <- "gps.ee_pressure_point_limited"

    } else if (form == "calibration"){

      vars = c("device_info_serial, c1, c2, c3, c4, c5, c6, tracker_id")
      DB <- "gps.ee_pressure_calibration_limited"

    }

    # calibration does not have a date time to query, others do

    if(form == "calibration"){
      Query = paste(
        "select", vars,
        "from", DB, "where",
        "device_info_serial", "in", blist)

        data <- RODBC::sqlQuery(db, query = Query)


    } else if(form != "calibration"){

      if(is.null(start) & is.null(end)){ # no start and end given
        Query = paste(
          "select", vars,
          "from", DB, "where",
          "device_info_serial", "in", blist)
      }
      if(is.null(start) & !is.null(end)){ # end time selected but no start
        Query = paste(
          "select", vars,
          "from", DB, "where",
          "device_info_serial", "in", blist,
          "and","date_time <=", entime)
      }
      if(!is.null(start) & is.null(end)){ # start time given but no end
        Query = paste(
          "select", vars,
          "from", DB, "where",
          "device_info_serial", "in", blist,
          "and", "date_time >=", sttime)
      }
      if(!is.null(start) & !is.null(end)){ # both
        Query = paste(
          "select", vars,
          "from", DB, "where",
          "device_info_serial", "in", blist,
          "and",
          "date_time >=", sttime,
          "and",
          "date_time <=", entime)
      }

      # data sqlQuery call
      data <- RODBC::sqlQuery(db, query = Query
                              ,as.is=c(FALSE, TRUE, rep(FALSE,20)) # constrain to readin datetime as character WITHOUT forcing to a TZ!
      )
      # then have to align Movetech and UvA in the same way as previous function
      names(data)[which(names(data) == "date_time")] <- "DateTime"
      data$DateTime <- as.POSIXct(data$DateTime,format = "%Y-%m-%d %H:%M:%S",tz="UTC") # convert date-time to correct format - here using "UTC" e.g. if through the year analysis

    }

    # for all types read in including calibration
    names(data)[which(names(data) == "device_info_serial")] <- "TagID"

    if(is.null(data)){
        data <- NULL
        warning(paste0("No pressure data for ", TagID, " so excluded"))
    }

    #data <- data.table::as.data.table(data)
    return(data)

  }

  # if you have NULL start or ends, then coerce the .track.prep function to accept through mapply
  if(is.null(start)){start <- rep("NULL",length(TagID))}
  if(is.null(end)){end <- rep("NULL",length(TagID))}

  # mapply
  stacked <- mapply(.pressure.prep, TagID=TagID, start=start, end=end)

  if(ncol(stacked) > 1){
    # if bird's return NULL data, then need to comvert to matrix array style of the lists as from mapply ordinarily
    stacked[sapply(stacked, is.null)] <- NULL # removes any null data
    stacked <- simplify2array(stacked, higher = (TRUE == "array")) # puts in matrix array class format
  } else if(is.null(stacked)){stop("No data extracted")}

  # need to also drop out the offending Tag with no data if there is one,
  # from the original TagID fed from e.g. a control file
  # and this retains the correct order as was in "stacked"
  TagID = unique(unlist(stacked[1,]))

  # if just one animal, assign Track class and be done with read in
  if(ncol(stacked) == 1){
    if(form == "mean"){
      dat = Pr(as.data.frame(stacked[,1]))
    } else{
      dat = as.data.frame(stacked[,1])
    }
  }

  # multiple animals list together and make TrackStack class as well as Track for individual data.frames
  if(ncol(stacked) > 1){

    # stacking the data read in
    dat <- list()
    for(b in 1:ncol(stacked)){
      if(form == "mean"){
        dat[[b]] <- Pr(as.data.frame(stacked[,b])) # assign Track class to each dataframe
      } else{
        dat[[b]] <- as.data.frame(stacked[,b])
      }

    }
    names(dat) <- TagID # name data frames by TagID

    if(form == "mean"){
      dat = PrStack(dat)
    }
  }
  return(dat)

} # close function


















