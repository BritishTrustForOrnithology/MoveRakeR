#' Reading in voltage data from the University of Amsterdam Bird Tracking (UvA-BiTS) database.
#'
#' @description
#' Connect and download voltage data directly from the UvA-BiTS phpPGAdmin SQL database.
#'
#' @details
#' As with the functions \code{\link{read_track_UvA}}, \code{\link{read_accn_UvA}} and \code{\link{read_pressure_UvA}},
#' \code{read_voltage_UvA} allows the user to extract sections
#' of data across multiple individuals and UTC date-times, here for voltage of tags.
#' This function works with the
#' table view in the UvA database called "ee_energy_limited" that contains
#' voltage and solar voltage charging data together with date-time stamps.
#' Note, this voltage information is for \emph{the data downloaded}, and so does not
#' necessarily reflect the current voltage of the tag up to present on the animal. This later data \emph{may}
#' be available in another database view for some projects: "ee_comm_limited", as every time the tag
#' connects with the system it's voltage is picked up, but so far
#' this has been found to be inconsistent for different tags/sites, with some TagIDs not holding this
#' information for reasons out of BTO control.
#
#' As with the other read.track functions, \code{read_voltage_UvA} is designed to do work with a control file of
#' start and end date_times per animal for TagIDs can be used as a lookup table.
#' WARNING: \code{read_voltage_UvA} requires access to the UvA-BiTS database in the first place, and further, it is
#' possible within these read in functions to extract large amounts of data. It is therefore advisable
#' to make as few direct calls to the databases and extract the data needed to be stored
#' and then read in again for subsequent analytical steps.
#'
#' \code{read_voltage_UvA} uses an ODBC connection to establish a link with the UvA-BiTS
#' database. I know this is said above but: WARNING: THIS MUST BE SET UP FIRST BEFORE USING THIS FUNCTION
#' and of course you must have prior access to the UvA-BiTS system with a username and password. Steps to do
#' this are provided here: \url{https://wiki.e-ecology.nl/index.php/How_to_access_the_e-Ecology_database}.
#' Extraction of GPS positional data from the database is made using R library \code{RODBC}
#' and the user must also first establish the database connection in their global environment
#' named as object \strong{'db'}, specifying whet the connection was named as in the help file steps above:
#' in this case called "GPS", using \code{RODBC::odbcConnect("GPS")}. See examples.
#'
#' \code{read_voltage_UvA} then uses \code{RODBC} to construct SQL statements.
#' The resultant \code{base::data.frames} objects to \code{Vo}, \code{VoStack} and \code{VoMultiStack} classes
#' if accessing downloaded energy data (via the "energy" form argument) or
#' .\code{VoCom}, \code{VoComStack} and \code{VoComMultiStack} classes if accessing communicated voltage
#' data when the tag checks into the system (via the "comm" form argument).
#' The user can also specify \emph{start} and and \emph{end} DateTimes (see below)
#' to specify subsections from the entire dataset available in repositories for individual
#' animals.
#'
#' @param TagID The individual tag identifier from the UvA-BiTS database.
#' @param start The start time to delineate subsets of the total data for the individual in UTC.
#' @param end The end time to delineate subsets of the total data for the individual in UTC.
#' The start and end times must be give in the format "2014-06-25 15:20:23".
#' Arguments \emph{start} and \emph{end} default to NULL, reading in the entire track
#' of the animal and must be given in' \emph{character()} format, which are then converted to
#' \code{base::POSIXct} format within the \code{read_track_MB} function. Functionality is also
#' available for situations where only one or other of start and end need to be specified,
#' with the the other defaulting to NULL.
#' @param form The form of the voltage to return, being two options as a character variable of: (1) "energy"
#' for the direct energy measurements of the tag that have been \emph{downloaded} from the tag; and (2) "comm"
#' for the communication voltage values, that give voltage info every time the tag connects with the system,
#' should this be available for the TagID (note it isn't for all!). Defaults to "energy".
#'
#' @return If the option "energy" is chosen in argument "form", then the final data outputted for individual animals
#' is a \code{Vo and data.frame} object of 15 variables (\emph{many of which are NA and not important)} of the following:
#' "TagID", "DateTime", "timesynced", "devicemode", "vsll", "vbat", "ssw", "vsoo", "vdrf", "vchr", "g", "c", "m", "pressure", and "temperature".
#' For energy (assessing charging etc), those of interest are: \strong{"vsll" and "vbat"} that give the solar charging and battery storage voltage (V).
#' If the form argument "comm" is selected, then a different layout is returned with a whole host of columns
#' that I'm not going to list here - many are often blank and NA, and I'm not sure this is consistently maintained,
#' however, of use "may" be the "vbat" and "bytes_communicated" columns. This again is a dataset obtained
#' each time the animal connects with the UvA-BiTS relay/base station network (you can see it in the raw log files actually)
#' and so theoretically it is possible to keep up to date with the tag voltage this route, but....
#' it is not clear why for some TagIDs this database view is not available and other it is, so it
#' is more inconsistent than the "energy" form argument.
#'
#' @seealso [MoveRakeR::read_track_UvA], [MoveRakeR::read_pressure_UvA], [MoveRakeR::read_accn_UvA]
#'
#' @examples
#'
#' db_file <- "GPS" # what you named the ODBC link on set up
#' db <- RODBC::odbcConnect(db_file) # currently required "db" to be named globally
#'
#' TagID = c('1', '2', '3') # replace with real TagIDs
#' start = c("2020-06-01 13:53:50", "2020-06-15 12:22:13", "2020-06-05 08:07:23")
#' end = c("2020-07-15 09:17:14", "2020-07-20 01:08:58", "2020-07-18 14:22:45")
#' dataVo <- read_voltage_UvA(TagID, start=start, end=end, form = "energy")
#' class(dataVo)
#'
#' # check voltage communication data
#' dataVoCom <- read_voltage_UvA(TagID, start=start, end=end, form = "comm")
#'
#' ####################
#' # Can just specify one date start/end for multiple birds too
#' start = c("2021-06-15 06:00:00")
#' end = c("2021-06-25 09:00:00")
#' dataVo <- read_voltage_UvA(TagID, start=start, end=end)
#' dataVo <- do.call('rbind', dataVo)
#'
#' # plot method
#' plot(dataVo)
#'
#' # drop unnecessary columns for simplicity
#' dataVo <- subset(dataVo, select = c(TagID, DateTime, vsll, vbat))
#'
#' ########## END EXAMPLE #########
#'
#' @export
read_voltage_UvA <- function(TagID, start=NULL, end=NULL, form = "energy"){

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
  .voltage_prep <- function(TagID, start, end){
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
    if(form == "energy"){

      vars = c("device_info_serial, date_time, timesynced, devicemode, vsll, vbat, ssw, vsoo, vdrf, vchr, g, c, m, pressure, temperature")
      DB <- "gps.ee_energy_limited"

    } else if(form == "comm"){

      vars = c("device_info_serial, date_time, date_time_end, date_time_utc, date_time_end_utc, action, bytes_communicated, status, network_join_time, number_identical_lines, non_ascii_char, sts_time, timesynced, devicemode, vbat, logb, logp, gpsii, comii, accii, swddd, last_gps_fix, latitude, longitude")
      DB <- "gps.ee_comm_limited"

    }

    # options of different views

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

    # for all types read in including calibration
    names(data)[which(names(data) == "device_info_serial")] <- "TagID"

    if(is.null(data)){
        data <- NULL
        warning(paste0("No data for ", TagID, " so excluded"))
    }

    #data <- data.table::as.data.table(data)
    return(data)

  }

  # if you have NULL start or ends, then coerce the .track.prep function to accept through mapply
  if(is.null(start)){start <- rep("NULL",length(TagID))}
  if(is.null(end)){end <- rep("NULL",length(TagID))}

  # stacked
  stacked <- mapply(.voltage_prep, TagID=TagID, start=start, end=end)

  if(ncol(stacked) > 1){
    # if bird's return NULL data, then need to comvert to matrix array style of the lists as from mapply ordinarily
    stacked[sapply(stacked, is.null)] <- NULL # removes any null data
    stacked <- simplify2array(stacked, higher = (TRUE == "array")) # puts in matrix array class format
  } else if(is.null(stacked)){stop("No data extracted")}

  # need to also drop out the offending Tag with no data if there is one,
  # from the original TagID fed from e.g. a control file
  # and this retains the correct order as was in "stacked"
  TagID = unique(unlist(stacked[1,]))

  ########### Class assignment for outputs
  # if just one animal, assign Track class and be done with read in
  if(ncol(stacked) == 1){
    if(form == "energy"){
      dat = Vo(as.data.frame(stacked[,1]))
    } else if(form == "comm"){
      dat = VoCom(as.data.frame(stacked[,1]))
    }
  }

  # multiple animals list together and make TrackStack class as well as Track for individual data.frames
  if(ncol(stacked) > 1){

    # stacking the data read in
    dat <- list()
    for(b in 1:ncol(stacked)){
      if(form == "energy"){
        dat[[b]] <- Vo(as.data.frame(stacked[,b])) # assign Vo class to each dataframe
      } else if(form == "comm"){
        dat[[b]] <- VoCom(as.data.frame(stacked[,b])) # assign VoCom class to each dataframe
      }

    }
    names(dat) <- TagID # name data frames by TagID

    if(form == "energy"){
      dat = VoStack(dat)
    } else if(form == "comm"){
      dat = VoComStack(dat)
    }
  }
  return(dat)

} # close function


















