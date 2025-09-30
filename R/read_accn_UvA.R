#' Read in acceleration data from the University of Amsterdam Bird Tracking (UvA-BiTS) database.
#'
#' @description
#' Connect and download acceleration data directly from the UvA-BiTS phpPGAdmin SQL database.
#' WARNING: THIS FUNCTION CAN READ IN A LOT OF DATA, USE SPARINGLY!
#'
#' @details
#' As with the function \code{\link{read_track_UvA}}, and \code{read_pressure_UvA}, \code{read_accn_UvA}  allows the user to extract sections
#' of data across multiple individuals and UTC date-times. This function liaises with the
#' table view in the UvA database called "ee_acceleration_limited" that contains
#' x, y, z data together with date-time stamps. Hopefully the user will have
#' specified a synced GPS sensor measurement with the acceleration so a simple merge of the date_times will then be
#' needed to align the positional and the pressure sensor data. NOTE IF THAT IS NOT THE CASE,
#' GET IN TOUCH AS IT WOULD REQUIRE A DIFFERNT DATA MANIPULATION.
#'
#' The accelerometer for UvA tags has been used in classification algorithms to define
#' animal behaviour in very fine detail. The user will have specified a number of sample measurements to be taken in the birdtracking
#' groundstation software. For 20Hz, a minimum of 20 measurements per second are typically specified to allow
#' subsequent use in algorithms to define behaviour. This function,
#' reads in the RAW accelerometer xyz data, for example for visualisation and further analysis.
#' However, for Lesser Black-backed Gulls, a random forest machine-learning classifier has been developed at the
#' University of Amsterdam (Shamoun-Baranes et al. 2017) that on request can be used to
#' provide a 10-fold classification per accelerometer dateTime second time stamp. When linked back to
#' the GPS position, it provides a powerful way of investigating animal behaviour. If behavioural classes are
#' required having deployed UvA tags then it is best to contact UvA-BiTS directly. Further,
#' there is also the assumption that the model originally used by Shamoun-Baranes et al. (2017), a
#' supervised method, is congruent and transferable to Lesser Black-backed Gulls elsewhere.
#'
#' As with the read.track functions, \code{read_accn_UvA} is designed to do work with a control file of
#' start and end date_times per animal for TagIDs can be used as a lookup table.
#' WARNING: \code{read_accn_UvA} can extract large amounts of data.
#' It is therefore advisable to make as few direct calls to the databases and extract the data
#' needed to be stored and then read in again for subsequent analytical steps.
#'
#' \code{read_accn_UvA} uses an ODBC connection to establish a link with the UvA-BiTS
#' database. WARNING: THIS MUST BE SET UP FIRST BEFORE USING THIS FUNCTION
#' and of course you must have prior access to the UvA-BiTS system with a username and password. Steps to do
#' this are provided here: \url{https://wiki.e-ecology.nl/index.php/How_to_access_the_e-Ecology_database}.
#' Extraction of GPS positional data from the database is made using R library \code{RODBC}
#' and the user must also first establish the database connection in their global environment
#' named as object \strong{'db'}, specifying whet the connection was named as in the help file steps above:
#' in this case called "GPS", using \code{RODBC::odbcConnect("GPS")}. See examples.
#'
#' \code{read_accn_UvA} then uses \code{RODBC} to construct SQL statements.
#' The resultant \code{base::data.frames} objects to \code{Accn}, \code{AccnStack} and \code{AccnMultiStack} classes.
#' The user can also specify \emph{start} and and \emph{end} DateTimes (see below)
#' to specify subsections from the entire dataset available in repositories for individual
#' animals. This function is very similar to read.pressure.UvA and could even be written as one,
#' but separated here for simplicity.
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
#'
#' @return The final data outputted for individual animals is a \code{Accn and data.frame} object of 29 variables for each
#' TagID; Type, TagID, pressure_avg (MB) and temperature_av (deg C). A further id column from the database is also retained.
#' It is assumed here the user will be familiar with these expected layouts.
#' Data at the individual level are always provided as a \code{Accn} class for multiple
#' TagIDs, a \code{AccnStack} object is returned.
#'
#' @references
#' Shamoun-Baranes, J., Bouten, W., van Loon, E. E., Meijer, C. & Camphuysen, C. J. (2016)
#' Flap or soar? How a flight generalist responds to its aerial environment.
#' Philosophical Transactions of the Royal Society B, \strong{371}, \emph{20150395}.
#'
#' @examples
#'
#' db.file <- "GPS" # what you named the ODBC link on set up
#' db <- RODBC::odbcConnect(db.file) # currently required "db" to be named globally
#'
#' ## Using read_accn_UvA - read in three birds with different start/end times in 2020
#' TagIDs = c('1', '2', '3') # replace with real TagIDs
#' start = c("2020-06-01 13:53:50", "2020-06-15 12:22:13", "2020-06-05 08:07:23")
#' end = c("2020-07-15 09:17:14", "2020-07-20 01:08:58", "2020-07-18 14:22:45")
#' dataAccn <- read_accn_UvA(TagIDs, start = start, end = end)
#'
#' class(dataAccn)
#'
#' @seealso [MoveRakeR::read_track_UvA], [MoveRakeR::read_pressure_UvA]
#'
#' @export
read_accn_UvA <- function(TagID,start=NULL,end=NULL){

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
  .accn_prep <- function(TagID, start, end){
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

    if(is.null(start) & is.null(end)){ # no start and end given
      Query = paste(
        "select", "device_info_serial, date_time, index, x_acceleration, y_acceleration, z_acceleration",
        "from", "gps.ee_acceleration_limited", "where",
        "device_info_serial", "in", blist)
    }
    if(is.null(start) & !is.null(end)){ # end time selected but no start
      Query = paste(
        "select", "device_info_serial, date_time, index, x_acceleration, y_acceleration, z_acceleration",
        "from", "gps.ee_acceleration_limited", "where",
        "device_info_serial", "in", blist,
        "and","date_time <=", entime)
    }
    if(!is.null(start) & is.null(end)){ # start time given but no end
      Query = paste(
        "select", "device_info_serial, date_time, index, x_acceleration, y_acceleration, z_acceleration",
        "from", "gps.ee_acceleration_limited", "where",
        "device_info_serial", "in", blist,
        "and", "date_time >=", sttime)
    }
    if(!is.null(start) & !is.null(end)){
      Query = paste(
        "select", "device_info_serial, date_time, index, x_acceleration, y_acceleration, z_acceleration",
        "from", "gps.ee_acceleration_limited", "where",
        "device_info_serial", "in", blist,
        "and",
        "date_time >=", sttime,
        "and",
        "date_time <=", entime)
    }

    # could do bird by bird for specific dates then.....with multiple sql calls?
    data <- RODBC::sqlQuery(db, query = Query
                            ,as.is=c(FALSE, TRUE, rep(FALSE,20)) # constrain to readin datetime as character WITHOUT forcing to a TZ!
    )

    # then have to align Movetech and UvA in the same way as previous function
    names(data)[which(names(data) == "date_time")] <- "DateTime"
    data$DateTime <- as.POSIXct(data$DateTime,format = "%Y-%m-%d %H:%M:%S",tz="UTC") # convert date-time to correct format - here using "UTC" e.g. if through the year analysis
    names(data)[which(names(data) == "device_info_serial")] <- "TagID"

    if(is.null(data)){
        data <- NULL
        warning(paste0("No pressure data for ", TagID, " so excluded"))
    } else{
      data <- data[order(data$TagID, data$DateTime),]
    }

    #data <- data.table::as.data.table(data)
    return(data)

  }

  # if you have NULL start or ends, then coerce the .track.prep function to accept through mapply
  if(is.null(start)){start <- rep("NULL",length(TagID))}
  if(is.null(end)){end <- rep("NULL",length(TagID))}

  # mapply is a good format
  stacked <- mapply(.accn_prep, TagID=TagID, start=start, end=end)

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
    dat = Accn(as.data.frame(stacked[,1]))
  }

  # multiple animals list together and make TrackStack class as well as Track for individual data.frames
  if(ncol(stacked) > 1){

    # stacking the data read in
    dat <- list()
    for(b in 1:ncol(stacked)){
      dat[[b]] <- Accn(as.data.frame(stacked[,b])) # assign Track class to each dataframe
    }
    names(dat) <- TagID # name data frames by TagID
    dat = AccnStack(dat)

  }
  return(dat)

} # close function


















