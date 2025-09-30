#' Access animal IDs and station_name from the UvA-BiTS database
#'
#' @description
#' Access the UvA-BiTS database views to obtain TagIDs and station_names
#'
#' @details
#' Two functions \code{get_UvA_station_name} and \code{get_UvA_Animals} allow the user to query the
#' UvA-BiTS database to assess what station_names (i.e. seabird colonies perhaps) are available to the user
#' and what TagIDs may have been deployed at those sites. This is useful if you want to extract all the data
#' for a given site between time periods, and saves having to look up TagIDs manually. Note,
#' \code{get_UvA_station_name} is called from \code{get_UvA_Animals}.
#'
#' Further functions now added for \code{get_uva_length_TagID} to query gps.ee_tracking_speed_limited via SQL on the server how many GPS data rows we
#' have per TagID, and \code{get_uva_trackingfile} to query gps.ee_trackingfile_limited for info on log files on the system and data amounts.
#'
#' @param station_name If provided, \code{get_UvA_Animals} searches for the chosen station_name
#' from the UvA-BiTS database, defaults to NULL, i.e. all TagIDs across all station_names returned.
#' @param TagID The TagID of the animal matching 'device_info_serial' on the UvA-BiTS pgAdmin portal; defaults to NULL returning all birds
#' but any custom selections of animals possible.
#'
#' @return
#' Simple dataframes with information based on the UvA-BiTS format of those table views,
#' but subsetted and renamed columns.\code{get_UvA_station_name()}, returns simply, three columns of:
#' \emph{keyname}, \emph{station_name} and \code{project_id}, and then for \code{get_UvA_Animals}, columns
#' of \emph{project_id}, \emph{TagID}, \emph{DateTime_start}, \emph{DateTime_end}, \emph{key_name} and
#' \emph{station_name} are returned.
#'
#' @examples
#'
#' db.file <- "GPS" # what you named the ODBC link on set up
#' db <- RODBC::odbcConnect(db.file) # currently required "db" to be named globally
#'
#' # Find out which animals we have at Isle of May
#' get_UvA_Animals("May") # Colony name
#' get_UvA_Animals() # returns all TagIDs across sites
#'
#' # Assess what sites you have data for under your account
#' get_UvA_station_name()
#'
#' # put it all together by reading in all TagIDs for a one month period for selected colony
#' test <- read_track_UvA(TagID = get_UvA_Animals("May")$TagID, start = "2020-02-01 01:00:00", end = "2020-03-01 01:00:00")
#'
#' # also now options to query number of rows per TagID
#' get_uva_length_TagID( TagID = c('1', '2')) # replace 1 and 2 with actual TagID names
#' all_fixes = get_uva_length_TagID()
#'
#' test = get_uva_trackingfile( TagID = c('1'))
#' unique(test$TagID)
#' sum(test$n_errors)
#'
#' @seealso [MoveRakeR::read_track_UvA]
#'
#' @export
get_UvA_Animals <- function(station_name = NULL){

  # All the trackers available to you
  DB <- "gps.ee_track_session_limited"
  vars = c("device_info_serial, start_date, end_date, project_id")
  Query = paste("select", vars,"from", DB)
  data <- RODBC::sqlQuery(db, query = Query)

  # All the stations available to you
  data2 <- get_UvA_station_name()

  # merge
  data3 <- merge(data, data2,by = "project_id") # could I'm sure do this as a single sql merge!

  # any future dates make NAs
  # Note I'm not sure that the start dates are necessarily reliable e.g. not sure if Others have
  # entered data and used time tag was "started up" and not deployed.....

  data3$start_date <- lubridate::with_tz(data3$start_date, tzone = "UTC")
  data3$end_date <- lubridate::with_tz(data3$end_date, tzone = "UTC")

  names(data3)[which(names(data3) == "device_info_serial")] <- "TagID"
  names(data3)[which(names(data3) == "start_date")] <- "DateTime_start"
  names(data3)[which(names(data3) == "end_date")] <- "DateTime_end"

  # Note, rename any Tags deployed at Havergate
  # haradcoding this for now
  tid_hav <- c(5873,5866,5880,5876,5875,5865,5868,5870,5872,5969,5874,5860,5970,5863,5877,5881)
  data3$station_name[which(data3$TagID %in% tid_hav)] <- "Havergate"
  #data3$station_name[which(data3$station_name == "Orfordness")] <- "Orford Ness"

  if(!is.null(station_name)){
    data3 <- data3[data3$station_name %in% station_name,]
  }

  return(data3)

}

#' @rdname get_UvA_Animals
#' @export
get_UvA_station_name <- function(){

  # All the stations available to you
  DB2 <- "gps.ee_project_limited"
  vars2 = c("key_name, station_name, project_id")
  Query2 = paste("select", vars2,"from", DB2)
  data2 <- RODBC::sqlQuery(db, query = Query2)

  return(data2)
}

#' @rdname get_UvA_Animals
#' @export
get_uva_trackingfile <- function(TagID=NULL){

  DB2 <- "gps.ee_trackingfile_limited"
  vars2 = c("id, date_reported, filename, last_modified_date, device_info_serial, filesize, parsed_date,version,
            last_found_date, n_lines, n_data_lines, n_tracking_records, n_communication_records, n_energy_records, n_acceleration_records,
            n_errors, n_settings_records, filepath")

  if(!is.null(TagID)){

    blist = paste0("'",TagID,"'", sep = ",", collapse = " ")
    blist = substr(blist,1,nchar(blist)-1)
    blist = paste0("(",blist,")")

    Query2 = paste("select", vars2,"from", DB2,
                   "WHERE", "device_info_serial", "IN", blist)
  } else{
    Query2 = paste("select", vars2,"from", DB2)
  }

  data2 <- RODBC::sqlQuery(db, query = Query2)

  names(data2)[which(names(data2) == "device_info_serial")] <- "TagID"

  #if(!is.null(TagID)){
  #  data2 <- data2[data2$TagID %in% TagID,]
  #}
  return(data2)

}

#' @rdname get_UvA_Animals
#' @export
get_uva_length_TagID <- function(TagID=NULL){


  DB2 <- "gps.ee_tracking_speed_limited"
  vars2 =  "device_info_serial, COUNT('device_info_serial') AS n_row, SUM(COUNT('device_info_serial')) OVER() AS total_count"

  if(!is.null(TagID)){

    blist = paste0("'",TagID,"'", sep = ",", collapse = " ")
    blist = substr(blist,1,nchar(blist)-1)
    blist = paste0("(",blist,")")

    Query2 = paste(
      "SELECT",vars2,
      "FROM", DB2,
      "WHERE", "device_info_serial", "IN", blist,
      "GROUP BY device_info_serial")

    # ON SERVER:
    # SELECT device_info_serial, COUNT('device_info_serial') AS n_row, SUM(COUNT('device_info_serial')) OVER() AS total_count
    # from gps.ee_tracking_speed_limited where device_info_serial in (5873, 492) GROUP BY device_info_serial


  } else{

    # all TagIDs

    Query2 = paste(
      "SELECT",vars2,
      "FROM", DB2,
      "GROUP BY device_info_serial")

  }

  data2 <- RODBC::sqlQuery(db, query = Query2)
  names(data2)[which(names(data2) == "device_info_serial")] <- "TagID"
  return(data2)

}




