% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_UvA_Animals.R
\name{get_UvA_Animals}
\alias{get_UvA_Animals}
\alias{get_UvA_station_name}
\alias{get_uva_trackingfile}
\alias{get_uva_length_TagID}
\title{Access animal IDs and station_name from the UvA-BiTS database}
\usage{
get_UvA_Animals(station_name = NULL)

get_UvA_station_name()

get_uva_trackingfile(TagID = NULL)

get_uva_length_TagID(TagID = NULL)
}
\arguments{
\item{station_name}{If provided, \code{get_UvA_Animals} searches for the chosen station_name
from the UvA-BiTS database, defaults to NULL, i.e. all TagIDs across all station_names returned.}

\item{TagID}{The TagID of the animal matching 'device_info_serial' on the UvA-BiTS pgAdmin portal; defaults to NULL returning all birds
but any custom selections of animals possible.}
}
\value{
Simple dataframes with information based on the UvA-BiTS format of those table views,
but subsetted and renamed columns.\code{get_UvA_station_name()}, returns simply, three columns of:
\emph{keyname}, \emph{station_name} and \code{project_id}, and then for \code{get_UvA_Animals}, columns
of \emph{project_id}, \emph{TagID}, \emph{DateTime_start}, \emph{DateTime_end}, \emph{key_name} and
\emph{station_name} are returned.
}
\description{
Access the UvA-BiTS database views to obtain TagIDs and station_names
}
\details{
Two functions \code{get_UvA_station_name} and \code{get_UvA_Animals} allow the user to query the
UvA-BiTS database to assess what station_names (i.e. seabird colonies perhaps) are available to the user
and what TagIDs may have been deployed at those sites. This is useful if you want to extract all the data
for a given site between time periods, and saves having to look up TagIDs manually. Note,
\code{get_UvA_station_name} is called from \code{get_UvA_Animals}.

Further functions now added for \code{get_uva_length_TagID} to query gps.ee_tracking_speed_limited via SQL on the server how many GPS data rows we
have per TagID, and \code{get_uva_trackingfile} to query gps.ee_trackingfile_limited for info on log files on the system and data amounts.
}
\examples{

db.file <- "GPS" # what you named the ODBC link on set up
db <- RODBC::odbcConnect(db.file) # currently required "db" to be named globally

# Find out which animals we have at Isle of May
get_UvA_Animals("May") # Colony name
get_UvA_Animals() # returns all TagIDs across sites

# Assess what sites you have data for under your account
get_UvA_station_name()

# put it all together by reading in all TagIDs for a one month period for selected colony
test <- read_track_UvA(TagID = get_UvA_Animals("May")$TagID, start = "2020-02-01 01:00:00", end = "2020-03-01 01:00:00")

# also now options to query number of rows per TagID
get_uva_length_TagID( TagID = c('1', '2')) # replace 1 and 2 with actual TagID names
all_fixes = get_uva_length_TagID()

test = get_uva_trackingfile( TagID = c('1'))
unique(test$TagID)
sum(test$n_errors)

}
\seealso{
\link{read_track_UvA}
}
