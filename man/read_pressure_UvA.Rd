% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_pressure_UvA.R
\name{read_pressure_UvA}
\alias{read_pressure_UvA}
\title{Reading in pressure data from the University of Amsterdam Bird Tracking (UvA-BiTS) database.}
\usage{
read_pressure_UvA(TagID, start = NULL, end = NULL, form = "mean")
}
\arguments{
\item{TagID}{The individual tag identifier from the UvA-BiTS database.}

\item{start}{The start time to delineate subsets of the total data for the individual in UTC.}

\item{end}{The end time to delineate subsets of the total data for the individual in UTC.
The start and end times must be give in the format "2014-06-25 15:20:23".
Arguments \emph{start} and \emph{end} default to NULL, reading in the entire track
of the animal and must be given in' \emph{character()} format, which are then converted to
\code{base::POSIXct} format within the \code{read.track.MB} function. Functionality is also
available for situations where only one or other of start and end need to be specified,
with the the other defaulting to NULL.}

\item{form}{The form of the pressure to return, being three options as a character variable of: (1) "mean"
for the mean pressure values averaged across the n-2 measurements (top and bottom removed) from UvA database "pressure_burst_limited"; (2) "raw" for
the raw pressure and temperature (p) and (t), i.e. I think from absolute measures prior to conversion to MB and degC,
for all measurements (1-10) from database pressure_point_limited; and (3) "calibration", which I think
are tag-specific metrics used in tag sensor calibration, but I don't know how. The option defaults to "mean".}
}
\value{
If the option "mean" is chosen in argument "form", then the final data outputted for individual animals
is a \code{Pr and data.frame} object of six variables of Type, TagID, pressure_avg (MB) and temperature_av (deg C). A further id column from the database is also retained.
If the option "raw" is selected in "form" then the output is a data.frame of seven variables being:
TagID, DateTime, burst_id, flag, point_seq, p and t and if option "calibration" is chosen in "form"
then the data are returned as a data.frame with columns of: TagID, c1, c2, c3, c4, c5, c6 and tracker_id.
It is assumed here the user will be familiar with these expected layouts.
For data returned from the "mean" option, data at the individual level are always provided as a \code{Pr} class for multiple
TagIDs, a \code{PrStack} object is returned. For all other data options, no class is assigned.
}
\description{
Connect and download pressure data directly from the UvA-BiTS phpPGAdmin SQL database.
}
\details{
As with the function \code{\link{read_track_UvA}}, and \code{\link{read_accn_UvA}}, \code{read_pressure_UvA}  allows the user to extract sections
of data across multiple individuals and UTC date-times. This function liaises with the
table view in the UvA database called "ee_pressure_burst_limited" that contains
pressure and temperature data together with date-time stamps. Hopefully the user will have
specified a synched GPS sensor measurement with the pressure so a simple merge of the date_times will then be
needed to align the positional and the pressure sensor data.

The user will have specified a number of pressure measurements to be taken in the birdtracking
groundstation software. A standard of ten measurements per GPS fix are typically recommended. The UvA wizards
then process the data by ranking the samples taken, and removing the highest and lowest and then taking a mean
of the remaining (e.g. eight) points, to give what is seen in the database as an averaged pressure measurement.
The same thing is also done for temperature.

As with the read.track functions, \code{read_pressure_UvA} is designed to do work with a control file of
start and end date_times per animal for TagIDs can be used as a lookup table.
WARNING: \code{read_pressure_UvA} requires access to the UvA-BiTS database in the first place, and further, it is
possible within these read in functions to extract large amounts of data. It is therefore advisable
to make as few direct calls to the databases and extract the data needed to be stored
and then read in again for subsequent analytical steps.

\code{read_pressure_UvA} uses an ODBC connection to establish a link with the UvA-BiTS
database. WARNING: THIS MUST BE SET UP FIRST BEFORE USING THIS FUNCTION
and of course you must have prior access to the UvA-BiTS system with a username and password. Steps to do
this are provided here: \url{https://wiki.e-ecology.nl/index.php/How_to_access_the_e-Ecology_database}.
Extraction of GPS positional data from the database is made using R library \code{RODBC}
and the user must also first establish the database connection in their global environment
named as object \strong{'db'}, specifying whet the connection was named as in the help file steps above:
in this case called "GPS", using \code{RODBC::odbcConnect("GPS")}. See examples.

\code{read_pressure_UvA} then uses \code{RODBC} to contruct SQL statements.
The resultant \code{base::data.frames} objects to \code{Pr}, \code{PrStack} and \code{PrMultiStack} classes.
The user can also specify \emph{start} and and \emph{end} datetimes (see below)
to specify subsections from the entire dataset available in repositories for individual
animals.
}
\examples{

db.file <- "GPS" # what you named the ODBC link on set up
db <- RODBC::odbcConnect(db.file) # currently required "db" to be named globally

## Example using direct calls to UvA-BiTS database using SQL coding for date-time start and ends per animal
# Note, you can extract multiple birds with the same start-ends this way too
TagIDs <- "('1','2','3')" # replace with real TagIDs
sttime <- "'2020-05-30 17:00:00'"
entime <- "'2020-06-02 17:00:00'"

# But you can see the 10 pressure values from the tag (unprocessed)
data <- RODBC::sqlQuery(db, query =
                         paste(
                           "select", "device_info_serial, date_time, burst_id, flag, point_seq, p, t",
                           "from", "gps.ee_pressure_point_limited", "where",
                           "device_info_serial", "in", TagIDs,
                           "and",
                           "date_time >=", sttime,
                           "and",
                           "date_time <=", entime)
)

# Pressure bursts - EXTRACTED FROM THE FUNCTION read_pressure_UvA
data <- RODBC::sqlQuery(db, query =
                         paste(
                           "select", "device_info_serial, date_time, id, pressure_avg, temp_avg", #clock_source, fp, gps_sync_status, prs_iv, prs_mode, prs_sa,
                           "from", "gps.ee_pressure_burst_limited", "where",
                           "device_info_serial", "in", TagIDs,
                           "and",
                           "date_time >=", sttime,
                           "and",
                           "date_time <=", entime)
)

## Using read_pressure_UvA - read in three birds with different start/end times in 2020
TagIDs = c('1', '2', '3')
start = c("2020-06-01 13:53:50", "2020-06-15 12:22:13", "2020-06-05 08:07:23")
end = c("2020-07-15 09:17:14", "2020-07-20 01:08:58", "2020-07-18 14:22:45")
dataPr <- read_pressure_UvA(TagIDs, start = start, end = end)

class(dataPr)

}
\seealso{
\link{read_track_UvA}, \link{read_accn_UvA}
}
