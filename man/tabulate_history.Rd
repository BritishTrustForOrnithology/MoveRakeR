% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tabulate_history.R
\name{tabulate_history}
\alias{tabulate_history}
\title{Tabulated datetime summary history file of raw tracking data}
\usage{
tabulate_history(data, by = NULL)
}
\arguments{
\item{data}{Input data object, with required columns: TagID, DateTime, longitude, latitude.}

\item{by}{Additional vector of characters referring to other levels to evaluate a summary by.
If "year" is chosen and year is not in the data, then this is added via \code{lubridate::year()}.}
}
\value{
A \code{tbl} of TagID, start, end and number of fixes, duration (end minus start) and duration of monitoring,
using gapsections as determined through the user's selection earlier in the process e.g. \code{clean_GPS}. This
function is also used in \code{plot_leaflet_trips} for one of the tabs in the Shiny app.
}
\description{
A simple presentation of start/end of data spans, and number of fixes for Track objects.
}
\examples{

# summarise the data
data_summary <- tabulate_history(data) # ignores any by variable supplied and evaluates by TagID

tabulate_history2(data, by = c("year")) # by year - if year not found, will add in column using lubridate::year()

tabulate_history2(data, by = c("year", "gapsec")) # if wanting any number of further levels, note gapsec also used to evaluate "Dur_days_mon" for true monitoring between gaps in the data

}
