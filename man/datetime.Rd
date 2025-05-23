% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datetime.R
\name{datetime}
\alias{datetime}
\title{Wrapper to convert date-times to \code{POSIXct} format}
\usage{
datetime(
  DateTime,
  auto = TRUE,
  format = "\%d/\%m/\%Y \%H:\%M:\%S",
  tz = "UTC",
  na.omit = TRUE
)
}
\arguments{
\item{DateTime}{The single or vector  of character DateTimes to be converted.}

\item{auto}{Logical for whether autamatic detection of DateTimes are to be used, or if
specific format and tz arguments are to be used instead, defaults to TRUE.}

\item{format}{If auto is FALSE, then the format argument is used to provide a specific
format for \code{as.POSIXct} to accept, defaulting to "\%d/\%m/\%Y \%H:\%M:\%S".}

\item{tz}{Similar to format, if auto is FALSE, then the time zone argument for
\code{as.POSIXct} can be provided, defaulting to "UTC".}
}
\value{
A converted vector of DateTimes in \code{POSIXct} format. NA will be returned if there
is a mismatch in the character vector and the required DateTime format conversion.
}
\description{
Wraps the \code{base::as.POSIXct} function to enable easy character transformation
for routinely-used formats.
}
\details{
Often it is a task to transform character vector DateTimes to POSIX format for
proper manipulation. This function gives a very basic wrap of this to allow automatic
detection of the format provided for SOME formats, or allow the user to specify,
and to be applied of listed Track-style objects. Acceptable "automatic" detections
are only provided for: "YYYY-mm-dd", "yy-mm-dd", "dd/mm/YYYY", "dd/mm/yy" dates and
"HH:MM:SS", "HH:MM" times in all combinations.
}
