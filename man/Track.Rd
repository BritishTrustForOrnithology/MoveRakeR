% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classes.R
\name{Track}
\alias{Track}
\alias{TrackStack}
\alias{TrackMultiStack}
\alias{Trip}
\alias{Trip2}
\alias{TripStack}
\alias{TripMultiStack}
\title{Track constructor classes for MoveRakeR}
\usage{
Track(data)

TrackStack(data)

TrackMultiStack(data)

Trip(data)

Trip2(data)

TripStack(data)

TripMultiStack(data)
}
\arguments{
\item{data}{An input base::data.frame for a tracking dataset that is to be
coerced to the designated class.}
}
\value{
For tracking data, returns a \code{Track}, \code{TrackStack} or
\code{TrackMultiStack} class object and for Trip data, returns a
\code{Trip}, \code{TripStack} or \code{TripMultiStack} object.
}
\description{
Functions to append object classes of \code{Track}, \code{TrackStack} and \code{TrackMultiStack},
obtained respectively using their respective namesake functions respectively; These
object classes are used in the \code{MoveRakeR} workflow, but are not essential for operation.
}
\details{
Three special object classes are returned from initial read in functions such as
\code{\link{read_track_MB}} and \code{\link{read_track_UvA}}. \code{Track} is typically representative
of an individual animal's time-series, but an 'rbinded' \code{Track}
object of several animals, i.e. multiple TagIDs, will also be accepted as a \code{Track}
object and processed for each individual, thus \code{Track} objects represent a single \code{data.frame} class; \code{TrackStack} and
\code{TrackMultiStack} objects are nested \code{list} formats, and (as currently implemented)
\emph{must be treated as one individual per data.frame}. These stacked listed methods
are intended for efficient application of routines for groups of animals. Objects can be coerced into
\code{Track}, \code{TrackStack} and \code{TrackMultiStack} classes but they require the following
minimum column names: \strong{TagID}, \strong{DateTime}, \strong{longitude} and \strong{latitude} -
see \code{\link{read_track_MB}} and \code{\link{read_track_UvA}}
for details of the full list of named variables contained from those extractions.
Further checking of whether objects belong to special classes are provided by: \code{link{is_Track}},
\code{link{is_TrackStack}} and \code{\link{is_TrackMultiStack}}, and coercing of two
\code{TrackStack} objects is provided by \code{\link{track_multi_stack}}.

Further classes are specified for "trips" in the same stacked format as above
coercing to \code{Trip}, \code{TripStack} and \code{TripMultiStack} after
both \code{\link{define_trips}} and \code{\link{trip_stats}} functions have been run,
Trip classes eventually generated by the latter function.
}
\seealso{
\link{Accn}, \link{Pr}, \link{track_multi_stack}, \link{is_Track}, \link{is_TrackStack}, \link{is_TrackMultiStack}, \link{is_Trip}, \link{is_TripStack}, \link{is_TripMultiStack}
}
