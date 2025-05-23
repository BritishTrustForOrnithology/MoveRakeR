% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classes_voltage.R
\name{is_VoCom}
\alias{is_VoCom}
\alias{is_VoComStack}
\alias{is_VoComMultiStack}
\alias{is_VoCom2}
\title{is_VoCom}
\usage{
is_VoCom(x, strict = TRUE)

is_VoComStack(x, strict = TRUE)

is_VoComMultiStack(x, strict = TRUE)

is_VoCom2(x, strict = TRUE)
}
\arguments{
\item{x}{Object to be assesed.}

\item{strict}{For is_ functions, logical for should the class check search strictly for the
specific sub-class, e.g. VoStack, or VoMultiStack (default = TRUE), but if FALSE
then the function checks as a normal "is" or inherits for the super class, e.g. VoCom (even if object is a VComS or VComMS).}
}
\value{
Logical \code{TRUE} if \code{x} belongs to class \code{\link{VoCom}}, \code{\link{VoComStack}} or \code{\link{VoComMultiStack}}, otherwise returns \code{FALSE}.
}
\description{
Check that an object is of class \code{\link{VoCom}}, class \code{\link{VoComStack}} or class \code{\link{VoComMultiStack}}.
}
