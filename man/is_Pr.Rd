% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classes_pressure.R
\name{is_Pr}
\alias{is_Pr}
\alias{is_PrStack}
\alias{is_PrMultiStack}
\alias{is_Pr2}
\title{is_Pr}
\usage{
is_Pr(x, strict = TRUE)

is_PrStack(x, strict = TRUE)

is_PrMultiStack(x, strict = TRUE)

is_Pr2(x, strict = TRUE)
}
\arguments{
\item{x}{Object to be assesed.}

\item{strict}{For is_ functions, logical for should the class check search strictly for the
specific sub-class, e.g. PrStack, or PrMultiStack (default = TRUE), but if FALSE
then the function checks as a normal "is" or inherits for the super class, e.g. Pr (even if object is a PS or PMS).}
}
\value{
Logical \code{TRUE} if \code{x} belongs to class \code{\link{Pr}}, \code{\link{PrStack}} or \code{\link{PrMultiStack}}, otherwise returns \code{FALSE}.
}
\description{
Check that an object is of class \code{\link{Pr}}, class \code{\link{PrStack}} or class \code{\link{PrMultiStack}}.
}
