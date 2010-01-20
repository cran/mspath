\name{isAllFixed-methods}
\docType{methods}
\alias{isAllFixed}
\alias{isAllFixed-methods}
%\alias{isAllFixed,mspathCoefficients-method}
\alias{isAllFixed,mspathEstimatedCoefficients-method}

\title{Test for fixed objects}
\description{Return \code{TRUE} if coefficients have been fixed in
  advance so that there are no estimable values.  Objects without
  estimates are assumed fixed, while those with classes indicating
  estimates but all parameters actually fixed are also fixed.}
\section{Methods}{
  \describe{
    \item{isAllFixed}{\code{signature(object="mspathCoefficients")}:
      \code{TRUE} because all values are fixed in advance.}
    \item{isAllFixed}{\code{signature(object="mspathCoefficients")}:
      usually \code{FALSE}, but \code{TRUE} if all parameters are fixed.}
}}
\note{Motivation: if the user has fixed all the parameter values in
  advance, it's probably not worth showing them again when displaying
  the results.  The anticipated clients are the internal print methods
  of this package.}
\keyword{methods}
