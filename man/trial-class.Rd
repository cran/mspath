\name{trial-class}
\docType{class}
\alias{trial-class}
\alias{trial}

\title{Class "trial"}
\description{Holds results of a trial done by \code{\link{doTrials}}}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{trial(args, results)}.
}
\section{Slots}{
	 \describe{
    \item{\code{args}:}{The \code{"list"} of arguments for this trial,
      as produced by \code{\link{doTrials}}' \code{generator}.}
    \item{\code{result}:}{The interesting information from the trial, as
      returned by \code{\link{doTrials}}' \code{executor}.}
  }
}
\section{Methods}{
No methods defined with class "trial" in the signature.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Ross Boylan \email{ross@biostat.ucsf.edu}}
\note{ You may wish to provide methods that are meaningful for your
  particular analysis.  You can access the raw results with the basic
  slot syntax, e.g., \code{aTrial@args}}

\seealso{To understand the context in which this is used, see \code{\link{doTrials}}.
}
\keyword{classes}
\keyword{optimize}
\keyword{iteration}
\keyword{utilities}
\keyword{datagen}
\keyword{models}
