\name{effort}
\alias{effort}
\alias{effort-methods}
\alias{effort,mspathCalculator-method}

\title{Get measures of effort to perform a calculation}
\description{
  Generic function for calculators returning estimates of the amount
  of work involved in the computation.
  Exact details are likely to change.}
\usage{
effort(calc)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{calc}{The \code{\linkS4class{mspathCalculator}} or subclass for
    which we want the effort estimate.}
}
\details{
  Only call this after invoking \code{\link{calculate}}.
}

\value{A single number giving the best estimate of likely
  effort for the active cases.  This is useful for gauging the
  \emph{relative} times of 
  different subsets; the absolute values have no particular
  meaning.  In particular, they are not in seconds.Subject to change and the exact history of the calculator.}

\section{Methods}{
  \describe{
    \item{calc = "mspathCalculator"}{standard case}
}}

\note{In the future values disaggregated down  to individual cases or
  some intermediate level may be returned.}

\author{Ross Boylan}

\seealso{\code{\linkS4class{mspathCalculator}},
  \code{link{estimateWork}} for a richer set of information on likely
  effort.
}

\keyword{utilities }% at least one, from doc/KEYWORDS
\keyword{methods}
\keyword{models}


