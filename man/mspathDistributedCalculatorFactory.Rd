\name{mspathDistributedCalculatorFactory}
\alias{mspathDistributedCalculatorFactory}

\title{Make a Constructor for mspathCalculator Objects}
\description{
  This function returns a function that behaves like
  \code{\link{mspathCalculator}}, except that it produces a distributed
  calculator as the result of the call.  The object returned is
  sometimes referred to as a factory or a constructor.
}
\usage{
mspathDistributedCalculatorFactory(
comm=0,
profile = FALSE)
}
\arguments{
  \item{comm}{The MPI communicator to use.  The default ordinarily
    suffices.}

  \item{profile}{\code{TRUE} to enable profiling.}
}
\value{Returns a function that, when called with the arguments to
  \code{\link{mspathCalculator}}, produces a new
  \cite{\linkS4class{mspathDistributedCalculator} object.  That object
    will have the distributed calculator-specific arguments set with the
  values used in the calling this function.}. 
}
\author{Ross Boylan}
\seealso{\code{\linkS4class{mspathDistributedCalculator}},
    \code{\link{mspathDistributedCalculator}},
    \code{\link{mspathCalculator}},  
  \code{\link{mspath}}, \pkg{Rmpi}
}
\keyword{models }% at least one, from doc/KEYWORDS
\concept{distributed}
\concept{MPI}
