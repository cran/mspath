\name{results}
\alias{results}
\alias{results-methods}
\alias{results,mspathCalculator-method}

\title{Retrieve Results of Multi-State Path Calculation}
\description{
  Get results of previous calculations.
}
\usage{
results(calc)
}
\arguments{
  \item{calc}{A previously evaluated calculator, which should be a
    \code{\linkS4class{mspathCalculator}} or subclass.}
}
\value{Currently \code{numeric} of length 6
  with the following entries, each being a sum over the active cases:
  \item{1: -2 Log-likelihood}{Minus twice the log-likelihood.  This will
    only be a 
    meaningful value if the calculation had an appropriate
    \code{do.what}.}
  \item{2: Number of Cases}{This counts subjects, each of which may have
  several records.}
  \item{3: Good Paths}{Number of distinct paths consistent with the data
    and the model.}
  \item{4: Good Nodes}{Presumed to be the best single predictor of effort,
    in particular of floating-point operations.  This is the number of
    unique nodes on good paths.}
  \item{5: Bad Nodes}{Number of nodes considered that fell outside of the
    good paths.  May vary with the pruning strategy used, but at any
    rate note that these nodes need not be constructed.}
  \item{6: Good Path Nodes}{This is the sum of all the nodes in all the
    good paths, ignoring the fact that some nodes are shared between
    paths.  This is a measure of the work effort that a naive
    implementation would require.  The current implementation is not naive.}
  Which of those values has useful data depends in particular on the
  value of \code{do.what}.  Other, more complex, types of return values
  are possible.
}
\section{Methods}{
\describe{

\item{calc = "mspathCalculator"}{standard case }
}}
\author{Ross Boylan}
\seealso{\code{\linkS4class{mspathCalculator}, \link{estimateWork}}.}
\keyword{models }% at least one, from doc/KEYWORDS
\keyword{methods}
\keyword{univar}

