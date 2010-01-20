\name{estimateWork}
\alias{estimateWork}
\alias{estimateWork-methods}
\alias{estimateWork,mspathCalculator-method}

\title{Estimate Amount of Work for Computations}

\description{
  Compute estimates of the amount of work a calculator will need to
  do for each individual case.  These may be much quicker to compute
  than the actual likelihood evaluation.}
\usage{
estimateWork(calc)
}

\arguments{
  \item{calc}{The calculator for which we want the work estimate.
    Should be \code{\linkS4class{mspathCalculator}} or subclass.}
}
\details{
  Subject to change.  Various estimates of node and path counts, for the
  particular model and data currently in the calculator.
  I think this always does all cases, but need to check.
}
\value{A \code{matrix} whose rows are individual cases.  The columns are
  \item{ID }{ID of the case for this row.}
  \item{Good Nodes}{Presumed to be the best single predictor of effort,
    in particular of floating-point operations.  This is the number of
    unique nodes on good paths.}
  \item{Good Paths}{Number of distinct paths for this case, consistent
    with the model and the data.}
  \item{Bad Nodes}{Number of nodes considered that fell outside of the
    good paths.  May vary with the pruning strategy used, but at any
    rate note that these nodes need not be constructed.}
  \item{Good Path Nodes}{This is the sum of all the nodes in all the
    good paths, ignoring the fact that some nodes are shared between
    paths.  This is a measure of the work effort that a naive
    implementation would require.  The current implementation is not
    naive.}
  \item{Steps}{The average number of steps on the good paths.  If some
    paths can end before others, this value may not be an integer.}
}
\section{Methods}{
\describe{

\item{calc = "mspathCalculator"}{standard case}
}}
\note{ The returned value is not stored in the calculator.
  
\cite{\linkS4class{mspathDistributedCalculator}} uses this function to schedule
work.

}
\author{Ross Boylan}
\seealso{\cite{\linkS4class{mspathCalculator}}, \code{\link{results}}.}
\keyword{utilities}% at least one, from doc/KEYWORDS
\keyword{methods}
\keyword{univar}
