\name{q2}
\alias{q2}
\docType{data}
\title{Allowed Transitions: q2}
\description{
  Matrix of allowed transitions among 5 states.
}

\format{
  A 5x5 \code{matrix} of 0's and 1's indicating allowed transitions
  betweeen states.  The diagonal is ignored.
}
\details{
  This matrix allows only transitions up to the next-highest state.  It
  is appropriate, for example, for progressive diseases in an analysis
  for which the time step on the path is sufficiently small that
  transitions across multiple states are not present in a single step.
}
\source{
  Made up.
}
\examples{
library(mspath)
data(q2, e2, sim2)
 r <- mspath(fib~time, misc=TRUE, ematrix=e2, qmatrix=q2, inits=rep(.5, 9), subject=id,
             data=sim2, stepnumerator=1, stepdenominator=1, initprobs=c(1.0, 0, 0, 0, 0),
             do.what=0)
}
\keyword{datasets}
