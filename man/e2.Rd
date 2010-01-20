\name{e2}
\alias{e2}
\docType{data}
\title{Allowed Misclassifications: e2}
\description{
  Matrix of allowed transitions (e for errors) among 5 states.
}

\format{
  A 5x5 \code{matrix} of 0's and 1's indicating allowed
  misclassifications among real and observed states.  The diagonal is
  ignored.
}
\details{
  This matrix says that a given true state may be observed as being up
  to 2 states below the true state.  However, the highest state is
  observed accurately.  True states are never observed as being in a
  higher than true state.
}
\source{
  Made up.
}
\examples{
library(mspath)
data(e2, q2, sim2)
 r <- mspath(fib~time, misc=TRUE, ematrix=e2, qmatrix=q2, inits=rep(.5, 9), subject=id,
             data=sim2, stepnumerator=1, stepdenominator=1, initprobs=c(1.0, 0, 0, 0, 0),
             do.what=0)
}
\keyword{datasets}
