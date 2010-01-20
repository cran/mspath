\name{runTime}
\alias{runTime}
\title{ runTime constructor}
\description{
  Create a runTime object associated with a particular job.
}
\usage{
runTime(job)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{job}{ Any object, though much of the rest of the system expects
    this to be a vector of the cases analyzed.}
}
\details{
  The constructor also records the current time; thus, when the object
  is created matters.  Later calls will need to set information about
  when the job completed.
}
\value{
  An S4 \code{\linkS4class{runTime}} object.
}
\author{Ross Boylan}
\note{The newly created object is incomplete, since it doesn't have
  information about when the job finished.

  Users are not likely to care about this object or function.
}
\seealso{\code{\linkS4class{runTime}}}
\keyword{programming}
\keyword{environment}
