\name{alldone}
\alias{alldone}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Distributed Calculation Shut-Down}
\description{Shut down the distributed calculation}
\usage{
alldone(comm = 0)
}
\arguments{
  \item{comm}{The MPI communicator to shut down.}
}
\details{
  Note that the distributed environment itself may remain and need
  to be shutdown separately.  For example, if \command{lamboot}
  started the MPI session, you still need to execute \command{lamhalt}.

  Only the master (rank 0) process should execute this command.
}
\value{
  The return value from \command{mpi.exit()}.
}
\author{Ross Boylan}
\seealso{\code{\link{master}},
  \code{\linkS4class{mspathDistributedCalculator}},
  \pkg{Rmpi}
}
\keyword{environment} % at least one, from doc/KEYWORDS
\concept{distributed }% __ONLY ONE__ keyword per line
\concept{MPI}
