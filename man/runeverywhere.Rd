\name{runeverywhere}
\alias{runeverywhere}

\title{Top Level of Distributed Calculation for Multi-State Path Model}
\description{
  Ordinarily every node in a distributed calculation invokes this
  function.  It determines whether it is running on a master (rank 0) or
  slave, and invokes the appropriate function.  This permits SPMD-style
  programming despite the difference between the master and slave
  processes.
}
\usage{
runeverywhere(channel = "kickStart.R", comm = 0)
}
\arguments{
  \item{channel}{The source of commands to execute on the master
    process.  See \code{\link{master}} for details.}
  \item{comm}{ The MPI communicator to use.  In most cases, the default
    should suffice.}
}
\details{This is just a thin wrapper around \code{\link{master}} and \code{\link{slave}}.
}
\value{
  Terminates the \R session at the end.
}
\author{Ross Boylan}
\note{ Do \emph{not} load the \code{Rmpi} library before invoking this
  function.  \emph{Do} set up the MPI environment and launch all the
  necessary \R processes.
}
\seealso{\code{\link{master}}, \code{\link{slave}}}
\keyword{models }% at least one, from doc/KEYWORDS
\keyword{utilities}% __ONLY ONE__ keyword per line
\concept{distributed}
\concept{MPI}

