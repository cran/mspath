\name{slave}
\alias{slave}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Prepare for Distributed Multi-State Path Model Computations on Slaves}
\description{
  This function kicks off the main loop of distributed slave processes.
  The master process sends them work, and eventually an indication to
  shut down.  At that point the function shuts down \R.
}
\usage{
slave(comm = 0)
}
\arguments{
  \item{comm}{An MPI communicator to use to talk to the master process}
}
\details{
  The slave's outer loop receives the arguments from which to construct
  a local calculator.  It will exit if it gets \code{NULL} arguments.
  Once the calculator is set up, the slave recieves free parameters
  values.  Finally, it repeatedly asks the master for cases to evaluate
  and returns the results of evaluating those cases to the master.
  Generally there will be many sets of cases evaluated for each set of
  parameters, and many sets of parameters for each set of arguments.
}
\value{Returns likelihoods to the master process.  This function never
  returns, since it shuts down the R process when done.
}
\author{Ross Boylan}
\note{Usually invoked from \code{\link{runeverywhere}}.  You must set up
  the necessary MPI environment for the slave to do useful work.  Do not
  load the Rmpi library, but be sure that \code{library(Rmpi)} will be
  able to access the library.
}
\seealso{\code{\link{runeverywhere}}, \code{\link{master}},
  \code{\linkS4class{mspathDistributedCalculator}},
  \pkg{Rmpi}
}
\keyword{utilities }% at least one, from doc/KEYWORDS
\keyword{ models}% __ONLY ONE__ keyword per line
\concept{distributed}
\concept{MPI}
