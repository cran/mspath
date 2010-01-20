\name{master}
\alias{master}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Start Multi-State Path Model Distributed Calculations on Master Node}
\description{
  This is the top-level function for the master (rank 0) process in a
  distributed computation. It does any necessary setup, and then
  executes the R commands given in the \command{channel} argument.
  After executing the commands it shuts down the slaves and itself,
  exiting \R.
}
\usage{
master(channel = "kickStart.R", comm = 0)
}
\arguments{
  \item{channel}{A source of R commands to execute within the
    distributed environment.  They should create and use a
    \code{\linkS4class{mspathDistributedCalculator}}.  Invoking
    \code{\link{mspath}} with the optional \code{comm} argument will
    satisfy that requirement.}
  \item{comm}{The MPI communicator to use.  Usually the default will suffice.}
}
\details{
  Before invoking this command you must establish the necessary MPI
  environment and launch appropriate slave processes.  This routine does
  not spawn slave processes, in part so it's useful on systems without
  that capability.

  The code you provide by the \code{channel} should not attempt to shut
  things down itself; \code{master} always does so at the end.
}
\value{
  The return value of the final \code{\link[Rmpi]{mpi.exit}}.
}
\author{Ross Boylan}
\note{ Requires package \pkg{Rmpi} exist and be loadable via
  \code{library(Rmpi)}.  It will be loaded automatically; you should not
  load it yourself.

  See the details section for more about the necessary setup.

  Since the function exits \R without saving, you will need to arrange
  to store or display your results.
}
\seealso{\pkg{Rmpi},
    \code{\linkS4class{mspathDistributedCalculator}},
    \code{\link{mspath}}
    }
\keyword{ utilities }% at least one, from doc/KEYWORDS
\concept{ MPI}% __ONLY ONE__ keyword per line
\concept{distributed}
