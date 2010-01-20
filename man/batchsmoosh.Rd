\name{batchsmoosh<--methods}
\docType{methods}
\alias{batchsmoosh<-}
\alias{batchsmoosh<--methods}
\alias{batchsmoosh<-,ANY,ANY-method}
\alias{batchsmoosh<-,mspath,mspath-method}
\alias{batchsmoosh<-,list,list-method}
\alias{batchsmoosh<-,mspathFull,mspathFull-method}
\alias{batchsmoosh<-,mspathCalculator,mspathCalculator-method}
\alias{batchsmoosh<-,mspathCoefficients,mspathCoefficients-method}
\alias{batchsmoosh<-,mspathEstimatedCoefficients,mspathEstimatedCoefficients-method}
\title{Methods for Function batchsmoosh<-}
\description{
  Eliminate unnecessary objects from a batch of simulations.

  Replaces the left-hand size, \code{x}, by the right-hand side,
\code{val}, if the two are identical.  Otherwise replaces parts of
\code{x} by identical parts of \code{val}.  May also simply erase bulky
objects from \code{x} if they should be easy to reconstruct.  }

\usage{batchsmoosh(x) <- value # x and value should have same type}
\details{Simulations, particularly distributed simulations, tend to
  produce lots of duplicate information.  Sometimes the information is
  identical from simulation to simulation, but will be represented in
  different objects.  Sometimes the information is simply bulky, such as
  a simulated data set that can be reproduced given only the random
  seed.

  \code{batchsmoosh} reduces memory use by replacing substantively
  identical objects with the same object reference, namely the one on
  the right-hand side.  It also deletes some bulky information entirely.

  It may be wise to preserve one unaltered object to be sure of having
  sample values for everything.

  The function typically operates recursively.
  }
\section{Methods}{
\describe{

\item{x = "ANY", value = "ANY"}{Points \code{x} to the same object as \code{value} if
  \code{identical(x, value)}.}

\item{x = "list", value = "list"}{This will only be effective if
  \code{x} and \code{value} have corresponding entries at comparable
  indices.  They must have the same length.}

\item{x = "mspath", value = "mspath"}{Strips out large data arguments
  and replaces others if they are duplicates.}

\item{x = "mspathFull", value = "mspathFull"}{Handles estimation results.}

\item{x = "mspathCalculator", value = "mspathCalculator"}{Everything
  except the results are blown away.}

\item{x = "mspathCoefficients", value = "mspathCoefficients"}{Replace
  identical subobjects.}

\item{x = "mspathEstimatedCoefficients", value =
  "mspathEstimatedCoefficients"}{Replace identical subobjects.}
}}
\examples{
  # Create simulation results.  In real life, the list elements would
  # be more interesting.
  data <- runif(200)
  dosim <- function(i) list(result=runif(1), input=data, run=i)
  myrun <- lapply(seq(300), dosim)
  # Note that in a typical distributed scenario, each simulation
  # result would have a different input object, even though
  # they would all be substantively identical.

  # Compress the simulation results

  # The master object has 2 roles.
  # First, when substantively identical objects are not the same
  # object, the value in master will be inserted in all places.
  # Second, the master preserves all the original values.
  mymaster <- myrun[[1]]

  mysmoosh <- function(x) {
    batchsmoosh(x) <- mymaster
    x
  }
  myrun <- lapply(myrun, mysmoosh)
  # and preserve one copy of full data
  myrun[[1]] <- mymaster

  # myrun is now a reduced memory version of itself
}
\keyword{methods}
\keyword{internal}

