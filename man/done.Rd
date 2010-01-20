\name{done}
\alias{done}
\alias{done-methods}
\alias{done,mspathCalculator-method}
\alias{done,mspathDistributedCalculator-method}

\title{Finished with a Calculator}

\description{
  When you no longer need a calculator, call this function.
  It will perform any necessary cleanup.}
\usage{done(calc)}
\arguments{
  \item{calc}{The calculator you no longer need.  Should be
    \code{mspathCalculator-class} or an extension.}
}

\details{
  This is a generic function.  You should call it even if you
  don't think any cleanup is necessary.  It ensures proper cleanup of
  the \code{C++} objects.  }

\section{Methods}{ \describe{

\item{calc = "mspathCalculator"}{ Clean up a regular calculator.}

\item{calc = "mspathDistributedCalculator"}{Clean up a distributed
  calculator, including letting all slaves no the computation is done.
  However, the overall session will remain up for future use.
  }
}}
\value{Returns the updated \code{calc}, but you shouldn't really refer
  to either the old or new value after calling this function.}
\note{You should call this function even if you
  don't think any cleanup is necessary.  You don't know what is
  happening behind the scenes.}
\author{Ross Boylan}
\seealso{\code{\linkS4class{mspathCalculator}},
  \code{\linkS4class{mspathDistributedCalculator}}}
\keyword{methods}
\keyword{utilities}
