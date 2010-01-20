\name{calculate}
\alias{calculate}
\alias{calculate-methods}
\alias{calculate,mspathCalculator,ANY,ANY,ANY-method}
\alias{calculate,mspathDistributedCalculator,numeric,missing,missing-method}
\title{calculate}
\description{
   Tell a calculator to perform its main calculation.
 }
 \usage{calculate(calc, params, activeCases, do.what)}
 \arguments{
   \item{calc}{The calculator object, \linkS4class{mspathCalculator} or
     an extension.}
   \item{params}{Optional "numeric" vector of the free parameters of the
     calculator.  If missing, use the values already in the calculator.}
   \item{activeCases}{Optional "numeric" vector of ID's of cases over
     which the calculation will be performed.  Note these will be
     converted to \code{integer} before use, and they are case ID's, not
     indices into the data.  \code{activeCases} should be a subset of
     the cases in the calculator; ID's that are not will be ignored.  As
     a special case, \code{integer()} for this argument means
     \dQuote{use all cases.}  If missing, use the values already in the
     calculator.}
   \item{do.what}{Optional \code{integer} giving the code for the type of
     computation to perform.  If missing, use the value already in the
     calculator.}
 }
 \details{Performs the necessary calculation by invoking \code{C++}.
   The calculation may be time-consuming, which is why the computation
   must be invoked by this method rather than happening automatically.
   }
 \value{Returns the \code{calc} argument, updated with the return value
   and any state changes, including those from the optional arguments.
   You \emph{must} use the returned value, not the one passed in as an
   argument, after the call.

  \code{\link{results}} and \code{\link{minus2loglik}}, among other
  methods, will extract the results.  The exact results depend upon the
  mode (\code{do.what}) of the calculation; sometimes only counts of
  paths and related parameters are returned.
   }
\section{Methods}{
\describe{

  \item{calc = "mspathCalculator", params = "ANY", activeCases = "ANY",
    do.what = "ANY"}{
    Perform the multi-state path model calculation.}

  \item{calc = "mspathDistributedCalculator", params = "numeric",
    activeCases = "missing", do.what = "missing"}{
    Distribute the multi-state calculation across processors.  That
    calculation will be for all cases, and always computes the
    likelihood; thus the 2 related arguments \emph{must} be omitted.
    Different subsets of cases are distributed to different processors.
    }
}}
\note{These methods are mostly for internal use.
  You \emph{must} always and exclusively use the return value, not
  the original \code{calc}, after calling this function.  As an
  incentive, this is the only way to get the results.  Results are not
  in a separate object, because returning a separate results object
  would make it awkward to return an updated calculator.}
\author{Ross Boylan}
\seealso{\code{\linkS4class{mspathCalculator}},
  \code{\linkS4class{mspathDistributedCalculator}}
}

\keyword{methods}
\keyword{models}
\keyword{multivariate}
\keyword{nonlinear}
\keyword{survival}
\keyword{interface}
