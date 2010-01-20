\name{mspath-class}
\docType{class}
\alias{mspath-class}
\alias{mspathFull-class}

\alias{nBadNodes}
\alias{nCases}
\alias{nGoodNodes}
\alias{nGoodPathNodes}
\alias{nGoodPaths}
\alias{optresults<-}
\alias{optresults}
\alias{printFooter}

%\alias{minus2loglik-methods}
\alias{nBadNodes-methods}
\alias{nCases-methods}
\alias{nGoodNodes-methods}
\alias{nGoodPathNodes-methods}
\alias{nGoodPaths-methods}
\alias{optresults<--methods}
\alias{optresults-methods}
\alias{printFooter-methods}

\alias{minus2loglik,mspath-method}
\alias{coef,mspath-method}
\alias{matrixCoef,mspath-method}
\alias{sd,mspath-method}
\alias{coef,mspathFull-method}
\alias{sd,mspathFull-method}
\alias{nBadNodes,mspath-method}
\alias{nCases,mspath-method}
\alias{nGoodNodes,mspath-method}
\alias{nGoodPathNodes,mspath-method}
\alias{nGoodPaths,mspath-method}
\alias{optresults<-,mspath-method}
\alias{optresults,mspath-method}
\alias{print,mspath-method}
\alias{print,mspathFull-method}
\alias{printFooter,mspath-method}
\alias{show,mspath-method}

\title{Results of a multi-state path calculation}
\description{These classes hold the results of likelihood estimation
  from a call to \code{\link{mspath}}, either with (\code{mspathFull}) or
  without (\code{mspath}) modeling observational measurement error.
}
\section{Objects from the Class}{
  Result of calls to \code{\link{mspath}}.
}
\section{Slots}{
	 \describe{
    \item{\code{transCoef}:}{Estimated transition coefficients, class
      \code{"mspathCoefficients"}.}
    \item{\code{errCoef}:}{Only \code{mspathFull} has this slot, which
      gives the estimated observation error coefficients.  Slot class
      \code{\linkS4class{mspathCoefficients}}.}
    \item{\code{calc}:}{The \code{"mspathCalculator"} used to produce
      this object. }
    \item{\code{overlap}:}{Indices in original data frame that were
      eliminated because of overlapping observations (multiple
      observations in the same discrete time period.  Object of class \code{"numeric"}.}
    \item{\code{call}:}{The call to \code{\link{mspath}} that created
      this object.  Slot has class \code{"call"}. }
    \item{\code{callArgs}:}{A \code{"list"} with the resolved
      values specified by the user in the call.  Each argument captured
      by \dots will be present, but omitted arguments will be absent,
      even if they have defaults.

      To understand the difference between this slot and the previous
      one, suppose one invokes \code{mspath(qmatrix=b4, \dots)}, and
      then later redefines \code{b4}.  Then \code{call} will have
      \code{qmatrix=b4} but \code{callArgs$qmatrix} will have the value
      of \code{b4} at the time of the call.}
    \item{\code{opt}:}{The object returned by the call to
      \code{\link{optim}} that is used for the likelihood maximization.
      Slot class \code{"list"}.}
  }
}
\section{Methods}{
  \describe{
    \item{coef}{Estimated coefficients for the transitions, a
      \code{\linkS4class{mspathCoefficients}} object.}
    \item{matrixCoef}{A list of transition matrices of coefficients.
      The optional \code{coeff} argument is a vector of strings, each
      being the name of a coefficient of interest; otherwise all are
      reported.}
    \item{sd}{\code{signature(x="mspath", na.rm="ANY")}: Estimated
      standard deviation of coefficients, or 0 if they are fixed.
      \code{na.rm} is required for compatibility with the general
      interface for this function, but probably won't work properly.}
    \item{minus2loglik}{\code{signature(x = "mspath")}: -2 time the
      estimated log-likelihood. }
    \item{nBadNodes}{\code{signature(x = "mspath")}: Number of invalid nodes generated. }
    \item{nCases}{\code{signature(x = "mspath")}: Number of cases, i.e.,
      unique id's, in the input data.  The number of individual
      observations will generally be greater than this.}
    \item{nGoodNodes}{\code{signature(x = "mspath")}: Number of unique
      good nodes on these paths.  Note that nodes may be shared between
      paths, and some "good" nodes turn out not to be on any good paths.
      Expected likelihood computation time is roughly proportional to this number. }
    \item{nGoodPathNodes}{\code{signature(x = "mspath")}: Total number
      of nodes in all the good paths.  goodPathNodes/goodPaths is the
      average path length.  goodNodes/goodPathNodes estimates the
      speedup from sharing nodes between paths vs a naive implementation
      that treats each path independently. }
    \item{nGoodPaths}{\code{signature(x = "mspath")}: Number of complete
      good paths on which to compute a likelihood. }
    \item{optresults}{\code{signature(x = "mspath")}: The results of the
      inner call to \code{\link{optim}}. }
    \item{optresults<-}{\code{signature(x = "mspath")}: For internal use
      only. }
    \item{print}{\code{signature(x="mspath")}: print results.  If there
      is an error model, the default behavior is only to show the
      transition or error coefficients if they were not completely fixed
      in advance; use the optional \code{showAll=TRUE} argument to show
      the fixed coefficients as well.  Other arguments, including
      general printing parameters and \code{coeff} are passed to the
      coefficient printing routines.}
    \item{printFooter}{\code{signature(x = "mspath")}: For internal use only. }
	 }
}

\author{Ross Boylan \email{ross@biostat.ucsf.edu}}
\note{ The counts are counts over all cases, so the number good paths,
  for example, is the sum of all the good paths for each case.
  Currently the counts are not reliable for distributed calculations.
  
  This class currently exists mostly to print out the estimated
  coefficients.  It records other information to preserve the context of
  the computation, but doesn't currently do much with it.  As need arises,
  the class will be enhanced.

  In particular, the class only provides the raw coefficients and their
  mapping to cells in the matrices.  It does not multiply by covariates,
  calculate implied transition or error probabilities, or otherwise work
  out the implications of the model.

  The related msm package does work out many of the implications of the
  estimated coefficients (in fact, it does not present the raw
  coefficients at all in its standard print method).  Almost none of the
  operations there (markov, continuous time models) translate directly
  into the context of this model (non-markov, discrete time).  Also, msm
  uses S3 classes, while this package uses S4.  The only user
  implication is that the S4 help system is available.
  
  In principle this class could hold sets of coefficients that did not
  come from maximization; in particular, the coefficients do not have to
  be estimated ones.  Neither the class nor its methods are designed for
  that use, however.
}

\seealso{\code{\link{mspath}} creates these objects.  The key values are
  in \code{\linkS4class{mspathCoefficients}}, which will usually be the
  \code{\linkS4class{mspathEstimatedCoefficients}} subclass.  See
  \code{\link{optim}} for details of its results object, which is
  preserved here.  \code{\linkS4class{mspathCalculator}} describes the
  calculator.
}
%\examples{
%}
\keyword{classes}
\keyword{models}
