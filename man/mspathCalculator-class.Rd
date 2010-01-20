\name{mspathCalculator-class}
\docType{class}
\alias{mspathCalculator-class}
\alias{mspathAbstractCalculator-class}

\alias{activeCases}
\alias{activeCases-method}
\alias{activeCases,mspathCalculator-method}

\alias{activeCases<-}
\alias{activeCases<--methods}
\alias{activeCases<-,mspathCalculator,numeric-method}

% commented out items have a separate documentation file

%\alias{calculate,mspathCalculator,ANY,ANY,ANY-method}
%\alias{done,mspathCalculator-method}
%\alias{effort,mspathCalculator-method}
%\alias{estimateWork,mspathCalculator-method}
%\alias{minus2loglik,mspathCalculator-method}

\alias{nActiveCases}
\alias{nActiveCases-methods}
\alias{nActiveCases,mspathCalculator-method}

%\alias{nCases}
%\alias{nCases-methods}
\alias{nBadNodes,mspathCalculator-method}
\alias{nCases,mspathCalculator-method}
\alias{nGoodNodes,mspathCalculator-method}
\alias{nGoodPathNodes,mspathCalculator-method}
\alias{nGoodPaths,mspathCalculator-method}

\alias{optimizeWork}
\alias{optimizeWork-methods}
\alias{optimizeWork,mspathCalculator,numeric,numeric-method}

\alias{params<-}
\alias{params<--methods}
\alias{params<-,mspathCalculator,numeric-method}

%\alias{results,mspathCalculator-method}

\alias{useActualTimes}
\alias{useActualTimes-methods}
\alias{useActualTimes,mspathCalculator,numeric-method}

\alias{simulateObservations}
\alias{simulateObservations-methods}
\alias{simulateObservations,mspathCalculator-method}

\title{Calculator for Multi-State Path Models}
\description{This calculator calculates the likelihood and related
  quantities for a multi-state path model with possible non-Markov
  properties.  Alternately, it can generated a simulated data set.
  Though you can use a calculator directly, it is usually more
  convenient to invoke \code{\link{mspath}}.  If you use it, you must
  follow the rules about the proper sequence in which to invoke
  different methods, and you must always use the possibly updated
  calculator object that many methods return.
    }
    
\section{Objects from the Class}{
Create objects of this class by calling \code{\link{mspathCalculator}}
or \code{\link{mspathCalculatorFromArgs}}.
}
\section{Slots}{
	 \describe{
% if you update these, copy them to mspathDistributedCalculatorClass.Rd
    \item{\code{args}:}{A \code{list} of arguments to the C function call.  As such,
      they must be in the correct order, with all necessary
      \code{as.xxx}'s performed.}
    \item{\code{manager}:}{Reference to the \code{C++} object that
      performs the calculations.  Object of class \code{"externalptr"}.
      Much of the state of the \code{mspathCalculator} is really in this
      \code{C++} object. The persistence of information between calls
      permits faster calculations, but raises the possibility for
      errors.  Do \emph{not} do anything with the \code{manager}. }
    \item{\code{do.what}:}{\code{"integer"} with the code for the
      desired operation.  This will be interpreted by the \code{C++}
      routines, and is subject to being changed as a side-effect of
      user-level operations. }
    \item{\code{subset}:}{\code{"integer"} of the ID's of cases for
      which the likelihood should be evaluated.  The restriction to
      integers makes the interface with \code{C} easier. If this vector
      is empty, it means analyze all cases.}
    \item{\code{results}:}{Object of class \code{"numeric"} holding the
      results of the calculation. In the future, it may hold other
      types.  If there has not been a calculation with the current
      calculator values, this slot will have length 0.  See
      \code{\link{results}} for details.}
  }
}
\section{Methods}{
  \describe{
    Setup:

    \item{activeCases<-}{\code{signature(calc = "mspathCalculator",
	value = "numeric")}:
      \code{value} is a vector of the ID's (which will be converted to
      \code{integer}) of the active subset.  They should be a subset of
      the cases in the calculator; ID's that are not will be ignored.
      To reset to using all cases, do \code{activeCases(calc) <- integer()}.}
    \item{params<-}{\code{signature(calc = "mspathCalculator", value =
	"numeric")}:
      Sets the parameter values, but does not perform any calculations. }

    Calculation:
    
    \item{calculate}{
      \code{signature(calc = "mspathCalculator", params = "ANY",
	activeCases = "ANY", do.what = "ANY")}:
      Perform the requested calculation, return \code{calc} with the
      desired results.  The other arguments are optional; if set, they
      will set the corresponding values in \code{calc} before the
      calculation.  The returned calculator will have those new values. }
    \item{estimateWork}{\code{signature(calc = "mspathCalculator")}:
      Return a \code{matrix} with each row being an individual case and
      each column some quantity that might be related to the amount of
      computation required for the case. See \code{\link{estimateWork}}
      for details.}

    \item{simulateObservations}{\code{signature(object="mspathCalculator")}:
      Return a \code{data.frame} of simulated observations based on the
      actual observations and the model.  See the discussion in
      \code{\link{mspath}} for details.  This method uses R's random
      number generator; the caller must set that up appropriately before
      the simulation.}

    Analysis:
    
    \item{activeCases}{\code{signature(calc = "mspathCalculator")}:
      If a subset of the cases are active, returns an "integer" vector
      of the ID's of the active cases.  Otherwise, return an empty vector. }
    \item{effort}{\code{signature(calc = "mspathCalculator")}:
      Compute a single number giving the best estimate of likely
      effort.  This is useful for gauging the \emph{relative} times of
      different subsets; the absolute values have no particular
      meaning.  In particular, they are not in seconds. }
    \item{minus2loglik}{\code{signature(calc = "mspathCalculator")}: 
      Returns -2 times the computed log-likelihood.  The value is only
      meaningful if you have previously requested a computation. }
    \item{nActiveCases}{\code{signature(calc = "mspathCalculator")}:
      count the number of cases currently under analysis, restricted to
      the active subset if any.}
    \item{nCases}{\code{signature(calc = "mspathCalculator")}:
      The number of cases in the calculator.  This counts all cases, not
      just the active subset.  Each case consists of one or more
      consecutive records with the same ID.}
    \item{nBadNodes}{\code{signature(x = "mspathCalculator")}: Number of
      invalid nodes generated. } 
    \item{nGoodNodes}{\code{signature(x = "mspathCalculator")}: Number of unique
      good nodes on these paths.  Note that nodes may be shared between
      paths, and some "good" nodes turn out not to be on any good paths.
      Expected likelihood computation time is roughly proportional to this number. }
    \item{nGoodPathNodes}{\code{signature(x = "mspathCalculator")}: Total number
      of nodes in all the good paths.  goodPathNodes/goodPaths is the
      average path length.  goodNodes/goodPathNodes estimates the
      speedup from sharing nodes between paths vs a naive implementation
      that treats each path independently. }
    \item{nGoodPaths}{\code{signature(x = "mspathCalculator")}: Number of complete
      good paths on which to compute a likelihood. }
    \item{results}{\code{signature(calc = "mspathCalculator")}:
      Returns the results of the calculations.  You must have requested
      calculations first for this to be meaningful.  Currently it is a
      numeric vector, but that may change.  See \code{\link{results}}
      for details. }

    Cleanup:
    \item{done}{\code{signature(calc = "mspathCalculator")}:
      Call this when you are all done with the computations and this
      calculator.  This ensures proper cleanup of the \code{C++} objects. }

    Internal:
    \item{optimizeWork}{\code{signature(calc = "mspathCalculator",
	effort = "numeric", chunks = "numeric")}:
      for internal use only. }
    \item{useActualTimes}{\code{signature(calc = "mspathCalculator",
	chunks = "numeric")}:
      for internal use only. }
  }
}

\author{Ross Boylan }
\note{Ordinarily one creates a calculator and then asks it to evaluate
  the likelihoods of different parameter sets, finally extracting the
  results.
  \code{
    calc <- mspathCalculator(...)
    calc <- calculate(calc, params)
    ll <- minus2loglik(calc)
    done(calc)
  }
}
\seealso{Creation:
  \code{\link{mspathCalculator}},
  \code{\link{mspathCalculatorFromArgs}}.
  Setup:
  \code{\link{params<-}},
  \code{\link{estimateWork}}.  
  Calculation:
  \code{\link{calculate}},
  \code{\link{estimateWork}}.
  Analysis:
  \code{\link{minus2loglik}},
  \code{\link{effort}},
  \code{\link{results}}.
  Cleanup:
  \code{\link{done}}.
}
\keyword{classes}
\keyword{univar}
\keyword{methods}
\keyword{models}
\keyword{multivariate}
\keyword{nonlinear}
\keyword{survival}
\keyword{interface}
