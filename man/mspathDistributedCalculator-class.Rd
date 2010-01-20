\name{mspathDistributedCalculator-class}
\docType{class}
\alias{mspathDistributedCalculator-class}
%\alias{calculate,mspathDistributedCalculator,numeric,missing,missing-method}
%\alias{done,mspathDistributedCalculator-method}
\alias{params<-,mspathDistributedCalculator,numeric-method}


\title{Distributed Calculator for Multi-State Path Models}
\description{This calculator distributes the calculation of the
    likelihood to slave processes, which compute a multi-state path
    model with possible non-Markov properties.  Though you can use this
    class directly, it is usually more convenient to invoke
    \code{\link{mspath}}.  This is a subclass of (aka "extends")
      \code{\linkS4class{mspathCalculator}}; the slaves also use that
      class to perform their computations.
    }
    
\section{Objects from the Class}{
Create objects of this class by calling
\code{\link{mspathDistributedCalculator}}, and clean them up by calling
\code{\link{done}}.  One can also create a factory object via
\code{\link{mspathDistributedCalculatorFactory}} and create instances
via that object.
}
\section{Slots}{
	 \describe{
    \item{\code{comm}:}{MPI communicator (\code{"integer"}) }
    \item{\code{work}:}{Work estimate, to aid scheduling (\code{"matrix"})}
    \item{\code{worki}:}{Sorted indices into the rows of \code{work}
      such that \code{worki[1]} is the row with the most work,
      \code{worki[2]} has the 2nd biggest workload, and so on (\code{"integer"})}
    \item{\code{analyzer}:}{Holds a \code{\linkS4class{runAnalyzer}}, but
	initially empty. (\code{"ANY"}) }
    \item{\code{mode}:}{\code{requestCode} or \code{timeRequestCode},
      depending on whether we are profiling (\code{"integer"}) }
    \item{\code{cuts}:}{show how to cut the work into jobs.  These are
      indices into \code{worki} (\code{"numeric"}). }
    \item{\code{maxlen}:}{The longest list of ID's that will be sent (\code{"integer"}) }
    \item{\code{cutids}:}{ID's of first case in each group (\code{"integer"}) }
    \item{\code{evals}:}{Number of evaluations of this calculator.
      Useful for determining if we need initial timings or setup. Object
      of class \code{"integer"}} 

    From \code{\linkS4class{mspathCalculator}}:
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
\section{Extends}{
Class \code{"mspathCalculator"}, directly.
}
\section{Methods}{
  \describe{
    \item{calculate}{\code{signature(calc =
	"mspathDistributedCalculator", params = "numeric", activeCases =
	"missing", do.what = "missing")}:
      Perform the requested calculation by distributing it across slave
      processes.  Returns \code{calc} with the desired results.  Notice
      that distribution evaluates a given set of parameters (if
      \code{params} are not given the existing ones will be used) for
      all cases.  Different subsets of the cases go to different processors.}
    \item{done}{\code{signature(calc = "mspathDistributedCalculator")}:
      Call this to cleanup.  Note it will leave the overall MPI session
      running.  Returns an updated \code{calc}, but you shouldn't refer
      to the old or new values after calling this method.}
    \item{params<-}{\code{signature(calc =
	"mspathDistributedCalculator", value = "numeric")}:
      Sets the free parameters (including for the slaves), but performs
      no calculation. }
	 }

}

\author{Ross Boylan}
\note{Using this class requires proper initialization of the MPI environment.}

\seealso{\code{\linkS4class{mspathCalculator}}, \code{\link{mspath}}
}

\keyword{classes}
\keyword{models}
\concept{MPI}
\concept{distributed}
