\name{mspathCalculator}
\alias{mspathCalculator}
\alias{mspathCalculatorFromArgs}

\title{Constructors for mspathCalculator Objects}
\description{
  Make \code{\linkS4class{mspathCalculator}} objects.  This is primarily
  for internal use.
}
\usage{
mspathCalculator(do.what, params, allinits, misc, subject, time, state, qvector, evector, covvec,
                    constrvec, misccovvec, miscconstrvec, baseconstrvec, basemiscconstrvec, 
                    pathvars, pathoffset, pathconstrvec,
                    initprobs, nstates, nintens, nintenseffs, nmisc, nmisceffs, nobs, npts,
                    ncovs, ncoveffs, nmisccovs, nmisccoveffs,
                    npatheffs,
                    isexact, fixedpars, stepnumerator, stepdenominator)
mspathCalculatorFromArgs(args, do.what=1)

}
\arguments{
  See \code{\link{mspath}} and the documentation on the \code{C++} code
  for the full details.
  
  \item{do.what}{Type of computation to perform, as interpreted by the
    \code{C++} code.}
  \item{params}{Ordinarily, the precise values here may be irrelevant,
    since they can be reset on specific calls to \cite{\link{calculate}}.
    However, the size of this argument must be correct.}
  \item{allinits}{The \dQuote{initial} values for all fixed and free
    parameters.  Only the fixed values are used; the rest come from
    \code{params}, which may be reset during the life of the
    calculator.}
  \item{misc}{0 = no misclassification; 1 = full misclassification; 2 =
    simple, fixed misclassification.}
  \item{subject}{The ID, which must be an integer, for each row of
    data.  The ID's should be sorted in ascending order.}
  \item{time}{time of each observation, in ascending order within
    cases.}
  \item{state}{The state of each observation.  States should be numbered
    1 through \code{nstates}.}
  \item{qvector}{Vectorized matrix of allowed transitions, 1 for
    allowed, 0 for not allowed.}
  \item{evector}{Vectorized matrix of allowed misclassifications (on
    relevant if \code{do.what} is not 0).}
  \item{covvec}{vectorized matrix of covariate values.}
  \item{constrvec}{constraints for each covariate}
  \item{misccovvec}{Vectorized matrix of covariate values for
    misclassification (only relevant if \code{misc} = 1).}
  \item{miscconstrvec}{list of constraints for each misclassification
    covariate}
  \item{baseconstrvec}{constraints on baseline transition intensities}
  \item{basemiscconstrvec}{constraints on baseline misclassification
    probabilities}
  \item{pathvars}{\code{character} vector of the names of
    history-dependent variables.}
  \item{pathoffset}{add this \code{double} to every time 0 in the paths}
  \item{pathconstrvec}{constraints on path effects on intensities}
  \item{initprobs}{initial state occupancy probabilities}
  \item{nstates}{number of states}
  \item{nintens}{number of intensity parameters}
  \item{nintenseffs}{number of distinct intensity parameters}
  \item{nmisc}{number of misclassification rates}
  \item{nmisceffs}{number of distinct misclassification rates}
  \item{nobs}{number of observations in the data set}
  \item{npts}{number of individuals/cases in the data set}
  \item{ncovs}{number of covariates on transition rates}
  \item{ncoveffs}{number of distinct covariate effect parameters}
  \item{nmisccovs}{number of distinct misclassification parameters}
  \item{nmisccoveffs}{number of distinct misclassification effects}
  \item{npatheffs}{number of distinct path (history) effects on
    transitions} 
  \item{isexact}{non-0 if we observed time of entry to absorbing states
    exactly (e.g., death)}
  \item{fixedpars}{which parameters to fix}
  \item{stepnumerator}{\code{integer} This divided by
    \code{stepdenominator} give the maximum step size in the discrete
    approximation, in the same scale as \code{time}.}
  \item{stepdenominator}{\code{integer} see \code{stepnumerator}}

  \item{args}{These arguments for \code{mspathCalculatorFromArgs},
    unlike those for \code{mspathCalculator}, must be processed with
    \code{as.integer} and other appropriate forms for the call to
    \code{C++}.
    }

}
\value{Returns a new \cite{\linkS4class{mspathCalculator} object}.
}
\author{Ross Boylan}
\note{To use this properly you need to use the correct 1 or 0-based
  indexing (which varies by argument) and know the exact rules for
  unrolling matrices into vectors.   }
\seealso{\code{\linkS4class{mspathCalculator}}, 
  \code{\link{mspath}}, and the documentation for the \code{C++} code.
  }
\keyword{models }% at least one, from doc/KEYWORDS

