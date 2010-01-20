\name{mspathDistributedCalculator}
\alias{mspathDistributedCalculator}

\title{Constructors for mspathCalculator Objects}
\description{
  Generic function to compute the likelihood and or counts of the model
  described in the calculator under the given parameters.  It does so by
  updating the calculator itself.
}
\usage{
mspathDistributedCalculator(do.what, params, allinits, misc, subject, time, state, qvector, evector, covvec,
                    constrvec, misccovvec, miscconstrvec, baseconstrvec, basemiscconstrvec, 
                    pathvars, pathoffset, pathconstrvec,
                    initprobs, nstates, nintens, nintenseffs, nmisc, nmisceffs, nobs, npts,
                    ncovs, ncoveffs, nmisccovs, nmisccoveffs,
                    npatheffs,
                    isexact, fixedpars, stepnumerator, stepdenominator,
comm=0,
profile = FALSE)
}
\arguments{
  See \code{\link{mspath}} and the documentation on the \code{C++} code
  for the full details.
  
  \item{do.what}{Type of computation to perform, as interpreted by the
    \code{C++} code.  This may be ignored, or only apply to the
    non-distributed calculators created by this one (need to check).}
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

  \item{comm}{The MPI communicator to use.  The default ordinarily
    suffices.}

  \item{profile}{\code{TRUE} to enable profiling.}
}
\value{Returns a new \cite{\linkS4class{mspathDistributedCalculator} object}.
}
\author{Ross Boylan}
\note{The \pkg{Rmpi} library is loaded on object creation, and
  must be accessible.   }
\seealso{\code{\linkS4class{mspathDistributedCalculator}}, 
  \code{\link{mspath}}, \pkg{Rmpi}
}
\keyword{models }% at least one, from doc/KEYWORDS
\concept{distributed}
\concept{MPI}
