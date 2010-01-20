\name{mspath-internal}

%%%%% functions
\alias{mspath.check.consistency}
\alias{mspath.check.ematrix}
\alias{mspath.check.model}
\alias{mspath.check.qmatrix}
\alias{mspath.check.state}
\alias{mspath.check.times}
\alias{mspath.integerTime}
\alias{mspath.make.constraint}
\alias{mspath.process.covs}
\alias{mspath.process.path}
\alias{mspath.remove.overlap}
\alias{printOptim}

%%%%% constants
\alias{doneCode}
\alias{requestCode}
\alias{resultCode}
\alias{timeRequestCode}

\alias{doubleType}
\alias{integerType}

\alias{root}
\alias{getlib}

%%%% global vars


\title{mspath Internals}

\description{These are functions, constants and global variables used
  internally by the package. They'll be in the global namespace until
  the package gets put in a namespace.

}
\usage{
mspath.check.consistency( qmatrix, misc,subject=NULL,
                                  state, time)
mspath.check.ematrix(ematrix, qmatrix)
mspath.check.model(state, subject, qmatrix)
mspath.check.qmatrix(qmatrix)
mspath.check.state(nstates, state)
mspath.check.times(time, subject)
mspath.make.constraint(covlabels, constraint, nmatrix)
mspath.integerTime(t, stepnumerator, stepdenominator)
mspath.process.covs(covariates, data, constraint, nobs, nmatrix)
mspath.process.path(pathvars, constraint, nmatrix)
mspath.remove.overlap(subject, time, state, final.rows,
                                  stepnumerator, stepdenominator)
printOptim(x, ...)
}
\section{Constants}{
  The following codes are used as tags for MPI messages:
  \describe{
    \item{requestCode}{Slave requests data to evaluate; Master asks
      slave to evaluate}
    \item{timeRequestCode}{Master asks slave to evaluate and time}
    \item{resultCode}{Slave sends results of evaluation}
    \item{doneCode}{Master tells slave to shut down, or slave confirms
      same.}
  }

  These codes have MPI type constants:
  \describe{
    \item{integerType}{Integer}
    \item{doubleType}{Doubles (reals)}
  }

  MPI rank constants:
  \describe{
    \item{root}{Master process rank}
    }

  }
  
\section{Other Variables}{\describe{
  \item{getlib}{\code{eval} this to load the necessary distributed
    processing library}
  }
}
\keyword{internal}
