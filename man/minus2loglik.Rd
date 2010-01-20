\name{minus2loglik}

\alias{minus2loglik}
\alias{minus2loglik-methods}
\alias{minus2loglik,mspathCalculator-method}

\title{Retrieve Multi-State Path Model Likelihood}

\description{
  Returns -2 * the calculator's previously computed log-likelihood, if
  any. 
}
\usage{
minus2loglik(x)
}
\arguments{
  \item{x}{A previously evaluated calculator, which should be a
\cite{\linkS4class{mspathCalculator}} or subclass}
}
\value{
  -2 * log-likelihood, a \code{double}.
}
\section{Methods}{
  \describe{
    \item{x = "mspathCalculator"}{standard case}
}}
\note{Use this function, rather than a less-reliable unpacking of
  \code{\link{results}}, to get the computed likelihood.

  You must create and evaluate the calculator with appropriate
  \code{do.what} arguments to get a useful likelihood.
}
\author{Ross Boylan}
\seealso{\code{\linkS4class{mspathCalculator}}}
\keyword{ models }% at least one, from doc/KEYWORDS
\keyword{methods}
