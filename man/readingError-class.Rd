\name{readingError-class}
\docType{class}
\alias{readingError-class}
\alias{readingError}
\alias{bitMask}
\alias{bitMask,readingError-method}
\alias{boolMask}
\alias{boolMask,readingError-method}
\alias{coef,readingError-method}
\alias{mean,readingError-method}

\title{Class "readingError"}
\description{Describes errors in readings for discrete states.  This is
  a special type of \code{\link{matrix}} in which rows are the true states and
  columns are observed states.  The diagonal is mostly ignored, since it
  must be 1 - sum of other elements on the row.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{readingError(...)}
where the arguments are the usual ones for \code{\link{matrix}}.
}
\section{Slots}{
	 \describe{
    \item{\code{.Data}:}{Object of class \code{"matrix"} }
  }
}
\section{Extends}{
Class \code{"\linkS4class{matrix}"}, from data part.
Class \code{"\linkS4class{structure}"}, by class "matrix", distance 2.
Class \code{"\linkS4class{array}"}, by class "matrix", distance 2.
Class \code{"\linkS4class{vector}"}, by class "matrix", distance 3, with explicit coerce.
Class \code{"\linkS4class{vector}"}, by class "matrix", distance 4, with explicit coerce.
}
\section{Methods}{
  \describe{
    \item{bitMask}{\code{signature(object = "readingError")}: returns a
      matrix with 1's on off-diagonal positive elements, 0 elsewhere. } 
    \item{boolMask}{\code{signature(object = "readingError")}: return a
      matrix with|code{TRUE} on off-diagonal positive elements,
      \code{FALSE} elsewhere.}
    \item{coef}{\code{signature(object = "readingError")}: returns a
      vector with only the off-diagonal positive elements, starting with
    the first row, then the second, and so on.}
    \item{mean}{\code{signature(x = "readingError")}: The arguments are
      one or more \code{readingError}'s.  This returns a
      \code{readingError} whose entries are the means of each of the
      cells in the arguments. Note that extra arguments to \code{mean}
      elsewhere in \R provide optional tuning values for the
      calculation; that is not the case here.}
	 }
}
\references{Bacchetti, Peter and Boylan, Ross (2009) \dQuote{Estimating Complex
  Multi-State Misclassification Rates for Biopsy-Measured Liver Fibrosis
  in Patients with Hepatitis C,} \emph{The International Journal of
  Biostatistics}: Vol. 5 : Iss. 1, Article 5.
  DOI:
  10.2202/1557-4679.1139
  \url{http://www.bepress.com/ijb/vol5/iss1/5}
}
\author{Ross Boylan }
\note{This class is designed to work with \code{mspath} and may not be
  appropriate for other uses.  In particular, \code{coef} returns values
  in the order expected by the \code{inits} argument to
  \code{\link{mspath}}, which is the transpose of the usual order for \R. }

\seealso{\code{\link{matrix}}, \code{\link{mspath}}, and sample data
  provided with this package.
}
\examples{
library(mspath)
re <- readingError(c(.73, .27, 0, 0, 0,
                               .02, .73, .25, 0, 0,
                               0, .02, .90, .08, 0,
                               0, 0, 0, .88, .13,
                               0, 0, 0, .07, .93),
                             byrow=TRUE, nrow=5, ncol=5)
coef(re)
}
\keyword{classes}
\keyword{array}
\keyword{datasets}
