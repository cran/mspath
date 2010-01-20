\name{mspathEstimatedCoefficients-class}
\docType{class}
\alias{mspathEstimatedCoefficients-class}
\alias{coef,mspathEstimatedCoefficients-method}
\alias{sd,mspathEstimatedCoefficients-method}
\alias{show,mspathEstimatedCoefficients-method}
\alias{print,mspathEstimatedCoefficients-method}
\alias{mspathEstimatedCoefficients}

\title{mspathEstimatedCoefficients}
\description{This class describes estimated coefficients for the
  multi-state path model.  It knows which coefficients are fixed before
  estimation and which are constrained to be equal.  It knows about the
  errors of the estimates as well.  Ordinary users will only care about
  the arguments to the print function.}
\section{Objects from the Class}{
  Create these objects by calling \code{mspathCoefficients(permit, params, offset, baseConstrVec, covVars,
  constrVec, pathVars, pathConstrVec, fixed=integer(0), n=Inf,
  var=numeric(0))}.  \code{var} is the estimated variance for all free
parameters in this and other coefficients.  For other
parameters, see slot definitions for details, except that arguments to
this function can use more general types like \code{numeric}.
}
\section{Slots}{
	 \describe{
    \item{\code{fixed}:}{Indices, if any, of parameters fixed at the
      outset (\code{"integer"}).}
    \item{\code{n}:}{Sample size on which estimates are based (\code{"numeric"}).}
    \item{\code{foundSE}:}{\code{"logical" TRUE} if the estimate appears
    to have converged.}
    \item{\code{var}:}{\code{"numeric"} estimate of the variances of the
      effective parameters, with 0's for the fixed parameters.  Only valid if
      \code{foundSE == TRUE}; otherwise it will be \code{numeric(0)}.
      \emph{Careful:} the variances passed in to the constructor are for
    all free parameters in all coefficients; this slot has variances for
  both free and fixed parameters for this coefficient set only.}

    The remaining items are inheritted from \code{\linkS4class{mspathCoefficients}}:

    \item{\code{permit}:}{\code{"matrix"} of \code{"logical"} values
      indicating which entries are permitted.  The diagonal is always \code{FALSE}.}
    \item{\code{params}:}{All the model parameters, both free and fixed, in the canonical
      order for mspath. (\code{"numeric"}) }
    \item{\code{baseConstrVec}:}{\code{"integer"}, starting at 1, for
      constraints on the intercepts. }
    \item{\code{covVars}:}{Names of the covariates, if any
      (\code{"character"}).}
    \item{\code{constrVec}:}{\code{"integer"}, starting at 1, with
      constraints on the covariates.  Empty vector if no covariates. }
    \item{\code{pathVars}:}{Names of path-dependent variables, if any (\code{"character"}). }
    \item{\code{pathConstrVec}:}{\code{"integer"}, starting at 1, with
      constraints on the path variables.  Empty if no path variables. }
    \item{\code{map}:}{\code{map["vname"]} gives a \code{matrix} of
      \code{integer}'s.  Each entry that is non-zero indicates that, for
      variable \code{"vname"} in that matrix position, the coefficient
      comes from the corresponding \code{parameter}.  Use
      \code{"intercept"} to retrieve the intercepts, and the names of
      covariates or path-dependent variables to retrieve those values.
      This slot is a \code{"list"}. }
  }
}
\section{Extends}{
Class \code{"mspathCoefficients"}, directly.
}
\section{Methods}{
  \describe{
    \item{isAllFixed}{\code{signature(object="mspathCoefficients")}:
      usually \code{FALSE}, but \code{TRUE} if all parameters are fixed.}
    \item{coef}{\code{signature(object="mspathEstimatedCoefficients")}: Vector of
      all coefficients, fixed and free.  These are the effective
      coefficients, meaning that if constraints make two coefficients
      equal, only a single term will appear for them.}
    \item{sd}{\code{signature(x="mspathCoefficients"), na.rm="ANY"}: Vector of
      estimated standard deviations of the results of \code{coef()}.
      The \code{na.rm} probably won't work correctly.}
    \item{print}{\code{signature(x="mspathEstimatedCoefficients", coeff, \dots)}: print
      out the coefficient values.  The printout consists of blocks of
      coefficients related to a particular covariate.  Each line
      indicates the matrix element(s) the coefficient applies to, the
      index in  the parameter list from which the element comes
      (negative index if fixed), 
      the estimated (or fixed) value of the coefficient, the estimated
      standard error of the estimate (or \code{NA} for fixed values),
      and various measures of statistical significance.

      Parameters that apply to several covariates because of constraints
      will appear in several places.

      The optional argument \code{coeff} is a \code{character} vector of
      coefficient names.  If provided, only those coefficients will be
      printed out, in the order given.

      \dots allows one to use further arguments to control the details
      of the printout; see \code{\link{printCoefmat}} for details.

      Purists will note that neither \code{coeff} nor \code{\dots} is part
      of the method signature.  They are acceptable arguments to the
      function, however.

      Returns \code{invisible(x)}.
    }
    \item{show}{\code{signature(object="mspathEstimatedCoefficients")}: print the
	object, returning \code{invisible(NULL)}.}
  }}

%\references{ ~put references to the literature/web site here ~ }
\author{ Ross Boylan \email{ross@biostat.ucsf.edu}}
\note{ \code{foundSE} will be \code{TRUE} if the hessian is positive
  definite.  In that case, \code{var} has the diagonal of the inverse of
  the hessian.
  
}

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{mspath}} gives full information about paramter order and
  constraints.  \code{\linkS4class{mspathCoefficients}} is my parent
  class.
  \code{\link{printCoefmat}} describes additional optional arguments to
  the print function.
 }
\examples{
# 5 x 5, history but no covariates
permissible <- matrix(c( 0, 1, 1, 0, 1,
                         1, 0, 1, 1, 1,
                         0, 0, 0, 0, 1,
                         1, 0, 0, 0, 1,
                         0, 0, 0, 0, 0), ncol=5, byrow=TRUE)
pathConstraint <- c(1, 1, 2, 3, 1, 1, 4, 5, 2, 6,
                   7, 8, 2,  8, 7, 8, 9, 10, 11, 12)
baseConstraint <- c(1, 1, 2, 3, 1, 1, 4, 5, 2, 6)

pathNames <- c("TIS", "TSO")
fixed <- c(3, 10, 11)
var <- seq(from=30, by=5, length.out=15)^2
c <- mspathEstimatedCoefficients(permissible,
                                 params= 101:118,
                                 baseConstrVec=baseConstraint,
                                 pathVars=pathNames,
                                 pathConstrVec = pathConstraint,
                                 fixed=fixed,
                                 n=50,
                                 var=var)

# default show method
c

# optional print arguments
print(c, coeff=c("intercept", "TIS"), digits=2)
}
\keyword{classes}
