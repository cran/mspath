\name{mspathCoefficients-class}
\docType{class}
\alias{mspathCoefficients-class}
\alias{mspathCoefficients}
\alias{matrixCoef}
\alias{matrixCoef-methods}
\alias{matrixCoef,mspathCoefficients-method}
\alias{coef,mspathCoefficients-method}
\alias{sd,mspathCoefficients-method}
\alias{isAllFixed,mspathCoefficients-method}
\alias{print,mspathCoefficients-method}
\alias{show,mspathCoefficients-method}

\title{Class "mspathCoefficients"}
\description{This class contains coefficients for the multi-state path
  model.  It knows about constraints.  End users need not be aware of
  this class, except for the arguments to the print function.}

\section{Objects from the Class}{
Objects can be created by calls of the form
\code{mspathCoefficients(permit, params, offset, baseConstrVec, covVars,
  constrVec, pathVars, pathConstrVec)}.  The arguments are the same as
for the slots, though the type restrictions are less severe; any numeric
is fine.  Note that \code{"intercept"} is not a legal name to pass in
for \code{covVars} or \code{pathVars}, and that names should be unique
across the last two arguments.  I.e., don't give a covariate the same
name as a path variable.

}
\section{Slots}{
	 \describe{
    \item{\code{permit}:}{\code{"matrix"} of \code{"logical"} values
      indicating which entries are permitted.  The diagonal is always \code{FALSE}.}
    \item{\code{params}:}{All the model parameters, in the canonical
      order for mspath.  There is no distinction between free and fixed
      parameters in this class; use
      \code{\linkS4class{mspathEstimatedCoefficients}} if that's a
      concern. (\code{"numeric"}) }
    \item{\code{baseConstrVec}:}{\code{"integer"}, starting at 1, for
      constraints on the intercepts. }
    \item{\code{covVars}:}{Names of the covariates, if any
      (\code{"character"}).}
    \item{\code{constrVec}:}{\code{"integer"}, starting at 1, with
      constraints on the covariates.  Empty vector if no covariates. }
    \item{\code{pathVars}:}{Names of path-dependent variables, if any (\code{"character"}). }
    \item{\code{pathConstrVec}:}{\code{"integer"}, starting at 1, with
      constraints on the path variables.  Empty if no path variables. }
    \item{\code{iEff}:}{\code{"integer"} indices of effective parameters
      in \code{params}.  This permits identification of the subset of
      \code{params} of interest.}
    \item{\code{map}:}{\code{map["vname"]} gives a \code{matrix} of
      \code{integer}'s.  Each entry that is non-zero indicates that, for
      variable \code{"vname"} in that matrix position, the coefficient
      comes from the corresponding \code{parameter}.  Use
      \code{"intercept"} to retrieve the intercepts, and the names of
      covariates or path-dependent variables to retrieve those values.
      This slot is a \code{"list"}. }
  }
}
\section{Methods}{
  \describe{
    \item{coef}{\code{signature(object="mspathCoefficients")}: Vector of
      all coefficients}
    \item{matrixCoef}{A list of transition matrices of coefficients.
      The optional \code{coeff} argument is a vector of names of
      coefficients of interest; otherwise all are reported.}
    \item{sd}{\code{signature(x="mspathCoefficients", na.rm="ANY")}: Vector of
      0's of same length as coefficients.  This class is not for
      estimates.  na.rm is ignored.}
    \item{matrixCoef}{\code{signature(x="mspathCoefficients", ...)}: A list of
      matrices with the coefficient values for each matrix element and 0 for
      those not allowed.  The optional argument \code{coeff} gives
      the names of terms to return; use \code{"intercept"} for the intercept.}
    \item{isAllFixed}{\code{signature(object="mspathCoefficients")}:
      \code{TRUE} because all values are fixed in advance.}
    \item{print}{\code{signature(x="mspathCoefficients", \dots)}: print
      out the coefficient values.  The printout consists of blocks of
      coefficients related to a particular covariate.  Each line
      indicates the matrix element(s) the coefficient applies to, the
      index in  the parameter list from which the element comes, and
      the value of the coefficient.  For the matrix element, \code{i ->
	j} indicates row \code{i}, column {j}, which is either the
      transition from \code{i} to \code{j} or true state \code{i}
      observed as state \code{j}.

      Parameters that apply to several covariates because of constraints
      will appear in several places.

      The optional named argument \code{coeff} is a \code{character} vector of
      coefficient names.  If provided, only those coefficients will be
      printed out, in the order given.  Use \code{"intercept"} to get
      the intercept.

      \dots allows one to use further arguments to control the details
      of the printout; see \code{\link{printCoefmat}} for details.

      Returns \code{invisible(x)}.
    }
    \item{show}{\code{signature(object="mspathCoefficients")}: print the
	object, returning \code{invisible(NULL)}.}
   }}
%\references{ ~put references to the literature/web site here ~ }
\author{Ross Boylan \email{ross@biostat.ucsf.edu} }
\note{For full details of the ordering of the different values and the
  interpretation of the constraints, see \link{mspath}.

  In places above where the names of covariates or coefficients are
  called for, the exact meaning is names in the design matrix.  So, for
  example, if \code{avar} is a \code{factor}, the actual names you will
  enter are \code{avar2}, \code{avar3}, etc (exact details depend on
  your choice of contrasts and the levels of the variables).

  It is likely in the future this will include more methods, for example
  to multiply coefficients by covariates.

  The terminology above is a bit loose and confusing.  Sometimes
  covariate or coefficient refers to terms that include the intercept
  and the path variables.  Sometimes coefficient refers to a particular
  estimated parameter.  And sometimes, as in the \dQuote{names of
    covariates} it refers to a covariate plus a factor addition.
  }

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\linkS4class{mspathEstimatedCoefficients}} for estimated
  coefficients, \code{\link{mspath}} for general background, and
  \code{\link{printCoefmat}} for optional arguments to the print function.
}
\examples{
permissible <- matrix(0, nrow=3, ncol=3)
permissible <- row(permissible)+1 == col(permissible)
covNames <- c("a", "b")
pathNames <- c("TIS", "TSO")
covConstraint <- seq(4)
covConstraint[4] <- 2
pathConstraint <- c(1, 1, 2, 3)
c <- mspathCoefficients(permissible,
                   params=seq(from=.1, by=.1, len=18),
                   offset=10,
                   baseConstrVec=seq(2),
                   covVars=covNames,
                   constrVec=covConstraint,
                   pathVars <- pathNames,
                   pathConstrVec <- pathConstraint
                   )
print(c)

}
\keyword{classes}
