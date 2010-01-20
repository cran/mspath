\name{mspath}
\title{Multi-state Path-Dependent Models}
\alias{mspath}
\description{

  Ordinarily, fit a path-dependent (non-Markov) multi-state model by maximum
  likelihood. Observations of the process can be made at arbitrary times. 
  Covariates can be fitted to the transition intensities.
  When the true state is observed with error, a model can be fitted
  which simultaneously estimates the state transition intensities and
  misclassification probabilities, with optional covariates on both
  processes.

  This function will also perform some auxiliary operations, including
  counting paths and generating data according to the model.  See
  \code{do.what}.

  Based on the Christopher Jackson's msm package v 0.3.3.  The key
  differences are that
  this package can handle non-Markov models, and it does so using a
  discrete-time approximation.  Note that while some of the inputs to
  msm were transition rates or misclassification probabilities, the
  corresponding terms here are the intercepts of multinomial logits.
  Finally, \code{fromto} and related options do not exist; they are not
  appropriate in a path-dependent setting and have also been dropped
  from later versions of msm.

  The interface to msm changed with more recent releases to a style that
  combined specification of allowed transitions or observation errors
  and their values.  This would not be appropriate here, since 0 is a
  legitimate value for a coefficient.
  msm also has considerably richer options for
  handling the different kinds of information conveyed by the times of
  different observations.  There is no theoretical problem with handling
  those options here; there just hasn't been a need to implement those
  options.

}
\usage{
mspath ( formula, qmatrix, misc = FALSE, ematrix, inits, subject,
      covariates = NULL, constraint = NULL, misccovariates = NULL,
      miscconstraint = NULL, qconstraint=NULL, econstraint=NULL,
      pathvars = NULL, pathoffset = 0, pathconstraint = NULL,
      initprobs = NULL, 
      data = list(), 
      isexact = FALSE,
      fixedpars = NULL, stepnumerator = 1, stepdenominator = 1,
      do.what = 1, testing,
      comm, profile = FALSE,
      calcFactory = mspathCalculator,
      seed,
      trace,
      \dots )
}
\arguments{
  \item{formula}{ A formula giving the vectors containing
    states and the  corresponding observation times. For example,
    
    \code{states ~ times}


    Observed states should be in the set \code{1, \dots, n}, where
    \code{n} is the number of states.  Use state \code{0}---\emph{not}
    missing---when the state is not observed but the covariates are.
    Note that analysis will exclude any observation with missing
    covariates.  }

  \item{qmatrix}{Matrix of indicators for the allowed transitions.
    If a transition is allowed from state \eqn{r} to state \eqn{s},
    then \code{qmatrix} should have \eqn{(r,s)} entry 1, otherwise
    it should have \eqn{(r,s)} entry 0. The diagonal of \code{qmatrix}
    is ignored. For example,\cr
    
    \code{
    rbind(
    c( 0, 1, 1 ),
    c( 1, 0, 1 ),
    c( 0, 0, 0 )
    )
  }\cr
  
    represents a 'health - disease - death' model, with transitions
    allowed from health to disease, health to death, disease to health, 
    and disease to death. 
  }

  \item{misc}{Set \code{misc = TRUE} if misclassification between
    observed and underlying states is to be modelled, \code{FALSE} if
    observation is assumed accurate, or \code{SIMPLE} (no quotes)
    if the values given in \code{inits}, in conjunction with
    \code{econstraint}, specify the probabilities of measurement
    error.

    In the last case, those probabilities are fixed constants
    and you should omit \code{misccovariates} and
    \code{miscconstraint}.  All probabilities should be between 0 and 1,
    as should 1 - their sum, which the routine computes as the
    probability of accurate observation.  For \code{SIMPLE} the values
    are interpreted directly, rather than via a multinomial logit
    transformation.  You do not need to list them in \code{fixedpars};
    the routine always considers them fixed.
  }

  \item{ematrix}{
    (required when \code{misc == TRUE}) Matrix of indicators for the allowed
misclassifications. 
    The rows represent underlying states, and the columns represent
    observed states.
    If an observation of state \eqn{s} is possible when the subject
    occupies underlying state \eqn{r}, then \code{ematrix} should have
    \eqn{(r,s)} entry 1, otherwise
    it should have \eqn{(r,s)} entry 0. The diagonal of \code{ematrix}
    is ignored. For example, \cr

    \code{
      rbind(
    c( 0, 1, 0 ),
    c( 1, 0, 1 ),
    c( 0, 1, 0 )
    )
  }\cr
  
    represents a model in which misclassifications are only permitted
    between adjacent states. 
  }

  \item{inits}{(required) Vector of initial parameter estimates for the
    optimisation.  The \dQuote{Details} section below gives full
    interpretation of the parameters.  Parameters are in this order:

    - transition intercepts (reading across first rows of intensity
    matrix, then second row ... )

    - covariate effects on transitions

    - history-dependent path variables effects on transitions

    - misclassification intercepts (reading across first row of
    misclassification matrix, then second row ...)

    - covariate effects on misclassification

    Covariate effects are given in the following order,

    - effects of first covariate on transition/misclassification matrix elements (reading across
    first row of matrix, then second row ...)

    - effects of second covariate ... 
  }

  \item{subject}{Vector of subject identification numbers, when the data
    are specified by \code{formula}. If missing, then all observations
    are assumed to be on the same subject. These must be sorted so that
    all observations on the same subject are adjacent.}

  \item{covariates}{Formula representing the covariates on the
  transition intensities, for example,

    \code{~ age + sex + treatment}
    
  }

  \item{constraint}{A list of one vector for each named covariate. The
  vector indicates which covariate effects are
  constrained to be equal. Take, for example, a model with five
  transition intensities and two covariates. Specifying\cr
    
    \code{constraint = list (age = c(1,1,1,2,2),  treatment = c(1,2,3,4,5))}\cr

    constrains the effect of age to be equal for the first three
    intensities, and equal for the fourth and fifth. The effect of
    treatment is assumed to be different for each intensity. Any vector of
    increasing numbers can be used as indicators. The intensity parameters are
    assumed to be ordered by reading across the rows of the
    transition matrix, starting at the first row.

    For categorical covariates, defined using \code{factor(covname)},
    specify constraints as follows:\cr

    \code{list(..., covnameVALUE1 = c(...), covnameVALUE2 = c(...), ...)}\cr
    
    where \code{VALUE1}, \code{VALUE2}, ... are the levels of the factor.
    Make sure the \code{contrasts} option is set appropriately, for
    example, the default \code{options(contrasts=c(contr.treatment,
      contr.poly))} sets the first (baseline) level of unordered factors to
    zero.

    To assume no covariate effect on a certain transition, set its
    initial value to zero and use the \code{fixedpars} argument to fix
    it during the optimisation.
    
  }

  \item{misccovariates}{A formula representing the covariates on the 
  misclassification probabilities, analogously to \code{covariates}.
  }

  \item{miscconstraint}{A list of one vector for each named covariate on
    misclassification probabilities. The vector indicates which
    covariate effects on misclassification probabilities are
    constrained to be equal, analogously to \code{constraint}.
  }

  \item{qconstraint}{A vector of indicators specifying which
    transition intercepts are equal. For example,
    
    \code{qconstraint = c(1,2,3,3)}

    constrains the third and fourth transition intercepts to be equal, in a model
    with four allowed instantaneous transitions. 
  }

  \item{econstraint}{A similar vector of indicators specifying which
    misclassification intercepts are constrained to be
    equal. }
  
  \item{pathvars}{A vector of the names of path-dependent variables to
    use as covariates on transition intensities.  Allowed values
    currently include \code{TIS}, Time in State; \code{TSO}, Time Since
    Origin, \code{TIP}, Time in Previous states, and \code{LN(TIS)},
    \code{LN(TIP)} or \code{LN(TSO)} for natural logs of the same.}

  \item{pathoffset}{This value will be added to the origin time of
    path-dependent variables.  Use it pick where in the interval to use
    for your time values, or to avoid calculations with 0.}

  \item{pathconstraint}{Constraints on path variables' effects on
    transition rates.}
  
  \item{initprobs}{Currently only used in misclassification
    models.  Vector of assumed underlying state occupancy
    probabilities at each individual's first observation. Defaults to
    \code{c(1, rep(0, nstates-1))}, that is, in state 1 with a
    probability of 1.  The current implementation only supports 
    a single initial state.  
  }

  \item{data}{Optional data frame in which to interpret \code{subject} as
    well as the  time, state, and covariates defined in \code{formula},
    \code{covariates}, and \code{misccovariates} formulae.}

  \item{isexact}{By default, the transitions of the Markov process
  are assumed to take place at unknown occasions in between the
  observation times.  However, \code{isexact = TRUE} implies observation
  times for entry to absorbing states are exact.  Only paths with
  transitions at the observed time will be counted.  Typically, use this
  for death.  Note this option has no effect on non-absorbing states.
  It is an error to use this option when observation of the absorbing
  state is imprecise.}

  \item{fixedpars}{Vector of indices of parameters whose values will be
    fixed at their initial values during the optimisation. These
    correspond to indices of the \code{inits} vector, whose order is
    specified above. This can be useful for building complex models
    stage by stage. }

  \item{stepnumerator}{The calculation will use a time grid for each case
    with steps every \code{stepnumerator/stepdenominator}. This allows
    exact representation of rationals.  The numerator and denominator
    must both be integers.}
    
  \item{stepdenominator}{See \code{stepnumerator} just above.}
  
  \item{do.what}{
    % R 2.7 does not output the do.what header for ?mspath and other
    % text-oriented formats.  pdf is fine, as is R 2.10.  To avoid
    % unpleasantness for R 2.7, I have commented out the original text
    % and replaced it with something safer.
% \describe{
%     \item{1}{By default, calculates a maximimum likelihood.  To evaluate
%       a single likelihood, set all parameters to fixed. }
%     \item{0}{Count number of paths and related statistics without
%       evaluating the likelihood.}
%     \item{-1}{Get detailed counts (but not likelihoods) associated with
%       each case.  The return value is a matrix.}
%     \item{10}{Use the model to generate a random path for each
%       case. Returns a \code{data.frame} with simulated observed states
%       and times and all other data as observed.}
%  }
    1, the default, calculates a maximumlikelihood.  To evaluate a
    single likelihood, set all parameters to fixed.
    
    0 counts the number of paths and related statistics without
    evaluating a likelihood.
    
    \eqn{-1} get detailed counts, but not likelihoods, associated with
    each case.  The return value is a matrix.
    
    10 uses the model to simulate a random patch for each case,
    returning a \code{data.frame} with simulated observed states and times
    and all other data as observed.
}

  \item{testing}{This argument is  only for use by developers.  Set it
    to a list with the expected low-level arguments to test if that's
    what is actually generated.  The result will either be a stop or
    true; the underlying C code is not called.  See the code for the
    exact arguments.}
  
  \item{comm}{An optional MPI communicator.  If you provide it, perform a
    distributed calculation.  Ordinarily, \code{comm = 0}.  Ordinarily a
    call using this argument will be invoked from the commands supplied
    via \code{\link{master}}'s \code{channel} argument.  See that
    routine for more information on distributed calculation and the
    necessary setup.
  }

  \item{profile}{Profile the time taken in the distributed calculation.}

  \item{calcFactory}{Optional, advanced argument.  This should be a
    function that, when called with the arguments to the standard
    \code{\link{mspathCalculator}}, produces a calculator for internal
    use.  The \code{comm} and \code{profile} arguments will be ignored
    in this case.}

  \item{seed}{Optional seed to set for simulation (\code{do.what=10}).
    Use an \code{integer} for full precision; it will be cast to
    unsigned.} 
  
  \item{trace}{Optional list of arguments to \code{\link{checkpoint}}.
    If present, the objective function will be wrapped by
    \code{\link{checkpoint}} and every function evaluation's parameters
    and value will be recorded in the caller's environment and in files
    on disk.  See \code{\link{checkpoint}} for details.}

  \item{\dots}{Optional arguments to the general-purpose R
    optimization routine \code{\link{optim}}. Useful options include
    \code{method="BFGS"} for using a quasi-Newton optimisation
    algorithm, which can often be faster than the default Nelder-Mead.
    If the optimisation fails to converge, consider normalising the
    problem using, for example, \code{control=list(fnscale = 2500)}, for
    example, replacing 2500 by a number of the order of magnitude of the
    likelihood. If 'false' convergence is reported and the standard
    errors cannot be calculated due to a non-positive-definite Hessian,
    then consider tightening the tolerance criteria for convergence. If
    the optimisation takes a long time, intermediate steps can be
    printed using the \code{trace} argument of the control list. See
    \code{\link{optim}} for details.}
}
\value{Ordinarily a \code{\linkS4class{mspath}} object.  See it for
  details.  However, there are the following special cases:
  \item{\code{testing} non-empty}{Either returns \code{TRUE} or stops
    with an error if the calculated variables do not those in the
    \code{testing} list.  Specifying \code{testing} supersedes any
    actions given in \code{do.what}.}
  \item{\code{do.what=-1}}{Return a matrix with counts for each case.}
  \item{\code{do.what=10}}{Return a data frame with a random path as
    described above.}
}
\details{
  For each case, the model generates all possible paths consistent with
  the observed data, measurement model, and transition model.  It
  computes and sums the likelihood of each path, including the initial
  probability specified in \code{initprobs}.

  It is quite possible to specify a model which makes the observed data
  impossible; you will get an error in that case.

  Both the transition and misclassification models use the multinomial
  logit.  Self transition has assumed coefficients of 0; the other coefficients
  are multiplied by the covariates, summed and exponentioned.  All
  coefficients, including the intercept or baseline terms, are so transformed.
  With \eqn{J} possible outcomes and \eqn{p_j}{p(j)} the probability of
  the \eqn{j}'th outcome,
  the formula is \deqn{\newcommand{\B}{{\bf \beta}}\newcommand{\X}{{\bf X}}
    p_j = \frac{e^{\X\B_j}}{\displaystyle\sum_{k=0}^J e^{\X\B_k}}.}{
    p(j) = exp[X*b(j)]/sum{exp[X*b(k)], k=0 to J}.}
  % the extra spaces around { } in the first \eqn argument work around a bug
  % in the Rd conversion to LaTeX
  \eqn{ {\bf\beta}_j }{b(j)} is the vector of coefficients for outcome \eqn{j}{j} and
  \eqn{ {\bf X} }{X} are the covariates.  Calculations use the
  covariates at the endpoint of the transition to set the transition rate.
  
  \code{TIP}, Time in Previous states, is the start time of the current
  state.  Note it is time in all previous state{\emph{s}}, not just the
  previous state. It will remain constant as \code{TIS}, Time in State,
  changes.  If \code{pathOffset}=0, \code{TSO = TIP + TIS}.  \code{TIP}
  is a constant for the first state; we recommend constraining the
  corresponding coefficient to 0 if you allow effects of \code{TIP} to
  vary with stage.
  
  There must be enough information in the data on each state to estimate
  each transition rate, otherwise the likelihood will be flat and the
  maximum will not be found.  It may be appropriate to reduce the
  number of states in the model, or reduce the number of covariate
  effects, to ensure convergence.

  Choosing an appropriate set of initial values for the optimisation can
  also be important.  For flat likelihoods, 'informative' initial values
  will often be required.

  If a single discrete time point includes multiple observations, one is
  selected.  In the initial interval, the first point is used to get the
  correct start time.  For later times, selection favors observations
  with (non-0) state, and picks the last possible observation.  If any
  observations include state, the last among those is the one picked.
}
\section{Simulation}{
  \code{do.what=10} requests generation of a simulated \code{data.frame}
  based on the specified model and data.

  The first step of the simulation drops rows as in the likelihood
  estimation.  Rows with missing data may be dropped depending upon the
  options you have requested globally or in \code{data}.  Then rows in
  the same discrete time interval are dropped.

  Second, all the data are collected: \code{data} and \code{subject} and
  any variables used in \code{formula}, \code{covariates} and
  \code{misccovariates}.  Data not present in \code{data} will be pulled
  in from the appropriate environment and added to the output
  \code{data.frame}.  If this happens with \code{subject}, the name of
  the column will be \code{"subject"}; in these circumstances
  \code{subject} may not appear as a term in any formula.  Note that
  variables in \code{data} that are not referenced in formulae
  \emph{will} be retained for later analysis.

  The variable names will be those used in the formulae.  If the caller
  specifies \code{subject} with a name (e.g., \code{mspath(subject=id,
    \dots)}) that name will be the name of the subject variable;
  otherwise the name will be \code{"subject"}.  In either case, that
  name should not be used elsewhere.

  Third, for each case a true path and corresponding observed states
  will be simulated using the model.  Ordinarily the simulated data will
  have the same observation times as the real data.  However, if the
  simulated path enters an absorbing state and \code{isexact == TRUE}
  then the simulated data will show an observation at that time, using
  the then current covariates.  Furthermore, the simulated data will
  never have more than one observation in an absorbing state, since
  otherwise \code{mspath} will refuse to estimate for the simulated
  data.
  }
\note{The \code{trace} argument will cause tracing information to go
  into the caller's environment and onto the disk, as described in
  \code{\link{checkpoint}}.
  }
\references{
  Boylan, R.D. 2009. Multi-state Path-Dependent Models.  Working paper.
  University of California, San Francisco.
  
  Jackson, C.H., Sharples, L.D., Thompson, S.G. and Duffy, S.W. and
  Couto, E.  Multi-state Markov models for disease progression with
  classification error. \emph{The Statistician}, 52(2): 193--209 (2003)
    
  Jackson, C.H. and Sharples, L.D. Hidden Markov models for the
  onset and progresison of bronchiolitis obliterans syndrome in lung
  transplant recipients \emph{Statistics in Medicine}, 21(1): 113--128
  (2002).
    
  Kay, R.  A Markov model for analysing cancer markers and disease
  states in survival studies.  \emph{Biometrics} (1986) 42: 855--865.
  
  Gentleman, R.C., Lawless, J.F., Lindsey, J.C. and Yan, P.  Multi-state
  Markov models for analysing incomplete disease history data with
  illustrations for HIV disease.  \emph{Statistics in Medicine} (1994) 13(3):
  805--821.

  Satten, G.A. and Longini, I.M.  Markov chains with measurement error:
  estimating the 'true' course of a marker of the progression of human
  immunodeficiency virus disease (with discussion) \emph{Applied
    Statistics} 45(3): 275-309 (1996)
}

\seealso{\code{\link{master}}, \code{\link{checkpoint}},
  \code{\linkS4class{mspath}} return object.}
\author{Ross D. Boylan \email{ross@biostat.ucsf.edu}}

\keyword{models}
\concept{distributed}
\concept{MPI}
