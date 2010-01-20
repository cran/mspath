# This is a drop in replacement for the real mspathCalculators
# It is slow and has limited capability.  It is intended for
# verification of the results of the real calculators.

# 2009-11-10 RB fix subtle bug in determining if there is an observation
#               for a time.  Observation times that were rounded down previously
#               showed as lacking an observation at the new time.
# 2009-11-13 RB use floor, not round, to match logic of real code


## totalEffectMaker function
# helper to handle constraints
# returns a function that, given a constraint, returns the
# corresponding set of total coefficients (as a vector).
# Each call to the inner function has the constraints for one
# set of coefficients; you must make the calls in the same order
# as the coefficients are arrayed in allinits and pars.
# That order is
#   transition constants
#   transition covariates
#   transition path effects
#   misclassification constants
#   misclassification covariates
# Within covariates, first all the effects of the first covariate,
# then all the effects of the 2nd, and so on.
# Note that the constraints are in the normalized form produced by
# mspath: they are numbered consecutively from 1 within a particular set.
# This is different from what the user specifies, because the user
# need not use consecutive constraint values and is allowed to use the
# same constraint value for different covariates (without its meaning
# the two covariate effects are equal).
#
# Arguments to outer function:
# allinits are the initial values for all the parameters
# freepars are the values of the free parameters
# fixed pars are the indices of allinits that are fixed
totalEffectMaker <- function(allinits, pars, fixedpars) {
  # get the effective parameters
  effective <- allinits
  notfixed <- setdiff(seq(along=allinits), fixedpars)
  effective[notfixed] <- pars
  # effective will hold the *remaining* effective values
  function(constraint) {
    if (length(constraint)==0)
      return(NULL)
    total <- effective[constraint]
    m <- max(constraint)
    n <- length(effective)
    if (m<n)
      effective <<- effective[(m+1):n]
    else
      effective <<- NULL
    total
  }
}

# get path-dependent variables in canonical order
normalHistory <- function(pathvars) {
  canonical <-  c("TIS", "TIC", "LN(TIS)", "LN(TIC)")
  x <- match(pathvars, canonical)
  if (any(is.na(x)))
    stop(paste(pathvars[is.na(x)], "Unrecognized history variable", sep=": "))
  canonical[x[order(x)]]
}

# create appropriate model, given free parameters
buildModel <- function(calc, pars) {
  args <- calc@args
  nstates <- args$nstates
  totalMaker <- totalEffectMaker(args$allinits, pars, args$fixedpars)
  # transitions
  n <- args$nintens
  k <- totalMaker(args$baseconstrvec)
  b <- totalMaker(args$constrvec)
  other <- rep(0, n*args$nmisccovs)
  p <- totalMaker(args$pathconstrvec)
  # resequence path effects to be consistent with our internal order
  npath <- length(args$pathvars)
  if (npath>0) {
    p <- matrix(p, ncol=npath)
    c <- normalHistory(args$pathvars)
    x <- match(c, args$pathvars)
    p <- c(p[ , x])
  }
  permit <- matrix(as.logical(args$qvector), byrow=TRUE, ncol=nstates)
  coef <- matrix(c(k, b, other, p), byrow=TRUE, ncol=n)
  trans <- Specification(permit, coef)

  # measurement error
  if (args$misc == 0)
    # No misclassification
    return(Model(trans))
  # I don't think misc 1  or 2 differ at this point
  # 2 just happens to have all constraints fixed
  n <- args$nmisc
  k <- totalMaker(args$basemiscconstrvec)
  other <- rep(0, n*args$ncovs)
  b <- totalMaker(args$miscconstrvec)
  # no path effects on misclassification
  p <- rep(0, n*npath)
  
  permit <- matrix(as.logical(args$evector), byrow=TRUE, ncol=nstates)
  coef <- matrix(c(k, other, b, p), byrow=TRUE, ncol=n)
  if (args$misc == 2) {
    if (nrow(coef)>2){
      if (any(coef[-1,] != 0))
        stop("Simple Errors but too many coefficients")
      coef <- coef[1,]
    }
    err <- SimpleSpecification(permit, c(coef))
  }
  else
    err <- Specification(permit, coef)
  ErrorModel(trans, err)
}

# obsMaker function returns a function which is an iterator.
# The returned function returns, for each call, an  Observations object
# for a single case or NULL if no more data.
# First row of Observations is the constant 1.
# Next rows are the transition covariates.
# Next rows are the misclassification covariates.
#
# built out of raw data
# covariates have all values for covariate 1, then all for covariate 2, etc
obsMaker <- function(subject, time, state, covvec, ncovs, misccovvec, nmisccovs) {
  iStart <- 1 # index of next observation
  ncol <- ncovs+nmisccovs
  if (ncol>0)
    cov <- matrix(c(covvec, misccovvec), ncol=ncol)
  else
    cov <- matrix(nrow=length(subject), ncol=0)
  function() {
    if (iStart > length(subject))
      return(NULL)
    id <- subject[iStart]
    mask <- subject == id
    nobs <- sum(mask)
    iStart <<- iStart + nobs
    Observations(time[mask], state[mask],
                 rbind(rep(1, nobs),
                      t(cov[mask, , drop=FALSE])),
                 id
                )
  }
}

pullCovs <- function(covs, subset, ncovs) {
  if (ncovs==0)
    return(numeric(0))
  covs <- matrix(covs, ncol=ncovs)
  c(covs[subset,])
}

# make corresponding true paths object, inferring time steps
makeTruePaths <- function(obs, model, stepnumerator, stepdenominator) {
  datai <- floor(stepdenominator*time(obs)/stepnumerator + 0.5)
  datat <- datai*stepnumerator/stepdenominator
  mesht <- stepnumerator*seq(from=min(datai), to=max(datai))/stepdenominator
  obs@time <- datat
  TruePaths(mesht, model, obs)
}  

# return matrix
# each row is a different subject
# columns are the usual 6 return values (only some have good info)
# + 7th column with subject id
innerCompute <- function(calc) {
  args <- calc@args
  subject <- args$subject
  pars <- args$params
  active <- activeCases(calc)
  iActive <- rep(TRUE, length(subject))
  if (length(active)>0)
    iActive <- subject %in% active
  model <- buildModel(calc, args$params)
  ncovs <- args$ncovs
  nmisccovs <- args$nmisccovs
  obsIt <- obsMaker(subject[iActive], args$time[iActive],
                    args$state[iActive],
                    pullCovs(args$covvec, iActive, ncovs),
                    ncovs,
                    pullCovs(args$misccovvec, iActive, nmisccovs),
                    nmisccovs)
  obs <- obsIt()
  extract <- function(r) {
    if (args$isexact)
      isGood <- isGoodExact(r@pathMaker@truePaths)
    else
      isGood <- isGoodApprox(r@pathMaker@truePaths)
    list(ll=sum(r@likelihoods$likelihood[isGood]), nGoodPath=sum(isGood))
  }

  details <- matrix(nrow=0, ncol=7)
  while (! is.null(obs)){
    tp <- makeTruePaths(obs, model, args$stepnumerator, args$stepdenominator)
    pm <- CompletePathMaker(tp, args$pathoffset, normalHistory(args$pathvars))
    r <- evaluatePaths(pm, model)
    r2 <- extract(r)
    # we can't reliably compute total good nodes including duplicates
    # but we give an upper bound
    t <- matrix(c(r2$ll, 1, r2$nGoodPath, rep(0, 2),
                  r2$nGoodPath*ncol(pm@truePaths@paths),
                  obs@subjectid), nrow=1)
    details <- rbind(details, matrix(c(r2$ll, 1, r2$nGoodPath, rep(0, 2),
                                       r2$nGoodPath*ncol(pm@truePaths@paths),
                                       obs@subjectid), nrow=1))
    obs <- obsIt()
  }
  details
}


############## Class Definition
setClass("fakeCalculator", representation(args="list",
                                            do.what="integer",
                                            subset="integer",
                                            results="numeric",
                                            details="matrix"))
         # args are the C arguments.
         # do.what is the code for the type of computation to perform
         # subset is the id's of cases being analyzed.  If length is 0, do all
         # results of the last computation; empty vector if invalid
         # details are results per case
# Note that results has -2*LL, while details has LL of each case


setIs("fakeCalculator", "mspathAbstractCalculator")

############### Constructors
# maybe keep this name
fakeCalculator <- function(do.what, params, allinits, misc, subject, time, state, qvector, evector, covvec,
                    constrvec, misccovvec, miscconstrvec, baseconstrvec, basemiscconstrvec, 
                    pathvars, pathoffset, pathconstrvec,
                    initprobs, nstates, nintens, nintenseffs, nmisc, nmisceffs, nobs, npts,
                    ncovs, ncoveffs, nmisccovs, nmisccoveffs,
                    npatheffs,
                    isexact, fixedpars, stepnumerator, stepdenominator)
  {
      # the actual parameter values will be set later; we just need their size here
      p <- length(params)  

      npars <- nintenseffs + ncoveffs + nmisceffs + nmisccoveffs + npatheffs
      npath <- length(pathvars)
      notfixed <- setdiff(1:npars, fixedpars)
      if (!is.null(fixedpars))  {
        nfix <- length(fixedpars)
      }
      else {
        fixedpars <- -1
        nfix <- 0
      }
      if (!misc) {
          evector <- misccovvec <- miscconstrvec <- initprobs <- nms <- NULL
          nms <- 0
      }
      else {
          ematrix <- t(matrix(evector, nrow=nstates))
          diag(ematrix) <- 0
          esum <- apply(ematrix, 1, sum)
          nms <- max ( seq(along=esum)[esum > 0])
      }
      if (! missing(pathvars)){
                                        # deal with difference between C++ names and fake names
        pathvars[pathvars=="TSO"] <- "TIC"
        pathvars[pathvars=="LN(TSO)"] <- "LN(TIC)"
      }


      # we don't use path effects on misclassification probability
      pathmiscconstrvec <- NULL
      npathmisceffs <- 0
      args <- list(
                params = as.double(params),
                allinits = as.double(allinits),
                misc = as.integer(misc),
                p = as.integer(p),
                subject = as.integer(subject),
                time = as.double(time),
                state = as.integer(state),
                qvector = as.integer(qvector),
                evector = as.integer(evector),
                covvec = as.double(covvec),
                constrvec = as.integer(constrvec),
                misccovvec = as.double(misccovvec),
                miscconstrvec = as.integer(miscconstrvec),
                baseconstrvec = as.integer(baseconstrvec),
                basemiscconstrvec = as.integer(basemiscconstrvec),
                pathvars = as.character(pathvars),
                pathoffset = as.double(pathoffset),
                pathconstrvec = as.integer(pathconstrvec),
                pathmiscconstrvec = as.integer(pathmiscconstrvec),
                initprobs = as.double(initprobs),
                nstates = as.integer(nstates),
                nms = as.integer(nms),
                nintens = as.integer(nintens),
                nintenseffs = as.integer(nintenseffs),
                nmisc = as.integer(nmisc),
                nmisceffs = as.integer(nmisceffs),
                nobs = as.integer(nobs),
                npts = as.integer(npts),
                ncovs = as.integer(ncovs),
                ncoveffs = as.integer(ncoveffs),
                nmisccovs = as.integer(nmisccovs),
                nmisccoveffs = as.integer(nmisccoveffs),
                npath = as.integer(npath),
                npatheffs = as.integer(npatheffs),
                npathmisceffs = as.integer(npathmisceffs),
                isexact = as.integer(isexact),
                nfix = as.integer(nfix),
                fixedpars = as.integer(fixedpars),
                stepnumerator = as.integer(stepnumerator),
                stepdenominator = as.integer(stepdenominator),
                PACKAGE="mspath"
                )
      new("fakeCalculator",
          args = args,
          do.what = as.integer(do.what),
          subset = integer(0)
          )
    }

# primarily for use of distributed calculator
fakeCalculatorFromArgs <- function(args, do.what=1) {
  new("fakeCalculator",
      args = args,
      do.what = as.integer(do.what),
      subset = integer(0)
      )
}

################## Setup for Calculation

# set parameters
setMethod("params<-",
          signature(calc = "fakeCalculator", value = "numeric"),
          function(calc, value) {
            calc@results <- numeric()
            #  print("Setting params in regular calculator")
            calc@args$params <- as.double(value)
            calc@args$name <- "setParams"
            calc
          })



# Set the ID's of cases to analyze
# value == empty vector -> analyze all cases
setMethod("activeCases<-",
          signature(calc = "fakeCalculator", value = "numeric"),
          function(calc, value) {
            calc@results <- numeric()
            if (length(value) == 0) {
              calc@subset <- integer(0)
            }
            else {
              calc@subset <- sort(as.integer(value))
              # the C requires the  values to be sorted
            }
            calc
          })



################ Calculation

# perform the computation
# updates calc, including storing results in it
# the optional params, activeCases, and do.what set the appropriate values
# before computation.  These values will persist for later
# computations unless explicitly reset.
setMethod("calculate",
          signature(calc = "fakeCalculator"),
          function(calc, params, activeCases, do.what) {
            if (!missing(params))
              params(calc) <- params
            if (!missing(activeCases))
              activeCases(calc) <- as.integer(activeCases)
            if (!missing(do.what)) {
              calc@do.what <- as.integer(do.what)
              calc@args$do.what <- as.integer(do.what)
            }
            
            details <- innerCompute(calc)
                                        # drop subject id
            results <- details[,1:6, drop=FALSE]
            results[,1] <- -2*log(results[,1])
            results <- apply(results, 2, sum)
            calc@results <- results
            calc@details <- details
            calc
          })

# returns a matrix with counts in the columns
# steps may be fractional if tree has variable depth
# One row for each case in computer (not just activeCases)
setMethod("estimateWork",
          signature(calc = "fakeCalculator"),
          function(calc){
            oldcases <- activeCases(calc)
            calc@args$do.what <- as.integer(0)
            s <- sapply(unique(calc@args$subject), function(id) {
              calc <- calculate(calc, activeCases=id)
              x <- calc@results
              c(id, x[4], x[3], x[5], x[6], x[6]/x[3])
            })
            s <- t(s)
            dimnames(s) <- list(rep("", dim(s)[1]),
                                c("ID", "Good Nodes", "Good Paths", "Bad Nodes",
                                  "Good Path Nodes", "Steps"))
            # next step is essential for C and R states to be consistent
            # for the caller.  The caller will have the old
            # R states, so we must ensure that the C state matches it.
            activeCases(calc) <- oldcases
            s
          })


################ Analysis

# ID's of cases to analyze
# return empty list if it is all of them
# For callers it might make more sense to return calc@args$subject,
# but that would make it hard to restore the state.
# See estimateWork for an example.
setMethod("activeCases",
          signature(calc = "fakeCalculator"),
          function(calc) calc@subset)

# best estimate of likely effort, a number
setMethod("effort",
          signature(calc = "fakeCalculator"),
          function(calc) {
            calc@results[4]  # number of unique good nodes
          })


# -2 * log likelihood
setMethod("minus2loglik",
          signature(x = "fakeCalculator"),
          function(x) {
            x@results[1]
          })

# subjects under analysis
setMethod("nActiveCases",
          signature(calc = "fakeCalculator"),
          function(calc) {
            n <- length(calc@subset)
            if (n == 0)
              n <- nCases(calc)
            n
          })

# total subjects in study
# same value should be in results[2]
setMethod("nCases",
          signature(x = "fakeCalculator"),
          function(x) x@args$npts)

# Various Path Iteration Counts
setMethod("nGoodPaths",
          signature(x = "fakeCalculator"),
          function(x) {
            x@results[3]
          })
setMethod("nGoodNodes",
          signature(x = "fakeCalculator"),
          function(x) {
            x@results[4]
          })
setMethod("nBadNodes",
          signature(x = "fakeCalculator"),
          function(x) {
            x@results[5]
          })
setMethod("nGoodPathNodes",
          signature(x = "fakeCalculator"),
          function(x) {
            x@results[6]
          })

# vector of results
setMethod("results",
          signature(calc = "fakeCalculator"),
          function(calc)
          calc@results)

# matrix of per case results; one row per case, subject id at end
setGeneric("details",
           function(calc) standardGeneric("details"))
setMethod("details",
          signature(calc = "fakeCalculator"),
          function(calc)
          calc@details)

################# Destructors/Cleanup

# all done. clean up
setMethod("done",
          signature(calc = "fakeCalculator"),
          # nothing to do for this class
          function(calc) calc)
