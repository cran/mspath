### Function to fit multi-state path models using discrete time approximation
### with either arbitrary observation times or observed exact transition times
### with or without misclassification between true and underlying states

# TODO
# New handling of path-dependent vars
#    check elimination of path effects on misclassification
#    should I pass in npath like the other variables?
#    interpreting the result matrices
#       as I've augmented them with path effects
#
# separate fn for counts? or check the do.what arg
# Maybe delete arguments we'll never use. (mostly done)
# Figure out what support functions are appropriate.

# NOTE
# Unlike msm, the intercept (aka "baseline") terms are already on the logistic scale.
# So an intercept b contributes to an exp(b) term.
# Thus, we do no transformations of those terms here.

# Changes
# 03-Jan-2006 RB Eliminate most uses of factor for subject.
#                The transformation of subject id by factor was making it
#                difficult to tell which case an error concerned.
#                Also, the use of as.numeric(factor()) is not recommended
#                (by the factor documentation) because the results are
#                unstable.
#                However, tapply requires factor arguments, so I retain those.
#
# 23-Jan-2006 RB Create calculator classes and extend for distributed calculation.
#
# 27-Feb-2006 RB Add SIMPLE option to misc argument.
#
# 10-Apr-2006 RB Add profile argument to get stats from distributed computing
# 20 Nov 2006 RB deleted dead code (lik.mspath) and horrible global var (mspath.results) it used.
# 15 Dec 2006 RB add ability to trace each fn evaluation in optimization
# 28 Dec 2006 RB Create S4 results class and big changes to results code.
# 10 Jan 2007 RB misc fixes and tweaks to results object
# 18 Mar 2007 RB add ability to pass in arbitrary calculator factory + streamline code
#  2 Apr 2007 RB tweak argument capture, which wasn't working at all.
#  3 Apr 2007 RB factored different processing paths into different inner functions
#                Added path simulation.
# 16 Apr 2007 RB default initprobs were being computed only if misclassification.  fixed.
# 18 Apr 2007 RB add setting random seed.
#  6 Jul 2007 RB skip data/model consistency check for simulation
#                create batchsmoosh() to reduce memory use of simulation results
#  7 Sep 2007 RB change handling of duplicate entries in one time step
# 11 Sep 2009 RB allow inputs in which state is not observed.
#                change handling of duplicates, partly to account for unobserved state.


# See mspath.Rd for info on the inputs and outputs of this function
# It is the main external entry point.
mspath <- function(formula,   # formula with  observed Markov states   ~  observation times (required)
                qmatrix,    # matrix of 1s and 0s with indices of allowed transitions (diagonal is ignored) (required)
                misc = FALSE, # TRUE for full misclassification model
                              # SIMPLE for direct specification of constant probabilities
                ematrix = NULL,    # matrix of 1s and 0s with indices of allowed misclassfications (diagonal is ignored) (required)
                inits,      # initial values of optimisation or fixed values (required)
                subject = NULL, # optional, defaults to all the same if not given
                covariates = NULL, # formula specifying covariates on transition rates.
                constraint = NULL, # which intensities have covariates on them (as in Marshall et al.)
                misccovariates = NULL, # formula specifying covariates on misclassification probs
                miscconstraint = NULL, # which misc probs have covariates on them
                qconstraint = NULL, # constraints on equality of baseline intensities
                econstraint = NULL, # constraints on equality of baseline misc probs
                pathvars = NULL, # character vector of path-dependent variables
                pathoffset = 0,  # add this to time in path
                pathconstraint = NULL,  # constraints on equality of path effects on intensities
                initprobs = NULL,  # initial state occupancy probabilities
                data=list(),       # optional data frame in which to interpret variable names
                isexact = FALSE,   # true if entry time to absorbing states is observed exactly
                fixedpars = NULL, # specify which parameters to fix
                stepnumerator = 1,  # maximum step size in discrete approximation
                stepdenominator = 1,  # rational number; use integers
                do.what = 1, # 1 for likelihood, 0 for counts, -1 for detailed counts, 10 for simulate
                testing, # testing hook.  Provide list of expected args just before call to C
                comm, # number of communicator, if any (distributed calcs)
                profile=FALSE, # true to profile distributed calculation
                calcFactory = mspathCalculator, # function to create calculator
                seed, # optional random seed (integer)
                trace, # if present, list of arguments to checkpoint
                ... # options to optim
                )
  {
### CAPTURE CALL AND ARGUMENTS
    mycall <- match.call(expand.dots=FALSE)
    # mycall does not have args caller didn't specify
    # and the values that it does have are unevaluated expressions.
    # Next step gives actual values of all user-specified arguments.
    # This leaves out default values, but they don't seem worth preserving.
    myargs <- list()
    for (nm in names(mycall)[-1])
      if (nm != "...")
        if (nm == "subject")
          myargs[[nm]] <- eval(substitute(subject), data, parent.frame())
        else if (nm == "misc" && is(substitute(misc), "name"))
          myargs[[nm]] <- as.character(substitute(misc))
        else
          myargs[[nm]] <- get(nm)

    # tack on any ... arguments
    myargs <- c(myargs, list(...))


### CHECK STEP SIZE IS RATIONAL
    if (as.integer(stepnumerator) != stepnumerator)
      stop("Step size numerator must be an integer")
    if (as.integer(stepdenominator) != stepdenominator)
      stop("Step size denominator must be an integer")
    if (stepdenominator == 0)
      stop("Step size denominator is 0")
    
### INTENSITY MATRIX
      nstates <- dim(qmatrix)[1]
      diag(qmatrix) <- 0
      qmatrix <- mspath.check.qmatrix(qmatrix)
      qvector <- as.numeric(t(qmatrix)) # matrix to vector by filling rows first
      nintens <- sum(qmatrix)

      if (!is.null(qconstraint)) {
          if (length(qconstraint) != nintens)
            stop("baseline intensity constraint of length" ,length(qconstraint), "should be", nintens)
          baseconstrvec <- as.numeric(factor(qconstraint))
      }
      else 
        baseconstrvec <- 1:nintens
      nintenseffs <- max(baseconstrvec)
    if (is.null(initprobs))
      initprobs <- c(1, rep(0, nstates-1))

### MISCLASSIFICATION MATRIX
      if ( ! missing(misc) && is(substitute(misc), "name")) {
        if (as.character(substitute(misc)) == "SIMPLE") {
          if (! (missing(misccovariates) && missing(miscconstraint)))
            stop("SIMPLE misclassification error requires omitting misccovariates and miscconstraint.")
          miscType <- 2
        } else
          stop("Only legal values for misc are TRUE, FALSE, and SIMPLE without quotes.")
      } else if(misc)
          miscType <- 1
        else
          miscType <- 0

      if (miscType>0) {
          if (missing(ematrix)) stop("Misclassification matrix not given")
          diag(ematrix) <- 0
          ematrix <- mspath.check.ematrix(ematrix, qmatrix)
          evector <- as.numeric(t(ematrix)) # matrix to vector by filling rows first
          nmisc <- sum(ematrix)
          if (!is.null(econstraint)) {
              if (length(econstraint) != nmisc)
                stop("baseline misclassification constraint of length" ,length(econstraint), "should be", nmisc)
              basemiscconstrvec <- as.numeric(factor(econstraint))
          }
          else 
            basemiscconstrvec <- 1:nmisc
          nmisceffs <- max(basemiscconstrvec)
      }
      else {
          nmisc <- nmisceffs <- 0; ematrix <- evector <- basemiscconstrvec <- NULL
      }

      
### BASIC DATA: BY OBSERVATION TIME

          ## form vectors with subject ID, state and corresponding observation time
    mf <- model.frame(formula, data=data)
    state <- mf[,1]
    time <- mf[,2]
    droprows <- as.numeric(attr(mf, "na.action"))
    nobs <- length(c(state, droprows)) # = rows in original data.frame
    statetimerows.kept <- (1:nobs)[! ((1:nobs) %in% droprows)]
    subjectVariableName <- "subject"  # for use by simulator
    if (missing(subject)) {
      subject <- rep(1, dim(mf)[1])
    } else {
      subjectArg <- substitute(subject)
      if (is(subjectArg, "name"))
        subjectVariableName <- as.character(subjectArg)
      subject <- eval(subjectArg, data, parent.frame())
    }
          subjrows.kept <- (1:length(subject)) [!is.na(subject)]
          subject <- na.omit(subject)
          # convert to integer id if necessary
          npts <- length(unique(subject))      
          if (!is.numeric(subject) ||
              length(unique(as.integer(subject))) != npts) {
            subject <- match(subject, unique(subject))
          }

       covrows.kept <- misccovrows.kept <- 1:nobs

### COVARIATES
      if (!is.null(covariates)) {
          pc <- mspath.process.covs(covariates, data, constraint, nobs, nintens)
          covvec <- pc$covvec;  constrvec <- pc$constrvec;  covlabels <- pc$covlabels
          ncovs <- pc$ncovs;  ncoveffs <- pc$ncoveffs; covrows.kept <- pc$kept.rows
          covmeans <- pc$covmeans; covfactor <- pc$covfactor
      }
      else {
          ncovs <- ncoveffs <- 0
          covvec <- constrvec <- covlabels <- covmeans <- covfactor <- NULL
      }      

### MISCLASSIFICATION COVARIATES
      if (!is.null(misccovariates) && (miscType==1)) {
          pc <- mspath.process.covs(misccovariates, data, miscconstraint, nobs, nmisc)
          misccovvec <- pc$covvec;  miscconstrvec <- pc$constrvec;  misccovlabels <- pc$covlabels
          nmisccovs <- pc$ncovs;  nmisccoveffs <- pc$ncoveffs; misccovrows.kept <- pc$kept.rows
          misccovmeans <- pc$covmeans; misccovfactor <- pc$covfactor
      }
      else if (is.null (misccovariates) |! (miscType == 1)) {
          if (!is.null (misccovariates))
            warning("misccovariates have been specified, but misc is FALSE or SIMPLE. Ignoring misccovariates.")
          nmisccovs <- nmisccoveffs <- 0
          misccovvec <- miscconstrvec <- misccovlabels <- misccovmeans <- misccovfactor <- NULL
      }

### PATH-DEPENDENT VARIABLES (aka HISTORY) EFFECTS ON TRANSITIONS
    if (!is.null(pathvars)) {
      pc <- mspath.process.path(pathvars, pathconstraint, nintens)
      pathconstrvec <- pc$constrvec
      pathlabels <- pc$covlabels
      npath <- pc$ncovs
      npatheffs <- pc$ncoveffs
    }
    else {
      npath <- npatheffs <- 0
      pathconstrvec <- pathlabels <- NULL
    }
      

      
### DROP MISSING OR OVERLAPPING DATA      
      final.rows <- intersect(intersect(statetimerows.kept, subjrows.kept),
                              intersect(covrows.kept, misccovrows.kept))
      subject <- subject[ subjrows.kept %in% final.rows ]
      time <- time[ statetimerows.kept %in% final.rows ]
      state <- state[ statetimerows.kept %in% final.rows ]
      # final.rows gives indices in original data of corresponding entries
      # in subject, time, and state
      overlap <- mspath.remove.overlap(subject, time, state, final.rows,
                                       stepnumerator, stepdenominator)
      final.rows <- overlap$final.rows
      subject <- overlap$subject
      state <- overlap$state
      time <- overlap$time
      duplicate.rows <- overlap$duplicate.rows
      overlap.indices <- overlap$overlap
      if (ncovs>0) covvec <- as.numeric(t(matrix(covvec, ncol=ncovs)[covrows.kept %in% final.rows]))
      if (nmisccovs>0) misccovvec <- as.numeric(t(matrix(misccovvec, ncol=nmisccovs)[misccovrows.kept %in% final.rows]))
      nmiss <- length(setdiff(1:nobs, final.rows))
      plural <- if (nmiss==1) "" else "s"
      if (nmiss > 0) warning(nmiss, " record", plural, " dropped due to missing values")
      if (!do.what==10)
        mspath.check.consistency(qmatrix, miscType != 0, state=state, subject=subject, time=time)
      nobs <- length(state)
      npts <- length(unique(subject))      

### FORM LIST OF INITIAL PARAMETERS
      npars <- nintenseffs + ncoveffs + npatheffs + nmisceffs + nmisccoveffs
      plabs <- c(rep("qbase",nintenseffs), rep("qcov", ncoveffs), rep("hcov", npatheffs),
                 rep("ebase",nmisceffs), rep("ecov",nmisccoveffs))
      if (npars != length(inits)) {
          err.msg <- paste(length(inits),"initial values supplied, expected",
                           nintenseffs,"intensities,",
                           ncoveffs,"covariate effects,",
                           npatheffs,"path effects",
                           if (miscType != 0) paste(", ",nmisceffs,"misclassification probabilities and",
                                           nmisccoveffs,"misclassification covariate effects")
                           )
          stop(err.msg)
      }


### FIX SOME PARAMETERS IF REQUIRED
      if (miscType == 2) {
        # SIMPLE error specification.  Assure parameters are fixed.
        # nmisccoveffs must be 0, so nmisceffs are at end
        fixedpars <- sort(union(fixedpars, seq(npars-nmisceffs+1, npars)))
      }
      notfixed <- setdiff(1:npars, fixedpars)
      fixdiff <- setdiff(fixedpars, 1:npars)
      if (length(fixdiff) > 0)
        stop ( "Elements of fixedpars should be in 1, ..., ",npars, ", fixedpars contains ",paste(fixdiff,collapse=" ") )
      allinits <- inits
      if (length(fixedpars)==length(inits)) ## all parameters are fixed
        fixed <- TRUE
      else {
          inits <- inits[notfixed]
          fixed <- FALSE
      }

### DEFINE INNER COMPLETION FUNCTIONS
    
# The outer function behaves much differently depending on the
# arguments it gets, but all functions prepare the data in the same
# way.  That's what all the code above here does.  At this point,
# processing diverges.  Since future processing depends on all
# the variables defined so far, there is not a clean interface.
# In an implementation from scratch I might factor the code into
# the return objects and either reduce the interface to a few
# objects that could be passed or store them in instance variables.
# To minimize changes from existing code, in part so it will be
# easier to track future changes to the msm code from which this is
# derived, I instead define some nested functions.  Because
# of lexical scoping (which is found in R, not S), they will have
# access to all the variables defined so far.  Because these functions
# are all the last step, it does not matter that they can not
# easily update variables in this scope.
#
# I define functions rather than embed the logic in a big if statement
# in the interest of clarity.
# Ross Boylan 03 Mar 2007

       ## standard likelihood processing
doLikelihood <- function(trace) {
  # The explicit trace argument enables the missing(trace)
  # test below to work properly, i.e., detect whether
  # trace is missing from the outer call.
      if (do.what == 0)
	      fixed <- TRUE
      ## CALCULATE LIKELIHOOD AT INITIAL VALUES...
      if (fixed) {
        params <- inits
        calc <- calculate(calc, params=params)
        transCoef <- mspathCoefficients(qmatrix,
                                        params,
                                        offset=0,
                                        baseconstrvec,
                                        covlabels,
                                        constrvec,
                                        pathvars,
                                        pathconstrvec)
        if (miscType != 0)
          errCoef <- mspathCoefficients(ematrix,
                                        params,
                                        offset=nintenseffs+ncoveffs+npatheffs,
                                        basemiscconstrvec,
                                        misccovlabels,
                                        miscconstrvec,
                                        pathVars=NULL,
                                        pathConstrVec=NULL)
      }

      ## ... OR DO MAXIMUM LIKELIHOOD ESTIMATION
      else {
        objectivefn <-  function(params) {
          calc <<- calculate(calc, params=params)
          minus2loglik(calc)
        }
        if (! missing(trace)) {
          if (! "name" %in% names(trace))
              trace$name <- "mspathLikFn"
          if (! "fileName" %in% names(trace)) {
            trace$fileName <- trace$name
            trace$name <- paste(trace$name, "trace", sep=".")
          }
          trace$f <- objectivefn
          opt <- optim(inits, do.call(checkpoint, trace), hessian=TRUE, ...)
        }
        else {
          opt <- optim(inits, objectivefn, hessian=TRUE, ...)
        }
        params <- allinits
        params[notfixed] <- opt$par
        hess <- 0.5*opt$hessian # .5 to compensate for -2*LL
        if (all(eigen(hess)$values>0))
          var <- diag(solve(hess))
        else
          var <- numeric(0)
        transCoef <- mspathEstimatedCoefficients(qmatrix,
                                                 params,
                                                 offset=0,
                                                 baseconstrvec,
                                                 covlabels,
                                                 constrvec,
                                                 pathvars,
                                                 pathconstrvec,
                                                 fixedpars,
                                                 nobs,
                                                 var)

        if (miscType != 0)
          errCoef <- mspathEstimatedCoefficients(ematrix,
                                                 params,
                                                 offset=nintenseffs+ncoveffs+npatheffs,
                                                 basemiscconstrvec,
                                                 misccovlabels,
                                                 miscconstrvec,
                                                 pathVars=NULL,
                                                 pathConstrVec=NULL,
                                                 fixedpars,
                                                 nobs,
                                                 var)
      
      }
    ## FORM A MSPATH OBJECT FROM THE RESULTS
    mspathobject <- new("mspath",
                        transCoef=transCoef,
                        calc=calc,
                        overlap=duplicate.rows,
                        call=mycall,
                        callArgs=myargs
                        )
    if (miscType != 0)
      mspathobject <- new("mspathFull", mspathobject, errCoef=errCoef)
    if (! fixed) {
      optresults(mspathobject) <- opt
    }

    calc <- done(calc)
    mspathobject
  }
  
    ## special testing hook
doTesting <- function() {
  # It would be cleverer to do some kind of meta programming here
  # The argument list is already dated.
  return(stopifnot(inits == testing$inits,
                   allinits == testing$allinits,
                   misc == testing$misc,
                   subject == testing$subject,
                   time == testing$time,
                   state == testing$state,
                   qvector == testing$qvector,
                   evector == testing$evector,
                   covvec == testing$covvec,
                   constrvec == testing$constrvec,
                   misccovvec == testing$misccovvec,
                   miscconstrvec == testing$miscconstrvec,
                   baseconstrvec == testing$baseconstrvec,
                   basemiscconstrvec == testing$basemiscconstrvec,
                   pathvars == testing$pathvars,
                   pathoffset == testing$pathoffset,
                   pathconstrvec == testing$pathconstrvec,
                   initprobs == testing$initprobs,
                   nstates == testing$nstates,
                   nintens == testing$nintens,
                   nintenseffs == testing$nintenseffs,
                   nmisc == testing$nmisc,
                   nmisceffs == testing$nmisceffs,
                   nobs == testing$nobs,
                   npts == testing$npts,
                   ncovs == testing$ncovs,
                   ncoveffs == testing$ncoveffs,
                   nmisccovs == testing$nmisccovs,
                   nmisccoveffs == testing$nmisccoveffs,
                   npatheffs == testing$npatheffs,
                   isexact == testing$isexact,
                   fixedpars == testing$fixedpars,
                   plabs == testing$plabs,
                   stepnumerator == testing$stepnumerator,
                   stepdenominator == testing$stepdenominator))
}

    ## get counts for individual paths
doCounts <- function() {
  # implementation is a hack
  # Could be improved now that I am calling C routines
  # through an interface that permits arbitrary return objects.
  r <- estimateWork(calc)
  calc <- done(calc)
  return(r)
}

    ## generate random paths with full data
    # arguments nec for missing() tests to work
doSimulate <- function(data, covariates, misccovariates) {
  # assemble a data frame with all data, but possibly omitting some rows
  if (! missing(data)){
    if (subjectVariableName %in% names(data))
      simData <- data[final.rows,]
    else
      simData <- data.frame(subjectVariableName=subject, data[final.rows,])
  } else {
    simData <- data.frame(subjectVariableName=subject)
  }
  # simData now has subject and all vars in data argument
  # now add any that we got from the environment
  if (missing(covariates))
    vars <- c()
  else
    vars <- all.vars(covariates)
  if (!missing(misccovariates))
    vars <- c(vars, all.vars(misccovariates))
  if (subjectVariableName %in% vars)
    stop(paste(subjectVariableName,
               ", the subject variable, appears as a covariate",
               " or misclassification covariate.", sep=""))
  vars <- c(all.vars(formula), vars)
  vars <- unique(vars)
  for (v in vars[!(vars %in% names(simData))]) {
    simData <- data.frame(simData, eval(as.name(v), parent.frame(2))[final.rows])
    names(simData)[ncol(simData)] <- v
  }

  # identify values that will be simulated
  fterms <- terms(formula)
  keyvars <- as.character(attr(fterms, "variables"))[-1]
  # I think istate will always be 1, but just in case ...
  istate <- attr(fterms, "response")
  itime <- 3-istate
  keycols <- match(keyvars[c(istate, itime)], names(simData))

  # and go
  params(calc) <- inits[notfixed]
  raw <- simulateObservations(calc)
  simData <- simData[raw$row,]
  simData[,keycols] <- cbind(raw$state, raw$time)
  simData
}
    
### END INNER FUNCTION DEFINITIONS
    

    ## Testing hook--not for normal use
    if (! missing(testing))
      return(doTesting())

    if (missing(calcFactory) && !missing(comm))
      calcFactory <- mspathDistributedCalculatorFactory(comm, profile)
    calc <- calcFactory(do.what, inits, allinits, miscType, subject, time,
                        state,
                        qvector, evector, covvec, constrvec,
                        misccovvec, miscconstrvec,
                        baseconstrvec, basemiscconstrvec,
                        pathvars, pathoffset, pathconstrvec,
                        initprobs, nstates, nintens, nintenseffs,
                        nmisc, nmisceffs,                                 
                        nobs, npts, ncovs, ncoveffs, nmisccovs,
                        nmisccoveffs, npatheffs,
                        isexact, fixedpars, stepnumerator, stepdenominator)


    if (do.what == -1)
      return(doCounts())

    if (do.what == 10) {
      if (!missing(seed))
        set.seed(seed)
      return(doSimulate(data, covariates, misccovariates))
    }

    # In typical useage, none of the previous conditions apply and
    # we end here.
    doLikelihood(trace)
  }
                      

expit <- function(x){exp(x) / (1 + exp(x))}
logit <- function(x){log (x / (1 - x)) }


### Process covariates and covariates constraints, in preparation for being passed to the likelilhood optimiser
### This function is called for both sets of covariates (transition rates and the misclassification probs)

mspath.process.covs <- function(covariates, # formula:  ~ cov1 + cov2 + ...
                             data,
                             constraint,
                             nobs,       # number of observations including missing values
                             nmatrix     # number of transition intensities / misclassification probs
                             )
  {
      ## form covariate matrix to vectorise and pass to optimisation
      mm <- as.data.frame(model.matrix(covariates, data=data))
      covlabels <- names(mm)[-1]
      ncovs <- length(covlabels)
      mm <- mm[-1]
      mf <- model.frame(covariates, data=data)
      droprows <- as.numeric(attr(mf, "na.action"))
      kept.rows <- (1:nobs)[! ((1:nobs) %in% droprows)]
      ## centre the covariates about their means
##      covmeans <- apply(mm, 2, mean)
      covmeans <- rep(0, ncovs)
      covstds <- apply(mm, 2, sd)
      covfactor <- sapply(mf, is.factor)
      mm <- sweep(mm, 2, covmeans)
      ##       mm <- sweep(mm, 2, covstds, "/")
      covvec <- unlist(mm)
      if (is.null(constraint))
        constrvec <- 1:(nmatrix*ncovs)
      else {
          ## check and parse the list of constraints on covariates
          for (i in names(constraint))
            if (!(is.element(i, covlabels))){
                factor.warn <- if (is.factor(data[, i]))
                  "\n\tFor factor covariates, specify constraints using covnameCOVVALUE = c(...)"
                else ""                  
                stop("Covariate \"", i, "\" in constraint statement not in model.", factor.warn)
            }
          constrvec <- mspath.make.constraint(covlabels, constraint, nmatrix)
      }
      ncoveffs <- max(unique(constrvec))
      list(covvec=covvec, # vector of concatenated covariate data
           constrvec=constrvec, 
           covlabels=covlabels, # covariate names
           covfactor=covfactor, # indicators for whether each covariate is a factor
           ncovs=ncovs,         # number of covariates
           ncoveffs=ncoveffs,   # number of distinct covariate effect parameters
           covmeans=covmeans,
           covstds=covstds,
           kept.rows = kept.rows # rows of original data file not containing missing covariate values
           )
  }

## build complete vectorised list of constraints for covariates in covariates statement
mspath.make.constraint <- function(covlabels, constraint, nmatrix) {
    # inputs:
    #   covlabels = vector of strings
    #   constraint = list
    #        e.g. constraints = (x1=c(3,3,4,4,5), x2 = (0.1,0.2,0.3,0.4,0.4))       
    #        x1, x2 are elements of covlabels
    #        values are = for elements that should be equal
    #        Omitted labels are assumed all unique.
    #   nmatrix = number of non-0 elements in matrix
    #
    # Prerequisite: names(constraint) are elements of covlabels
    #
    # output:
    #   constrvec = raw constraint vector, e.g. for preceding example
    #        constrvec = c(1,1,2,2,3,4,5,6,7,7)
  constrvec <- numeric()
  maxc <- 0
  for (i in seq(along=covlabels)){
    if (is.element(covlabels[i], names(constraint))) {
      thiscon <- constraint[[match(covlabels[i], names(constraint))]]
      if (length(thiscon) != nmatrix)
        stop("\"",names(constraint)[i],"\"","constraint of length",length(constraint[i]),"should be",nmatrix)
      constrvec <- c(constrvec, maxc + as.numeric(factor(thiscon)))
    }
    # original else constrvec <- c(constrvec, (i-1)*nmatrix + 1:nmatrix)
    # RB revision
    else constrvec <- c(constrvec, maxc + 1:nmatrix)
    maxc <- max(constrvec)
  }
  constrvec
}

### Process path-dependent variables and constraints,
### in preparation for being passed to the likelilhood optimiser
### This function is called for  (transition rates and the misclassification probs)

mspath.process.path <- function(pathvars, # vector of names
                                constraint,
                                nmatrix   # number of transition intensities / misclassification probs
                                )
  {
      covlabels <- pathvars
      ncovs <- length(covlabels)
      if (is.null(constraint))
        constrvec <- 1:(nmatrix*ncovs)
      else {
          ## check and parse the list of constraints on covariates
          for (i in names(constraint))
            if (!(is.element(i, covlabels))){
                stop("Path Covariate \"", i, "\" in constraint statement not in history.")
            }
          constrvec <- mspath.make.constraint(covlabels, constraint, nmatrix)
      }
      ncoveffs <- max(unique(constrvec))
      list(constrvec=constrvec, 
           covlabels=covlabels, # covariate names
           ncovs=ncovs,         # number of covariates
           ncoveffs=ncoveffs    # number distinct parameters
           )
  }

### Check the consistency of the supplied data with the specified model 

mspath.check.consistency <- function(qmatrix, misc,subject=NULL,
                                  state, time)
  {
      mspath.check.state(nrow(qmatrix), state)
      if (!misc)
        mspath.check.model(state, subject, qmatrix)
      mspath.check.times(time, subject)
      invisible()
  }

### Check transition matrix indicators

mspath.check.qmatrix <- function(qmatrix)
{
    qmatrix <- as.matrix(qmatrix)
    if (nrow(qmatrix) != ncol(qmatrix))
      stop("Number of rows and columns of qmatrix should be equal")
    if (!all ( is.element(qmatrix, c(0,1)) ) )
      stop("Not all off-diagonal elements of qmatrix are 1 or 0")
    qmatrix
}

## Check misclassification matrix indicators

mspath.check.ematrix <- function(ematrix, qmatrix)
{
    ematrix <- as.matrix(ematrix)
    if (!all(dim(qmatrix) == dim(ematrix)))
      stop("Dimensions of qmatrix and ematrix should be the same")
    if (!all ( is.element(ematrix, c(0,1)) ))
      stop("Not all off-diagonal elements of ematrix are 1 or 0")
    ematrix
}

### Check elements of state vector

mspath.check.state <- function(nstates, state)
  {
      statelist <- if (nstates==2) "1, 2" else if (nstates==3) "1, 2, 3" else paste("1, 2, ... ,",nstates)
      if (length(setdiff(unique(state), 0:nstates)) > 0)
        stop("State vector contains elements not in ",statelist, ", or 0 for missing")
      invisible()
  }

### CHECK IF TRANSITION PROBABILITIES FOR DATA ARE ALL NON-ZERO
### (e.g. check for backwards transitions when the model is irreversible)

mspath.check.model <- function(state, subject, qmatrix)
{
    n <- length(state)
    diag(qmatrix) <- 0
    diag(qmatrix) <- - apply(qmatrix, 1, sum)
    Pmat <- MatrixExp(qmatrix)
    Pmat[Pmat < 1e-16] <- 0
    origi <- seq(along=state)[state>0]
    state <- state[state>0]  # only observations that have state
    fromstate <- c(NA, state[1:(n-1)])
    subj.num <- as.numeric(factor(subject))
    fromstate[subj.num != c(NA, subj.num[1:(n-1)])] <- NA
    tostate <- state

    unitprob <- apply(cbind(fromstate, tostate), 1, function(x) { Pmat[x[1], x[2]] } )
    if (identical(all.equal(min(unitprob, na.rm=TRUE), 0),  TRUE))
      {
          badobs <- min ( which(unitprob==0), na.rm = TRUE)
          stop ("Data inconsistent with transition matrix for model without misclassification:\n",
                "individual ", subject[badobs], " moves from state ", fromstate[badobs],
                " to state ", tostate[badobs], " at observation ", origi[badobs], "\n")
      }
    diag(qmatrix) <- 0
    absorbing <- which (apply (qmatrix, 1, sum) == 0)
    absabs <- (fromstate %in% absorbing) & (tostate %in% absorbing)
    if (any(absabs)) {
          badobs <- min( which (absabs) )
          warning("Absorbing - absorbing transition at observation ", origi[badobs])
      }
    invisible()
}

mspath.check.times <- function(time, subject)
  {
### CHECK IF OBSERVATIONS ARE ORDERED IN TIME WITHIN SUBJECT
      orderedpt <- tapply(time, subject, function(x) { all(!duplicated(x) & order(x)==1:length(x)) })
      if (any (!orderedpt)) {
          badsubjs <- sort(unique(subject))[ !orderedpt ]
          badlist <- paste(badsubjs, collapse=", ")
          plural <- if (length(badsubjs)==1) "" else "s"
          stop ("Observations within subject", plural, " ", badlist, " are not in strictly increasing order of time")
      }
### CHECK IF ANY INDIVIDUALS HAVE ONLY ONE OBSERVATION
      nobspt <- table(subject)
      if (any (nobspt == 1)) {
          badsubjs <- sort(unique(subject))[ nobspt == 1 ]
          badlist <- paste(badsubjs, collapse=", ")
          plural <- if (length(badsubjs)==1) "" else "s"
          has <-  if (length(badsubjs)==1) "has" else "have"
          warning ("Subject", plural, " ", badlist, " only ", has, " one observation")
      }
### CHECK IF OBSERVATIONS WITHIN A SUBJECT ARE ADJACENT
      ind <- tapply(1:length(subject), subject, length)
      imin <- tapply(1:length(subject), subject, min)
      imax <- tapply(1:length(subject), subject, max)
      adjacent <- (ind == imax-imin+1)
      if (any (!adjacent)) {  
          badsubjs <- sort(unique(subject))[ !adjacent ]
          badlist <- paste(badsubjs, collapse=", ")
          plural <- if (length(badsubjs)==1) "" else "s"
          stop ("Observations within subject", plural, " ", badlist, " are not adjacent in the data file")
      }
      invisible()
  }


## Like the C++ FixedTimeStepsGenerator::integerTime()
mspath.integerTime <- function(t, stepnumerator, stepdenominator){
  floor(t*stepdenominator/stepnumerator + 0.5)
}

# return information has overlapping rows (in rounded time) removed.
# We always retain the first observation for a case.
# If that rule doesn't apply, we retain the last observation in a
# time interval.  If at least one observation in the interval has known
# state, we use the last such observation (i.e., we prefer rows with an
# observed state to those without one).
#
mspath.remove.overlap <- function(subject, time, state, final.rows,
                                  stepnumerator, stepdenominator){
  # Inputs:
  # subject, time and state are vectors drawn from final.rows
  # of the original dataset.
  # E.g., state[i] == originalstate[final.rows[i]]
  # final.rows should include no missing values.
  # Time step is given by stepnumerator/stepdenominator
  # Inputs must be sorted by subject and the time.
  #
  # Outputs:
  # A list with the following elements:
  # subject, time, state, final.rows such that no case
  # has more than one observation in a single time period.
  # In general, it is not assured that state[i] = originalstate[final.rows[i]]
  # will hold (though it does now), since state may come from a more complex calculation.
  # A value of final.row[i] means that corresponding covariates should be used.
  # duplicate.rows are indices in original data frame of rows dropped as duplicate
  # 

  d <- data.frame(subj=subject,
                  state=state,
                  time=time,
                  intTime=mspath.integerTime(time, stepnumerator, stepdenominator),
                  fri=final.rows
                  )
  outerfn <- function(d1) {
    # d1 is data for 1 subject
    # return a dataframe with only the rows we want to keep
    d1$first <- d1$intTime == min(d1$intTime) # TRUE for first time interval
    do.call("rbind", by(d1, d1$intTime, innerfn))
  }

  innerfn <- function(d2) {
    # d2 is data from one subject and one time interval
    # return a single row from d2 with our preferred observation
    if (d2$first[1])
      return(d2[1,])
    if (any(d2$state>0))
      # limit to observations with observed state
      d2 <- d2[d2$state>0,]
    # pick the last
    return(d2[nrow(d2),])
  }
           
  r <- do.call("rbind", by(d, d$subj, outerfn))
  duplicate.rows <- setdiff(final.rows, r$fri)
  if (length(duplicate.rows)>0)
    warning(length(duplicate.rows), " observations dropped because of multiple observations",
            " at the same rounded time.")
  list(final.rows=r$fri,
       subject=r$subj,
       state=r$state,
       time=r$time,
       duplicate.rows=duplicate.rows)
}

###################### classes for results
setClass("mspath",
         representation(transCoef="mspathCoefficients",
                        calc="mspathAbstractCalculator",
                        overlap="numeric", # indices in original
                        # data frame of rows dropped as duplicates
                        call="call", # that produced this object
                        callArgs="list", # resolved values of call arguments
                        opt="list" # optional optimization result
                        ))

setMethod("print", signature(x="mspath"), function(x, ...) {
  print(x@transCoef, ...)
  printFooter(x, ...)
})

# printFooter generic definition stripped out and put in allGenerics.R.
setMethod("printFooter", signature(x="mspath"), function(x, ...) {
  cat(minus2loglik(x), "= -2*log-likehood for", nCases(x),"id's.\n")
  cat(nGoodPaths(x), "good paths with", nGoodNodes(x), "good nodes and",
      nBadNodes(x), "bad nodes.",
      "\nIf each path were evaluated separately, there would be",
      nGoodPathNodes(x),"good nodes.\n")  
  invisible(x)
})
          
setMethod("show", signature(object="mspath"), function(object)
          print(object))
# optresults generic definition stripped out and put in allGenerics.R.
# optresults<- generic definition stripped out and put in allGenerics.R.

setMethod("optresults", signature(x="mspath"), function(x) x@opt)
setMethod("optresults<-", signature(x="mspath"), function(x, value) {
  x@opt <- value
  x})

# both coef and sd are standard in R, so no setGeneric here
setMethod("coef", signature(object="mspath"),
          function(object) coef(object@transCoef))

setMethod("sd", signature(x="mspath", na.rm="ANY"),
          function(x, na.rm) sd(x@transCoef, na.rm))

# return matrix of coefficients
setMethod("matrixCoef",
          signature(x = "mspath"),
          function(x, ...) matrixCoef(x@transCoef))

# -2 * log likelihood
# minus2loglik generic definition stripped out and put in allGenerics.R.
setMethod("minus2loglik",
          signature(x = "mspath"),
          function(x) {
            minus2loglik(x@calc)
          })

# counts
# nCases generic definition stripped out and put in allGenerics.R.
setMethod("nCases",
          signature(x = "mspath"),
          function(x) {
            nCases(x@calc)
          })

# nGoodPaths generic definition stripped out and put in allGenerics.R.
setMethod("nGoodPaths",
          signature(x = "mspath"),
          function(x) {
            nGoodPaths(x@calc)
          })

# nGoodNodes generic definition stripped out and put in allGenerics.R.
setMethod("nGoodNodes",
          signature(x = "mspath"),
          function(x) {
            nGoodNodes(x@calc)
          })

# nBadNodes generic definition stripped out and put in allGenerics.R.
setMethod("nBadNodes",
          signature(x = "mspath"),
          function(x) {
            nBadNodes(x@calc)
          })

# nGoodPathNodes generic definition stripped out and put in allGenerics.R.
setMethod("nGoodPathNodes",
          signature(x = "mspath"),
          function(x) {
            nGoodPathNodes(x@calc)
          })


# batchsmoosh(a1) <- a2 replaces parts of a1 with part of a2 if identical.
# It may also delete some elements entirely.
# intended to remove needless object duplication from distributed calculations
setMethod("batchsmoosh<-",
          signature(x="ANY", value="ANY"),
          function(x, value) {
            if (identical(x, value))
              return(value)
            x
          })

setMethod("batchsmoosh<-",
          signature(x="mspath", value="mspath"),
          function(x, value) {
            batchsmoosh(x@transCoef) <- value@transCoef
            batchsmoosh(x@calc) <- value@calc
            if (identical(x@call, value@call))
              x@call <- value@call
            x@callArgs <- list()
            # opt left as is
            x
          })

setMethod("batchsmoosh<-",
          signature(x="list", value="list"),
          function(x, value) {
            if (identical(x, value))
              return(value)
            for (i in seq(along=x))
              batchsmoosh(x[[i]]) <- value[[i]]
            x
          })


# full results include an error model
setClass("mspathFull",
         representation(errCoef="mspathCoefficients"),
         contains="mspath")

setMethod("coef", signature(object="mspathFull"),
          function(object) c(coef(object@transCoef), coef(object@errCoef)))

setMethod("sd", signature(x="mspathFull", na.rm="ANY"),
          function(x, na.rm) c(sd(x@transCoef, na.rm), sd(x@errCoef, na.rm)))

# x is the result of a call to optim
# do nothing if convergent, otherwise print diagnostics.
printOptim <- function(x, ...) {
  if (! "convergence" %in% names(x)) {
    cat("Optimization does not seem to have been tried\n")
    return()
  }
  c <- x$convergence
  if (c==0)
    return()
  if (c==1)
    cat("WARNING: iteration limit reached\n")
  else if (c==10)
    cat("ERROR: degeneracy of Nelder-Mead simplex.\n")
  else if (c==51)
    cat("WARNING:", x$message,"\n")
  else if (c==52)
    cat("ERROR:", x$message,"\n")
  else
    cat("ERROR: Unrecognized convergence failure with code", as.character(c),
        "\nSee ?optim for clues.\n")
}

# call with all=TRUE to print even the fixed coefficients
setMethod("print", signature(x="mspathFull"),
          function(x, ... ) {
            showAll <- FALSE
            dotargs <- list(...)
            if ("showAll" %in% names(dotargs))
              showAll <- dotargs$showAll
            printOptim(optresults(x))
            if (showAll || ! isAllFixed(x@transCoef)) {
              cat("Transition Coefficients\n")
              print(x@transCoef, ...)
              cat("\n\n")
            }
            if (showAll || ! isAllFixed(x@errCoef)) {
              cat("Measurement Error Coefficients\n")
              print(x@errCoef, ...)
              cat("\n\n")
            }
            printFooter(x, ...)
          })

setMethod("batchsmoosh<-",
          signature(x="mspathFull", value="mspathFull"),
          function(x, value) {
            x <- callNextMethod()
            batchsmoosh(x@errCoef) <- value@errCoef
            x
          })
