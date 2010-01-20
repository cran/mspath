# Contains an estimated set of coefficients and constraints
#
# For: mspath
# By: Ross Boylan
# Created: 2006-11-22
#
# The mspath.form.output routine in the old mspath.R was the point of
# departure.

# Assumes that none of the covariate names will be "intercept" or the names of
# history-dependent variables.

# The anticipated context of use is that several coefficient objects will
# be created from a single estimation.  The following variables may
# have information related to other coefficients as well as the current set:
#   params
#   fixed
#   hessian
# Note that while "params" includes fixed and free parameters, and
# "fixed" has indices in "params", hessian has dimension of the *free*
# parameters only.
# "offset" also indexes into params.
# In contrast, the other variables given to the constructors refer only
# to the current coefficient set.  The values in the constraints are
# *not* indices in "params," but are used to pick out elements in the
# subset of params for these coefficients.  So the first constraint is 1.

# mspathCoefficients describes coefficients with constraints
setClass("mspathCoefficients", representation(
                   permit="matrix",  # matrix of 0/1 indicators for allowed values
                                     # note the slot holds logical values
                   params="numeric",    # parameter values (both free and fixed)
                   baseConstrVec="integer", # constraints on intercepts
                   covVars="character", # names of covariates
                   constrVec="integer", # vector of 1 based constraints for covs
                   pathVars="character", # path variables, if any
                   pathConstrVec="integer", # constraints on path variables
                   iEff="integer", # indices of effective parameters
                                   # iEff mostly for mspathEstimateCoefficients use
                   map="list"     # from total coefficients to params
                                  # map["a"] gives matrix corresponding to coefficient a
                                  # The entries, which occur only where permit==TRUE,
                                  # give the index in params for the corresponding
                                  # value
                                              ))


# mspathEstimatedCoefficients handles coefficients along
# with info on the estimation process
setClass("mspathEstimatedCoefficients", contains="mspathCoefficients",
         representation=representation(
           fixed = "integer",  # indices in params that were fixed
           n="numeric",     # number of sample points estimate is from
           foundSE = "logical", # did estimates converge?
           var = "numeric" #estimated variances, with 0's for fixed params
           # has same dimension as effective parameters, with 0
           # entries for the fixed values.
           # numeric(0) indicates poorly conditioned problem
           # fuller covariance analysis should use info from all coefficients,
           # so must be done at a higher level.
           ))

############################# mspathCoefficients ########################
# Constructor
# Simple, fixed parameters
# Since this class is intended as an internal helper class to mspath, it
# does little checking of its inputs.
mspathCoefficients <- function(
                   permit,  # matrix of 0/1 or logical indicators for allowed values
                   params,    # parameter values (both free and fixed)
                   offset=0,     # starting index in params
                   baseConstrVec, # constraints on intercepts
                   covVars, # names of covariates
                   constrVec, # vector of constraints for covs
                   pathVars, # path variables, if any
                   pathConstrVec # constraints on path variables
                   ) {
  # Input Requirements
  # permit has 0's (or FALSE) on diagonals
  # Constraints number 1 through the effective number of relevant params.
  # Each constraint vector restarts at 1.
  # covVars may be missing.  If so, constrVec may be missing and is ignored.
  # pathVars may be missing.  If so, pathConstrVec may be missing and is ignored.

  # infer parameters that were passed as args in old code
  nStates = nrow(permit) # number of states
  nMatrix = sum(permit)  # number of allowed values in permit
#unneeded  nPars = length(params)  # number of parameters, both free and fixed
  nBaseEff <- max(baseConstrVec)

  # new("mspathCoefficients", foo=bar) fails when bar is missing
  # We construct an argument list for do.call to avoid this
  innerArgs <- list(
                    Class = "mspathCoefficients",
                    permit = matrix(as.logical(permit), nrow=nrow(permit)),
                    baseConstrVec = as.integer(baseConstrVec),
                    params = params
                    )

  if (missing(covVars) || length(covVars)==0) {
    nCovs <- nCovEff <- 0
  } else {
    nCovs <- length(covVars)
    nCovEff <- max(constrVec)
    innerArgs$covVars <- covVars
    innerArgs$constrVec <- as.integer(constrVec)
  }
  if (missing(pathVars) || length(pathVars)==0) {
    nPath <- nPathEff <- 0
  } else {
    nPath <- length(pathVars)
    nPathEff <- max(pathConstrVec)
    innerArgs$pathVars <- pathVars
    innerArgs$pathConstrVec <- as.integer(pathConstrVec)
  }

  nTotal <- 1+nCovs+nPath
  nEff <- nBaseEff+nCovEff+nPathEff
  
  iEff = offset+seq(nEff)  # indices into effective parameters
  innerArgs$iEff <- as.integer(iEff)
  map <- list()  # keys will be covariate names, values matrices
                 # each matrix gives the iEff of its coefficient
  for (i in 0:(nCovs+nPath)) {
    if (i==0) {
      matrixName <- "intercept"
      parinds <- iEff[baseConstrVec]
    }
    else if (i<=nCovs){
      # covariates
      matrixName <- covVars[i]
      parinds <- iEff[nBaseEff + constrVec[seq((i-1)*nMatrix+1, i*nMatrix)]]
    }
    else {
      # path variables
      j <- i-nCovs
      matrixName <- pathVars[j]
      parinds <- iEff[nBaseEff + nCovEff +
                      pathConstrVec[seq((j-1)*nMatrix+1, j*nMatrix)]]
    }

    mat <- t(permit) # we take matrices by rows; R goes by columns
    mat[as.logical(t(permit))] <- parinds
    mat <- t(mat)
    dimnames(mat) <- list(paste("Stage", 1:nStates),
                          paste("Stage", 1:nStates))
    map[[matrixName]] <- mat
  }
  innerArgs$map <- map
  do.call("new", innerArgs)
}

# isAllFixed generic definition stripped out and put in allGenerics.R.
# We have no estimation info, and so assume fixed
setMethod("isAllFixed", signature(object="mspathCoefficients"),
          function(object) TRUE)

setMethod("coef", signature(object="mspathCoefficients"),
          function(object) object@params[object@iEff])

# I ignore na.rm, which is not so good
setMethod("sd", signature(x="mspathCoefficients", na.rm="ANY"),
          function(x, na.rm) rep(0.0, length(x@iEff)))
setMethod("sd", signature(x="mspathEstimatedCoefficients", na.rm="ANY"),
          function(x, na.rm) if (length(x@var)>0) sqrt(x@var)
          else rep(0, length(x@iEff))
          )
                               
setMethod("show", signature(object="mspathCoefficients"), function(object) {
  print(object)
  invisible(NULL)
  })

# Next method prints the total coefficients
# So one effective coefficient may appear in several spots
## setMethod("print", signature("mspathCoefficients"), function(x) {
##   trans <- paste(as.character(row(x@permit)), "->", as.character(col(x@permit)),
##                  sep="")
##   cat(" For  Val  Constraint\n")
##   for (v in names(x@map)) {
##     cat(v, "\n")
##     m <- x@map[[v]]
##     label <- trans[as.logical(m)]
##     mv <- x@params[m]
##     cat(paste(label, ": ", mv, " ", m[as.logical(m)], "\n", sep=""), "\n", sep="")
##   }
## })

# This method prints the effective coefficients, noting which matrix element(s)
# they apply to.
# If there is a constraint that crosses covariates, the effective
# coefficient will still be printed twice.  This is not likely in practice.
setMethod("print", signature(x="mspathCoefficients"), function(x, ...) {
  trans <- matrix(paste(as.character(row(x@permit)), "->",
                        as.character(col(x@permit)),
                 sep=""), nrow=nrow(x@permit))
  args <- list(...)
  if ("coeff" %in% names(args))
    coeffNames <- args[["coeff"]]  # single [] is wrong
  else
    coeffNames <- names(x@map)
  for (v in coeffNames) {
    cat(v, "\n")
    m <- x@map[[v]]
    iEff <- sort(unique(m[as.logical(m)]))
    cm <- matrix(c(iEff, x@params[iEff]), ncol=2)
    colnames(cm) <- c("Index", "Value")
    rownames(cm) <- lapply(iEff, function(i) paste(t(trans)[t(m==i)], collapse=" "))
    printCoefmat(cm, ...)
    cat("\n")
  }
  invisible(x)
})

# Return a list of matrices of coefficients
# matrixCoef generic definition stripped out and put in allGenerics.R.

setMethod("matrixCoef", signature(x="mspathCoefficients"), function(x, ...) {
  args <- list(...)
  if ("coeff" %in% names(args))
    coeffNames <- args[["coeff"]]  # single [] is wrong
  else
    coeffNames <- names(x@map)
  mats <- lapply(coeffNames, function(var) {
    m <- x@map[[var]]
    m[m>0] <- x@params[m[m>0]]
    m
  })
  names(mats) <- coeffNames
  mats
})

# batchsmoosh(a1) <- a2 replaces parts of a1 with part of a2 if identical.
# It may also delete some elements entirely.
# intended to remove needless object duplication from distributed calculations
if (!isGeneric("batchsmoosh<-"))
# batchsmoosh<- generic definition stripped out and put in allGenerics.R.

setMethod("batchsmoosh<-",
          signature(x="mspathCoefficients", value="mspathCoefficients"),
          function(x, value) {
            batchsmoosh(x@permit) <- value@permit
            # next line likely to do nothing
            batchsmoosh(x@params) <- value@params
            batchsmoosh(x@baseConstrVec) <- value@baseConstrVec
            batchsmoosh(x@covVars) <- value@covVars
            batchsmoosh(x@constrVec) <- value@constrVec
            batchsmoosh(x@pathVars) <- value@pathVars
            batchsmoosh(x@pathConstrVec) <- value@pathConstrVec
            batchsmoosh(x@iEff) <- value@iEff
            batchsmoosh(x@map) <- value@map
            x
          })

######################## mspathEstimatedCoefficients ##################

# Since this class is intended as an internal helper class to mspath, it
# does little checking of its inputs.
# See discussion at top of this file for background.  In particular,
# params, fixed, and hessian should include info for all coefficients
# not just the current one being constructed.
mspathEstimatedCoefficients <- function(
                   permit,  # matrix of 0/1 or logical indicators for allowed values
                   params,    # parameter values (both free and fixed)
                   offset=0,     # starting index in params
                   baseConstrVec, # constraints on intercepts
                   covVars, # names of covariates
                   constrVec, # vector of constraints for covs
                   pathVars, # path variables, if any
                   pathConstrVec, # constraints on path variables
                   fixed=integer(0), # indices of fixed parameters
                   n=Inf,   # number of sample points
                   var=numeric(0)      # estimated variances of free params for
                                       # all coefficients
                   ) {
  simple <- mspathCoefficients(permit, params, offset, baseConstrVec, covVars,
                               constrVec, pathVars, pathConstrVec)
  iEff <- simple@iEff #indices of effective parameters
  nPars <- length(params)  # ALL params, not just relevant to me
  free <- setdiff(iEff, fixed) # free params for these coefficients
  myVar <- numeric(0) # no variance until proven otherwise
  if (length(free) == 0)
    # no free parameters to estimate: deviant case
    # ambiguous, but we found all we needed (i.e., none)
    foundSE <- TRUE
  else {
    if (length(var)>0) {
      allfree <- setdiff(seq(along=params), fixed) # free for all
      iVar <- match(free, allfree) # where my values are in var
      var <- var[iVar]
      # and where they are going in myVar
      imyVar <- match(free, iEff)
      myVar <- rep(0, length(iEff))
      myVar[imyVar] <- var
      foundSE <- TRUE
    } else {
      foundSE <- FALSE
    }
  }
  new("mspathEstimatedCoefficients", simple,
      fixed=as.integer(fixed),
      n=n,
      var=myVar,
      foundSE=foundSE)
}

# check for all fixed
setMethod("isAllFixed", signature(object="mspathEstimatedCoefficients"),
          function(object) length(setdiff(object@iEff, object@fixed))==0)

# This method prints the effective coefficients, noting which matrix element(s)
# they apply to.
# If there is a constraint that crosses covariates, the effective
# coefficient will still be printed twice.  This is not likely in practice.
setMethod("print", signature("mspathEstimatedCoefficients"), function(x,  ...) {
  trans <- matrix(paste(as.character(row(x@permit)), "->",
                        as.character(col(x@permit)),
                 sep=""), nrow=nrow(x@permit))
  args <- list(...)
  if (! x@foundSE){
    cat("ERROR: HESSIAN WAS NOT POSITIVE-DEFINITE!!\n")
  }
  if ("coeff" %in% names(args))
    coeffNames <- args$coeff
  else
    coeffNames <- names(x@map)
  iFree <- setdiff(x@iEff, x@fixed) # global
  for (v in coeffNames) {
    cat(v, "\n")
    m <- x@map[[v]]
    # my* are indices just for this variable, but
    # they are indices into the global params vector
    myEff <- sort(unique(m[as.logical(m)]))
    myPrint <- ifelse(myEff %in% x@fixed, -myEff, myEff)
    myFixed <- intersect(myEff, x@fixed)
    myFree <- setdiff(myEff, myFixed)
    if (x@foundSE && length(myFree)>0) {
      se <- tval <- tsig <- numeric(length(myEff))
      se[] <- tval[] <- tsig[] <- NA
      # i indexes the vectors we are putting out
      i <- match(myFree, myEff)
      # j gives indices free vars in this coefficients' subset
      j <- match(myFree, x@iEff)
      se[i] <- sqrt(x@var[j])
      tval[i] <- x@params[myFree]/se[i]
      df <- x@n-length(x@params)+length(x@fixed)
      tsig[i] <- 2*pt(abs(tval[i]), df, lower.tail=FALSE)
      cm <- matrix(c(myPrint, x@params[myEff], se, tval, tsig), ncol=5)
      colnames(cm) <- c("Index", "Estimate", "Std Err", "t", "Pr(>|t|)")
    } else {
      cm <- matrix(c(myPrint, x@params[myEff]), ncol=2)
      colnames(cm) <- c("Index", "Estimate")
    }
    rownames(cm) <- lapply(myEff, function(i) paste(t(trans)[t(m==i)], collapse=" "))
    printCoefmat(cm, ...)
    cat("\n")
  }
})

setMethod("batchsmoosh<-",
          signature(x="mspathEstimatedCoefficients", value="mspathEstimatedCoefficients"),
          function(x, value) {
            x <- callNextMethod()
            batchsmoosh(x@fixed) <- value@fixed
            # not sure if the next two will save much space.
            # the items are singletons
            batchsmoosh(x@n) <- value@n
            batchsmoosh(x@foundSE) <- value@foundSE
            # almost certainly different @var
            x
          })
