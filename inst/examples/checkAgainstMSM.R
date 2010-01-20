# Compare the results of mspath and msm

library("msm") # using the 0.7.0 interface
# That interface has changed some since  the version of msm on which
# mspath is based.

source("loadmspath.R")

# One can not test calculations involving path dependency, since msm
# assumes there is none.  It uses relatively low probabilities of up
# moves, since if there is a significant probability of more than one
# move in a unit time the results are less likely to be comparable.
# mspath permits only one move per time step for the models used here,
# while msm, as a continuous-time model, allows infinite moves in unit
# time.  Let a be a coefficient estimated by msm, and b be the
# corresponding mspath coefficient.  Then the probability of an event
# in unit time is

# 1-exp(-exp(a)) for msm
# 1/(1+exp(-b)) for mspath
# These are not entirely comparable because, as noted, the event for msm
# includes not only a move up 1, but 2, 3, etc.  If the one-step move has
# almost all the probability, comparability should be good.
# Equating the probabilities and solving gives
# a =ln[ ln(1+exp(b))]
# b = ln[exp(exp(a))-1]
mspath_to_msm <- function(b) log(log(1+exp(b)))

msm_to_mspath <- function(a) log(exp(exp(a))-1)

# Convert between representation
# mats is a list of matrices of coefficients, nstates x nstates
# tranform is a conversion function
# permit is an optional logical matrix of permitted transitions
# mats[[1]] is the intercept; other terms are covariates
matrix_convert <- function(transform, mats, permit= (mats[[1]] != 0)) {
  diag(permit) <- FALSE
  intercept <- mats[[1]]
  newintercept <- intercept
  newintercept[permit] <- transform(intercept[permit])
  mats[[1]] <- NULL
  result <- lapply(mats, function(coef){
    coef[permit] <- (transform(coef+intercept)-newintercept)[permit]
    coef
  }
                   )
  result <- c(list(intercept=newintercept), result)
  lapply(result, function(m) {diag(m) <- 0; m})
}
                           
# use this on result of mspath
# only sensible if at most one covariate
mspathresult_to_msm <- function(mspathresult) {
  mask <- mspathresult@transCoef@permit
  mats <- matrixCoef(mspathresult)
  matrix_convert(mspath_to_msm, mats, mask)
}

# use on result of msm
msmresult_to_mspath <- function(msmresult) {
  intercept <- qmatrix.msm(msmresult, covariates=0)$estimate
  diag(intercept) <- 0
  permit <- (intercept> 0)
  intercept[permit] <- log(intercept[permit])
  mats <- list(intercept=intercept)
  n <- length(msmresult$Qmatrices) # logbaseline and baseline are always present
  if (n>3)
    stop("Converting msm result to msmpath is senseless with multiple covariates")
  if (n==3){
    covname <- names(msmresult$Qmatrices)[2]
    covlist <- list(1)
    names(covlist) <- covname
    coef <- qmatrix.msm(msmresult, covariates=covlist)$estimate
    diag(coef) <- 0
    coef[permit] <- log(coef[permit])
    coef <- coef-intercept
    mats <- c(mats, list(coef))
    names(mats)[2] <- covname
  }
  matrix_convert(msm_to_mspath, mats, permit)
}


# convert constraint vector to standard form of 1, 2, ...
normalizeConstraints <- function(c) match(c, sort(unique(c)))

# make a matrix with allowed values instead of 1s and 0s
# return list with matrix and normalized constraints
# WARNING: this will fail if there is a 0 value in a cell.
newmatrix <- function(oldmatrix, munchinits, constraints){
  diag(oldmatrix) <- 0
  if (!is.null(constraints))
    constraints <- normalizeConstraints(constraints)
  else
    constraints <- seq(sum(oldmatrix != 0))
  m <- t(oldmatrix)
  m[m!=0] <- munchinits(max(constraints))[constraints]
  list(matrix=t(m), constraints=constraints)
}

# call msm using mspath-style parameters
# This function will refuse to consider the followingn situations,
# which can not be rendered comparable:
#    any use of history dependent variables
#    more than one covariate (and that covariate should be binary)
#    attempt to do anything other than maximum likelihood
#    misc= anything other than SIMPLE (implementation limit only)
#
# Although there is no warning, if you use isexact=TRUE the results
# will only be comparable if the final state, and only the final state,
# is absorbing.
msmConvert <- function(myargs) {
  parentframe <- sys.frame(sys.parent())
  argnames <- names(myargs)
  paths <- c("pathvars", "pathoffset", "pathconstraint")
  oops <- intersect(argnames, paths)
  if (length(oops)>0)
    stop("msm can not handle path variables")
  if ("do.what" %in% argnames && myargs$do.what != 1)
    stop("msm can only handle do.what == 1")
  inits <- eval(myargs$inits, parentframe)
  myargs$inits <- NULL   # msm 0.5 and up does not use inits
  # return next n values in inits
  munchinits <- function(n) {
    x <- inits[seq(n)]
    inits <<- inits[-seq(n)]
    x
  }

  # fixedpars appears to have same interpretation as before
  # i.e., indices into free parameters
  if ("fixedpars" %in% argnames)
    fixedpars <- eval(myargs$fixedpars, parentframe)
  else
    fixedpars <- NULL

  # insert transition probabilities
  if ("qconstraint" %in% argnames)
    qcons <- eval(myargs$qconstraint, parentframe)
  else
    qcons <- NULL
  originits <- inits
  q <- newmatrix(eval(myargs$qmatrix, parentframe),
                 munchinits,
                 qcons)
  nfree <- max(q$constraints)
  q <- q$matrix
  qmspath <- q  # untransformed values
  mask <- q!=0
  ntrans <- sum(mask)
  q[mask] <- 1/(1+exp(-q[mask]))
  myargs$qmatrix <- q

  # process covariates, if any
  if ("covariates" %in% argnames) {
    if (length(all.vars(myargs$covariates))>1)
      stop("msm and mspath can not be equivalent with more than 1, binary covariate")
    if ("constraint" %in% argnames)
      cons <- eval(myargs$constraint, parentframe)[[1]]
    else
      cons <- NULL
    # we extract the mspath values and then transform them
    qcov <- newmatrix(mask, munchinits, cons)
    nfree <- nfree+max(qcov$constraints)
    qcov <- qcov$matrix
    qcov <- mspath_to_msm(qmspath+qcov)-mspath_to_msm(qmspath)
    # msm will now match mspath for covariate values 0 and 1
    covlist <- eval(myargs$constraint, parentframe)  # we just want the label
    covlist[[1]] <- t(qcov)[t(mask)]
    myargs$covinits <- covlist
  }
  
  # and error probabilities, if any
  if ("misc" %in% argnames) {
    misc <- myargs$misc  # not evaluated
    if (misc != quote(SIMPLE))
      stop("Current conversion only does misc=SIMPLE")
    if ("econstraint" %in% argnames)
      econs <- eval(myargs$econstraint, parentframe)
    else
      econs <- NULL
    e <- newmatrix(eval(myargs$ematrix, parentframe),
                   munchinits, econs)
    fixedpars <- c(fixedpars, seq(nfree+1, length.out=max(e$constraints)))
    myargs$fixedpars <- fixedpars
    myargs$ematrix <- e$matrix
    myargs$misc <- NULL
  }

  # death handling
  # This assumes that the last state is an absorbing state, and it is the only one
  if ("isexact" %in% argnames){
    if (eval(myargs$isexact, parentframe))
      myargs$death <- TRUE
    myargs$isexact <- NULL
  }

  # make easier comparison of coefficieints
  myargs$center <- FALSE

  # work around bugs in analytic shortcuts
  myargs$analyticp <- FALSE

  # eliminate mspath specific arguments
  myargs$stepnumerator <- myargs$stepdenominator <- NULL
  myargs$testing <- NULL
  myargs$comm <- NULL
  myargs$profile <- NULL
  myargs$calcFactory <- NULL
  myargs$seed <- NULL
  myargs$trace <- NULL

  # main event
  do.call("msm", myargs)
}

# Evaluate mspath at arguments from msm
# nerr is number of independent error effects
mspath_with_msm <- function(mspathargs, msmresult, nerr) {
  newparms <- msmresult_to_mspath(msmresult)
  permit <- (qmatrix.msm(msmresult, covariates=0)$estimate)>0
  newinits <- sapply(newparms, function(m) m[permit])
  oldinits <- mspathargs$inits
  nold <- length(oldinits)
  newinits <- c(newinits, oldinits[-seq(nold-nerr)])
  mspathargs$inits <- newinits
  mspathargs$fixedpars <- seq(along=newinits)
  mspathargs$constraint <- NULL
  mspathargs$qconstraint <- NULL
  do.call("mspath", mspathargs)
}

# calculate a bunch of related msm and mspath models
# return list of results
dobatch <- function(args1, dataset) {
                                        # create data
  simargs <- args1
  simargs$data <- dataset
  simargs$do.what <- 10
  simargs$seed <- 123999
  sdata <- do.call("mspath", simargs)

                                        # fit via mspath
  args2 <- args1
  args2$data <- sdata
  rmspath <- do.call("mspath", args2)

                                        # fit via msm
  rmsm <- msmConvert(args2)

                                        # try msm solution in mspath
  rmspath2 <- mspath_with_msm(args2, rmsm, 2)
  
                                        # return results
  list(msm=rmsm, mspath=rmspath, mspath2=rmspath2, data=dataset)
}

# print results returned above
batchprint <- function(r) {
  cat(minus2loglik(r$mspath), r$msm$minus2loglik,
      minus2loglik(r$mspath2), "= -2LL mspath, msm, mspath@msm optimal\n")
  cat("Coefficients in mspath parameterization\n")
  cat("from mspath:\n")
  print(coef(r$mspath))
  cat("from msm:\n")
  print(msmresult_to_mspath(r$msm))
  cat("\nCoefficients in msm parameterization")
  cat("from mspath:\n")
  print(mspathresult_to_msm(r$mspath))
  cat("from msm:\n")
  print(r$msm$opt$par)
}


# 5 states, last observed exactly
# D3 covariate ~ binomial(.6)
# Prespecified errors
qcons <- c(1, 1, 2, 2)
cons1 <- list(D3=qcons)
init1 <- c(-2, -1.8,# intercepts
           -1.0, -1.0, # D3
           .2, .05 # misclassification
           )
# set up for mspath
# you must provide data
args1 <- list(formula=fib~time,
              qmatrix=q2,
              misc=quote(SIMPLE),
              ematrix=e2,
              inits=init1,
              subject=quote(ssn),
              covariates=~D3,
              constraint=cons1,
              qconstraint=qcons,
              econstraint=c(1, 2, 1, 2, 1),
              isexact=FALSE,
              stepnumerator=1,
              stepdenominator=1,
              control=list(maxit=3000)
              )

r <- dobatch(args1, sim2)
batchprint(r)

# no covariates
args0 <- args1
args0$covariates <- NULL
args0$constraint <- NULL
args0$inits <- init1[-c(3, 4)]

r0 <- dobatch(args0, sim2)
batchprint(r0)
