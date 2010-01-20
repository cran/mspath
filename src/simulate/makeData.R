# add covariates to a dataset
# C1 continuous (normal)
# C2 continuous (exponential)
# D3 binary
# C4 "age" (generate start age uniform + time)
# K5 constant continuous normal
# K6 constant binary
# uses actual id's and times of data
# other = names of other variables to preserve
# seed is an optional integer for the random seed
makeData <- function(data, seed, id="ssn", time="time", other="fib") {
  if(!missing(seed))
    set.seed(seed)
  n <- nrow(data)
  d <- data.frame(data[,c(id, time, other)])
  d <- d[order(d[,id], d[,time]),]
  d <- data.frame(d, rnorm(n), rexp(n),
                  rbinom(n, 1, .6), runif(n, max=50),
                  rnorm(n),
                  rbinom(n, 1, .4))
  names(d) <- c(id, time, other, "C1", "C2", "D3", "C4", "K5", "K6")
  # sub is single case worth of data
  fixup <- function(sub) {
    sub[,"C4"] <- sub[1,"C4"]+sub[,time]
    sub[,c("K5", "K6")] <- sub[1,c("K5", "K6")]
    sub
  }
  ids <- d[,id]
  for (c in unique(ids))
    d[ids==c,] <- fixup(d[ids==c,])
  d
}

# make regular data
simpleMaker <- function(nCases=100, nSteps=10){
  ids <- rep(seq(nCases), each=nSteps)
  data.frame(id=ids,
             fib=1,
             t=rep(seq(0, nSteps-1), nCases),
             k=rnorm(nCases)[ids],
             x=rnorm(length(ids)))
}

# simulate outcomes for data and then fit model 
randomRun <- function(data, seed) {
  if(missing(seed))
    seed <- as.integer(runif(1, max=.Machine$integer.max)-.Machine$integer.max/2)
  # this is like final model in test4.R
  # 5 states, last observed exactly
# k ~ N(100, 1) fixed for each case
# x1 ~ N(0, 1)
# LN(TSO)
# TIS
# Prespecified errors
cons1 <- list(K5=c(1, 1, 2, 2), C1=rep(3, 4))
init1 <- c(seq(-1, by=.2, length=4),# intercepts
              0.5, 0.8, #k
              -1.0, # x1
              -0.5, .7, # history
              .2, .05 # misclassification
              )
  mymspath <- function(data, do.what, seed){
    mspath(fib~time, q2, misc=SIMPLE, e2,
            inits=init1,
            subject=ssn,
            covariates=~K5+C1,
            constraint=cons1,
            econstraint=c(1, 2, 1, 2, 1),
            pathvars=c("LN(TSO)", "TIS"),
            pathoffset=0.5,
            pathconstraint=list("LN(TSO)"=rep(1, 4), "TIS"=rep(2, 4)),
            data=data,
            isexact=TRUE,
            stepnumerator=1,
            stepdenominator=1,
           seed=seed,
           control=list(maxit=3000),
           do.what=do.what)
  }

  sim <- mymspath(data, 10, seed)
  # seed is irrelevant for next call, but mymspath needs it
  simparam <- init1
  nerr <- 2
  # start at true values
  #init1 <- c(rep(0, length(init1)-nerr), rep(.1, nerr))
  r <- mymspath(sim, 1, seed)
  list(optresults=optresults(r), modelParams=simparam, seed=seed)
}
 
# results is a list of results from randomRun when it returns full results
simanalyze <- function(results) {
  nSim <- length(results)
  modelParams <- results[[1]]$modelParams # they should all be the same
  cat(nSim, "simulations with true params =")
  cat(modelParams)
  cat("\n")
  # concentrate on the real results
  results <- lapply(results, function(x) x$result)
  good <- sapply(results, function(r) optresults(r)$convergence == 0)
  nBad <- nSim - sum(good)
  if (nBad>0) {
    cat("WARNING:", nBad, "estimates failed to converge.\n")
    results <- results[good]
  }
  # only convergent results for rest of report
  est <- sapply(results, function(r) coef(r))-modelParams
  # careful: we might have only 1 parameter
  if (class(est) == "matrix")
    est <- t(est)
  show(summary(est))
  cat("sim std dev =", sd(est))
  cat("\nestimate - true reported above.  Fitted sd below.\n")
  mysd <- sapply(results, function(r) sd(r))
  if (class(mysd) == "matrix")
    mysd <- t(mysd)
  show(summary(mysd))
  invisible()
}



# results is a list of results from randomRun when it returns optresults
# assumes the fixed parameters are at the end
simanalyze2 <- function(results) {
  nSim <- length(results)
  modelParams <- results[[1]]$modelParams # they should all be the same
  cat(nSim, "simulations with true params =")
  cat(modelParams)
  cat("\n")
  # concentrate on the real results
  results <- lapply(results, function(x) x$optresults)
  good <- sapply(results, function(r) r$convergence == 0)
  nBad <- nSim - sum(good)
  if (nBad>0) {
    cat("WARNING:", nBad, "estimates failed to converge.\n")
    results <- results[good]
  }
  # only convergent results for rest of report
  est <- sapply(results, function(r) r$par-modelParams[seq(length(r$par))])
  # careful: we might have only 1 parameter
  if (class(est) == "matrix")
    est <- t(est)
  show(summary(est))
  cat("sim std dev =", sd(est))
  cat("\nestimate - true reported above.  Fitted sd below.\n")
  mysd <- sapply(results, function(r) {
    hess <- 0.5*r$hessian # .5 to cancel -2*LL
    if (all(eigen(hess)$values > 0))
      var <- diag(solve(hess))
    else
      var <- rep(NA, length(diag(hess)))
    sqrt(var)
  })
  if (class(mysd) == "matrix")
    mysd <- t(mysd)
  show(summary(mysd))
  invisible()
}

# reduce memory useage of simulation results
compress <- function(results) {
  first <- results[[1]]
  mysmoosh <- function(x) {batchsmoosh(x) <- first; x}
  r2 <- lapply(results, mysmoosh)
  # if some info is NULL'd, keep the first
  r2[[1]] <- first
  r2
}
