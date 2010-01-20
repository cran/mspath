quit(save="no")

# This file is not intended to be run automatically.
# It verifies that fitting gives the same results
# in the distributed and non-distributed cases.
# The single CPU calculation takes about an hour on an Apple G5.
# You should have activated a distributed environment when you run this.
# So establish your clustering environment.
# mpirun C RMPISNOW
# source this script with echo=TRUE

# RMPISNOW and the associated RMPISNOWprofile are in the install directory
# It will invoke the following commands
# library("Rmpi")
# library("mspath")

data(q2, e2, sim2)

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
mymspath <- function(data, comm){
  args <- list(fib~time, q2, misc=quote(SIMPLE), e2,
               inits=init1,
               subject=quote(id),
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
               control=list(maxit=3000),
               do.what=1)
  if (!missing(comm))
    args$comm <- comm
  do.call(mspath, args)
}

# uniprocessor calculation
system.time(rone <- mymspath(sim2))
rone

# multiprocessor
system.time(rmulti <- mymspath(sim2, comm=0))
alldone(0)
rmulti

# acid test
all.equal(coef(rone), coef(rmulti))
all.equal(minus2loglik(rone), minus2loglik(rmulti))
all.equal(optresults(rone), optresults(rmulti))

