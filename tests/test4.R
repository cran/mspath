# Test against simulated data with known answers
library("mspath")
data(q2, e2)
data(sim1)

# sim1 is a full model
# 5 states, last observed exactly
# k ~ N(100, 1) fixed for each case
# x1 ~ N(0, 1)
# LN(TSO)
# TIS
# Prespecified errors
cons1 <- list(k=c(1, 1, 2, 2), x1=rep(3, 4))
init1 <- c(seq(-1, by=.2, length=4),# intercepts
              0.5, 0.8, #k
              -1.0, # x1
              -0.5, .7, # history
              .2, .05 # misclassification
              )

r <- mspath(fib~time, q2, misc=SIMPLE, e2,
            inits=init1,
            subject=id,
            covariates=~k+x1,
            constraint=cons1,
            econstraint=c(1, 2, 1, 2, 1),
            pathvars=c("LN(TSO)", "TIS"),
            pathoffset=0.5,
            pathconstraint=list("LN(TSO)"=rep(1, 4), "TIS"=rep(2, 4)),
            data=sim1,
            isexact=TRUE,
            fixedpars=seq(along=init1),
            stepnumerator=1,
            stepdenominator=1,
            do.what=1)

tol <- 1e-5
all.equal(minus2loglik(r), 77.74151, tolerance=tol)
nGoodPaths(r) == 906
nGoodPathNodes(r) == 8593
nCases(r) == 21
# Values from the "fake" calculator
#77.74151 = -2*log-likehood for 21 id's.
#906 good paths with 0 good nodes and 0 bad nodes. 
#If each path were evaluated separately, there would be 8593 good nodes.

            
# Now test if caches are handled properly in multiple calls
# as above, but with variable arguments
r <- mspath(fib~time, q2, misc=SIMPLE, e2,
            inits=init1,
            subject=id,
            covariates=~k+x1,
            constraint=cons1,
            econstraint=c(1, 2, 1, 2, 1),
            pathvars=c("LN(TSO)", "TIS"),
            pathoffset=0.5,
            pathconstraint=list("LN(TSO)"=rep(1, 4), "TIS"=rep(2, 4)),
            data=sim1,
            isexact=TRUE,
            stepnumerator=1,
            stepdenominator=1,
            do.what=0)
calcargs <- r@calc@args
c <- mspathCalculatorFromArgs(r@calc@args)
freeparm <- init1 <- c(seq(-1, by=.2, length=4),# intercepts
              0.5, 0.8, #k
              -1.0, # x1
              -0.5, .7 # history
                       )
# same as previous calc
params(c) <- init1
c <- calculate(c, do.what=1)
all.equal(minus2loglik(c), 77.74151, tolerance=tol)

# tweak params and subset
fp2 <- freeparm+c(-.1, .1, 0, -.15, 0.1, -0.1, .25, .1, -.05)
params(c) <- fp2
uid <- unique(sim1$id)
activeCases(c) <- c(uid[c(1,2)])
c <- calculate(c)
all.equal(minus2loglik(c),  7.156424, tolerance=tol)

# deliberately in reverse order
activeCases(c) <- c(uid[seq(21, 17)])
c <- calculate(c)
all.equal(minus2loglik(c),  24.23667, tolerance=tol)


activeCases(c) <- c(uid[10])
c <- calculate(c)
all.equal(minus2loglik(c),  2.508193, tolerance=tol)

activeCases(c) <- c(uid)
c <- calculate(c)
all.equal(minus2loglik(c),  75.43, tolerance=tol)


activeCases(c) <- c(uid[c(17, 18)])
c <- calculate(c)
all.equal(minus2loglik(c),  16.76237, tolerance=tol)

activeCases(c) <- integer()  # i.e, all
c <- calculate(c)
minus2loglik(c)
all.equal(minus2loglik(c), 75.43, tolerance=tol)

# back to first
params(c) <- init1
c <- calculate(c)
all.equal(minus2loglik(c), 77.74151, tolerance=tol)
