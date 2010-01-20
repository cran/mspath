library(mspath)
data(e2, q2, sim2)
tol <- 5e-5 # really e-7 since it  percentage

r <- mspath(fib~time, misc=TRUE, ematrix=e2, qmatrix=q2, inits=rep(.5, 9), subject=id,
             data=sim2, stepnumerator=1, stepdenominator=1, initprobs=c(1.0, 0, 0, 0, 0),
             do.what=0)
nCases(r)
nGoodPaths(r)
nGoodNodes(r)
nBadNodes(r)
nGoodPathNodes(r)
r0 <- r

# Next tests mirror mspathCEntry_test.cc, except for different data input.
# Note that goodNodes, badNodes, goodPathNodes may change depending on implementation

 r <- mspath(fib~time, misc=TRUE, ematrix=e2, qmatrix=q2,
             inits=c(-1.6, -1.6, -1.2, -2.5, -1.0, -2.0, -1.0, -2.0, -1.0),
             subject=id,
             data=sim2, stepnumerator=1, stepdenominator=1, initprobs=c(1.0, 0, 0, 0, 0),
             do.what=1, fixedpars=seq(9))

nCases(r) == nCases(r0)
nCases(r)
nGoodPaths(r) == nGoodPaths(r0)
nGoodPaths(r)
nGoodNodes(r) == nGoodNodes(r0)
nGoodNodes(r)
nBadNodes(r) == nBadNodes(r0)
nBadNodes(r)
nGoodPathNodes(r) == nGoodPathNodes(r0)
nGoodPathNodes(r)
all.equal(minus2loglik(r), 2017.767, tolerance=tol)
all.equal(coef(r), c(-1.6, -1.6, -1.2, -2.5, -1.0, -2.0, -1.0, -2.0, -1.0))
all.equal(sd(r), rep(0, 9))

# next call also tests handling of optional arguments isexact and control
 r <- mspath(fib~time, misc=TRUE, ematrix=e2, qmatrix=q2,
             inits=c(-1.6, -1.6, -1.2, -2.5, -1.0, -2.0, -1.0, -2.0, -1.0),
             subject=id,
             data=sim2, stepnumerator=1, stepdenominator=1, initprobs=c(1.0, 0, 0, 0, 0),
             do.what=1, fixedpars=seq(9), isexact=TRUE, control=list(fnscale=2000))

nCases(r) == nCases(r0)
nGoodPaths(r) < nGoodPaths(r0)
nGoodPaths(r)
nGoodNodes(r) < nGoodNodes(r0)
nGoodNodes(r)
nBadNodes(r)
nGoodPathNodes(r) < nGoodPathNodes(r0)
nGoodPathNodes(r)
# likelihood differs from previous mspath call
all.equal(minus2loglik(r), 2360.929, tolerance=tol)


                                        # iteration 3
 r <- mspath(fib~time, misc=TRUE, ematrix=e2, qmatrix=q2,
             inits=c(-2.12358, -2.05849, -1.79560, -3.12710,
               -0.55153, -2.08489, -1.36302, -1.71042, -0.84825),
             subject=id,
             data=sim2, stepnumerator=1, stepdenominator=1, initprobs=c(1.0, 0, 0, 0, 0),
             do.what=1, fixedpars=seq(9))

nCases(r) == nCases(r0)
nGoodPaths(r) == nGoodPaths(r0)
nGoodNodes(r) == nGoodNodes(r0)
nBadNodes(r) == nBadNodes(r0)
nGoodPathNodes(r) == nGoodPathNodes(r0)
all.equal(minus2loglik(r), 2713.066, tolerance=tol)

# Now test actual estimation
# subset data to speed things up
d <- mspath.subset(sim2)
r <- mspath(fib~time, misc=TRUE, ematrix=e2, qmatrix=q2,
            inits=c(-1.6, -1.6, -1.2, -2.5, -1.0, -2.0, -1.0, -2.0, -1.0),
            subject=id,
            data=d, stepnumerator=1, stepdenominator=1, initprobs=c(1.0, 0, 0, 0, 0),
            method="BFGS")
# I used digits=2 to hide small numerical variations
# between systems.  However, some knife edge cases may still flop around
# also the likelihood is printed in full precision.
#
# NOTE: I HAVE NO STRONG CHECK THAT THE VALUES ARE OPTIMAL.
# I'm just taking what the routine gives me.

print(r, digits=2)
rbest <- r

# Following results can be validated with pure R code in src/simulate.
# Note they use a single evaluation only, no optimization
r <- mspath(fib~time, misc=TRUE, ematrix=e2, qmatrix=q2,
            inits=c(-1.6, -1.6, -1.2, -2.5, -1.0, -2.0, -1.0, -2.0, -1.0),
            subject=id,
            data=d,
            stepnumerator=1, stepdenominator=1,
            initprobs=c(1.0, 0, 0, 0, 0),
            fixed=seq(9),
            method="BFGS")
nCases(r)
nGoodPaths(r)
nGoodPathNodes(r)
minus2loglik(rbest) < minus2loglik(r)
all.equal(minus2loglik(r), 349.1242, tolerance=tol)

# values near the optimum
r <- mspath(fib~time, misc=TRUE, ematrix=e2, qmatrix=q2,
            inits=c(1.89, -0.38, 1.31, -1.33,
              14.15, -1.31, -5.31, -0.24, -053),
            subject=id,
            data=d,
            stepnumerator=1, stepdenominator=1,
            initprobs=c(1.0, 0, 0, 0, 0),
            fixed=seq(9),
            method="BFGS")
nCases(r)
nGoodPaths(r)
nGoodPathNodes(r)
# Note: distinctly worse than the optimum, despite 2 digit agreement of coefficients.
all.equal(minus2loglik(r), 221.4708, tolerance=tol)
## end section validated against src/simulate

######## misc=SIMPLE optimization test with one fixed param
r <- mspath(fib~time, misc=SIMPLE, ematrix=e2, qmatrix=q2,
            inits=c(-1.6, -1.6, -1.2, -2.5, seq(.05, length=5, by=.01)),
            subject=id,
            data=d,
            stepnumerator=1, stepdenominator=1,
            initprobs=c(1.0, 0, 0, 0, 0),
            fixed=2,
            method="BFGS")
print(r, digits=2)
print(r, digits=2, showAll=TRUE) # should display the error terms

# tol is in percent
all.equal(coef(r), c(12.60, -1.60, 0.13, -0.12, seq(.05, length=5, by=.01)), tol=1)
all.equal(sd(r), c(291.92, 0, 0.49, 0.48, rep(0, 5)), tol=1)

# test reporting of various non-convergence
optresults(r)$convergence <- 1
r

optresults(r)$convergence <- 10
print(r, coeff="intercept", digits=2)

optresults(r)$convergence <- 51
optresults(r)$message <- "Test warning from L-BFGS-B"
print(r, coeff="intercept", digits=2)


optresults(r)$convergence <- 52
optresults(r)$message <- "Test error from L-BFGS-B"
print(r, coeff="intercept", digits=2)

## Check initial and optimal results with src/simulate as check.
## misc=SIMPLE
r <- mspath(fib~time, misc=SIMPLE, ematrix=e2, qmatrix=q2,
            inits=c(-1.6, -1.6, -1.2, -2.5, seq(.05, length=5, by=.01)),
            subject=id,
            data=d,
            stepnumerator=1, stepdenominator=1,
            initprobs=c(1.0, 0, 0, 0, 0),
            fixed=seq(9),
            method="BFGS")
nCases(r)
all.equal(minus2loglik(r), 341.7841, tolerance=tol)

r <- mspath(fib~time, misc=SIMPLE, ematrix=e2, qmatrix=q2,
            inits=c(12.60, -1.60, 0.13, -0.12, seq(.05, length=5, by=.01)),
            subject=id,
            data=d,
            stepnumerator=1, stepdenominator=1,
            initprobs=c(1.0, 0, 0, 0, 0),
            fixed=seq(9))
nGoodPaths(r)
all.equal(minus2loglik(r), 211.9785, tolerance=tol)
## end validatable portion

## Manager_test.cc::manager2 test replication

# Data_samples.cc::pData1c
data1c <- data.frame(id=c(4, 4, 5, 10, 10, 10),
                    time=c(2.3, 2.4, 2, 0, 0.1, 0.25),
                    fib=1+c(0, 1, 0, 0, 1, 2),
                    x1=c( .3, .3, 0, -1, -2, -1),
                    x2=c( 0, 2.1, 2.2, 2.3, 2.4, 2.5))

# pModel2(.025) =  Model(pSpec2(), pSpec3(), pathComputers0(initialTime))
# pSpec2
spec2 <- matrix(c(0, 1, 1,
                  1, 0, 1,
                  0, 0, 0), byrow=TRUE, nrow=3)
# makeSumLinearProduct2
# (makeConstantProduct3(&n));
#   pIntercept3
spec2.inits.intercept <- c( 0.9, -10.0, 0.2, -7.0)


# (makeDataProduct2(&n));
#   pSlope4
spec2.inits.slope <- c(10.1,  5.3,  8.1, -2.3,
                     2.4, -5.0, -2.1,  4.0)

# (makePathProduct3(&n));
#  picks out the first path-dependent var
#  pSlope5
spec2.inits.path <- c(-11, 8.4, 5.5, 15.3)

# pSpec3
spec3 <- matrix(c( 0, 1, 0,
                  1, 0, 0,
                  0, 0, 0), byrow=TRUE, nrow=3)
# makeConstantProduct4
#  pIntercept4
spec3.inits <- c(-1.8, -0.8)


 r <- mspath(fib~time, misc=TRUE, ematrix=spec3, qmatrix=spec2,
             covariates= ~ x1+x2,
             # misccovariates aren't used, so no need to constrain
             pathvars= c("TIS"),
             pathoffset= .025,
             inits=c(spec2.inits.intercept, spec2.inits.slope,
               spec2.inits.path, spec3.inits),
             subject=id,
             data=data1c, stepnumerator=1, stepdenominator=20, initprobs=c(1.0, 0, 0, 0, 0),
             do.what=1, fixedpars=seq(4*4+2),
             isexact=TRUE)
all.equal(minus2loglik(r), -2*log( 0.1848857*2.667778e-08 ), tolerance=1e-5)
nCases(r)
nGoodPaths(r)

# as above, without exact observation times of death
 r <- mspath(fib~time, misc=TRUE, ematrix=spec3, qmatrix=spec2,
             covariates= ~ x1+x2,
             # misccovariates aren't used, so no need to constrain
             pathvars= c("TIS"),
             pathoffset= .025,
             inits=c(spec2.inits.intercept, spec2.inits.slope,
               spec2.inits.path, spec3.inits),
             subject=id,
             data=data1c, stepnumerator=1, stepdenominator=20, initprobs=c(1.0, 0, 0, 0, 0),
             do.what=1, fixedpars=seq(4*4+2))


all.equal(minus2loglik(r), -2*log( 0.1848857*7.066139e-06 ), tolerance=1e-5)
nCases(r)
nGoodPaths(r)
