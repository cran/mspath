# test handling of lagged covariates,
# namely, test that calculations do NOT use lagged values
# Based on and validated against src/simulate/test02.R

library("mspath")

## Check on whether we use current covars or lagged ones
# Changed stepdenominator to 10 from 20 to avoid step between
# observations.  If there is such a step, setting a covariate
# at time 0 will affect the outcome even if there is no lagging.

## Manager_test.cc::manager2 test near-replication

# Data_samples.cc::pData1c
data1c <- data.frame(ssn=c(4, 4, 5, 10, 10, 10),
                    time=c(2.3, 2.4, 2, 0, 0.1, 0.3),
                    fib=1+c(0, 1, 0, 0, 1, 2),
                    x1=c( .3, .3, 0, -1, -2, -1),
                    x2=c( 0, 2.1, 2.2, 2.3, 2.4, 2.5))
data1c

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

# also checks that a subject with 1 observation has no effect (data1c vs d1c)
rreal <- mspath(fib~time, misc=TRUE, ematrix=spec3, qmatrix=spec2,
                covariates= ~ x1+x2,
                                        # misccovariates aren't used, so no need to constrain
                pathvars= c("TIS"),
                pathoffset= .025,
                inits=c(spec2.inits.intercept, spec2.inits.slope,
                  spec2.inits.path, spec3.inits),
                subject=ssn,
                data=data1c, stepnumerator=1, stepdenominator=10, initprobs=c(1.0, 0, 0, 0, 0),
                do.what=1, fixedpars=seq(4*4+2),
                isexact=TRUE)
rreal
# should be 34.47033 = -2*log-likehood for 3 id's.
# 7 good paths


# The second calculation introducs a different covariate value at time 0.
# This should have no effect if we use cov(i) for transition i-1 -> i.
d <- data1c
d$x2[1] <- 1000
d
rreal2 <- mspath(fib~time, misc=TRUE, ematrix=spec3, qmatrix=spec2,
                covariates= ~ x1+x2,
                                        # misccovariates aren't used, so no need to constrain
                pathvars= c("TIS"),
                pathoffset= .025,
                inits=c(spec2.inits.intercept, spec2.inits.slope,
                  spec2.inits.path, spec3.inits),
                subject=ssn,
                data=d, stepnumerator=1, stepdenominator=10, initprobs=c(1.0, 0, 0, 0, 0),
                do.what=1, fixedpars=seq(4*4+2),
                isexact=TRUE)
rreal2
# should be same as previous
