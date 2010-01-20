# test handling of paths for which we observe
# covariates but not state

# This is an independent check on mspath/tests/test7.R.
# I use the fake calculator, and select the correct data
# values by hand.

# Requires the following prerequisites.
# library("mspath")
# source("mspsim.R")
# source("fakeCalc.R")


## sort of like Manager_test.cc::manager2 test

# Data_samples.cc::pData1c
data1c <- data.frame(ssn=c(4, 4, 5, 10, 10, 10),
                    time=c(2.3, 2.4, 2, 0, 0.1, 0.5),
                    fib=1+c(0, 1, 0, 0, 1, 2),
                    x1=c( .3, .3, 0, -1, -2, -1),
                    x2=c( 0, 2.1, 2.2, 2.3, 2.4, 2.7))
baseline <- data1c[data1c$ssn==10,]
baseline

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

compute <- function(data) {
  mspath(fib~time, misc=TRUE, ematrix=spec3, qmatrix=spec2,
                covariates= ~ x1+x2,
                          # misccovariates aren't used, so no need to constrain
                pathvars= c("TIS"),
                pathoffset= .025,
                inits=c(spec2.inits.intercept, spec2.inits.slope,
                  spec2.inits.path, spec3.inits),
                subject=ssn,
                data=data, stepnumerator=1, stepdenominator=10, initprobs=c(1.0, 0, 0, 0, 0),
                do.what=1, fixedpars=seq(4*4+2),
                isexact=TRUE, calcFactor=fakeCalculator)
}

# baseline calculation
compute(baseline)

## adding cov-only obs in first interval has no effect
d1 <- rbind(baseline[1,],
#            c(10, 0.02, 0, 100, 100),
            baseline[2:3,]
            )
d1
compute(d1)

## adding cov-only in otherwise emtpy interval has effect
d2 <- rbind(baseline[1:2,],
            c(10, 0.4, 0, 3, -1),
            baseline[3,]
            )
d2
compute(d2)

## observations with state preferred over those without
## after 1st interval
d3 <- rbind(baseline[1:2,],
#            c(10, 0.4, 0, 100, 100),
            c(10, 0.4, 2, -100, -100),
#            c(10, 0.44, 0, 200, 200),
            baseline[3,]
            )
d3
compute(d3)

## use last of unobserved in an interval without observed
d3b <- rbind(baseline[1:2,],
#            c(10, 0.4, 0, 100, 100),
            c(10, 0.4, 0, -10, 50),
            baseline[3,]
            )
d3b
compute(d3b)

# use last observed
d3c <- rbind(d3[1:3,],
             c(10, 0.442, 1, 3, -3),
#             c(10, 0.443, 0, 5, 2),
             baseline[3,]
             )
d3c
compute(d3c)

## multiple cov-only observations in different intervals
d4 <- baseline
d4$time <- 2.9
d4 <- rbind(baseline[1:2, ],
            c(10, 0.4, 0, 2, 4),
            c(10, 0.5, 0, 0, 1),
            baseline[3,]
            )
d4
compute(d4)


## adding missing data that matches our assumptions without it has no effect
d5 <- baseline[c(1, 2, 2, 2, 3),]
d5$time[3:4] <- c(0.3, 0.4)
d5$fib[3:4] <- c(0, 0)
d5
compute(d5)
