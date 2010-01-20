# test handling of overlap using special test calculator
library("mspath")

# in first interval, favor the first observation
d1 <- data.frame(id=rep(201, 3), x=seq(3), time=c(0, 0.1, 0.5), s=1:3)
d1
q <- matrix(1, nrow=3, ncol=3)
in1 <- rep(1, 12)
myt <- list(inits=in1,
            allinits=in1,
            subject=rep(201, 2),
            time=c(0, 0.5),
            state=c(1, 3),
            qvector=c(0, 1, 1,  1, 0, 1, 1, 1, 0),
            covvec=c(1, 3),
            constrvec=1:6,
            baseconstrvect=1:6,
            initprobs=c(1, 0, 0),
            nstates=3,
            nintens=6,
            nintenseffs=6,
            nobs=2,
            npts=1,
            ncovs=1,
            stepnumerator=1,
            stepdenominator=2)

r <- mspath(s~time, q, inits=in1, subject=id, covariates=~x,
            data=d1, testing=myt,
            stepdenominator=2)
# if it does not throw an error we match expectations

# handling should be the same regardless of whether initial time is 0.
# It was not always so.
d2 <- d1
d2$time <- d1$time+10
myt2 <- myt
myt2$time <- myt$time+10

r <- mspath(s~time, q, inits=in1, subject=id, covariates=~x,
            data=d2, testing=myt2,
            stepdenominator=2)
