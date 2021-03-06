# Tests mspathCalculator without using the wrapping functions
# Use SimpleSpecification
# Same as first element in ModelBuilder_test.cc
#   modelBuilderSimple
#
# Note: the textual comparisons with previous output will check if all
# printed digits are equal.  In some cases this may be too stringent;
# rely on the all.equal tests for better guidance about whether something
# bad is going on.

library(mspath)

qmatrix <- matrix(c(FALSE, TRUE, TRUE, FALSE,
                    TRUE, FALSE, TRUE, FALSE,
                    FALSE, FALSE, FALSE, TRUE,
                    FALSE, TRUE, FALSE, FALSE),
                  nrow=4,
                  byrow=TRUE)
ematrix <- matrix(c(FALSE, TRUE, FALSE, FALSE,
                    TRUE, FALSE, FALSE,  TRUE,
                    FALSE, TRUE, FALSE,  TRUE,
                    TRUE, FALSE, FALSE, FALSE),
                  byrow=TRUE,
                  nrow = 4)
times <- c(1, 2, 5, 7,   101, 104, 106)
states <- c(1, 3, 3, 2,  1, 4, 4)
cov1 <- c(1.94, -2.46,  0.09,  2.80,   -0.60, -6.11, -1.15)
cov2 <- c(-4.23,  0.23, -4.22,  1.53,  3.63,  0.84,  2.42)
cov3 <- c(3.51,  1.93,  5.30,  1.03,   0.56, -3.32, -0.50) # apparently unused
inits <- c( -.4, 1.1, .3, # intercept
           -1.1, .3, -1.1, 1.1, 0.4, # covariates
           0.4, -0.9,  -0.59, -0.8, 2.15, 0.5, 1.0, 2.8, # paths
           # misclass
           .04, .28, .01, .02, .1 # intercept
           )

calc <- mspathCalculator (do.what=1, params=inits, allinits=inits,
                          misc=2, # i.e., SIMPLE
                          subject=c(rep(1,4), rep(23, 3)), time=times,
                          state=states, qvector=c(t(qmatrix)),
                          evector=c(t(ematrix)),
                          covvec=c(cov1, cov2),
                          constrvec=c(c(1, 1, 2, 1, 1, 2), c(3, 5, 3, 4, 5, 3)),
                          misccovvec=NULL, miscconstrvec=NULL,
                          baseconstrvec=c(1, 2, 3, 3, 2, 1),
                          basemiscconstrvec=c(1, 2, 3, 2, 4, 5), 
                          pathvars= c("LN(TIS)", "TIS", "TSO"),
                          pathoffset=1,
                          pathconstrvec= c(
                            c(1, 1, 1, 2, 2, 2),
                            c(3, 4, 5, 5, 4, 3),
                            c(6, 8, 7, 7, 6, 8)),
                          initprobs=c(1, 0, 0, 0), nstates=4,
                          nintens=6, nintenseffs=3,
                          nmisc=6, nmisceffs=5,
                          nobs=7, npts=2,
                          ncovs=2, ncoveffs=5,
                          nmisccovs=0, nmisccoveffs=0,
                          npatheffs=8,
                          isexact=FALSE,
                          fixedpars=NULL,
                          stepnumerator=1, stepdenominator=1)

tol <- 1e-7
calc <- calculate(calc)
nCases(calc)
nActiveCases(calc)
minus2loglik(calc)
results(calc)
all.equal(minus2loglik(calc), -2*log(0.0001066036 * 0.009361202 ), tolerance=tol)

# test of new calculator features
estimateWork(calc)
nCases(calc)
nActiveCases(calc)
effort(calc)
# should get same result as before
calc <- calculate(calc)
results(calc)
all.equal(minus2loglik(calc),  -2*log(0.0001066036 * 0.009361202 ), tolerance=tol)

# information explicitly validated from mspsim:
# likelihood, paths, 6 steps
activeCases(calc) <- 23
nCases(calc)
nActiveCases(calc)
calc <- calculate(calc)
all.equal(minus2loglik(calc),  -2*log(0.009361202 ), tolerance=tol)
results(calc)

# validated likelihood, paths, 7 steps
activeCases(calc) <- 1
nCases(calc)
nActiveCases(calc)
calc <- calculate(calc)
all.equal(minus2loglik(calc), -2*log(0.0001066036), tolerance=tol)
results(calc)

# revert to everyone
activeCases(calc) <- integer()
nCases(calc)
nActiveCases(calc)
calc <- calculate(calc)
all.equal(minus2loglik(calc),  -2*log(0.0001066036 * 0.009361202 ), tolerance=tol)
results(calc)

# try out of order, which was causing trouble.
# should be same as preceding.
activeCases(calc) <- as.integer(c(23, 1))
nCases(calc) # 2
nActiveCases(calc) # 2
activeCases(calc) # should be sorted ascending
calc <- calculate(calc)
all.equal(minus2loglik(calc),  -2*log(0.0001066036 * 0.009361202 ), tolerance=tol)

# test changing parameters
inits2 <- c(.8, .7, 2.4, # intercept
            .5, -1.2, -.8, .7, 2.3, # covariates
            0.3, .7, 1.2, -0.8, -.5, 1.1, .3, -1.0,  # paths
           # misclass
            .1, .15, .1, .2, .05  # intercept
            )

params(calc) <- inits2
nCases(calc)
nActiveCases(calc)
calc <- calculate(calc)
minus2loglik(calc)
-2*log(2.937975e-09 * 0.01080467 )
all.equal(minus2loglik(calc),  -2*log(2.937975e-09 * 0.01080467 ), tolerance=tol)
results(calc)

# subset
activeCases(calc) <- 1
calc <- calculate(calc)
results(calc)
all.equal(minus2loglik(calc),  -2*log(2.937975e-09 ), tolerance=tol)
activeCases(calc) <- 23
calc <- calculate(calc)
results(calc)
all.equal(minus2loglik(calc),  -2*log( 0.01080467 ), tolerance=tol)
activeCases(calc) <- integer()
calc <- calculate(calc)
all.equal(minus2loglik(calc),  -2*log(2.937975e-09 * 0.01080467 ), tolerance=tol)

# and change them back
params(calc) <- inits
nCases(calc)
nActiveCases(calc)
calc <- calculate(calc)
all.equal(minus2loglik(calc),  -2*log(0.0001066036 * 0.009361202 ), tolerance=tol)
results(calc)


##########  Test Fixed vs variable args
fixedpars <- c(3, 5, 6, 11, 19)
short <- inits[!seq(along.with=inits) %in% fixedpars]
short2 <- inits2[!seq(along.with=inits) %in% fixedpars]
calc <- mspathCalculator (do.what=1, params=short, allinits=inits,
                          misc=2, # i.e., SIMPLE
                          subject=c(rep(1,4), rep(23, 3)), time=times,
                          state=states, qvector=c(t(qmatrix)),
                          evector=c(t(ematrix)),
                          covvec=c(cov1, cov2),
                          constrvec=c(c(1, 1, 2, 1, 1, 2), c(3, 5, 3, 4, 5, 3)),
                          misccovvec=NULL, miscconstrvec=NULL,
                          baseconstrvec=c(1, 2, 3, 3, 2, 1),
                          basemiscconstrvec=c(1, 2, 3, 2, 4, 5), 
                          pathvars= c("LN(TIS)", "TIS", "TSO"),
                          pathoffset=1,
                          pathconstrvec= c(
                            c(1, 1, 1, 2, 2, 2),
                            c(3, 4, 5, 5, 4, 3),
                            c(6, 8, 7, 7, 6, 8)),
                          initprobs=c(1, 0, 0, 0), nstates=4,
                          nintens=6, nintenseffs=3,
                          nmisc=6, nmisceffs=5,
                          nobs=7, npts=2,
                          ncovs=2, ncoveffs=5,
                          nmisccovs=0, nmisccoveffs=0,
                          npatheffs=8,
                          isexact=FALSE,
                          fixedpars=fixedpars,
                          stepnumerator=1, stepdenominator=1)


calc <- calculate(calc)
all.equal(minus2loglik(calc),  -2*log(0.0001066036 * 0.009361202 ), tolerance=tol)
# this also tests the params optional argument interface
calc <- calculate(calc, params=short2)
all.equal(minus2loglik(calc),  -2*log(1.166600e-08*0.897739 ), tolerance=tol)


# previous short2 setting should persistent
calc <- calculate(calc, activeCases=23)
all.equal(minus2loglik(calc),  -2*log(0.897739 ), tolerance=3*tol)

activeCases(calc) <- integer()
params(calc) <- short
calc <- calculate(calc)
all.equal(minus2loglik(calc),  -2*log(0.0001066036 * 0.009361202 ), tolerance=tol)
