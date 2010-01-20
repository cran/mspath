# Tests the SimpleSpecification and mspathCalculator
# Same as first element in ModelBuilder_test.cc
#   modelBuilderSimple
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

r <- mspath(states~times, qmatrix, misc=SIMPLE, ematrix,
            inits, subject=c(rep(1,4), rep(23, 3)),
            covariates = ~ cov1 + cov2,
            constraint = list(
              cov1=c(1, 1, 2, 1, 1, 2),
	      cov2=c(3, 5, 3, 4, 5, 3)
              ),
            qconstraint = c(1, 2, 3, 3, 2, 1),
            econstraint = c(1, 2, 3, 2, 4, 5),
            pathvars =  c("LN(TIS)", "TIS", "TSO"),
            pathoffset = 1,
            pathconstraint = list(
              "LN(TIS)" = c(1, 1, 1, 2, 2, 2),
              TIS = c(3, 4, 5, 5, 4, 3),
              TSO = c(6, 8, 7, 7, 6, 8)),

            fixedpars = seq(along=inits))
tol <- 1e-7
nCases(r) # 2
nGoodPaths(r)  # should be 78
nGoodNodes(r) # just accepted the values that came out for next 3
nBadNodes(r)  # bad Nodes may change without that being an error
nGoodPathNodes(r)
# next line is the acid test
all.equal(minus2loglik(r), -2*log(0.0001066036 * 0.009361202 ), tolerance=tol)


# test of new calculator features
calc <- r@calc
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


# test changing parameters
# Because I only wanted a single iteration, all parameters above are fixed.
# So the calculators returned from mspath() don't respond to
# changing the parameters (which only changes the variable parameters).
# We use the following cheat to get things to work.
# test3.R tests the calculator more directly, but the current test
# uses something closer to the calculators and parameterization of
# mspath(), the public function, and so may have some value.
cargs <- calc@args
cargs$fixedpars <- as.integer(NULL) # undo fixed parameters
calc <- mspathCalculatorFromArgs(cargs)

# the old values work
calc <- calculate(calc)
all.equal(minus2loglik(calc),  -2*log(0.0001066036 * 0.009361202 ), tolerance=tol)

# tweak params
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
all.equal(minus2loglik(calc),  -2*log(2.937975e-09 * 0.01080467 ), tolerance=tol)
results(calc)

# test subsetting
activeCases(calc) <- 1
calc <- calculate(calc)
results(calc)
all.equal(minus2loglik(calc),  -2*log(2.937975e-09 ), tolerance=tol)
activeCases(calc) <- 23
calc <- calculate(calc)
results(calc)
all.equal(minus2loglik(calc),  -2*log( 0.01080467 ), tolerance=tol)
activeCases(calc) <- integer()  # i.e., all
calc <- calculate(calc)
all.equal(minus2loglik(calc),  -2*log(2.937975e-09 * 0.01080467 ), tolerance=tol)

# and change them back
params(calc) <- inits
nCases(calc)
nActiveCases(calc)
calc <- calculate(calc)
all.equal(minus2loglik(calc),  -2*log(0.0001066036 * 0.009361202 ), tolerance=tol)
results(calc)
