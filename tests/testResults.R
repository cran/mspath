# Test the mspath class object returned by the calculation
#
# Test Coverage:
# constructors? I just use new(); only other constructor is main mspath() fn
# unestimated (simple mspath class)  I should get this if all are fixed.
# standard case: variable transition, fixed MISC, constraints, convergent
# same, convergence failure (both neg eigen and opt reported non-convergence)
# both params estimated (see test1 for convergent case)
# both fixed
# facilities: show, print, print selected coefficients, print options,
#  print(showAll)
#  results: optresults, minus2loglik, nCases, nGoodPaths, nGoodNodes,
#           nBadNodes, nGoodPathNodes
# Note print is only method reimplemented for mspathFull


# Code borrowed from test2
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
## end borrowing
class(r)

r

# should be same as above, which uses show()
print(r)

# with fixed coefficients and formatting option
# showAll should not matter for this case, which is not an estimate
print(r, showAll=TRUE, digits=2)

# print subset of coefficients
# should print nothing with all fixed
print(r, digits=2, coeff=c("TIS", "intercept"))
      
# some of the values below may vary if algorithms change
# without it's being an error.  nBadNodes might change
# with smarter enumeration of the paths.  nGoodNodes might change.
# The others should be fixed.
nCases(r) # 2
nGoodPaths(r)  # should be 78
nGoodNodes(r) # just accepted the values that came out for next 3
nBadNodes(r)  # bad Nodes may change without that being an error
nGoodPathNodes(r)
tol <- 1e-7
all.equal(minus2loglik(r), -2*log(0.0001066036 * 0.009361202 ), tolerance=tol)


# Try to get optimization results when there are none.
optresults(r)


############# estimated coefficients
# hessian is not positive definite
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
            # my sample doesn't have enough variation
            # to estimate many of the params
            fixedpars = c(2, 3, 5, 7, 9, 10, 12, 13, 14, 15))
r

# next should show the fixed error coefficients
print(r, showAll=TRUE, digits=2)

names(optresults(r))

nCases(r) # 2
nGoodPaths(r)  # should be 78
nGoodNodes(r) # just accepted the values that came out for next 3
nBadNodes(r)  # bad Nodes may change without that being an error
nGoodPathNodes(r)

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

optresults(r)$convergence <- 997  # mystery value
print(r, coeff="intercept", digits=2)
