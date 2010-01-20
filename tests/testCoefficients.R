library("mspath")

permissible <- matrix(c(0, 1, 0, 0), nrow=2)
covConstraint <- c(1, 2)
baseConstraint <- c(1)
covNames <- c("a", "b")
estimate <- c(3, 4, 5.0)
pathNames <- c("TIS", "TSO")
pathConstraint <- seq(2)

# blow up with NULL pathVars?
mspathCoefficients(permissible, constrVec=covConstraint,
                        baseConstrVec=baseConstraint,
                        covVars=covNames,
                        params=estimate,
                        offset=0)

# blow up with NULL covariates?
mspathCoefficients(permissible,
                   params=c(-1, 35, 3),
                   offset=0,
                   baseConstrVec=baseConstraint,
                   pathVars = pathNames,
                   pathConstrVec = pathConstraint)


# blow up with both NULL?
mspathCoefficients(permissible,
                   params=c(-3),
                   offset=0,
                   baseConstrVec=baseConstraint
                   )

# works with everything?
mspathCoefficients(permissible,
                   params=c(.05, .1, .2, .3, .4),
                   offset=0,
                   baseConstrVec=baseConstraint,
                   covVars=covNames,
                   constrVec=covConstraint,
                   pathVars = pathNames,
                   pathConstrVec = pathConstraint
                   )

# in real life, we may have error coefficient estimates too
# so this should produce same result as above, despite
# having extra parameters on the end
mspathCoefficients(permissible,
                   params=c(.05, .1, .2, .3, .4, 1, 2, 3, 4, 5, 6, 7),
                   offset=0,
                   baseConstrVec=baseConstraint,
                   covVars=covNames,
                   constrVec=covConstraint,
                   pathVars = pathNames,
                   pathConstrVec = pathConstraint
                   )

# stick one at front, again should be same
# tests offset
mspathCoefficients(permissible,
                   params=c(7, .05, .1, .2, .3, .4, 1, 2, 3, 4, 5, 6),
                   offset=1,
                   baseConstrVec=baseConstraint,
                   covVars=covNames,
                   constrVec=covConstraint,
                   pathVars = pathNames,
                   pathConstrVec = pathConstraint
                   )


####### more challenging example with offsets

# 3x3, just up transitions
permissible <- matrix(0, nrow=3, ncol=3)
permissible <- row(permissible)+1 == col(permissible)
covConstraint <- seq(4)
covConstraint[4] <- 2
pathConstraint <- c(1, 1, 2, 3)
c <- mspathCoefficients(permissible,
                   params=seq(from=.1, by=.1, len=18),
                   offset=10,
                   baseConstrVec=seq(2),
                   covVars=covNames,
                   constrVec=covConstraint,
                   pathVars=pathNames,
                   pathConstrVec=pathConstraint
                   )
all.equal(coef(c), seq(from=1.1, by=.1, len=8))
all.equal(sd(c), rep(0, 8))
matrixCoef(c)
          
# the next 3 should produce identical output
c
show(c)
print(c)

# print only 1 coefficient of interest
print(c, coeff="a")

# and see if this give us all but intercept
print(c, coeff=covNames)

# check it doesn't blow up if some arguments are passed to lower level print
# digits=3 does nothing when there are only 2 digits
print(c, coeff="b", digits=3)

# next should be TRUE
isAllFixed(c)


###### 5 x 5, history but no covariates
permissible <- matrix(c( 0, 1, 1, 0, 1,
                         1, 0, 1, 1, 1,
                         0, 0, 0, 0, 1,
                         1, 0, 0, 0, 1,
                         0, 0, 0, 0, 0), ncol=5, byrow=TRUE)
pathConstraint <- c(1, 1, 2, 3, 1, 1, 4, 5, 2, 6,
                   7, 8, 2,  8, 7, 8, 9, 10, 11, 12)
baseConstraint <- c(1, 1, 2, 3, 1, 1, 4, 5, 2, 6)
c <- mspathCoefficients(permissible,
                        params= 101:118,
                        baseConstrVec=baseConstraint,
                        pathVars=pathNames,
                        pathConstrVec = pathConstraint)
c
matrixCoef(c, coeff=c("intercept"))
print(c, coeff=c("TSO"), digits=3)

isAllFixed(c) # TRUE

##### test estimated coefficients
fixed <- c(3, 10, 11)
var <- seq(from=30, by=5, length.out=15)^2
c <- mspathEstimatedCoefficients(permissible,
                                 params= 101:118,
                                 baseConstrVec=baseConstraint,
                                 pathVars=pathNames,
                                 pathConstrVec = pathConstraint,
                                 fixed=fixed,
                                 n=50,
                                 var=var)
all.equal(coef(c), 101:118)
allsd <- rep(0, 18)
allsd[setdiff(1:18, fixed)] <- sqrt(var)
all.equal(sd(c), allsd)

# test default show method
c
isAllFixed(c) # FALSE

print(c, coeff=c("intercept", "TIS"), digits=2)

## add extra values, reflecting a presumed estimation of
# transition and error probabilities.
# Results should be almost same as above, except fewer df.
var2 <- seq(from=30, by=5, length.out=25)^2
c <- mspathEstimatedCoefficients(permissible,
                                 params= 101:125,
                                 baseConstrVec=baseConstraint,
                                 pathVars=pathNames,
                                 pathConstrVec = pathConstraint,
                                 fixed=fixed,
                                 n=50,
                                 var=var2)
c

# same deal, but as if a whole earlier coefficient were estimated
# There are some fixed values in the earlier coefficient, so it
# has 10 parameters, 8 of which are free.
# same output as above, except indices are offset and different df
var3 <- c(seq(10, by=5, length=8), seq(from=30, by=5, length.out=18))^2
c <- mspathEstimatedCoefficients(permissible,
                                 params= c(seq(10)+20, 101:118),
                                 baseConstrVec=baseConstraint,
                                 pathVars=pathNames,
                                 pathConstrVec = pathConstraint,
                                 fixed=c(4, 8, fixed+10),
                                 n=50,
                                 offset=10,
                                 var=var3)
print(c, digits=3)


#### "estimated" case in which all are fixed
fixed <- seq(18)
# however, there are additional params, not involved in this
# coefficient.  This was confusing isAllFixed
c <- mspathEstimatedCoefficients(permissible,
                                 params= 101:120,
                                 baseConstrVec=baseConstraint,
                                 pathVars=pathNames,
                                 pathConstrVec = pathConstraint,
                                 fixed=fixed,
                                 n=50)
c
isAllFixed(c) # TRUE

### base, covariate, and path parameters
fixed <- c(3, 8, 15, 21, 22)
var <- seq(from=20, by=5, length.out=24)^2
covConstraint <- c(1, 2, 3, 4, 1, 5, 6, 1, 4, 7, 8, 8, 8, 9, 9, 9, 9, 10, 11, 11)
c <- mspathEstimatedCoefficients(permissible,
                                 params= 101:129,
                                 baseConstrVec=baseConstraint,
                                 covVars=covNames,
                                 constrVec=covConstraint,
                                 pathVars=pathNames,
                                 pathConstrVec = pathConstraint,
                                 fixed=fixed,
                                 n=100,
                                 var=var)
# selective first, in reverse order
print(c, coeff=c("TIS", "intercept"))

# all but the intercept
mycoef <- c(pathNames, covNames)
print(c, coeff=mycoef, digits=3)

# whole thing
print(c)

isAllFixed(c) # FALSE
