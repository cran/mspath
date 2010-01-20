# This will grow into a system that can simulate data for testing

############### Specification
setClass("Specification",
         representation(permit="matrix", coeff="matrix", permiti="matrix"),
         validity = function(object) {
           is.logical(object@permit) && is.numeric(object@coeff) &&
           sum(object@permit) == dim(object@coeff)[2]
         }
         )

# Specification desribes a multinomial logit
# permit is R x C (square for us)
# permit[r, c] == TRUE -> transition from r->c possible
# Multinomial is among all allowed c's for a given r.
# r->r is always possible.
# There are K such transitions (not counting diagonal).
# Reading as in a book, number them from 1 to K (@permiti)
# coeff[i, k] is for k'th permitted result, covariate i
Specification <- function(permit, coeff) {
  # get the indices in coeff for each TRUE entry in permit
  d <- dim(permit)
  permit <- as.logical(permit)
  dim(permit) <- d
  speci <- matrix(0, dim(permit)[1], dim(permit)[2])
  speci[t(permit)] <- seq(sum(permit))
  speci <- t(speci)
  new("Specification", permit=permit, coeff=coeff, permiti=speci)
}

likelihood <- function(spec, covs, states) {
            # states[1] = from; states[2] = to
            # in keeping with msm covs[,i] is i'th obs
            # covs[1] time  
            # covs[2] observed state or 0
            # covs[3+ ] observed and path-dependent covariates

            # multinomial logit transform
            transi <- spec@permiti[states[1],]
            transpermit <- spec@permit[states[1],]
            linear <- t(spec@coeff[,transi[transpermit], drop=FALSE]) %*% covs[3:length(covs)]
            expn <- exp(linear)
            exprow <- rep(0, dim(spec@permit)[2])
            exprow[transpermit] <- expn
            exprow[states[1]] <- 1.0
            r <- exprow[states[2]]/(sum(expn)+1.0)
            #cat(sprintf("%i->%i (%i@%f): %f\n", states[1], states[2],
            #covs[2], covs[1], r))
            r
          }

setGeneric("likelihood")

# generic written for Specification
nstates <- function(object) dim(object@permit)[1]

setGeneric("nstates")

############ SimpleSpecification
# here user simply specifies probabilities
# only transformation is computing the diagonal elements as 1 - row sum

# @prob is the final probability matrix
setClass("SimpleSpecification",
         representation(permit="matrix", coeff="numeric", prob="matrix"),
         validity = function(object) {
           is.logical(object@permit) && is.numeric(object@coeff) &&
           sum(object@permit) == length(object@coeff)
         }
         )

setClassUnion("AbstractSpecification", c("SimpleSpecification", "Specification"))
SimpleSpecification <- function(permit, coeff) {
  # get the indices in coeff for each TRUE entry in permit
  d <- dim(permit)
  permit <- as.logical(permit)
  dim(permit) <- d
  prob <- matrix(0, dim(permit)[1], dim(permit)[2])
  prob[t(permit)] <- coeff
  prob <- t(prob)
  diag(prob) <- 1-apply(prob, 1, sum)
  new("SimpleSpecification", permit=permit, coeff=coeff, prob=prob)
}

setMethod("likelihood",
          signature(spec="SimpleSpecification", covs="vector",
                    states="vector"),
          definition = function(spec, covs, states) {
            # states[1] = from; states[2] = to
            # covs[1] time  
            # covs[2] observed state or 0
            spec@prob[states[1], states[2] ]
          }
          )

############### Model


#base class has no measurement error
setClass("Model",
         representation(trans="Specification")
         )

Model <- function(trans) {
  new("Model", trans=trans)
}

pathLikelihood <- 
           # return likelihood of a single path
           # path is a matrix
           # path[1,] true state
           # path[2,] time
           # path[3,] observed state or 0 if not observed
           # path[4+, ] covariates -- there must be at least one
           # (remember the constant term)
           # path dependent variables are last
           function(model, path){
             result = 1.0
             npts = ncol(path)
             ndata = nrow(path)
             for (j in (2:npts)) {
               f <- likelihood(model@trans,
                 c(path[2:ndata, j]),
                 c(path[1, j-1], path[1, j])
                 )
               #cat("Pr at ", j," is ", f, "\n")
               result = result * f             }
             result
           }

setGeneric("pathLikelihood")

# Return all paths beginning with root
# return matrix has one row for each path and
# length(obstate) columns.
# obstate[1] gives the observed state or 0 for root
# obstate[2] for next step, and so on.
# May return a 0 row matrix if root is inconsistent with obsState
# allowed is a list of vectors of allowed transition states
recursePaths <- function(model, root, obsState, allowed){
  #cat(root, obsState, "\n")
  if (obsState[1] != 0 &&
      ! canObserve(model, obsState[1], root))
    # true state root inconsistent with observed state
    # cut off path generation
    return(matrix(root, nrow=0, ncol=length(obsState)))
  if (length(obsState) == 1)
    # terminal node
    return(matrix(root, 1, 1))
  m <- sapply(allowed[[root]],
              function(nextRoot)
              recursePaths(model, nextRoot, obsState[-1],
                           allowed),
              simplify=FALSE)
  #print(m)
  r <- do.call(rbind, m)
  cbind(rep(root, nrow(r)), r)
                                    
}

# Return matrix of all possible paths consistent with data and model
# each row is a path of true states
# obstate gives observed state or 0 for each step
generatePaths <- function(model, obsState){
  permit <- model@trans@permit
  diag(permit) <- TRUE
  allowed <- lapply(seq(nrow(permit)), function(i)
                        seq(ncol(permit))[permit[i,]])
  # allowed is now a list; element i is the vector of allowed states
  # to which i can move

  obsState[1] <- 0  # never check measurement at root
  
  recursePaths(model,
               1,   # always start in state 1
               obsState,
               allowed)
}

canObserve <- function(model, observedState, trueState) {
  observedState == 0 || observedState == trueState
}

setGeneric("canObserve")

setMethod("nstates",
          signature(object="Model"),
          definition = function(object) nstates(object@trans))

# generic for Model
isAbsorbing <- function(object, state) {
  if (missing(state)) state <- nstates(object)
  !any(object@trans@permit[state,])
}

## Model with Error
setClass("ErrorModel",
         representation(measurement="AbstractSpecification"),
         contains="Model",
         validity = function(object) {
           if( any(dim(object@measurement@permit) !=
               dim(object@trans@permit)))
             return("Both specs must have same dimensions")
           if(is(object@measurement, "Specification"))
             # SimpleSpecification ignores covariates
              if (nrow(object@measurement@coeff) !=
               nrow(object@trans@coeff))
             return("Both specs must be for same number of covariates")
           TRUE
         }
       )

ErrorModel <- function(trans, measure){
  new("ErrorModel", trans=trans, measurement=measure)
}

setMethod("pathLikelihood",
          signature(model="ErrorModel", path="matrix"),
          definition = function(model, path) {
            like = callNextMethod()
            # get indices where we have observation
            i <- 2:ncol(path)
            i <- i[path[3, 2:ncol(path)] != 0]
            for (j in seq(length(i))){
              #cat("obserror ")
              f <- likelihood(model@measurement,
                path[2:nrow(path), i[j]],
                c(path[1, i[j]], path[3, i[j]]))
              like = like * f
              #cat("Observtn factor ", j," at ", i[j], " is ", f, "\n")
            }
            like
          })

setMethod("canObserve",
          signature(model="ErrorModel", observedState="numeric",
          trueState="numeric"),
          definition = function(model, observedState, trueState) {
            callNextMethod() ||
            model@measurement@permit[trueState, observedState]
          }
          )
         
############ Observed Data for a Case

#time[t], path[t], covs[,t] are for t'th observation
# If observation has covariates but not state, path[t] == 0
setClass("Observations",
         representation(time="numeric", path="numeric",
                        covs="matrix", subjectid="numeric"),
         validity = function(object) {
           length(object@time) == length(object@path) &&
           length(object@path) == ncol(object@covs)
         }
         )

Observations <- function(time, path, covs=matrix(1, 1, length(time)), id=0){
  new("Observations", time=time, path=path, covs=covs, subjectid=id)
}

time <- function(object) object@time
state <- function(object) object@path


####### Possible True Paths and Times for a Case
# "True" as opposed to "observed"

# paths[i, j] is state at time time[j] for i'th path
# obsState[j] is observed state or 0 if no corresponding observation
# indexObs[j] is index in Observations (even if obsState == 0)
# obsState and indexObs are based on @obs
#
# calculated values.  They may be Inf
# first[i] is index of first time in absorbing state for path i
#   I assume no measurement error for absorbing state
#   Assume absorbing state == highest numbered state
# indexExact earliest index possible for absorbing w/exact obs
# indexApprox earliest index possible with approx time measurement
setClass("TruePaths",
         representation(time="numeric", paths="matrix",
                        obsState="numeric", indexObs="integer",
                        obs="Observations",
                        # next 3 should be integers or Inf
                        first="numeric",
                        indexApprox="numeric", indexExact="numeric"),
         validity = function(object) {
           length(object@time) == ncol(object@paths)
         }
         )

# paths is either a matrix as above or a Model
# If it's a Model appropriate paths will be generated
# If paths is not a Model you must specify the absorbing state.
TruePaths <- function(time, paths, observations, absorb = nstates(paths)){
  forceeval <- absorb
  obsTime <- time(observations)
  obsIndices <- matrix(seq(length(obsTime)), length(obsTime),
  length(time))
  comp <- outer(obsTime, time, '<=')
  indices <- ifelse(comp, obsIndices, 0)
  maxi <- apply(indices, 2, max)
  isTrueObs <- maxi != c(0, maxi[1:(length(maxi)-1)])
  obsState <- rep(0, length(time))
  obsState[isTrueObs] <- state(observations)[maxi[isTrueObs]]
  if (is(paths, "Model")) {
    # this is a horrible hack
    m <- paths
    paths <- generatePaths(m, obsState)
  }
  indexExact <- findFirstIndex(obsState, absorb)
  first <- apply(paths, 1, function(r) findFirstIndex(r, absorb))
  if (indexExact == Inf) {
    # never observed in absorbing state
    indexApprox <- Inf
  } else {
    if (indexExact > 1) {
      upToObs <- isTrueObs[1:(indexExact-1)]
      # earliest time with approx observation is 1 after last observed
      indexApprox <- 1 + max(seq(length(upToObs))[upToObs])
    } else {
      indexApprox <- 1
    }
  }
    
  new("TruePaths", time=time, paths=paths,
      obsState=as.integer(obsState), indexObs=as.integer(maxi),
      obs=observations, first=first,
      indexExact=indexExact, indexApprox=indexApprox)
}

# default generic (for TruePath)
nPaths <- function(object) nrow(object@paths)
setGeneric("nPaths")

# return True/False for whether path is good under exact observation
# of absorption time
isGoodExact <- function(object) {
  object@first == object@indexExact
}

# same with approximate observation of absorption time
isGoodApprox <- function(object) {
  object@first >= object@indexApprox &
  object@first <= object@indexExact
}

# return matrix of covariates, one column per true path time period
pathCovariates <- function(truePaths){
  truePaths@obs@covs[,truePaths@indexObs]
}

# helper

# return first index of vec with value state
# Inf if no match
findFirstIndex <- function(vec, state) {
  suppressWarnings(min(seq(along=vec)[vec==state]))
}

########## Make complete path and covariates for analysis
setClass("CompletePathMaker",
         representation(timeOffset="numeric", # to avoid 0's
                        truePaths="TruePaths",
                        history="character")
         )

# history gives names of path-dependent variables to include
# allowed values are
#   "TIS" = Time in State
#   "TIC" = Time in Case (not really path-dependent)
#   "LN(TIS) = ln(Time in State)
#   "LN(TIC) = ln(Time in case)
CompletePathMaker <- function(truePaths, timeOffset=0, history=c("")){
  new("CompletePathMaker", timeOffset=timeOffset,
      truePaths=truePaths, history=history)
}

# Returns a matrix whose columns are successive steps
# [1, ] true state
# [2, ] time
# [3, ] observed state, 0 if none
# [4+, ] covariates
# [n, ] path dependent vars, if any
#  TIS will always precede TIC
path <- function(pm, i){
  # i is index of path to generate
  i <- as.integer(i)
  truePaths <- pm@truePaths
  if ( i > nrow(truePaths@paths))
    stop("Requested path ", i, ", but only ",
         nrow(truePaths@paths), " available")
  path <- truePaths@paths[i,]
  obs <- pathCovariates(truePaths)
  time <- truePaths@time
  # compute history-dependent variables
  caseTime <- time-time[1]  # time since start of case
  isJump <- path != c(0, path[1:length(path)-1]) # true on jumps
  jumpi <- seq(length(isJump))[isJump] # indices of just the jumps
  iLastJump <- jumpi[cumsum(isJump)]
  stateTime <- time-time[iLastJump] # time in current state
  # for path variables, we want to use the lagged values
  caseTime <- c(0, caseTime[1:(length(caseTime)-1)])
  stateTime <- c(0, stateTime[1:(length(stateTime)-1)])
  r <- rbind(path,
             time,
             truePaths@obsState,
             obs)
  if (any("TIS" == pm@history))
    r <- rbind(r, stateTime+pm@timeOffset)
  if (any("TIC" == pm@history))
    r <- rbind(r, caseTime+pm@timeOffset)
  if (any("LN(TIS)" == pm@history))
    r <- rbind(r, log(stateTime+pm@timeOffset))
  if (any("LN(TIC)" == pm@history))
    r <- rbind(r, log(caseTime+pm@timeOffset))
  r
}

setMethod("nPaths", "CompletePathMaker",
          function(object) nPaths(object@truePaths)
          )

####### Results

# likelihoods and the values used to construct them
setClass("PathResults",
         representation(likelihoods="data.frame",
                        pathMaker="CompletePathMaker",
                        model="Model")
         )

# return PathResults
evaluatePaths <- function(pathMaker, model){
  i <- seq(nPaths(pathMaker))
  like <- sapply(i, function(i){
    pathLikelihood(model, path(pathMaker, i))
  })
  new("PathResults",
      likelihoods = data.frame(likelihood=like,
        P=pathMaker@truePaths@paths),
      pathMaker=pathMaker,
      model=model)
}

setMethod("show", "PathResults",
          function(object) cat(sum(object@likelihoods$likelihood),
                               nrow(object@likelihoods), "paths\n",
                               if (isAbsorbing(object@model)) cat(
                               sum(object@likelihoods$likelihood[isGoodApprox(object@pathMaker@truePaths)]),
                               sum(isGoodApprox(object@pathMaker@truePaths)), "approx paths\n",
                               sum(object@likelihoods$likelihood[isGoodExact(object@pathMaker@truePaths)]),
                               sum(isGoodExact(object@pathMaker@truePaths)), "exact paths\n"))
          )

setMethod("summary", "PathResults",
          function(object) print(if (isAbsorbing(object@model)) {
                                        data.frame(object@likelihoods,
                                                   approx=isGoodApprox(object@pathMaker@truePaths),
                                                   exact=isGoodExact(object@pathMaker@truePaths))
                                 } else {
                                        object@likelihoods
                                      })
          )

