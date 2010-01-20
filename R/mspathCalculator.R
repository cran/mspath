# This class abstracts out the core calculation.
# Subclasses distribute the calculation.


# If calc is a calculator object, useage is
# Instantiate it with the data and model specification
# calc <- calculate(calc) to compute the likelihood
# results(calc), minus2loglik(calc), effort(calc) to look at results
#     Note those calls will blow up if you haven't called calculate() first.
# calc <- done(calc) when you no longer need it.


########################################################################
# DESIGN NOTES
#
# This class currently maintains DUPLICATE DATA in R and C++.
# So it's rather easy to get these out of sync.  We rely mostly
# on the discipline of users, because the alternatives would
# require more work (and this is really for internal use only).
# The alternatives that occur to me are
#
# 1) Maintain the state only in C++.
#    Would require writing accessors for that material in C++.
#    More significantly, by saving the parameters in R we can
#    reuse existing interfaces when calling into C++.  Without that info
#    in R, we would need new C++ interfaces, which would require new
#    code/design.  (I suppose we could use the hypothetical new
#    accessors to fetch the info out of C++ into R, and then use that
#    to make calls with the new interfaces.  But this would require
#    a very complete set of info to exist in C++ AND IT DOES NOT
#    CURRENTLY EXIST THERE.  Rather, that info is used to build a set
#    of objects and the original info is tossed.  So either one would
#    need to add storage for the data in C++, or else construct
#    fairly complicated routines to pull the info back out of the C++
#    objects.  In some cases, exact recovery might be impossible (e.g.,
#    the effective parameter values are in C++, but the mix of fixed
#    and variable values is not.)
#
# 2) Use a reference-based OO scheme.
#    S4, which this code uses, has the R functional style in which
#    the fact that a function changes an object does not imply
#    the object will change for the caller.  If a function changes,
#    for example, the parameters or active cases for the calculator,
#    the calculator for the parent will not necessarily reflect that
#    unless the function returns the new object AND the caller uses
#    that object only.
#
#    One solution would be to use R.oo or R.oop, which are reference
#    based.  That way, functions could change the state of the object
#    and the caller would get the changes.
#
#    The S4 object has an externalptr to the C++ object.  When the S4
#    object is copied, which conceptually occurs whenever it is updated
#    in R, the copies share the object the externalptr points to.  Thus,
#    externalptr introduces reference semantics into the object.  By using
#    reference semantics for the whole R level object (the calculator),
#    we get overall semantic consistency.
#
#    This probably would have made more sense from the start, but converting
#    the existing S4 code to one of the other schemes would be tedious.
#
# 3) Use only replacement operators when updates are involved.
#    R requires that "foo<-" be defined such that it returns the object
#    with the updated values.  This new value automatically replaces
#    the old one in the caller's space:
#       foo(calc) <- newvalue
#    will update calc without the need for explicit coding by the caller.
#    Conversely,
#       bar(calc)
#    will not change calc, uncless bar returns a new value and the caller
#    does
#       calc <- bar(calc).
#    Unfortunately, this would lead to some very unnatural expressions, e.g.
#       calculate(calc) <- list()  # or any other dummy
#       selectAll(calc) <- anything
#
#  4) A related design issue is how to handle RETURN VALUES.  The usual idiom
#     is that they are in separate RESULT OBJECTS, e.g.,
#       results <- calculate(calc).
#     This doesn't work here, because it is impossible to update the calc object
#     if the only thing returned is the result.  (The result could contain an
#     updated calc object, but it would require great discipline on the part of
#     the caller to remember to pull this out and use it always.)  For that
#     reason, we return a calculator that contains the result.
#
#  5) Because the computation can be very expensive, and because we sometimes
#     need to reset values before computation (e.g., cases and parameters) we
#     DELAY COMPUTATION and require an explicit call ("calculate()") to trigger
#     it.  This is driven partly by the flow in the distributed computation,
#     in which the parameter and case values are set separately and with
#     different frequencies (one parameter reset for many case resets).
#
#  Finally, with the possibility that multiple R objects may share the same
#  C++ object comes the possibility that they will attempt to free or
#  FINALIZE/destroy it MULTIPLE TIMES.  Without reference-based R objects,
#  responsibility for avoiding that must rest with the client.  (With references
#  one could save an indicator for the state of the C++ object in the R object,
#  and refuse to finalize twice.)
##################################################################################


############## Class Definition
setClass("mspathCalculator", representation(args="list",
                                            manager="externalptr",
                                            do.what="integer",
                                            subset="integer",
                                            results="numeric"))
         # args are the C arguments.  It is essential they be in the same order
         # as expected by the inner C function call.
         # manager is an opaque object used by C
         # do.what is the code for the type of computation to perform
         # subset is the id's of cases being analyzed.  If length is 0, do all
         # results of the last computation; empty vector if invalid


setClassUnion("mspathAbstractCalculator", "mspathCalculator")

############### Constructors

mspathCalculator <- function(do.what, params, allinits, misc, subject, time, state, qvector, evector, covvec,
                    constrvec, misccovvec, miscconstrvec, baseconstrvec, basemiscconstrvec, 
                    pathvars, pathoffset, pathconstrvec,
                    initprobs, nstates, nintens, nintenseffs, nmisc, nmisceffs, nobs, npts,
                    ncovs, ncoveffs, nmisccovs, nmisccoveffs,
                    npatheffs,
                    isexact, fixedpars, stepnumerator, stepdenominator)
  {
      # the actual parameter values will be set later; we just need their size here
      p <- length(params)  
      state <- state - 1  # In R, work with states 1, ... n. In C, work with states 0, ... n-1

      npars <- nintenseffs + ncoveffs + nmisceffs + nmisccoveffs + npatheffs
      npath <- length(pathvars)
      notfixed <- setdiff(1:npars, fixedpars)
      if (!is.null(fixedpars))  {
        fixedpars <- fixedpars - 1
        nfix <- length(fixedpars)
      }
      else {
        fixedpars <- -1
        nfix <- 0
      }
      if (!misc) {
          evector <- misccovvec <- miscconstrvec <- NULL
          nms <- 0
      }
      else {
          ematrix <- t(matrix(evector, nrow=nstates))
          diag(ematrix) <- 0
          esum <- apply(ematrix, 1, sum)
          nms <- max ( seq(along=esum)[esum > 0])
      }

      # we don't use path effects on misclassification probability
      pathmiscconstrvec <- NULL
      npathmisceffs <- 0
      args <- list(
                params = as.double(params),
                allinits = as.double(allinits),
                misc = as.integer(misc),
                p = as.integer(p),
                subject = as.integer(subject),
                time = as.double(time),
                state = as.integer(state),
                qvector = as.integer(qvector),
                evector = as.integer(evector),
                covvec = as.double(covvec),
                constrvec = as.integer(constrvec),
                misccovvec = as.double(misccovvec),
                miscconstrvec = as.integer(miscconstrvec),
                baseconstrvec = as.integer(baseconstrvec),
                basemiscconstrvec = as.integer(basemiscconstrvec),
                pathvars = as.character(pathvars),
                pathoffset = as.double(pathoffset),
                pathconstrvec = as.integer(pathconstrvec),
                pathmiscconstrvec = as.integer(pathmiscconstrvec),
                initprobs = as.double(initprobs),
                nstates = as.integer(nstates),
                nms = as.integer(nms),
                nintens = as.integer(nintens),
                nintenseffs = as.integer(nintenseffs),
                nmisc = as.integer(nmisc),
                nmisceffs = as.integer(nmisceffs),
                nobs = as.integer(nobs),
                npts = as.integer(npts),
                ncovs = as.integer(ncovs),
                ncoveffs = as.integer(ncoveffs),
                nmisccovs = as.integer(nmisccovs),
                nmisccoveffs = as.integer(nmisccoveffs),
                npath = as.integer(npath),
                npatheffs = as.integer(npatheffs),
                npathmisceffs = as.integer(npathmisceffs),
                isexact = as.integer(isexact),
                nfix = as.integer(nfix),
                fixedpars = as.integer(fixedpars),
                stepnumerator = as.integer(stepnumerator),
                stepdenominator = as.integer(stepdenominator),
                PACKAGE="mspath"
                )
      mgr <- do.call(".Call", c("makeManager", args))
      new("mspathCalculator",
          args = args,
          do.what = as.integer(do.what),
          manager = mgr,
          subset = integer(0)
          )
    }

# primarily for use of distributed calculator
mspathCalculatorFromArgs <- function(args, do.what=1) {
  mgr <- do.call(".Call", c("makeManager", args))
  new("mspathCalculator",
      args = args,
      do.what = as.integer(do.what),
      manager = mgr,
      subset = integer(0)
      )
}

################## Setup for Calculation

# set parameters
# params<- generic definition stripped out and put in allGenerics.R.
setMethod("params<-",
          signature(calc = "mspathCalculator", value = "numeric"),
          function(calc, value) {
            calc@results <- numeric()
            #  print("Setting params in regular calculator")
            calc@args$params <- as.double(value)
            calc@args$name <- "setParams"
            do.call(".Call", c("setParams", calc@manager, calc@args))
            calc
          })



# Set the ID's of cases to analyze
# value == empty vector -> analyze all cases
# activeCases<- generic definition stripped out and put in allGenerics.R.
setMethod("activeCases<-",
          signature(calc = "mspathCalculator", value = "numeric"),
          function(calc, value) {
            calc@results <- numeric()
            if (length(value) == 0) {
              calc@subset <- integer(0)
              .Call("selectAll", calc@manager, PACKAGE=calc@args$PACKAGE)
            }
            else {
              calc@subset <- sort(as.integer(value))
              # the C requires the  values to be sorted
              .Call("selectSubset", calc@manager, calc@subset, PACKAGE=calc@args$PACKAGE)
            }
            calc
          })



################ Calculation

# perform the computation
# updates calc, including storing results in it
# the optional params, activeCases, and do.what set the appropriate values
# before computation.  These values will persist for later
# computations unless explicitly reset.
# calculate generic definition stripped out and put in allGenerics.R.
setMethod("calculate",
          signature(calc = "mspathCalculator"),
          function(calc, params, activeCases, do.what) {
            if (!missing(params))
              params(calc) <- params
            if (!missing(activeCases))
              activeCases(calc) <- as.integer(activeCases)
            if (!missing(do.what)) {
              calc@do.what <- as.integer(do.what)
              calc@args$do.what <- as.integer(do.what)
            }
            results <- .Call("compute", calc@manager, calc@do.what, PACKAGE=calc@args$PACKAGE)
            calc@results <- results
            calc
          })

# returns a matrix with counts in the columns
# steps may be fractional if tree has variable depth
# One row for each case in computer (not just activeCases)
# estimateWork generic definition stripped out and put in allGenerics.R.
setMethod("estimateWork",
          signature(calc = "mspathCalculator"),
          function(calc){
            oldcases <- activeCases(calc)
            calc@args$do.what <- as.integer(0)
            s <- sapply(unique(calc@args$subject), function(id) {
              calc <- calculate(calc, activeCases=id)
              x <- calc@results
              c(id, x[4], x[3], x[5], x[6], x[6]/x[3])
            })
            s <- t(s)
            dimnames(s) <- list(rep("", dim(s)[1]),
                                c("ID", "Good Nodes", "Good Paths", "Bad Nodes",
                                  "Good Path Nodes", "Steps"))
            # next step is essential for C and R states to be consistent
            # for the caller.  The caller will have the old
            # R states, so we must ensure that the C state matches it.
            activeCases(calc) <- oldcases
            s
          })

# return simulation result
# simulateObservations generic definition stripped out and put in allGenerics.R.
setMethod("simulateObservations",
          signature(object="mspathCalculator"),
          function(object) {
            .Call("simulate", object@manager, PACKAGE=object@args$PACKAGE)
          })

################ Analysis

# ID's of cases to analyze
# return empty list if it is all of them
# For callers it might make more sense to return calc@args$subject,
# but that would make it hard to restore the state.
# See estimateWork for an example.
# activeCases generic definition stripped out and put in allGenerics.R.
setMethod("activeCases",
          signature(calc = "mspathCalculator"),
          function(calc) calc@subset)

# best estimate of likely effort, a number
# effort generic definition stripped out and put in allGenerics.R.
setMethod("effort",
          signature(calc = "mspathCalculator"),
          function(calc) {
            calc@results[4]  # number of unique good nodes
          })


# -2 * log likelihood
setMethod("minus2loglik",
          signature(x = "mspathCalculator"),
          function(x) {
            x@results[1]
          })

# subjects under analysis
# nActiveCases generic definition stripped out and put in allGenerics.R.
setMethod("nActiveCases",
          signature(calc = "mspathCalculator"),
          function(calc) {
            n <- length(calc@subset)
            if (n == 0)
              n <- nCases(calc)
            n
          })

# total subjects in study
# same value should be in results[2]
setMethod("nCases",
          signature(x = "mspathCalculator"),
          function(x) x@args$npts)

# Various Path Iteration Counts
setMethod("nGoodPaths",
          signature(x = "mspathCalculator"),
          function(x) {
            x@results[3]
          })
setMethod("nGoodNodes",
          signature(x = "mspathCalculator"),
          function(x) {
            x@results[4]
          })
setMethod("nBadNodes",
          signature(x = "mspathCalculator"),
          function(x) {
            x@results[5]
          })
setMethod("nGoodPathNodes",
          signature(x = "mspathCalculator"),
          function(x) {
            x@results[6]
          })

# vector of results
# results generic definition stripped out and put in allGenerics.R.
setMethod("results",
          signature(calc = "mspathCalculator"),
          function(calc)
          calc@results)



################# Destructors/Cleanup

# all done. clean up
# done generic definition stripped out and put in allGenerics.R.
setMethod("done",
          signature(calc = "mspathCalculator"),
          # nothing to do for this class
          function(calc) calc)

setMethod("batchsmoosh<-",
          signature(x="mspathCalculator", value="mspathCalculator"),
          function(x, value) {
            # mostly toss stuff
            x@args <- list()
            #x@manager <- NULL not legal
            batchsmoosh(x@do.what) <- value@do.what
            x@subset <- integer()
            # likely different, and needed
            batchsmoosh(x@results) <- value@results
            x
          })
