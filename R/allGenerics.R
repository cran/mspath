### This file contains all the generic definitions for the mspath package.
### It is cleaner to define them all here, to avoid potential sequencing
### issues that arise if they are in later files.

########## generics from checkPoint.R #############



########## generics from mspathCalculator.R #############

################## Setup for Calculation

# set parameters
setGeneric("params<-",
           function(calc, value) standardGeneric("params<-"))

# Set the ID's of cases to analyze
# value == empty vector -> analyze all cases
setGeneric("activeCases<-",
           function(calc, value) standardGeneric("activeCases<-"))

################ Calculation

# perform the computation
# updates calc, including storing results in it
# the optional params, activeCases, and do.what set the appropriate values
# before computation.  These values will persist for later
# computations unless explicitly reset.
setGeneric("calculate",
           function(calc, params, activeCases, do.what) standardGeneric("calculate"))

# returns a matrix with counts in the columns
# steps may be fractional if tree has variable depth
# One row for each case in computer (not just activeCases)
setGeneric("estimateWork",
           function(calc) standardGeneric("estimateWork"))

# return simulation result
setGeneric("simulateObservations",
           function(object) standardGeneric("simulateObservations"))

################ Analysis

# ID's of cases to analyze
# return empty list if it is all of them
# For callers it might make more sense to return calc@args$subject,
# but that would make it hard to restore the state.
# See estimateWork for an example.
setGeneric("activeCases",
           function(calc) standardGeneric("activeCases"))

# best estimate of likely effort, a number
setGeneric("effort",
           function(calc) standardGeneric("effort"))

# subjects under analysis
setGeneric("nActiveCases",
           function(calc) standardGeneric("nActiveCases"))

# vector of results
setGeneric("results",
           function(calc) standardGeneric("results"))

################# Destructors/Cleanup

# all done. clean up
setGeneric("done",
           function(calc) standardGeneric("done"))



########## generics from mspathCoefficients.R #############

setGeneric("isAllFixed",
           function(object) standardGeneric("isAllFixed"))

# Return a list of matrices of coefficients
setGeneric("matrixCoef",
                      function(x, ...) standardGeneric("matrixCoef"))

setGeneric("batchsmoosh<-",
             function(x, value) standardGeneric("batchsmoosh<-"))



########## generics from mspathDistributedCalculator.R #############

    # chop up work
setGeneric("optimizeWork",
           function(calc, effort, chunks) standardGeneric("optimizeWork"))

# use actual times of run optimize assignment
setGeneric("useActualTimes",
           function(calc, chunks) standardGeneric("useActualTimes"))



########## generics from mspath.R #############

setGeneric("printFooter", function(x, ...)
           standardGeneric("printFooter"))

setGeneric("optresults", function(x)
           standardGeneric("optresults"))

setGeneric("optresults<-", function(x, value)
           standardGeneric("optresults<-"))

# -2 * log likelihood
setGeneric("minus2loglik",
           function(x) standardGeneric("minus2loglik"))

# counts
setGeneric("nCases",
           function(x) standardGeneric("nCases"))

setGeneric("nGoodPaths",
           function(x) standardGeneric("nGoodPaths"))

setGeneric("nGoodNodes",
           function(x) standardGeneric("nGoodNodes"))

setGeneric("nBadNodes",
           function(x) standardGeneric("nBadNodes"))

setGeneric("nGoodPathNodes",
           function(x) standardGeneric("nGoodPathNodes"))



########## generics from readingError.R #############

# Return matrix with 1's on off-diagonal positive elements
setGeneric("bitMask", function(object)
           standardGeneric("bitMask"))

# return matrix with TRUE on off-diagonal positive elements
setGeneric("boolMask", function(object)
           standardGeneric("boolMask"))



########## generics from runTime.R #############

setGeneric("job",
           function(runTime) standardGeneric("job"))

setGeneric("remoteTime<-",
           function(runTime, value) standardGeneric("remoteTime<-"))

# rank clashes with name of already defined, unrelated function
# in base
setGeneric("mpirank<-",
           function(runTime, value) standardGeneric("mpirank<-"))

setGeneric("startTime",
           function(runTime) standardGeneric("startTime"))

setGeneric("endTime",
           function(runTime) standardGeneric("endTime"))

setGeneric("wallTime",
           function(runTime) standardGeneric("wallTime"))

setGeneric("waitTime",
           function(runTime) standardGeneric("waitTime"))

setGeneric("cpuTime",
           function(runTime) standardGeneric("cpuTime"))

setGeneric("mpirank",
           function(runTime) standardGeneric("mpirank"))

setGeneric("addResult",
           function(analyzer, runTime) standardGeneric("addResult"))

# prepare for now round of actual results
setGeneric("newRun",
           function(analyzer) standardGeneric("newRun"))

# produce table ready for analysis

setGeneric("smoosh",
           function(analyzer) standardGeneric("smoosh"))



########## generics from subset.R #############



########## generics from trial.R #############



########## generics from utils.R #############



########## generics from zzz.R #############

