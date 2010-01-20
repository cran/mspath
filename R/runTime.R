# Some utilities for analyzing run-time performance

setClass("runTime", representation(job="ANY", start="numeric", end="numeric",
                                   remote="numeric", rank="numeric"))
         # start = wall clock when job started
         # end = wall clock when ended
         # remote = stats from remote processor
         #   1 = user cpu
         #   2 = system cpu
         #   3 = wall clock time of main job
         #   4 = wall clock waiting for root
         # rank = rank of remote process


runTime <- function(job)
  new("runTime", job=job, start=proc.time()[3])

# job generic definition stripped out and put in allGenerics.R.
setMethod("job", "runTime",
          function(runTime) runTime@job)

# remoteTime<- generic definition stripped out and put in allGenerics.R.
setMethod("remoteTime<-",
          signature(runTime="runTime", value="numeric"),
          function(runTime, value) {
            runTime@end <- proc.time()[3]
            runTime@remote <- value
            runTime
          })

# rank clashes with name of already defined, unrelated function
# in base
# mpirank<- generic definition stripped out and put in allGenerics.R.
setMethod("mpirank<-" ,
          signature(runTime="runTime", value="numeric"),
          function(runTime, value) {
            runTime@rank <- value
            runTime
          })

# startTime generic definition stripped out and put in allGenerics.R.
setMethod("startTime",
          signature(runTime="runTime"),
          function(runTime) runTime@start)

# endTime generic definition stripped out and put in allGenerics.R.
setMethod("endTime",
          signature(runTime="runTime"),
          function(runTime) runTime@end)

# wallTime generic definition stripped out and put in allGenerics.R.
setMethod("wallTime",
          signature(runTime="runTime"),
          function(runTime) runTime@end - runTime@start)

# waitTime generic definition stripped out and put in allGenerics.R.
setMethod("waitTime",
          signature(runTime="runTime"),
          function(runTime) runTime@remote[4])

# cpuTime generic definition stripped out and put in allGenerics.R.
setMethod("cpuTime",
          signature(runTime="runTime"),
          function(runTime) sum(runTime@remote[1:2]))

# mpirank generic definition stripped out and put in allGenerics.R.
setMethod("mpirank",
          signature(runTime="runTime"),
          function(runTime) runTime@rank)


# this works only with some runTime's
setClass("runAnalyzer", representation(estimate="ANY", actual="list", prior="list"))

runAnalyzer <- function(estimate)
  new("runAnalyzer", estimate=estimate, prior=list())

# addResult generic definition stripped out and put in allGenerics.R.
setMethod("addResult",
          signature(analyzer="runAnalyzer", runTime="runTime"),
          function(analyzer, runTime){
            analyzer@actual <- c(analyzer@actual, list(runTime))
            analyzer
          })

# prepare for now round of actual results
# newRun generic definition stripped out and put in allGenerics.R.
setMethod("newRun",
          signature(analyzer="runAnalyzer"),
          function(analyzer) {
            n <- length(analyzer@prior)+1
            analyzer@prior[[n]] <- analyzer@actual
            analyzer@actual <- list()
            analyzer
          })

# produce table ready for analysis

# smoosh generic definition stripped out and put in allGenerics.R.
setMethod("smoosh",
          signature(analyzer="runAnalyzer"),
          function(analyzer){
            x <- sapply(analyzer@actual, function(runTime) {
              cases <- runTime@job
              r <- apply(analyzer@estimate[analyzer@estimate[,'ID'] %in% cases,
                                           ,drop=FALSE],
                         2, sum)
              r['ID'] <- cases[1]
              c( r, cpuTime(runTime), wallTime(runTime), waitTime(runTime),
                startTime(runTime), endTime(runTime), mpirank(runTime))
            })
            x <- t(x)
            colnames(x) <- c(gsub(" ", "", colnames(analyzer@estimate)),
                             "cpu", "wall", "wait", "start", "end", "rank")
            x
          })
