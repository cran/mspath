# General facilities to do repeated trials of an analysis
# Runs until stopped by a flag file.
# Intended use is to check sensitivity of estimates.

# Ross Boylan ross@biostat.ucsf.edu
# 01-Oct-2007
# (C) 2007 Regents of University of California
# Distributed under the Gnu Public License v2 or later at your option

### Holds all key information for a trial
### args is likely to be more compact than the full arguments to the
### inner function being exercised.
setClass("trial",
         representation(args="list", result="ANY"))
trial <- function(args, result)
  new("trial", args=args, result=result)


### Carry out trials until told to stop.  Return list of <trial>'s,
### also writing them to disk.  Performs checkpointing and progress reporting.
doTrials <- function(generator,
                     executor,
                     stopFileName="cancel",
                     resultsFile="trials.RData",
                     nTrials = 10,
                     nTime = 60*15,
                     ... ){
  # generator is a function taking the optional arguments ... and returning
  # a named list of arguments for executor.
  #
  # executor is a function which takes arguments produced by generator
  # and performs what ever operation you are testing.  It returns an object
  # containing whatever results you want to preserve.
  #
  # doTrials runs until the file stopFileName appears in its working directory.
  # Once the file appears, and the current run finishes, it writes list of trials
  # objects to resultsFile and returns that value.
  #
  # Progress reporting and checkpointing occur after every nTrials trials or the passage
  # of nTime seconds.  doTrials writes a message to the terminal and writes the results
  # so far to (alternately) resultsFile with a .0 or .1 suffix.
  
  trials = NULL  # will be list of results
  ckpt.lastSave <- 1 # alternate 0/1 for file to write to
  ckpt.lastTime <- Sys.time()  # last time saved
  ckptName <- function(n) paste(resultsFile, n, sep=".")
  while(!file.exists(stopFileName)){
    myargs <- generator(...)
    if (is.null(myargs))
        break
    myresult <- do.call(executor, myargs)
    if (is.null(trials))
      trials <- list(trial(myargs, myresult))
    else
      trials <- c(trials, list(trial(myargs, myresult)))
    n <- length(trials)
    now <- Sys.time()
    # write to disk and report progress
    if (n%%nTrials == 0 || difftime(now, ckpt.lastTime, units="secs") > nTime) {
      ckpt.lastSave <- 1 - ckpt.lastSave
      save(trials, file=ckptName(ckpt.lastSave))
      ckpt.lastTime <- now
      cat(n, "trials at", format(now), "\n")
    }
  }
                                        # remove older checkpoint file first in case they are big
  file.remove(ckptName(1 - ckpt.lastSave))
  save(trials, file=resultsFile)
  file.remove(ckptName(ckpt.lastSave))
  
  trials
}
