##### Distributed processing with Rmpi, which is required
# mpi.isend only in 0.5.1

## You may need to modify the next line to help it find the library

getlib <- Quote(require("Rmpi"))

### Some constants for MPI

# Tags
requestCode <- 1  # requests for work
timeRequestCode <- 4   # request work and timing
resultCode <- 2   # has a result
doneCode <- 3     # no more work to do



# Types
integerType <- 1
doubleType <- 2

# Misc
root <- 0  # rank of master


### mspathDistributedCalculator class
setClass("mspathDistributedCalculator", representation("mspathCalculator", comm="integer",

                                                       work="matrix", worki="integer",
                                                       analyzer="ANY", mode="integer",
                                                       cuts="numeric", maxlen="integer",
                                                       cutids="integer", evals="integer"))
   # comm is the communicator to use.
   # work holds estimates of work effort for each case
   # See estimateWork(mspathCalculator) for the format.
   # worki holds indices of rows in work, sorted in descending order of work
   # analyzer holds a runAnalyzer, but is initially NULL
   # mode is requestCode or timeRequestCode, depending on whether we're profiling
   # cuts show how to cut of the work into jobs.  Indices into worki
   # maxlen is longest list of ID's that will be sent
   # cutids are ID's of first case in each group
   # evals is number of evaluations

# use this to create a function that, given the standard args
# (i.e., without comm),
# makes an appropriate distributed calculator object.
mspathDistributedCalculatorFactory <- function(comm=0, profile=FALSE) {
  # easiest thing is to change the default values
  f <- mspathDistributedCalculator
  formals(f)$comm <- comm
  formals(f)$profile <- profile
  f
}

mspathDistributedCalculator <- 
function(do.what, params, allinits, misc, subject, time, state, qvector, evector, covvec,
                    constrvec, misccovvec, miscconstrvec, baseconstrvec, basemiscconstrvec, 
                    pathvars, pathoffset, pathconstrvec,
                    initprobs, nstates, nintens, nintenseffs, nmisc, nmisceffs, nobs, npts,
                    ncovs, ncoveffs, nmisccovs, nmisccoveffs,
                    npatheffs,
                    isexact, fixedpars, stepnumerator, stepdenominator, comm=0, profile=FALSE)
  { eval(getlib)
    calc <- new("mspathDistributedCalculator",
                comm=as.integer(comm),
                mspathCalculator(do.what, params, allinits, misc, subject, time, state, qvector, evector, covvec,
                    constrvec, misccovvec, miscconstrvec, baseconstrvec, basemiscconstrvec, 
                    pathvars, pathoffset, pathconstrvec,
                    initprobs, nstates, nintens, nintenseffs, nmisc, nmisceffs, nobs, npts,
                    ncovs, ncoveffs, nmisccovs, nmisccoveffs,
                    npatheffs,
                    isexact, fixedpars, stepnumerator, stepdenominator),
                mode=as.integer(requestCode),
                evals=as.integer(0))
    args <- calc@args
#print("Broadcasting arguments for distributed calc")
    mpi.bcast.Robj(args, comm=comm) # I am master

    if (profile) {
      calc@mode <- as.integer(timeRequestCode)
    }
    w <- estimateWork(as(calc, "mspathCalculator")) # do not distribute work calculation
    colnames(w) <- gsub(" ", "", colnames(w))
    w <- cbind(w, ncases=1)
    calc@work <- w
    workType <- "BadNodes"  # use for first cut estimate of work effort
    calc <- optimizeWork(calc, w[,workType], chunks=10)
    calc@analyzer <- runAnalyzer(calc@work)
    
    # let everyone get max size receive buffer
    #was mpi.bcast(as.integer(maxlen), integerType, comm=comm)
    # it may change, so be very conservative
    mpi.bcast(as.integer(nCases(calc)), integerType, comm=comm)
    calc
  }

    # chop up work
# optimizeWork generic definition stripped out and put in allGenerics.R.
setMethod("optimizeWork",
          signature(calc = "mspathCalculator", effort = "numeric", chunks= "numeric"),
          function(calc, effort, chunks=10) {
                                        # effort is a vector of estimated work per case
                                        # try for chunks of of work per CPU
            wi <- as.integer(sort(effort, index.return=TRUE, decreasing=TRUE)$ix)
            calc@worki <- wi
            cuts <- fixedSchedule(effort[wi],( mpi.comm.size(calc@comm)-1)*chunks)
            calc@cuts <- cuts
            ncuts <- length(cuts)
                                        # the schedule is such that the last entries will always have the most cases
            cutids <- 0  # will be id's, now indices in worki
            if (ncuts>1) {
              maxlen <- cuts[ncuts]-cuts[ncuts-1]
              cutids <- c(cutids, cuts[1:(ncuts-1)])
            } else {
              maxlen <- cuts[ncuts]
            }
            calc@maxlen <- maxlen
                                        # pre-cache identifier for each job
            calc@cutids <- as.integer(calc@work[wi[cutids+1] , "ID"])
            calc
          })

# use actual times of run optimize assignment
# useActualTimes generic definition stripped out and put in allGenerics.R.
setMethod("useActualTimes",
          signature(calc = "mspathCalculator", chunks = "numeric"),
          function(calc, chunks=10) {
            results <- data.frame(smoosh(calc@analyzer))
            fit <- lm(cpu~GoodNodes+GoodPaths+BadNodes+GoodPathNodes+ncases+Steps, data=results)
            fit$coefficients[1] <- 0 # blank the intercept
                                        # I tried copying the Steps coefficient to the intercept and deleting
                                        # the former, since the covariate is always 1 below.
                                        # But that didn't work, because other parts of fit had the old setup.
            p <- predict(fit, data.frame(calc@work))
                                        # recode negative values, if any, to smallest positive one
            neg <- (p <= 0)
            p[neg] <- min(p[!neg])
            optimizeWork(calc, p, chunks)
          })

setMethod("params<-", signature=c("mspathDistributedCalculator", "numeric"),
          function (calc, value) {
            calc <- callNextMethod()
            # The preceding call does as.double, which we need,
            # even though root doesn't compute individual likelihoods.
            mpi.bcast.Robj(calc@args$params, comm=calc@comm)
            calc
          }
          )


# central computation/distribution
# This would be invoked on the master process.
# Hmm... I wonder if the params are ever distributed if they aren't set here....
setMethod("calculate",
          signature(calc = "mspathDistributedCalculator", params="numeric",
                    # I am not set up to distribute cases (which presumably
                    # would then use different parameter sets).
                    # I always do a full likelihood calculation.
                    activeCases="missing", do.what="missing"),
          function(calc, params, activeCases) {
            if (! missing(params))
              params(calc) <- params
            sum <- 0
            active <- rep(TRUE, mpi.comm.size(calc@comm)-1) # True for each active slave
            #cat("active entries: ", length(active),"\n")
            starts <- list()  # holds runTime's

            i <- 0  # index of last cut handled
            lasti <- 0 # last index in worki used
            result <- double(2)
            remote <- double(4)

            # service requests for work
            while (any(active)) {
              #print("waiting for request")
              result <- mpi.recv(result, doubleType, mpi.any.source(), mpi.any.tag(), comm=calc@comm)
              info <- mpi.get.sourcetag()
              source <- info[1]
              tag <- info[2]
              #cat("Got request tag ", tag, "from", source, "\n")
              if (tag==requestCode) {
                if (i< length(calc@cuts)) {
                  # work remains
                  i <- i+1
                  currenti <- calc@cuts[i]
                  ids <- as.integer(calc@work[calc@worki[(lasti+1):currenti], "ID"])
                  lasti <- currenti
                  if (calc@mode == timeRequestCode) {
                    starts[[i]] <- runTime(ids)
                  }

#                  mpi.isend(id, integerType, source, requestCode, comm=calc@comm)
                  # the variable "source" gives the destination below
                  # it was the source of the earlier transmission.
                  mpi.send(as.integer(ids), integerType, source, calc@mode, comm=calc@comm)
                  #cat("sent item", i, ":", ids,"\n")
                } else {
                  # all done; let slave know
#                  mpi.isend(as.integer(0), integerType, source, doneCode, comm=calc@comm)
                  mpi.send(as.integer(0), integerType, source, doneCode, comm=calc@comm)
                }

              } else if (tag==resultCode) {
                #cat("sum =", sum, "   result=", result, "\n")
                sum <- sum+result[2]
                if (calc@mode == timeRequestCode){
                  id <- as.integer(result[1])
                  remote <- mpi.recv(remote, doubleType, source, resultCode, comm=calc@comm)
                  iold <- match(id, calc@cutids)
                  runTime <- starts[[iold]]
                  remoteTime(runTime) <- remote
                  mpirank(runTime) <- source
                  calc@analyzer <- addResult(calc@analyzer, runTime)
                }
              } else if (tag==doneCode) {
#                cat("Deactivating", source)
                active[source] <- FALSE
#                cat("Something still active? ", any(active))
              } else
                stop(paste("Unknown tag ", tag))
            }
                
            calc@results[1] <- sum
            if (calc@mode == timeRequestCode) {
              calc <- useActualTimes(calc)
              if (calc@evals > 0)
                # time first and second pass
                # since 2nd is better optimized
                # and has less overhead
                calc@mode <- as.integer(requestCode)
              else
                calc@analyzer <- newRun(calc@analyzer)
            }
            calc@evals <- calc@evals+as.integer(1)
            calc
          })

setMethod("done", signature=c("mspathDistributedCalculator"),
          function(calc) {
            # slaves are waiting for more parameters to be broadcast
            mpi.bcast.Robj(NULL, comm=calc@comm)
            # if I were good, I'd wait for confirming messages
            callNextMethod()
          })


### execute this on each slave process
slave <- function(comm=0){
  # Implementation Note: I don't wait on any of the non-blocking requests because
  # the blocking receives are sufficient to assure syncronization.
  eval(getlib)
  request <- 0

  print("Started Slave")
  repeat {
    args <- mpi.bcast.Robj(comm=comm)
#    cat("Received args ")
    if (is.null(args))
        break
    # a naive call to mspathCalculator will subtract 2 from state
    calc <- mspathCalculatorFromArgs(args)
    maxlen <- as.integer(nCases(calc)) # mostly so I have a buffer
        # also holds worst case buffer size
    maxlen <- mpi.bcast(maxlen, integerType, comm=comm) # I receive
    cases <- integer(maxlen)  # will receive cases to be evaluated
  # decided to look into buffer behavior
    repeat {
      guess <- mpi.bcast.Robj(comm=comm)
#      cat("Received parameters ")
#      print(guess)
      if (is.null(guess))
        break
      params(calc) <- guess
                                        # now get ready for individual requests
      repeat {
                                        # ask for work
#        mpi.isend(as.double(0), doubleType, root, requestCode, comm=comm, request=request)
        mpi.send(as.double(0), doubleType, root, requestCode, comm=comm)
        request <- request+1
        cases <- mpi.recv(cases, integerType, root, mpi.any.tag(), comm=comm)
        goodCases <- cases[seq(mpi.get.count(integerType))]
#        cat("R::slave received cases ", goodCases, "\n")
        info <- mpi.get.sourcetag()
        tag <- info[2]
        if (tag == doneCode) {
#          mpi.isend(as.double(0), doubleType, root, doneCode, comm=comm, request)
          mpi.send(as.double(0), doubleType, root, doneCode, comm=comm)
          request <- request+1
          break
        }
                                        # evaluate case and return result
        if (tag == timeRequestCode)
          t0 <- proc.time()
        calc <- calculate(calc, activeCases=goodCases)
        if (tag == timeRequestCode)
          t1 <- proc.time()
#        cat("R::slave sending likelihood ", minus2loglik(calc), "\n")
#        mpi.isend(as.double(minus2loglik(calc)), doubleType, root, resultCode,
#                  comm=comm, request=request)
        mpi.send(as.double(c(goodCases[1], minus2loglik(calc))), doubleType, root, resultCode,
                  comm=comm)
        request <- request+1
        if (tag == timeRequestCode) {
          t2 <- proc.time()[3]  # elapsed time
          td <- t1-t0
          # send user cpu, sys cpu, wallclock elapsed, wallclock wait all in seconds
          mpi.send(as.double(c(td[1:3], t2-t1[3])), doubleType, root, resultCode,
                   comm=comm)
        }
      }
#     print("No more cases.  Breaking to parameter reception")
    }
    print("Null parameters.  Destroying calculator")
    calc <- done(calc)
  }
  print("Null args.  Shutting down and exiting")
  mpi.quit()
}

# return indices to cut input work into roughly equal chunks
fixedSchedule <- function(work, ngroups) {
  # work is a vector of work effort, sorted in descending order
  # ngroups is the desired number of work chunks
  #   If some of the individual work items are big, you may
  #   get less than ngroups back.
  #
  # returns indices in work to divide it up
  #  These are the right-hand endpoints
  #  So where[1] is the index of the last work item
  #  to include in the first group 
  tot <- sum(work)
  cut <- tot/ngroups
  t <- 0  # running sum of work
  where <- c()  # output cutpoints
  for (i in seq(length(work)-1)) {
    t <- t+work[i]
    if (t >= cut) {
      where <- append(where, i)
      t <- 0
    }
  }
  where <- append(where, length(work))
  where
 }
 


# execute this function on the master node
# The code that is run should set up a distributed calculator and then use it.
# Don't forget to call done.
master <- function(channel="kickStart.R", comm=0){
  source(channel)
  alldone(comm)
}

# call just before leaving master
alldone <- function(comm=0) {
  # slaves  should be waiting for args for calculator construction
  mpi.bcast.Robj(NULL, comm=comm)
  mpi.exit()
}


# this provides an easy way to write top level scripts
runeverywhere <- function(channel="kickStart.R", comm=0){
  eval(getlib)
  if (mpi.comm.rank(comm=comm) == root)
    master(channel, comm)
  else
    slave(comm)
}
