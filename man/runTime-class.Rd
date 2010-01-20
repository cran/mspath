\name{runTime-class}
\docType{class}
% maybe also \alias{foo-methods}
% and perhaps \alias{foo-generic}
% I may have imagined the latter
\alias{runTime-class}
% \alias{addResult} is documented separately
\alias{addResult,runAnalyzer,runTime-method}
\alias{cpuTime}
\alias{cpuTime,runTime-method}
\alias{endTime}
\alias{endTime,runTime-method}
\alias{job}
\alias{job,runTime-method}
\alias{mpirank<-}
\alias{mpirank<-,runTime,numeric-method}
\alias{mpirank}
\alias{mpirank,runTime-method}
\alias{remoteTime<-}
\alias{remoteTime<-,runTime,numeric-method}
\alias{startTime}
\alias{startTime,runTime-method}
\alias{waitTime}
\alias{waitTime,runTime-method}
\alias{wallTime}
\alias{wallTime,runTime-method}

\title{Class "runTime"}
\description{	This low-level class captures the time it takes to run a
  particular job.  This is for further analysis by \code{\linkS4class{runAnalyzer}}.}
\section{Objects from the Class}{
  Create with \code{runTime(job)}.  It automatically records
  the time it was created, so don't make it until you're ready.

  The sequence of events is that the job is created locally, started
  remotely, finished remotely, and completed locally.  Scheduling and
  transmission delays may occur.
}
\section{Slots}{
	 \describe{
    \item{\code{job}:}{Some indicator of the job whose time is being
      described.  Could be the job object itself or, in current
      application, the id of the case being analyzed.
      Object of class \code{"ANY"}}
    \item{\code{start}:}{time in seconds since start of process that job
      began.
      Object of class \code{"numeric"}}
    \item{\code{end}:}{time in seconds since start of process that job
      ended (set when \code{remote<-} is called).
      Object of class \code{"numeric"}}
    \item{\code{remote}:}{seconds of user, system, wall clock, and wait
      time on the remote system.  wall clock time is between start of
      remote computation and end of main computations; wait time is
      time waiting for response from root.
      Object of class \code{"numeric"}}
    \item{\code{rank}:}{MPI rank of job.
      Object of class \code{"numeric"}}
  }
}
\section{Methods}{
  \describe{
    \item{addResult}{
      \code{signature(analyzer = "runAnalyzer", runTime = "runTime")}:
      Once a \code{runTime} object is complete, use this method to add
      it to an analyzer for later analysis.  This uses the \R idiom in
      which the return value is the updated \code{runAnalyzer} object. }
    \item{cpuTime}{\code{signature(runTime = "runTime")}: CPU seconds
      the job took.  If the job executed remotedly, this is only the
      remote CPU time.}
    \item{endTime}{\code{signature(runTime = "runTime")}:
      time in seconds since start of process that job
      ended (set when \code{remote<-} is called).
      Object of class \code{"numeric"} }
    \item{job}{\code{signature(runTime = "runTime")}:
      Get the job that goes with this \code{runTime}.}
    \item{mpirank<-}{\code{signature(runTime = "runTime", value =
	"numeric")}:
      Sets the MPI rank of the job, once it is known.  The return value
      is the \code{runTime} object with the updated rank. }
    \item{mpirank}{\code{signature(runTime = "runTime")}:
      Get the MPI rank of the job. }
    \item{remoteTime<-}{\code{signature(runTime = "runTime", value =
	"numeric")}:
      Return a \code{runTime} object updated with the vector of time
      values from the remote job.  See slot \code{remote}  for the exact
      form the argument should take.  This also sets the \code{endTime}
      of the object as a side-effect. }
    \item{startTime}{\code{signature(runTime = "runTime")}:
      time in seconds since start of process that job began.
      Object of class \code{"numeric"}. }
    \item{waitTime}{\code{signature(runTime = "runTime")}:
      Number of wall-clock seconds the remote jobs was waiting for a
      response from root after the remote job's completion. }
    \item{wallTime}{\code{signature(runTime = "runTime")}: Number of
      seconds from the time the master process initiated the job to the
      time it was finished with it (i.e. \code{endTime - startTime}). }
	 }

  
}
\author{Ross Boylan}
\note{Resolution of time is system-dependent; it's whatever
  \code{\link{proc.time}} can get.

  The creator of the object is responsible for assuring that the method
  definitions above are true.  In particular,
  \enumerate{
    \item Create the \code{runTime} object only when the corresponding
    job is started.
    \item Populate \code{mpirank} with a proper value.
    \item Set appropriate values with \code{remoteTime<-} and make this
    call exactly when all processing on the job is done.
  }
  In order to retain the updated values, you must capture the return
  value of the update methods, in keeping with the general functional
  style of \R.
}

\seealso{\code{\link{runTime}} constructor,
  \code{\linkS4class{runAnalyzer}},
  \code{\link{proc.time}}
}

\examples{
### Make a runTime object
# at the start of the job
rt <- runTime("Job1")
# in real life, remote computations would then ensue
mpirank(rt) <- 3
Sys.sleep(1.5) # time passes
# when we get the job back
# next line is c( user cpu, system cpu, wall clock time, wall clock wait)
remoteTime(rt) <- c(.05, .01, 1.0, .15)

### Use it
cat("Job", job(rt), "executed on MPI rank", mpirank(rt))
cat("It took", wallTime(rt),
    "seconds, out of which remote waited for master",
    waitTime(rt), "seconds.") 
}
\keyword{classes}
\keyword{methods}
