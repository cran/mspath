\name{runAnalyzer-class}
\docType{class}
\alias{runAnalyzer-class}
\alias{runAnalyzer}
\alias{newRun}
\alias{newRun,runAnalyzer-method}
% smoosh is fully documented in a separate file
\alias{smoosh,runAnalyzer-method}


\title{Class to Analyze Runtimes}
\description{
  Assists the analysis of the factors affecting the performance of
  jobs.  It requires individual jobs to be identified by the ID's of the
  cases the job computed.
}

\section{Objects from the Class}{
  \code{\link{runAnalyzer}(estimate)} creates one of these objects.  The
  \code{estimate} should contain estimates of the runtimes of the jobs
  that will later be added.
}

\section{Slots}{
  \describe{
    \item{\code{estimate}:}{A \code{"matrix"} or \code{"data.frame"} with
      one row for each case to be analyzed.  There must be a column
      named \sQuote{ID} whose values match those identifying a job in
      the \code{\linkS4class{runTime}}s.  Other columns contain possible
      predictors of how much computation the case will require.}
    \item{\code{actual}:}{An initially empty \code{"list"} that will
      hold the \code{\linkS4class{runTime}}'s of the jobs.}
    \item{\code{prior}:}{Only relevant if you conduct more than one
      round of analysis, i.e., call \code{newRun}.  In that case it will
      be a \code{list} of \code{list}'s.  The first entry holds the
      values in \code{actual} for the first run, the second entry holds
      the second run, and so on.}
  }
}
\section{Methods}{
  \code{\link{addResult}}{
    \code{signature(analyzer = "runAnalyzer", runTime = "runTime")}:
    to add a job and its time.
    \describe{
    \item{newRun}{\code{signature(analyzer = "runAnalyzer")}:
      Return the \code{analyzer} argument, ready for a new round of analysis. }
    \item{smoosh}{\code{signature(analyzer = "runAnalyzer")}: ... }
    \code{\link{smoosh}}}{\code{signature(analyzer = "runAnalyzer")}:
    returns a \code{data.frame} or \code{matrix} summarizing the
    results.}}
}

\section{Background}{
  This class assists in the following general setting.  You have
  computations to perform on cases; each job may calculate values for
  one or more cases, e.g., you may distribute the calculation of a
  likelihood.

  You also have information that you think will predict how long the
  cases will take to compute.

  You divide the cases into jobs, perform the computations, and then
  want to see how well your predictors explain various measures of
  performance.

  Based on this analysis, or perhaps randomly, you may partition the
  cases in a different way and try again.

  This class collects the results of all the runs (including prior
  rounds if you choose to do more than one round), and can provide a
  summary of the performance versus the predictors.
}

\details{
  
  \code{runAnalyzer(estimate)} constructs an object of this class.
  \code{estimate} should be a \code{dataframe} or \code{matrix} with
  each row giving information for a single case.  The \sQuote{ID}
  column, which must exist, is the ID of the case.  Other columns give
  other values believed to predict how long the computation on the case
  will take. It should be sensible to sum a predictor across cases to
  predict how long the cases will take.

  A \code{\linkS4class{runTime}} object includes information
  about a particular job, in particular various measures of time.  To
  use the object with a \code{runAnalyzer} requires that the job be
  constructed with the list of ID's it will analyze, e.g.,
  \code{aRunTime <- runTime(c(1, 56, 90))} for a job that will evaluate
  cases 1, 56, and 90. If \code{ra} is a \code{runAnalyzer}, then add
  the results of a run with \code{ra <- addResult(ra, aRunTime)}.

  Once you are done \code{results <- smoosh(ra)} gets you a summary of
  the runs.  Each row corresponds to a single job; the \sQuote{ID}
  column will have the ID of the first case in the job.  Don't be
  fooled; the rest of the row applies to the whole job, and hence to all
  the cases in the job.  The columns give the sum of the predictors for
  each case in the job, and the various performance measures, labelled
  appropriately: cpu, wall, wait, start, end, rank.  See
  \linkS4class{runTime} for their meaning.

  \emph{If} you wish to repeat the exercise, use
  \code{ra <- newRun(ra)}.

  Currently, evaluation of the performance of the predictors is up to
  you, and if you want to look at previous runs you will need to pull
  them out with \code{ra@prior}.
}
\note{Subject to change.  Not terribly general at the moment, and rather
hackish.}
\author{Ross Boylan}
\seealso{
  \code{\link{addResult}} to populate;
  \code{\link{smoosh}} to analyze;
  \code{\linkS4class{runTime}} for internal data.
}
\examples{
   # slightly contrived, since non-distributed
   estimate <- data.frame(ID=c(30, 50), estimate=exp(c(30, 50)))
   analyzer <- runAnalyzer(estimate)
   for( x in c(30, 50) ) {
     rt <- runTime(x)
     mpirank(rt) <- 1
     tjob <- system.time(factorial(x))
     remoteTime(rt) <- c(tjob[1:3], 0) # last number is delay wait
     analyzer <- addResult(analyzer, rt)
   }
   result <- smoosh(analyzer)
}
\keyword{classes}
\keyword{utilities}
