\name{smoosh}
\alias{smoosh}
\title{Analyze Runtime Estimates and Actual Values}
\description{
  This method combines the observed runtimes of jobs with estimates of
  same.
  The analysis is left for the caller to do.
}
\usage{
smoosh(analyzer)
}
\arguments{
  \item{analyzer}{A \code{\linkS4class{runAnalyzer}} previously populated with the
    runtimes of jobs.}
}

\value{
  
  A summary of the runs, as either a \code{link{matrix}} or
  \code{\link{data.frame}}.  Each row corresponds to a single job; the
  \sQuote{ID} column will have the ID of the first case in the job.  Don't be
  fooled; the rest of the row applies to the whole job, and hence to all
  the cases in the job.  The columns give the sum of the predictors
  (from the \code{estimate} given when the
  \code{\linkS4class{runAnalyzer}} was constructed) for
  each case in the job, and the various performance measures from the
  \code{\linkS4class{runTime}} for the job, labelled
  appropriately: cpu, wall, wait, start, end, rank.  See
  \linkS4class{runTime} for their meaning.
}
\author{Ross Boylan}
\seealso{\code{\linkS4class{runAnalyzer}}, \code{\linkS4class{runTime}}}
\keyword{classes}
\keyword{utilities}
\keyword{datagen}

