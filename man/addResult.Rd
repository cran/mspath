\name{addResult}
\alias{addResult}
\title{Add a Job Run's Results to the Analyzer}
\description{
  Once you have timing information on a job, use this function to add
  the results to the analyzer.
}
\usage{
addResult(analyzer, runTime)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{analyzer}{An  \code{\linkS4class{runAnalyzer}} }
  \item{runTime}{A \code{\linkS4class{runTime}} with the job and how
    long it took} 
}
\value{The \code{analyzer}, suitably updated.  You \emph{must} capture
  this value for the new data to be permanent.}
\author{Ross Boylan}
\seealso{\code{\linkS4class{runAnalyzer}} provides fuller discussion and
  example.}
\keyword{classes}

