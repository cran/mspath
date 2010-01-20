\name{fixedSchedule}
\alias{fixedSchedule}
\title{fixedSchedule }
\description{
 Return indices to cut input work into roughly equal chunks
}
\usage{
fixedSchedule(work, ngroups)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{work}{\code{vector} of work effort, sorted in descending order}
  
  \item{ngroups}{ desired number of work chunks.
    If some of the individual work items are big, you may
    get less than \code{ngroups} back.}
}
\details{Does simple fixed scheduling, dividing up cases into batches
  with roughly equal work effort.  Dynamic scheduling would adjust to
  the run-time conditions; since we have uniform slave processors and
  expect run-time to be fairly repeatable, fixed scheduling should
  suffice.

  However, this may initially be used with a proxy of actual run time in
  \code{work}; in that case, it may help to collect the actual run-times
  and repartition the work.
}
\value{
The indices in work to divide it up.
These are the right-hand endpoints, so [1] is the index of the last work
item to include in the first group.
}

\author{Ross Boylan}
\keyword{utilities}
\keyword{iteration}
\concept{distributed}
