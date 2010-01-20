\name{mspath.subset}
\alias{mspath.subset}

\title{subset a dataset based on estimated work}
\description{
  Produces a subset of a dataframe or matrix drawn from cases that will
  be quick to analyze.  Ordinarily, there will be repeated observations
  per case. 
}
\usage{
mspath.subset(data, lop=.3, id="id", time="time", nCases = 50)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{The dataframe or matrix to be subsetted.  Items should be
    sorted by id and then time.}
  \item{lop}{This fraction of cases will be dropped from eligibility for
    the subset.  These are the cases with the highest amount of
    estimated work.}
  \item{id}{The character name of the column with the case identifier.}
  \item{time}{The function assumes that computation will be an
    increasing function of total time a case covers, as indicated by
    this column.  The difference between the first and last time is the
    total case time.}
  \item{nCases}{ Approximate number of cases to produce in the subset.
    The actual number may differ slightly.  Note this is the number of
    cases, not the number of records.}
}
\details{
Case selection is deterministic, so that repeated calls will generate
the same data.
}
\value{
  \code{data} subsetted
}
\author{Ross Boylan}
\note{This function is mostly for internal testing, though you may find
  it useful in other contexts.  In situations in which evaluation can be
  very slow or memory intensive, and is strongly dependent on some
  feature of the data, analysis of a subset can be a lot easier.

  A case with only one record will be assumed to take no time, since
  first and last time are identical.

  Actual computation effort need not be linear in time; the subsetting
  is sensible for any monotonic increasing function of time.  Time need
  not be a perfect predictor of effort for the subsetting to be useful.
}
\examples{
  data("sim3")
  short <- mspath.subset(sim3)
}
\keyword{ manip }
\keyword{ datagen }
