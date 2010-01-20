\name{doTrials}
\alias{doTrials}

\title{ Run trials until told to stop}
\description{
Run a user-defined analysis repeatedly with user-defined sets of parameters.
Stops when it detects a particular file's existence on the disk.  Returns
results and writes them to disk; also writes interim results to disk.
}
\usage{
doTrials(generator, executor, stopFileName = "cancel", resultsFile = "trials.RData", nTrials = 10, nTime = 60 * 15, \dots)
}

\arguments{
  \item{generator}{\code{generator} is a function taking the optional arguments \dots and returning
a list (ordinarily a named list) of arguments for
  \code{executor}. \code{generator} can return \code{NULL} to make
  \code{doTrials} finish.}
  \item{executor}{\code{executor} is a function which takes arguments
  produced by \code{generator} and performs the operation you are
  testing.  It returns an object containing the results you want to
  preserve.  The arguments are preserved automatically (see below).}
  \item{stopFileName}{Before every trial check if \code{stopFileName}
  exists, and stop if it does. }
  \item{resultsFile}{When done, write the results of the trials to
  \code{resultsFile}.  Before then, this file name with .0 or .1
  appended holds results so far. }
  \item{nTrials}{After every \code{nTrials} perform checkpointing.}
  \item{nTime}{After \code{nTime} seconds have passed, perform checkpointing. }
  \item{\dots}{Optional arguments passed to \code{generator}.}
}
\details{
  Checkpointing writes the results so far to a file (named from
  \code{resultsFile} with, alternately, .0 and .1 appended) and prints a  progress
  message on the controlling terminal.  Every time either \code{nTrials}
  trials or \code{nTime} seconds have passed since the last checkpoint a
  new one will occur; whichever is hit first will control checkpointing.
}
\value{
  A \code{list} of \code{\linkS4class{trial}}'s, in the order they were
  performed.  \code{\linkS4class{trial}} holds the arguments for the
  trial (produced by \code{generator}) and the results (produced by
  \code{executor}).
  
}
%\references{ ~put references to the literature/web site here ~ }
\author{Ross Boylan \email{ross@biostat.ucsf.edu} }
\note{This function was designed to check the robustness of estimates.
  The generator can produce varying initial values, algorithmic choices,
  and tuning  parameters, for example.  The executor then takes those
  and calls to a function of interest.

  The generator can produce values randomly, using a deterministic
  scheme, or some mix of the two.  The deterministic scheme may be
  infinite.  If not, \code{generator} should return \code{NULL} when it
  is done, and the function will finish.

  The executor is responsible for recording whatever information is of
  interest, apart from the arguments it receives.  Relevant values could
  include convergence, parameter estimates, and run-time, among other things.

  Results objects returned by estimators commonly include a lot of
  information, including the arguments of the function call.  To save
  space (and thus time too) you may wish to record only the statistics
  you want, or blank out the data elements you don't need.  The
  arguments returned by \code{generator} are likely to be more compact
  than those to the function being tested; for example,
  \code{generator} might return an index or name that \code{executor}
  translates into a large set of parameters.

  The checkpoint files are deleted on exit from this function.
}
\seealso{\code{\linkS4class{trial}}}
\examples{
## since the function runs forever, we don't want to really run it
\dontrun{

# create a set of parameters
testgen <- function(...) {
  p <- rnorm(3, ...)
  list(a=p[1], b=p[2], c=p[3])
}

# estimate a function using those parameters
testexec <- function(a, b, c) {
  Sys.sleep(10)  # some long computation
  a+b+c  # result, sensitive to the parameters
}

## Note that parameters, as used above, are not the same as the
## parameters of the inner function the executor runs.  The parameters
## here might be the initial value  for the parameters of the inner model.

r <- doTrials(testgen, testexec, nTrials=20, nTime=30)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{optimize}
\keyword{iteration}
\keyword{utilities}
\keyword{datagen}
\keyword{models}
