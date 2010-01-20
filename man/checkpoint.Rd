\name{checkpoint}
\alias{checkpoint}
\title{Checkpointing and progress information}
\description{
 To checkpoint the optimization of function \code{f} use
 \code{checkpoint(f)} instead as an argument to the optimizer.
 Each time the optimizer calls the function the parameter values are
 recorded in a table in the caller's environment and on disk.
}
\usage{
checkpoint(f, name = paste(substitute(f), ".trace", sep = ""), fileName = substitute(f), nCalls = 1, nTime = 60 * 15, frame = parent.frame())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{f}{ The function to be optimized, as would be passed to the optimizer.}
  \item{name}{The name of the variable to be written into in the
    caller's environment.  Defaults to \var{name}.\code{trace}.}
  \item{fileName}{The stem of the filename that will hold the \R object \code{name}
    given above.  By default it has the same name as that variable.
    The actual filenames will have \samp{0} and
    \samp{1} appended to them; the two files are written alternately.}
  \item{nCalls}{Writes to disk will occur every \code{nCalls}}
  \item{nTime}{Writes to disk will occur whenver at least \code{nTime}
    seconds have passed since the last write. }
  \item{frame}{This is the frame into which the results are written.}
}
\details{
  If either the \code{nCalls} or \code{nTime} test is passed, the data
  will be written to disk.

  Any variable named \code{name} in \code{frame} will be erased.

  \code{name} is a \code{\link{data.frame}}, each of whose rows
  corresponds to a single invocation of the objective function
  \code{f}.  The name of the row is iteration count.  It columns are
  \describe{
    \item{\code{time}}{The system time (\code{\link{Sys.time}}) of the
      call}
    \item{\code{val}}{The value of \code{f} with the indicated
	parameters}
      \item{parameters}{Each parameter value for the call}
    }
    
}
\value{A \code{function} that takes the same arguments as \code{f} and
  returns the same values, while recording information about the calls.
}
\author{Ross Boylan}
\note{Although this function is part of the \pkg{\link{mspath}} package,
  it is completely general and can be used in many other settings.

  Writing a variable into the caller's environment violates the usual R
  scoping rules.

  Each \dQuote{iteration} of an optimizer may produce \emph{many} calls
  to the objective function.

  The determination of when to write to a file is not ideal.  It should
  be sensitive to missing arguments, so that if one of the two tests is
  specified the other is ignored.  Furthermore, the writeout caused by a
  time test will not reset the counter for the number of cases.  For
  example,
  suppose you used \code{nCases=5, nTime=60}.  If 60 seconds pass and
  the 4th case is written out, the fifth case will still be written out
  (and the 10'th, and so on).  However, if the 5th case occurs before 60
  seconds pass, the 60 second clock resets.

  If the process is interrupted it is likely that the \code{val} of the
  last call will be unavailable, since it has not been computed.

  Restarting currently consists of initializing the optimizer with the
  last recorded values of the previous run.  Some optimizers build up
  information as they go; thus the result is not quite equivalent to
  restarting in the middle.  It might be desirable to have a function
  that could replay the value already computed.

  In general, more insight into the optimizaion process would be
  desirable, e.g., measures of convergence.  C code would need to be
  modified to achieve that.
}
\seealso{\cite{link{optim}}}
\keyword{optimize }% at least one, from doc/KEYWORDS
\keyword{file}
\keyword{iteration}
\keyword{utilities}

