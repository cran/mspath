# wrapper to do checkpointing

# Ross Boylan ross@biostat.ucsf.edu
# 06-Jan-2006
# (C) 2006 Regents of University of California
# Distributed under the Gnu Public License v2 or later at your option

# If you want to checkpoint the optimization of a function f
# Use checkpoint(f) instead.  See below for other possible arguments.

# default operation for checkpoint(fnfoo) is to record the iterations
# in fnfoo.trace in the calling environment

# WARNING: Any existing variable with name in argument name
# will be deleted from the indicated frame
checkpoint <- function(f,
                       name = paste(substitute(f), ".trace", sep=""),
                       fileName = substitute(f),
                       nCalls = 1,
                       nTime = 60*15,
                       frame = parent.frame()) {
  # f is the objective function
  # frame is where to put the variable name
  # name will be a data.frame with rows containing
  #   iteration, time, value, parameters
  # fileName is the stem of the name to save for checkpointing
  #  saving will alternate between files with 0 and 1 appended
  # Saving to disk will happen every nCalls or nTime seconds,
  # whichever comes first
  if (exists(name, where=frame))
      rm(list=name, pos=frame)
  ckpt.lastSave <- 0 # alternate 0/1 for file to write to
  ckpt.lastTime <- Sys.time()  # last time saved
  function(params, ...) {
    p <- as.list(params)
    names(p) <- seq(length(params))
    if (exists(name, where=frame, inherits=FALSE)) {
      progress <- get(name, pos=frame)
      progress <- rbind(progress,
                        data.frame(row.names=dim(progress)[1]+1, time=Sys.time(),
                        val=NA, p)) # R 2.0 doesn't know deparse.level=0
    } else
        progress <- data.frame(row.names=1, time=Sys.time(), val=NA, p)
    n <- dim(progress)[1]
    # write to disk
    if (n%%nCalls == 0 || progress[n, 1]- ckpt.lastTime > nTime) {
      ckpt.lastSave <<- (ckpt.lastSave+1) %% 2
      save(progress, file=paste(fileName, ckpt.lastSave, sep=""))
      ckpt.lastTime <<- progress[n, 1]
    }
    v <- f(params, ...)
    progress[n, 2] <- v
    assign(name, progress, pos=frame)
    v
  }
}
