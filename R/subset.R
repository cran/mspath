# subset data to speed things up
# return subsetted dataframe with about nCases
# data is a data frame or matrix
# column id is the case id
# column time is a time-like measure
# total time of a case (last-first) predicts computation effort
# Drop the top lop cases, then select about nCases from remainder.
# return subset of data
mspath.subset <- function(data, lop=.3, id="id", time="time", nCases = 50){
  s <- data[,id] # s = subject
  T <- data[,time] # T = time
  n <- length(s)
  # identify and discard long case
  iStart <- seq(along=s)[s != c(min(s)-1, s[1:(n-1)])]
  tStart <- T[iStart]
  tEnd <- T[c(iStart[-1]-1, n)]
  tLen <- tEnd-tStart
  tCut <- quantile(tLen, 1-lop)
  # throw out longest 30% of cases
  # compute time is geometric in duration of case
  iShort <- iStart[tLen<tCut]
  nShort <- length(iShort)
  if (nShort/nCases >= 2)
    iShort <- iShort[seq(1, to=nShort, by=floor(nShort/nCases))]
  data[s %in% s[iShort] , ]
}

