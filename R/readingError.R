setClass("readingError", contains="matrix")
readingError <- function(...) new("readingError", matrix(...))

# Return matrix with 1's on off-diagonal positive elements
# bitMask generic definition stripped out and put in allGenerics.R.
setMethod("bitMask", signature(object="readingError"),
          function(object) {
            m <- ifelse(object>0, 1, 0)
            diag(m) <- 0
            m
          })

# return matrix with TRUE on off-diagonal positive elements
# boolMask generic definition stripped out and put in allGenerics.R.
setMethod("boolMask", signature(object="readingError"),
          function(object) bitMask(object) == 1)

# return vector of off-diagonal positive elements
# gives all values in 1st row, then all in 2nd, etc
setMethod("coef", signature(object="readingError"),
          function(object) t(object)[t(boolMask(object))])

# Return a readingError that averages the inputs
# Extra args are more readingError's--not like main fn
setMethod("mean", signature(x="readingError"),
          function(x, ...) {
            for(m in list(...)){
              x <- x+m
            }
            x/nargs()
          })
