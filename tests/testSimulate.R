# Test generation of simulated paths.
# These are very basic sanity checks.
# If this runs without generating a stop, all's well.

library("mspath")
doSim <- 10

# transitions have no absorbing states to assure that
# generated times exactly match observation times.
permit <- matrix(c(0, 1, 0, 0,
                   0, 0, 1, 0,
                   0, 0, 0, 1,
                   0, 0, 1, 0),
                 nrow=4, byrow=TRUE)
data1 <- data.frame(id=c(1, 1, 5, 5, 5),
                    state=c(1, 2, 2, 3, 4),
                    time=c(0, 1.1, 0, 5.6, 6.8),
                    x1=c(0, 0, 1.3, 1.5, 18),
                    x2=c(15, 15, 23, 23, 23))
simdata <- mspath(state~time, qmatrix=permit, inits=rep(.4, 8),
                  covariates=~x1, data=data1, subject=id,
                  do.what=doSim)

conform <- function(sim, dat) {
  stopifnot(nrow(sim) == nrow(dat),
            ncol(sim) == ncol(dat),
            setequal(names(simdata), names(dat)),
            all(sim$id == dat$id),
            all(sim$state >= 1),
            all(sim$state <= 4),
            # since there are no absorbing states, and steps of 1
            # simulated times should roughly match real times
            all(sim$time == round(dat$time)),
            # next line works in presence of NA's
            isTRUE(all.equal(sim$x1, dat$x1)),
            isTRUE(all.equal(sim$x2, dat$x2))
            )
}

# check that we retain all the data
# including covariates not in formulae
conform(simdata, data1)

# missing observations dropped
data2 <- data1
data2$x1[3] <- NA
simdata <- mspath(state~time, qmatrix=permit, inits=rep(.4, 8),
                  covariates=~x1, data=data2, subject=id,
                  do.what=doSim, seed=5943288)
conform(simdata, data2[-3,])


# drop overlapping observations
data2b <- data1
data2b$time[5] <- 6.1
simdata <- mspath(state~time, qmatrix=permit, inits=rep(.4, 8),
                  covariates=~x1, data=data2b, subject=id,
                  do.what=doSim, seed=-123456789)
# the exact behavior in case of overlap is subject to change
# so an error here might not indicate a problem.
# Now we use the last of the overlapping observations.
conform(simdata, data2b[c(-4),])


# NA but not in model are retained
data3 <- data1
data3$x2[3] <- NA
simdata <- mspath(state~time, qmatrix=permit, inits=rep(.4, 8),
                  covariates=~x1, data=data3, subject=id,
                  do.what=doSim, seed=345.89)
conform(simdata, data3)

# import of data (x5) from environment
x5 <- c(88, 99, 77, 23, 22)
simdata <- mspath(state~time, qmatrix=permit, inits=rep(.4, 8),
                  covariates=~x5, data=data1, subject=id,
                  do.what=doSim)
data4 <- data.frame(data1, x5=x5)
conform(simdata, data4)
