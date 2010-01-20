################## test data

transSpec <- Specification(matrix(c(FALSE, TRUE, FALSE,
                                    TRUE, FALSE, FALSE,
                                    TRUE, TRUE, FALSE), 3),
                           matrix(c(0.9, 10.1, 2.4, -11,
                                    -10, 5.3, -5, 8.4,
                                    0.2, 8.1, -2.1, 5.5,
                                    -7, -2.3,  4, 15.3), 4)
                           )

errSpec <- Specification(matrix(c(FALSE, TRUE, FALSE,
                                  TRUE, FALSE, FALSE,
                                  FALSE, FALSE, FALSE), 3),
                         matrix(c(-1.8, 0, 0, 0,
                                  -0.8, 0, 0, 0), 4)
                         )


fullModel <- ErrorModel(transSpec, errSpec)


# for the fullModel check of Model_test
# This is the first case in pData1
obs2 <- Observations(c(2.3, 2.4), c(1, 2),
                     matrix(c(rep(1, 2), .3, .3,  0, 2.1),
                            3, byrow=TRUE))
tp2 <- TruePaths(seq(2.3, 2.4, by=.05),
                 matrix(c(1, 2, 2,  1, 2, 1,  1, 1, 2,  1, 1, 1), 4,
                        byrow=TRUE), obs2, absorb=3)
pm2 <- CompletePathMaker(tp2, .025, "TIS")
r2 <- evaluatePaths(pm2, fullModel)

# 3rd case in pData1b, used in EvaluatorRecorder_test
# along with r2
obs3 <- Observations(c(0, 0.1, 0.25), c(1, 2, 2),
                     matrix(c(rep(1, 3), -1, -2, -1,  2.3, 2.4, 2.5),
                     3, byrow=TRUE))
tp3 <- TruePaths(seq(0, 0.25, by=1/20),matrix(byrow=TRUE, ncol=6,
c(1, 2, 2, 2, 2, 2,
  1, 2, 2, 2, 2, 1,
  1, 2, 2, 2, 1, 2,
  1, 2, 2, 2, 1, 1,
  1, 2, 2, 1, 2, 2,
  1, 2, 2, 1, 2, 1,
  1, 2, 2, 1, 1, 2,
  1, 2, 2, 1, 1, 1,
  1, 2, 1, 2, 2, 2,
  1, 2, 1, 2, 2, 1,
  1, 2, 1, 2, 1, 2,
  1, 2, 1, 2, 1, 1,
  1, 2, 1, 1, 2, 2,
  1, 2, 1, 1, 2, 1,
  1, 2, 1, 1, 1, 2,
  1, 2, 1, 1, 1, 1,
  1, 1, 2, 2, 2, 2,
  1, 1, 2, 2, 2, 1,
  1, 1, 2, 2, 1, 2,
  1, 1, 2, 2, 1, 1,
  1, 1, 2, 1, 2, 2,
  1, 1, 2, 1, 2, 1,
  1, 1, 2, 1, 1, 2,
  1, 1, 2, 1, 1, 1,
  1, 1, 1, 2, 2, 2,
  1, 1, 1, 2, 2, 1,
  1, 1, 1, 2, 1, 2,
  1, 1, 1, 2, 1, 1,
  1, 1, 1, 1, 2, 2,
  1, 1, 1, 1, 2, 1,
  1, 1, 1, 1, 1, 2,
  1, 1, 1, 1, 1, 1)),
                 observations=obs3, absorb=3)
pm3 <- CompletePathMaker(tp3, .025, "TIS")
r3 <- evaluatePaths(pm3, fullModel)

# manager2 test
# pData1c 3rd case (moves to absorbing)
obs3c <- Observations(c(0, 0.1, 0.25), c(1, 2, 3),
                     matrix(c(rep(1, 3), -1, -2, -1,  2.3, 2.4, 2.5),
                     3, byrow=TRUE))
tp3c <- TruePaths(seq(0, 0.25, by=1/20), fullModel, obs3c)
pm3c <- CompletePathMaker(tp3c, .025, "TIS")
r3c <- evaluatePaths(pm3c, fullModel)


## mbb = modelBuilderBasic test in ModelBuilder_test
mbb.transSpec <- Specification(matrix(c(FALSE, TRUE, TRUE, FALSE,
                                        TRUE, FALSE, TRUE, FALSE,
                                        FALSE, FALSE, FALSE, TRUE,
                                        FALSE, TRUE, FALSE, FALSE),
                                      nrow=4,
                                      byrow=TRUE),
                               matrix(c(-.4, .2, 1.1, 1.1, .2, -.4),
                                      nrow=1))
mbb.model <- Model(mbb.transSpec)

# from pData7
mbb.obs1 <- Observations(c(1, 2, 5, 7), c(1, 3, 3, 2),
                     matrix(rep(1, 4), 1))
mbb.tp1 <- TruePaths(seq(7), mbb.model, mbb.obs1)
mbb.pm1 <- CompletePathMaker(mbb.tp1)
mbb.r1 <- evaluatePaths(mbb.pm1, mbb.model)

mbb.obs2 <- Observations(c(101, 104, 106), c( 1, 4, 4),
                         matrix(1, 1, 3))
mbb.tp2 <- TruePaths(seq(101, 106), mbb.model, mbb.obs2)
mbb.pm2 <- CompletePathMaker(mbb.tp2)
mbb.r2 <- evaluatePaths(mbb.pm2, mbb.model)

## mbe = Model Builder with Error test
mbe.errSpec <- Specification(matrix(c(FALSE, TRUE, FALSE, FALSE,
                                      TRUE, FALSE, FALSE,  TRUE,
                                      FALSE, TRUE, FALSE,  TRUE,
                                      TRUE, FALSE, FALSE, FALSE),
                                    byrow=TRUE,
                                    nrow = 4),
                             matrix(c(.05, .05, .3, .05, .3, .3),
                                    nrow=1))
mbe.model <- ErrorModel(mbb.transSpec, mbe.errSpec)
mbe.obs1 <- Observations(c(1, 2, 5, 7), c(1, 3, 3, 2),
                     matrix(rep(1, 4), 1))
mbe.tp1 <- TruePaths(seq(7), mbe.model, mbe.obs1)
mbe.pm1 <- CompletePathMaker(mbe.tp1)
mbe.r1 <- evaluatePaths(mbe.pm1, mbe.model)

mbe.obs2 <- Observations(c(101, 104, 106), c( 1, 4, 4),
                         matrix(1, 1, 3))
mbe.tp2 <- TruePaths(seq(101, 106), mbe.model, mbe.obs2)
mbe.pm2 <- CompletePathMaker(mbe.tp2)
mbe.r2 <- evaluatePaths(mbe.pm2, mbe.model)


## mbc = modelBuilderCovs
mbc.transSpec <- Specification(matrix(c(FALSE, TRUE, TRUE, FALSE,
                                        TRUE, FALSE, TRUE, FALSE,
                                        FALSE, FALSE, FALSE, TRUE,
                                        FALSE, TRUE, FALSE, FALSE),
                                      nrow=4,
                                      byrow=TRUE),
                              matrix(c(0.7, 1.7, -1.4, -1.4, 1.7, 0.7,
                                       0.3, 0.3, 0.3, 0.3, 0.3, 0.3,
                                       1.2, -0.4, 1.2, 1.2, -0.4, 0.5,
                                       rep(0, 6)
                                        ),
                                      nrow=4, byrow=TRUE))

mbc.errSpec <- Specification(matrix(c(FALSE, TRUE, FALSE, FALSE,
                                      TRUE, FALSE, FALSE,  TRUE,
                                      FALSE, TRUE, FALSE,  TRUE,
                                      TRUE, FALSE, FALSE, FALSE),
                                    byrow=TRUE,
                                    nrow = 4),
                             matrix(c(.3, .3, 1.1, .9, 1.1, .9,
                                      rep(0, 12),
                                      -1.5, -1.5, 0, 0, -.8, -.8),
                                    nrow=4, byrow=TRUE))

mbc.model <- ErrorModel(mbc.transSpec, mbc.errSpec)
mbc.obs1 <- Observations(c(1, 2, 5, 7), c(1, 3, 3, 2),
                     matrix(c(rep(1, 4),
                              1.94, -2.46,  0.09,  2.80,
                              -4.23,  0.23, -4.22,  1.53,
                              3.51,  1.93,  5.30,  1.03),
                            byrow=TRUE, nrow=4))
mbc.tp1 <- TruePaths(seq(7), mbc.model, mbc.obs1)
mbc.pm1 <- CompletePathMaker(mbc.tp1)
mbc.r1 <- evaluatePaths(mbc.pm1, mbc.model)

mbc.obs2 <- Observations(c(101, 104, 106), c( 1, 4, 4),
                         matrix(c(rep(1, 3),
                                   -0.60, -6.11, -1.15,
                                  3.63,  0.84,  2.42,
                                   0.56, -3.32, -0.50),
                                byrow=TRUE, nrow=4))
mbc.tp2 <- TruePaths(seq(101, 106), mbc.model, mbc.obs2)
mbc.pm2 <- CompletePathMaker(mbc.tp2)
mbc.r2 <- evaluatePaths(mbc.pm2, mbc.model)

#mbp = ModelBuilder with Path test (modelBuilderPath)
mbp.transSpec <- Specification(matrix(c(FALSE, TRUE, TRUE, FALSE,
                                        TRUE, FALSE, TRUE, FALSE,
                                        FALSE, FALSE, FALSE, TRUE,
                                        FALSE, TRUE, FALSE, FALSE),
                                      nrow=4,
                                      byrow=TRUE),
                              matrix(c(-.4, .2, 1.1, 1.1, .2, -.4,
                                       -1.1, 1.1, .4, .4, 1.1, -1.1,
                                       .4, -0.8, -0.9, -0.9, .4, -0.8,
                                       .05, .05, .05, .3, .3, .3),
                                      nrow=4, byrow=TRUE))

mbp.errSpec <- Specification(matrix(c(FALSE, TRUE, FALSE, FALSE,
                                      TRUE, FALSE, FALSE,  TRUE,
                                      FALSE, TRUE, FALSE,  TRUE,
                                      TRUE, FALSE, FALSE, FALSE),
                                    byrow=TRUE,
                                    nrow = 4),
                             matrix(c(.5, .5, -.51, .5, -.51, -.51,
                                      rep(2.8, 6),
                                      -.1, 2.12, -.1, 2.12, -.1, -.1,
                                      rep(1.0, 6)),
                                    nrow=4, byrow=TRUE))
# history has different order from C Test code
# coefficients were reordered appropriately
mbp.history <- c("TIS", "TIC", "LN(TIS)")
mbp.model <- ErrorModel(mbp.transSpec, mbp.errSpec)
mbp.obs1 <- Observations(c(1, 2, 5, 7), c(1, 3, 3, 2),
                     matrix(c(rep(1, 4)),
                            byrow=TRUE, nrow=1))
mbp.tp1 <- TruePaths(seq(7), mbp.model, mbp.obs1)
mbp.pm1 <- CompletePathMaker(mbp.tp1, timeOffset=1, history=mbp.history)
mbp.r1 <- evaluatePaths(mbp.pm1, mbp.model)

mbp.obs2 <- Observations(c(101, 104, 106), c( 1, 4, 4),
                         matrix(c(rep(1, 3)),
                                byrow=TRUE, nrow=1))
mbp.tp2 <- TruePaths(seq(101, 106), mbp.model, mbp.obs2)
mbp.pm2 <- CompletePathMaker(mbp.tp2, timeOffset=1, history=mbp.history)
mbp.r2 <- evaluatePaths(mbp.pm2, mbp.model)

# Reordered path coefficient order relative to C
# mba = modelBuilderAll
mba.transSpec <- Specification(matrix(c(FALSE, TRUE, TRUE, FALSE,
                                        TRUE, FALSE, TRUE, FALSE,
                                        FALSE, FALSE, FALSE, TRUE,
                                        FALSE, TRUE, FALSE, FALSE),
                                      nrow=4,
                                      byrow=TRUE),
                              matrix(c(-.4, 1.1, .3, .3, 1.1, -.4,
                                       -1.1, -1.1, .3, -1.1, -1.1, .3,
                                       -1.1, 0.4, -1.1, 1.1, 0.4, -1.1,
                                       rep(0, 6),
                                        -0.59, -0.8, 2.15, 2.15, -0.8, -0.59,
                                       0.5, 2.8, 1.0, 1.0, 0.5, 2.8,
                                       0.4, 0.4, 0.4, -0.9, -0.9, -0.9
                                       ),
                                      ncol=6, byrow=TRUE))

mba.errSpec <- Specification(matrix(c(FALSE, TRUE, FALSE, FALSE,
                                      TRUE, FALSE, FALSE,  TRUE,
                                      FALSE, TRUE, FALSE,  TRUE,
                                      TRUE, FALSE, FALSE, FALSE),
                                    byrow=TRUE,
                                    nrow = 4),
                             matrix(c( 1.32, 1.32, -0.1, 1.32, -0.1, -0.1,
                                      rep(0, 6),
                                      rep(0, 6),
                                       2.2, 3.3, 0.53, 4.4, 1.1, -1.5,
                                      rep(.33, 6),
                                       -0.46, -0.27,  -0.46, -0.27,  -0.46, -0.46,
                                      rep(1.04, 6)
                                      ),
                                    ncol=6, byrow=TRUE))
mba.history <- c("TIS", "TIC", "LN(TIS)")
mba.model <- ErrorModel(mba.transSpec, mba.errSpec)
mba.obs1 <- Observations(c(1, 2, 5, 7), c(1, 3, 3, 2),
                     matrix(c(rep(1, 4),
                            1.94, -2.46,  0.09,  2.80,
                            -4.23,  0.23, -4.22,  1.53,
                             3.51,  1.93,  5.30,  1.03),
                            byrow=TRUE, ncol=4))
mba.tp1 <- TruePaths(seq(7), mba.model, mba.obs1)
mba.pm1 <- CompletePathMaker(mba.tp1, timeOffset=1, history=mba.history)
mba.r1 <- evaluatePaths(mba.pm1, mba.model)

mba.obs2 <- Observations(c(101, 104, 106), c( 1, 4, 4),
                         matrix(c(rep(1, 3),
                                  -0.60, -6.11, -1.15,
                                  3.63,  0.84,  2.42,
                                  0.56, -3.32, -0.50),
                                byrow=TRUE, ncol=3))
mba.tp2 <- TruePaths(seq(101, 106), mba.model, mba.obs2)
mba.pm2 <- CompletePathMaker(mba.tp2, timeOffset=1, history=mba.history)
mba.r2 <- evaluatePaths(mba.pm2, mba.model)


# mbs = modelBuilderSimple test: given, fixed errors
# similar to above

mbs.transSpec <- Specification(matrix(c(FALSE, TRUE, TRUE, FALSE,
                                        TRUE, FALSE, TRUE, FALSE,
                                        FALSE, FALSE, FALSE, TRUE,
                                        FALSE, TRUE, FALSE, FALSE),
                                      nrow=4,
                                      byrow=TRUE),
                              matrix(c(-.4, 1.1, .3, .3, 1.1, -.4,
                                       -1.1, -1.1, .3, -1.1, -1.1, .3,
                                       -1.1, 0.4, -1.1, 1.1, 0.4, -1.1,
                                       rep(0, 6),
                                        -0.59, -0.8, 2.15, 2.15, -0.8, -0.59,
                                       0.5, 2.8, 1.0, 1.0, 0.5, 2.8,
                                       0.4, 0.4, 0.4, -0.9, -0.9, -0.9
                                       ),
                                      ncol=6, byrow=TRUE))

mbs.errSpec <- SimpleSpecification(matrix(c(FALSE, TRUE, FALSE, FALSE,
                                            TRUE, FALSE, FALSE,  TRUE,
                                            FALSE, TRUE, FALSE,  TRUE,
                                            TRUE, FALSE, FALSE, FALSE),
                                          byrow=TRUE,
                                          nrow = 4),
                                   c( .04, .28, .01, .28, .02, .1 ))
mbs.history <- c("TIS", "TIC", "LN(TIS)")
mbs.model <- ErrorModel(mbs.transSpec, mbs.errSpec)
mbs.obs1 <- Observations(c(1, 2, 5, 7), c(1, 3, 3, 2),
                     matrix(c(rep(1, 4),
                            1.94, -2.46,  0.09,  2.80,
                            -4.23,  0.23, -4.22,  1.53,
                             3.51,  1.93,  5.30,  1.03),
                            byrow=TRUE, ncol=4))
mbs.tp1 <- TruePaths(seq(7), mbs.model, mbs.obs1)
mbs.pm1 <- CompletePathMaker(mbs.tp1, timeOffset=1, history=mbs.history)
mbs.r1 <- evaluatePaths(mbs.pm1, mbs.model)

mbs.obs2 <- Observations(c(101, 104, 106), c( 1, 4, 4),
                         matrix(c(rep(1, 3),
                                  -0.60, -6.11, -1.15,
                                  3.63,  0.84,  2.42,
                                  0.56, -3.32, -0.50),
                                byrow=TRUE, ncol=3))
mbs.tp2 <- TruePaths(seq(101, 106), mbs.model, mbs.obs2)
mbs.pm2 <- CompletePathMaker(mbs.tp2, timeOffset=1, history=mbs.history)
mbs.r2 <- evaluatePaths(mbs.pm2, mbs.model)

# mbs2 = modelBuilderSimple test 2
# as above, with different parameters
# test2.R uses this

mbs2.transSpec <- Specification(matrix(c(FALSE, TRUE, TRUE, FALSE,
                                        TRUE, FALSE, TRUE, FALSE,
                                        FALSE, FALSE, FALSE, TRUE,
                                        FALSE, TRUE, FALSE, FALSE),
                                      nrow=4,
                                      byrow=TRUE),
                              matrix(c( .8, .7, 2.4, 2.4, .7, .8,
                                       .5, .5, -1.2, .5, .5, -1.2,
                                       -.8, 2.3, -.8, .7, 2.3, -.8,
                                       rep(0, 6),
                                       1.2, -.8, -.5, -.5, -.8, 1.2,
                                       1.1, -1, .3, .3, 1.1, -1.0,
                                       .3, .3, .3, .7, .7, .7
                                       ),
                                      ncol=6, byrow=TRUE))

mbs2.errSpec <- SimpleSpecification(matrix(c(FALSE, TRUE, FALSE, FALSE,
                                            TRUE, FALSE, FALSE,  TRUE,
                                            FALSE, TRUE, FALSE,  TRUE,
                                            TRUE, FALSE, FALSE, FALSE),
                                          byrow=TRUE,
                                          nrow = 4),
                                   c( .1, .15, .1, .15, .2, .05))
mbs.history <- c("TIS", "TIC", "LN(TIS)")
mbs2.model <- ErrorModel(mbs2.transSpec, mbs2.errSpec)
mbs2.tp1 <- TruePaths(seq(7), mbs2.model, mbs.obs1)
mbs2.pm1 <- CompletePathMaker(mbs2.tp1, timeOffset=1, history=mbs.history)
mbs2.r1 <- evaluatePaths(mbs2.pm1, mbs2.model)

mbs2.tp2 <- TruePaths(seq(101, 106), mbs2.model, mbs.obs2)
mbs2.pm2 <- CompletePathMaker(mbs2.tp2, timeOffset=1, history=mbs.history)
mbs2.r2 <- evaluatePaths(mbs2.pm2, mbs2.model)

# and tweak some fixed parameters
# fix  3  5  6 11 19 at inits value; rest is inits2
# result is
# [1]  0.80  0.70  0.30*
#      0.50  0.30* -1.10*  0.70  2.30
#      0.30  0.70 -0.59* -0.80
# [13] -0.50  1.10  0.30 -1.00
#      0.10  0.15  0.01*  0.20  0.05
mbs2b.transSpec <- Specification(matrix(c(FALSE, TRUE, TRUE, FALSE,
                                        TRUE, FALSE, TRUE, FALSE,
                                        FALSE, FALSE, FALSE, TRUE,
                                        FALSE, TRUE, FALSE, FALSE),
                                      nrow=4,
                                      byrow=TRUE),
                              matrix(c( .8, .7, 0.3, 0.3, .7, .8,
                                       .5, .5, 0.3, .5, .5, 0.3,
                                       -1.1, 2.3, -1.1, .7, 2.3, -1.1,
                                       rep(0, 6),
                                       -0.59, -.8, -.5, -.5, -.8, -0.59,
                                       1.1, -1, .3, .3, 1.1, -1.0,
                                       .3, .3, .3, .7, .7, .7
                                       ),
                                      ncol=6, byrow=TRUE))

mbs2b.errSpec <- SimpleSpecification(matrix(c(FALSE, TRUE, FALSE, FALSE,
                                            TRUE, FALSE, FALSE,  TRUE,
                                            FALSE, TRUE, FALSE,  TRUE,
                                            TRUE, FALSE, FALSE, FALSE),
                                          byrow=TRUE,
                                          nrow = 4),
                                   c( .1, .15, .01, .15, .2, .05))
mbs2b.model <- ErrorModel(mbs2b.transSpec, mbs2b.errSpec)
mbs2b.tp1 <- TruePaths(seq(7), mbs2b.model, mbs.obs1)
mbs2b.pm1 <- CompletePathMaker(mbs2b.tp1, timeOffset=1, history=mbs.history)
mbs2b.r1 <- evaluatePaths(mbs2b.pm1, mbs2b.model)

mbs2b.tp2 <- TruePaths(seq(101, 106), mbs2b.model, mbs.obs2)
mbs2b.pm2 <- CompletePathMaker(mbs2b.tp2, timeOffset=1, history=mbs.history)
mbs2b.r2 <- evaluatePaths(mbs2b.pm2, mbs2b.model)
