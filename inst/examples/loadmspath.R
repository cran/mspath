# Same effect as library(mspath) without the fuss;
# also loads R simulation code and some data.

# Assumes the C code has been created by
# makefile -f Makefile.full lib
# run in src subdirectory.

# Assumes it is being run from top level of package source,
# i.e., the mspath/ directory.

# Why use this script?
#
# Speed: change source code files and read in new code without
# needing to rebuild the whole library.
#
# Convenience: gives ready access to simulation code, which
# is not present in binary package, and the data.


topdir=getwd()
dyn.load(paste(topdir, "/src/mspath.so", sep="" ))
for (x in c("allGenerics", "checkPoint", "mspathCoefficients",
            "mspath", "mspathCalculator",
            "mspathDistributedCalculator",
            "runTime", "subset", "utils", "zzz")) {
  fn <- paste(topdir, "/R/", x, ".R", sep="")
  source(fn)
}
for (x in c("mspsim", "fakeCalc", "makeData")){
  fn <- paste(topdir, "/src/simulate/", x, ".R", sep="")
  source(fn)
}
for (x in c("e2", "q2", "sim1", "sim2", "sim3")) {
  fn <- paste(topdir, "/data/", x, ".RData", sep="")
  load(fn)
}

