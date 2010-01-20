### Force dynamic loading of the C code library (mspath.so)
.First.lib <- function(lib, pkg) library.dynam( "mspath", pkg, lib )

## R Extensions manual says the next function is rarely needed,
#  and implies it's only an issue on MS Windows.
#  Still, to be safe, particularly for development, here it is.
.Last.lib <- function(libpath) library.dynam.unload("mspath", libpath)

# Note: library.dynam is not for use from the command line
