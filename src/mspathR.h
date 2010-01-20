

#ifndef mspathR_h
#define mspathR_h 1

#define R_NO_REMAP 1
#include <R.h> 
#include <Rinternals.h> 

#include "Data.h"  

extern "C"{

SEXP makeManager(
/* 285: */
#line 10155 "mspath.web"

SEXP params,

SEXP allinits,

SEXP misc,


SEXP p,

SEXP subjvec,
SEXP timevec,
SEXP statevec,
SEXP qvector,
SEXP evector,
SEXP covvec,
SEXP constraint,
SEXP misccovvec,
SEXP miscconstraint,
SEXP baseconstraint,
SEXP basemiscconstraint,


SEXP history,
SEXP initialOffset,
SEXP pathconstraint,
SEXP pathmiscconstraint,

SEXP initprobs,
SEXP nst,
SEXP nms,
SEXP nintens,
SEXP nintenseffs,
SEXP nmisc,
SEXP nmisceffs,
SEXP nobs,
SEXP npts,
SEXP ncovs,
SEXP ncoveffs,
SEXP nmisccovs,
SEXP nmisccoveffs,


SEXP nhistory,
SEXP npatheffs,
SEXP npathmisceffs,


SEXP isexact,

SEXP nfix,
SEXP fixedpars,


SEXP stepNumerator,
SEXP stepDenominator

/* :285 */
#line 10106 "mspath.web"
);






SEXP setParams(SEXP ptr,/* 285: */
#line 10155 "mspath.web"

SEXP params,

SEXP allinits,

SEXP misc,


SEXP p,

SEXP subjvec,
SEXP timevec,
SEXP statevec,
SEXP qvector,
SEXP evector,
SEXP covvec,
SEXP constraint,
SEXP misccovvec,
SEXP miscconstraint,
SEXP baseconstraint,
SEXP basemiscconstraint,


SEXP history,
SEXP initialOffset,
SEXP pathconstraint,
SEXP pathmiscconstraint,

SEXP initprobs,
SEXP nst,
SEXP nms,
SEXP nintens,
SEXP nintenseffs,
SEXP nmisc,
SEXP nmisceffs,
SEXP nobs,
SEXP npts,
SEXP ncovs,
SEXP ncoveffs,
SEXP nmisccovs,
SEXP nmisccoveffs,


SEXP nhistory,
SEXP npatheffs,
SEXP npathmisceffs,


SEXP isexact,

SEXP nfix,
SEXP fixedpars,


SEXP stepNumerator,
SEXP stepDenominator

/* :285 */
#line 10113 "mspath.web"
);



SEXP selectSubset(SEXP ptr,SEXP subset);


SEXP selectAll(SEXP ptr);





SEXP compute(SEXP ptr,SEXP do_what);



SEXP simulate(SEXP ptr);



void finalizeManager(SEXP ptr);

}

#endif  

/* :284 */
#line 13424 "mspath.web"

/* 287: */
#line 10278 "mspath.web"

