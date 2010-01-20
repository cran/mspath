
#include "mspathR.h"


#include <memory> 
#include "Manager.h"
#include "ModelBuilder.h"

extern "C"{
#include <R.h> 
/* 293: */
#line 10413 "mspath.web"

std::auto_ptr<mspath::Model> makeModel(
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
#line 10415 "mspath.web"
){
using namespace mspath;

/* 294: */
#line 10452 "mspath.web"


int n= LENGTH(history);
char**cptr= (char**)R_alloc(n,sizeof(char*));
for(int i= 0;i<n;i++){
size_t l= strlen(CHAR(STRING_ELT(history,i)));



cptr[i]= (char*)R_alloc(l+1,sizeof(char));
strcpy(cptr[i],CHAR(STRING_ELT(history,i)));
}

/* :294 */
#line 10418 "mspath.web"

ModelBuilder mb(REAL(params),REAL(allinits),*INTEGER(p),
*INTEGER(nst),*INTEGER(nfix),INTEGER(fixedpars));
std::auto_ptr<Model> pm= mb.makeModel(
INTEGER(misc),INTEGER(qvector),INTEGER(evector),
INTEGER(constraint),INTEGER(miscconstraint),
INTEGER(baseconstraint),INTEGER(basemiscconstraint),
INTEGER(pathconstraint),INTEGER(pathmiscconstraint),
REAL(initprobs),
INTEGER(nms),INTEGER(nintens),
INTEGER(nintenseffs),INTEGER(nmisc),INTEGER(nmisceffs),
INTEGER(ncovs),INTEGER(ncoveffs),
INTEGER(nmisccovs),INTEGER(nmisccoveffs),


INTEGER(nhistory),const_cast<const char**> (cptr),REAL(initialOffset),
INTEGER(npatheffs),INTEGER(npathmisceffs));

#if 0
std::cout<<"Model intensities:"<<std::endl
<<m.intensity()<<std::endl
<<"Model misclassifications: "<<std::endl
<<m.misclassification()<<std::endl;
#endif
return pm;
}

/* :293 */
#line 10289 "mspath.web"

/* 288: */
#line 10300 "mspath.web"


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
#line 10303 "mspath.web"
){
using namespace mspath;

std::auto_ptr<Model> pm= makeModel(/* 286: */
#line 10215 "mspath.web"

params,

allinits,

misc,


p,

subjvec,
timevec,
statevec,
qvector,
evector,
covvec,
constraint,
misccovvec,
miscconstraint,
baseconstraint,
basemiscconstraint,


history,
initialOffset,
pathconstraint,
pathmiscconstraint,

initprobs,
nst,
nms,
nintens,
nintenseffs,
nmisc,
nmisceffs,
nobs,
npts,
ncovs,
ncoveffs,
nmisccovs,
nmisccoveffs,


nhistory,
npatheffs,
npathmisceffs,


isexact,

nfix,
fixedpars,


stepNumerator,
stepDenominator


/* :286 */
#line 10306 "mspath.web"
);


Data*pd= new Data(INTEGER(subjvec),*INTEGER(nobs),
*INTEGER(npts),REAL(timevec),
INTEGER(statevec),
REAL(covvec),*INTEGER(ncovs),
REAL(misccovvec),*INTEGER(nmisccovs));
#if 0
std::cout<<"Data has "<<pd->nPersons()<<
" people in "<<pd->nObs()<<" records."<<std::endl;
#endif

Manager*pmanager= new Manager(pd,pm.release(),
*INTEGER(stepNumerator),*INTEGER(stepDenominator),
(*INTEGER(isexact))!=0);


SEXP ptr;
Rf_protect(ptr= R_MakeExternalPtr(pmanager,R_NilValue,R_NilValue));
R_RegisterCFinalizer(ptr,(R_CFinalizer_t)finalizeManager);
Rf_unprotect(1);
return ptr;

}

/* :288 */
#line 10290 "mspath.web"

/* 289: */
#line 10338 "mspath.web"

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
#line 10339 "mspath.web"
){
using namespace mspath;
std::auto_ptr<Model> pm= makeModel(/* 286: */
#line 10215 "mspath.web"

params,

allinits,

misc,


p,

subjvec,
timevec,
statevec,
qvector,
evector,
covvec,
constraint,
misccovvec,
miscconstraint,
baseconstraint,
basemiscconstraint,


history,
initialOffset,
pathconstraint,
pathmiscconstraint,

initprobs,
nst,
nms,
nintens,
nintenseffs,
nmisc,
nmisceffs,
nobs,
npts,
ncovs,
ncoveffs,
nmisccovs,
nmisccoveffs,


nhistory,
npatheffs,
npathmisceffs,


isexact,

nfix,
fixedpars,


stepNumerator,
stepDenominator


/* :286 */
#line 10341 "mspath.web"
);
Manager*pmanager= static_cast<Manager*> (R_ExternalPtrAddr(ptr));
pmanager->setModel(pm);
return ptr;
}

/* :289 */
#line 10291 "mspath.web"

/* 290: */
#line 10348 "mspath.web"

SEXP compute(SEXP ptr,SEXP do_what){
using namespace mspath;
Manager*pmanager= static_cast<Manager*> (R_ExternalPtrAddr(ptr));
SEXP newvec;
Rf_protect(newvec= Rf_allocVector(REALSXP,6u));
double*returned= REAL(newvec);
std::stringstream serror;
try{
pmanager->go(returned,*INTEGER(do_what));
*returned*= -2;
}catch(std::exception&exc){
serror<<"Caught exception: "<<exc.what();
}catch(...){
serror<<"Some non-standard exception was thrown"<<
std::endl;
}
if(!serror.str().empty()){
finalizeManager(ptr);
Rf_error("%s",serror.str().c_str());
}
Rf_unprotect(1);
return newvec;
}


/* :290 */
#line 10292 "mspath.web"

/* 292: */
#line 10385 "mspath.web"

#include <sstream> 
#include <Rinternals.h> 
SEXP selectSubset(SEXP ptr,SEXP subset){
using namespace mspath;
Manager*pmanager= static_cast<Manager*> (R_ExternalPtrAddr(ptr));
SubsetDataIterator::IDList pSub(
new Int1D(INTEGER(subset),LENGTH(subset)));
std::ostringstream buf;
std::size_t count= pSub->size();
buf<<"mspath::selectSubset got "<<LENGTH(subset)<<" ID's in selectSubset: ";
for(size_t i= 0;i<count;i++)
buf<<" "<<(*pSub)[i]<<";";
buf<<std::endl;

pmanager->setSubset(pSub);
return ptr;
}

SEXP selectAll(SEXP ptr){
using namespace mspath;
Manager*pmanager= static_cast<Manager*> (R_ExternalPtrAddr(ptr));
pmanager->setAll();
return ptr;
}

/* :292 */
#line 10293 "mspath.web"

/* 295: */
#line 10479 "mspath.web"

SEXP simulate(SEXP ptr){
using namespace mspath;
typedef RandomPathGenerator::Results Results;

Manager*pmanager= static_cast<Manager*> (R_ExternalPtrAddr(ptr));
GetRNGstate();
std::auto_ptr<Results> pResults= 
pmanager->simulate();

SEXP rvalue,names,state,time,iobs;

/* 296: */
#line 10502 "mspath.web"

const int nCol= 3u;
const char*cnames[]= {"state","time","row"};


Rf_protect(rvalue= Rf_allocVector(VECSXP,nCol));
Rf_protect(names= Rf_allocVector(STRSXP,nCol));
for(size_t i= 0u;i<nCol;i++){
SET_STRING_ELT(names,i,Rf_mkChar(cnames[i]));
}
Rf_setAttrib(rvalue,R_NamesSymbol,names);


/* :296 */
#line 10491 "mspath.web"

/* 297: */
#line 10516 "mspath.web"


size_t n= pResults->size();
Rf_protect(state= Rf_allocVector(INTSXP,n));
Rf_protect(time= Rf_allocVector(REALSXP,n));
Rf_protect(iobs= Rf_allocVector(INTSXP,n));
int*cstate= INTEGER(state);
double*ctime= REAL(time);
int*ciobs= INTEGER(iobs);
for(size_t i= 0u;i<n;i++){
RandomPathGenerator::SimResult&r= (*pResults)[i];

cstate[i]= r.state()+1u;
ctime[i]= r.time();
ciobs[i]= r.obsIndex()+1u;
}

/* :297 */
#line 10492 "mspath.web"

/* 298: */
#line 10534 "mspath.web"


SET_VECTOR_ELT(rvalue,0u,state);
SET_VECTOR_ELT(rvalue,1u,time);
SET_VECTOR_ELT(rvalue,2u,iobs);


/* :298 */
#line 10493 "mspath.web"


PutRNGstate();
Rf_unprotect(5u);
return rvalue;
}


/* :295 */
#line 10294 "mspath.web"

/* 291: */
#line 10375 "mspath.web"


void finalizeManager(SEXP ptr){
using namespace mspath;
Manager*pmanager= static_cast<Manager*> (R_ExternalPtrAddr(ptr));
delete pmanager;
R_ClearExternalPtr(ptr);
}

/* :291 */
#line 10295 "mspath.web"

}

/* :287 */
#line 13425 "mspath.web"

/* 83: */
#line 3887 "mspath.web"

