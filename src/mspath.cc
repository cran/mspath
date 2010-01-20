

#include <R.h> 

#include <string> 
#include <sstream> 

#include "basic.h"
#include "mspath.h"
#include "Data.h"
#include "Manager.h"
#include "ModelBuilder.h"

namespace mspath{
/* 309: */
#line 10887 "mspath.web"

extern "C"void mspathCEntry(
/* 304: */
#line 10768 "mspath.web"

int*do_what,


double*params,

double*allinits,

int*misc,


int*p,

int*subjvec,
double*timevec,
int*statevec,
int*qvector,
int*evector,
double*covvec,
int*constraint,
double*misccovvec,
int*miscconstraint,
int*baseconstraint,
int*basemiscconstraint,


const char**history,
double*initialOffset,
int*pathconstraint,
int*pathmiscconstraint,

double*initprobs,
int*nst,
int*nms,
int*nintens,
int*nintenseffs,
int*nmisc,
int*nmisceffs,
int*nobs,
int*npts,
int*ncovs,
int*ncoveffs,
int*nmisccovs,
int*nmisccoveffs,


int*nhistory,
int*npatheffs,
int*npathmisceffs,


int*isexact,

int*nfix,
int*fixedpars,


int*stepNumerator,
int*stepDenominator,
double*returned

/* :304 */
#line 10889 "mspath.web"

)
{
using namespace mspath;

ModelBuilder mb(params,allinits,*p,*nst,*nfix,fixedpars);
std::auto_ptr<Model> pm= mb.makeModel(
misc,qvector,evector,
constraint,miscconstraint,
baseconstraint,basemiscconstraint,
pathconstraint,pathmiscconstraint,
initprobs,
nms,nintens,
nintenseffs,nmisc,nmisceffs,ncovs,ncoveffs,
nmisccovs,nmisccoveffs,
nhistory,history,initialOffset,npatheffs,npathmisceffs);

#if 0
std::cout<<"Model intensities:"<<std::endl
<<m.intensity()<<std::endl
<<"Model misclassifications: "<<std::endl
<<m.misclassification()<<std::endl;
#endif


Data*pd= new Data(subjvec,*nobs,*npts,timevec,
statevec,
covvec,*ncovs,
misccovvec,*nmisccovs);
#if 0
std::cout<<"Data has "<<pd->nPersons()<<
" people in "<<pd->nObs()<<" records."<<std::endl;
#endif








Manager*pmanager= new Manager(pd,pm.release(),
*stepNumerator,*stepDenominator,(*isexact)!=0);
std::stringstream serror;
try{
pmanager->go(returned,*do_what);
*returned*= -2;
}catch(std::exception&exc){
serror<<"Caught exception: "<<exc.what();
}catch(...){
serror<<"Some non-standard exception was thrown"<<
std::endl;
}
if(!serror.str().empty()){
serror<<std::endl;
delete pmanager;
error("%s",serror.str().c_str());
}
delete pmanager;
pmanager= 0;
}

/* :309 */
#line 10883 "mspath.web"

}

/* :308 */
#line 13423 "mspath.web"

/* 284: */
#line 10091 "mspath.web"

