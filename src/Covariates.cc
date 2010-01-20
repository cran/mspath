
#include "Covariates.h"
#include "Environment.h"  
namespace mspath{
/* 273: */
#line 9584 "mspath.web"


bool MatrixCovariates::isChanged(Environment&theEnv,
ScratchData*thepMemento){
Memento*pMemento= dynamic_cast<Memento*> (thepMemento);
if(!pMemento)
return true;
if(pMemento->iObservation==theEnv.iObservation())
return false;

#if 0

if(myData.nrows()==0)
return false;
#endif
rawValues(theEnv);
size_t n= mypMemento->cache.size();

for(size_t i= 0;i<n;++i)
if(mypMemento->cache[i]!=pMemento->cache[i])
return true;
return false;
}



Double1D&MatrixCovariates::rawValues(Environment&theEnv){
if(mypMemento==0)
mypMemento= new Memento(myData,theEnv);
else if(mypMemento->iObservation!=theEnv.iObservation())
mypMemento->capture(myData,theEnv);

return mypMemento->cache;
}


ScratchData*MatrixCovariates::memento(){
if(mypMemento==0)
return 0;
return new Memento(*mypMemento);
}

ScratchData*
MatrixCovariates::memento(
ScratchData**theppMemento){
if(*theppMemento==0){
*theppMemento= memento();
return*theppMemento;
};
*dynamic_cast<Memento*> (*theppMemento)= *mypMemento;
return*theppMemento;
}


Double1D&MatrixCovariates::values(Environment&theEnv){
return rawValues(theEnv);
}

/* 274: */
#line 9656 "mspath.web"



MatrixCovariates::Memento::Memento(
const Double2D&theData,
const Environment&theEnv):
iObservation(theEnv.iObservation()),
cache(theData.col(iObservation)){}


void
MatrixCovariates::Memento::capture(
const Double2D&theData,
const Environment&theEnv){
iObservation= theEnv.iObservation();
cache= theData.col(iObservation);
}

/* :274 */
#line 9642 "mspath.web"



/* :273 */
#line 9569 "mspath.web"

/* 275: */
#line 9676 "mspath.web"



Double1D&PathCovariates::values(Environment&theEnv){
Node*pNode= &(theEnv.currentNode());
Node*prior= pNode->previous();
if(prior==0)
prior= pNode;
myValues= prior->modelData()[myIndices];
return myValues;
}


/* :275 */
#line 9570 "mspath.web"

}

/* :272 */
#line 13420 "mspath.web"

/* 269: */
#line 9461 "mspath.web"

