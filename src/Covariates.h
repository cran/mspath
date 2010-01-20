
#ifndef Covariates_h
#define Covariates_h 1

#include "basic.h"
#include "ScratchData.h"

namespace mspath{
class Environment;
/* 94: */
#line 4605 "mspath.web"


class AbstractCovariates:public ScratchData{
public:
AbstractCovariates(){}

virtual~AbstractCovariates(){}



virtual bool isChanged(Environment&theEnv,
ScratchData*thepMemento)= 0;



virtual ScratchData*memento()= 0;






virtual ScratchData*memento(ScratchData**theppMemento)= 0;


virtual Double1D&values(Environment&theEnv)= 0;
};


/* :94 */
#line 4574 "mspath.web"

/* 95: */
#line 4639 "mspath.web"

class MatrixCovariates:public AbstractCovariates{
public:
MatrixCovariates(const Double2D&theData):
AbstractCovariates(),
myData(theData),mypMemento(0)
{}

virtual~MatrixCovariates(){
delete mypMemento;
}


virtual bool isChanged(Environment&theEnv,
ScratchData*thepMemento);
virtual ScratchData*memento();
virtual ScratchData*memento(ScratchData**theppMemento);
virtual Double1D&values(Environment&theEnv);

protected:
virtual Double1D&rawValues(Environment&theEnv);

struct Memento:public ScratchData{


Memento(const Double2D&theData,
const Environment&theEnv);




void capture(const Double2D&theData,
const Environment&theEnv);
IObservation iObservation;
Double1D cache;
};




const Double2D&myData;
Memento*mypMemento;
};

/* :95 */
#line 4575 "mspath.web"

/* 96: */
#line 4697 "mspath.web"

class PathCovariates:public AbstractCovariates{
public:
PathCovariates(const TIndirect1D&theIndices):
myIndices(theIndices),myValues(theIndices.size()){}

virtual~PathCovariates(){}

virtual bool isChanged(Environment&theEnv,
ScratchData*thepMemento){return true;}

virtual ScratchData*memento(){return 0;}
virtual ScratchData*memento(ScratchData**theppMemento){
return 0;
}


virtual Double1D&values(Environment&theEnv);

protected:
const TIndirect1D myIndices;
Double1D myValues;
};

/* :96 */
#line 4576 "mspath.web"

}
#endif

/* :93 */
#line 13419 "mspath.web"

/* 272: */
#line 9564 "mspath.web"

