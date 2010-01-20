
#ifndef Model_h
#define Model_h 1

#include <boost/ptr_container/ptr_vector.hpp> 
#include <iosfwd> 
#include <memory> 

#include "basic.h"
#include "HistoryComputer.h"
#include "MSPathError.h"
#include "Node.h"
#include "Specification.h"

namespace mspath{


class Environment;
class AbstractCovariates;

class Model{
public:
/* 51: */
#line 3131 "mspath.web"


typedef boost::ptr_vector<HistoryComputer> TComputerContainer;


typedef ModelData TModelData;
typedef Array1D<State> TState1D;

/* :51 */
#line 3116 "mspath.web"

/* 52: */
#line 3156 "mspath.web"

Model(AbstractSpecification*thepTransition,
AbstractSpecification*thepMisclassification,
TComputerContainer*thepComputerContainer= 0,
State theInitialState= 0u)
throw(InconsistentModel,BadInitialProbs,OneInitialState)
:
myInitialState(theInitialState),
mypTransition(thepTransition),
mypMisclassification(thepMisclassification),
mypComputerContainer(thepComputerContainer){
validate();



myIsAbsorbing.resize(thepTransition->nStates());
myInitProbs.resize(nStates());
for(size_t i= 0;i<myInitProbs.size();++i){
if(i==myInitialState)
myInitProbs[i]= 1.0;
else
myInitProbs[i]= 0.0;
myIsAbsorbing[i]= true;
for(State j= 0u;j<nStates();++j){
if(i!=j&&mypTransition->isPermissible(i,j)){
myIsAbsorbing[i]= false;
break;
};
};
}
}


void release(ScratchPad*thePad){
mypTransition->release(thePad);
if(mypMisclassification.get())
mypMisclassification->release(thePad);
}

/* :52 */
#line 3117 "mspath.web"

/* 54: */
#line 3216 "mspath.web"




size_t nPathDependentVariables()const{
if(mypComputerContainer.get())
return mypComputerContainer->size();
return 0u;
}


size_t nStates()const{
return mypTransition->nStates();
}

/* :54 */
#line 3118 "mspath.web"

/* 55: */
#line 3234 "mspath.web"



void fillModelData(Node&theNode,Environment&theEnv);



void evaluate(Node&theNode,Environment&theEnv)throw(DataModelInconsistency);


protected:




void validate()const throw(InconsistentModel,BadInitialProbs,OneInitialState);

public:

/* :55 */
#line 3119 "mspath.web"

/* 56: */
#line 3258 "mspath.web"



bool isPossibleTransition(State theFrom,State theTo)const{
return theFrom==theTo||
mypTransition->isPermissible(theFrom,theTo);
}


bool isPossibleObservation(State theTrue,State theObserved)const{


if(theTrue==theObserved)
return true;
return mypMisclassification.get()!=0&&
mypMisclassification->isPermissible(theTrue,theObserved);
}

bool isPossibleObservation(State theTrue,ObsState theObserved)const{


return theObserved<0||isPossibleObservation(theTrue,static_cast<State> (theObserved));
}



const Double1D&initialProbabilities()const{
return myInitProbs;}


const bool isAbsorbing(State s)const{
return myIsAbsorbing[s];
}

/* :56 */
#line 3120 "mspath.web"

/* 57: */
#line 3306 "mspath.web"


void simulatePath(Environment&env);
State simulateObservation(Environment&env);

/* :57 */
#line 3121 "mspath.web"

/* 58: */
#line 3312 "mspath.web"

friend std::ostream&operator<<(std::ostream&ostr,
const Model&model);


/* :58 */
#line 3122 "mspath.web"

protected:
/* 53: */
#line 3196 "mspath.web"


const State myInitialState;


std::auto_ptr<AbstractSpecification> mypTransition;


std::auto_ptr<AbstractSpecification> mypMisclassification;


std::auto_ptr<TComputerContainer> mypComputerContainer;


Double1D myInitProbs;


std::vector<bool> myIsAbsorbing;

/* :53 */
#line 3124 "mspath.web"

};
}
#endif


/* :50 */
#line 13416 "mspath.web"

/* 233: */
#line 8454 "mspath.web"

