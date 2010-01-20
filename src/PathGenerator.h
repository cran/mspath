
#ifndef PathGenerator_h
#define PathGenerator_h 1

#include <stdexcept> 
#include <vector>   

#include "basic.h"
#include "Environment.h"
#include "MSPathError.h"
#include "NodeFactory.h"
#include "Recorder.h"
#include "StateTimeClassifier.h"
#include "SuccessorGenerator.h"

namespace mspath{
/* 41: */
#line 2605 "mspath.web"

class AbstractPathGenerator{

public:
AbstractPathGenerator(Environment*pEnv):
mypEnvironment(pEnv){}
virtual~AbstractPathGenerator(){}



virtual void startSession();
virtual void startCase();
virtual void startTree(const StatePoint&sp,double probability)
throw(std::runtime_error,std::logic_error);
virtual void finishCase();
virtual void finishSession();

protected:

Environment&environment(){return*mypEnvironment;}
const Environment&environment()const{
return*mypEnvironment;}


const inline TimePoint&nextTimePoint()const{
Path::size_type n= environment().path().size();
return environment().timeSteps()[n];
}


Environment*mypEnvironment;
};
/* :41 */
#line 2589 "mspath.web"

/* 42: */
#line 2641 "mspath.web"

class PathGenerator:public AbstractPathGenerator{

public:
typedef AbstractPathGenerator Super;

PathGenerator(Environment*pEnv,Recorder*pRec,
StateTimeClassifier*pSTC,
SuccessorGenerator*pSG):
AbstractPathGenerator(pEnv),mypRecorder(pRec),
mypStateTimeClassifier(pSTC),
mypSuccessorGenerator(pSG){}

virtual~PathGenerator(){}


virtual void startSession();
virtual void startCase();
virtual void startTree(const StatePoint&sp,double probability)
throw(std::runtime_error,std::logic_error);
virtual void finishCase();
virtual void finishSession();

protected:

Recorder&recorder(){return*mypRecorder;}
StateTimeClassifier&stateTimeClassifier(){
return*mypStateTimeClassifier;}
SuccessorGenerator&successorGenerator(){
return*mypSuccessorGenerator;}


void nextBranch();


Recorder*mypRecorder;
StateTimeClassifier*mypStateTimeClassifier;
SuccessorGenerator*mypSuccessorGenerator;

};

/* :42 */
#line 2590 "mspath.web"

/* 43: */
#line 2724 "mspath.web"

class RandomPathGenerator:public AbstractPathGenerator{

public:
typedef AbstractPathGenerator Super;
/* 44: */
#line 2782 "mspath.web"

class SimResult{
public:
SimResult(ObsState s,Time t,IObservation i):
myState(s),myTime(t),myObsIndex(i){}

ObsState state()const{return myState;}
Time time()const{return myTime;}
IObservation obsIndex()const{return myObsIndex;}

protected:
ObsState myState;
Time myTime;
IObservation myObsIndex;
};

typedef std::vector<SimResult> Results;

/* :44 */
#line 2729 "mspath.web"


RandomPathGenerator(Environment*pEnv,
StateTimeClassifier*pSTC,
Model*pModel,
bool isExact,
Results*pResults):
AbstractPathGenerator(pEnv),
myIsExact(isExact),
mypStateTimeClassifier(pSTC),
mypModel(pModel),
mypResults(pResults){}

virtual~RandomPathGenerator(){}


virtual void startSession();
virtual void startCase();
virtual void startTree(const StatePoint&sp,double probability)
throw(std::runtime_error,std::logic_error);
virtual void finishCase();
virtual void finishSession();


Results&results(){
return*mypResults;}

protected:

StateTimeClassifier&stateTimeClassifier(){
return*mypStateTimeClassifier;}
Model&model(){
return*mypModel;}
bool isExact()const{return myIsExact;}

/* 45: */
#line 2801 "mspath.web"


void nextStep();
void addLastPoint();
void recordObservation();

/* :45 */
#line 2764 "mspath.web"



bool myIsExact;


StateTimeClassifier*mypStateTimeClassifier;
Model*mypModel;
Results*mypResults;

};

/* :43 */
#line 2591 "mspath.web"


}
#endif 

/* :40 */
#line 13444 "mspath.web"

/* 214: */
#line 7945 "mspath.web"

