
#include <stdexcept> 

#include "PathGenerator.h"
namespace mspath{
/* 218: */
#line 8066 "mspath.web"

void AbstractPathGenerator::startSession(){}

void PathGenerator::startSession(){
Super::startSession();
mypRecorder->startSession();
}

void AbstractPathGenerator::finishSession(){}

void PathGenerator::finishSession(){
Super::finishSession();
mypRecorder->finishSession();
}


void AbstractPathGenerator::startCase(){
environment().path().reserve(environment().timeSteps().size());
}

void PathGenerator::startCase(){
Super::startCase();
recorder().startCase();
}

void AbstractPathGenerator::finishCase(){}

void PathGenerator::finishCase(){
Super::finishCase();
recorder().finishCase();
}

/* :218 */
#line 7951 "mspath.web"

/* 215: */
#line 7959 "mspath.web"

void AbstractPathGenerator::startTree(const StatePoint&sp0,double p)
throw(std::runtime_error,std::logic_error){
environment().pathClear();

environment().pathPush(sp0,nextTimePoint());

}

void
PathGenerator::startTree(const StatePoint&sp0,double p)
throw(std::runtime_error,std::logic_error){
Super::startTree(sp0,p);

stateTimeClassifier().startTree(&(environment()));



recorder().startTree(p);


if(!stateTimeClassifier().isTerminal(sp0,&(environment())))
nextBranch();
else{

recorder().goodPath(environment().path());
environment().pathPop();
}


recorder().finishTree();
stateTimeClassifier().finishTree(&(environment()));
}
/* :215 */
#line 7952 "mspath.web"

/* 216: */
#line 8023 "mspath.web"

void PathGenerator::nextBranch(){


const Node&baseNode= environment().currentNode();



const TimePoint&newTimePoint= nextTimePoint();
Time newTime= newTimePoint.time();

const State n= successorGenerator().lastState();
const State startState= baseNode.state();
for(State endState= successorGenerator().firstState();
endState<=n;
endState++){
if(!successorGenerator().isPossibleTransition(startState,endState))
continue;
/* 217: */
#line 8048 "mspath.web"

StatePoint nextSP(endState,newTime);
if(stateTimeClassifier().isOK(nextSP,newTimePoint,
&(environment()))){

environment().pathPush(nextSP,newTimePoint);
recorder().goodNode();
if(stateTimeClassifier().isTerminal(nextSP,&(environment())))
recorder().goodPath(environment().path());
else
nextBranch();

environment().pathPop();
}else{
recorder().impermissible();
}

/* :217 */
#line 8041 "mspath.web"

}
}

/* :216 */
#line 7953 "mspath.web"

/* 219: */
#line 8099 "mspath.web"

/* 220: */
#line 8107 "mspath.web"


void RandomPathGenerator::startSession(){
Super::startSession();
results().clear();
}

void RandomPathGenerator::startCase(){
Super::startCase();
}

void RandomPathGenerator::finishCase(){
Super::finishCase();
}

void RandomPathGenerator::finishSession(){
Super::finishSession();
}

/* :220 */
#line 8100 "mspath.web"

/* 221: */
#line 8130 "mspath.web"

void
RandomPathGenerator::startTree(const StatePoint&sp0,double p)
throw(std::runtime_error,std::logic_error){
Super::startTree(sp0,p);


stateTimeClassifier().startTree(&(environment()));
model().fillModelData(environment().currentNode(),environment());



if(environment().matchesObservation())
recordObservation();


while(!stateTimeClassifier().isTerminal(
environment().currentNode().statePoint(),
&(environment()))
)
nextStep();



if(!environment().matchesObservation())
addLastPoint();


environment().pathClear();
stateTimeClassifier().finishTree(&(environment()));
}

/* :221 */
#line 8101 "mspath.web"

/* 222: */
#line 8169 "mspath.web"

void
RandomPathGenerator::nextStep(){
const State lastState= environment().currentNode().state();
const TimePoint&nextTP= nextTimePoint();
Time nextTime= nextTP.time();

StatePoint nextSP= StatePoint(lastState,nextTime);
environment().pathPush(nextSP,nextTP);


model().simulatePath(environment());


model().fillModelData(environment().currentNode(),environment());


if(environment().matchesObservation())
recordObservation();

}
/* :222 */
#line 8102 "mspath.web"

/* 223: */
#line 8196 "mspath.web"

void
RandomPathGenerator::addLastPoint(){
if(!isExact()){
const State finalState= environment().currentNode().state();
do{
const TimePoint&tp= nextTimePoint();
environment().pathPush(StatePoint(finalState,tp.time()),
tp);
model().fillModelData(environment().currentNode(),environment());
}while(!environment().matchesObservation());

}
recordObservation();
}

/* :223 */
#line 8103 "mspath.web"

/* 224: */
#line 8215 "mspath.web"

void
RandomPathGenerator::recordObservation(){
ObsState s= -1;
if
(environment().data().hasObservedState(environment().iObservation()))
s= static_cast<ObsState> (model().simulateObservation(environment()));

results().push_back(
SimResult(s,
environment().timePoint().time(),
environment().iObservation()));
}
/* :224 */
#line 8104 "mspath.web"


/* :219 */
#line 7954 "mspath.web"

}

/* :214 */
#line 13445 "mspath.web"

/* 32: */
#line 2366 "mspath.web"

