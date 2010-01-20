
#include "StateTimeClassifier.h"

#include <cassert> 

#include "TimePoint.h"
#include "TimeSteps.h"

/* 227: */
#line 8294 "mspath.web"


mspath::StateTimeClassifier::StateTimeClassifier(Model*pModel)
throw(mspath::InconsistentModel):
mypModel(pModel),
myFirstTime(pModel->nStates()){}

/* :227 */
#line 8281 "mspath.web"

/* 231: */
#line 8376 "mspath.web"

void
mspath::StateTimeClassifier::startTree(Environment*thepEnv){
const TimeSteps&ts= thepEnv->timeSteps();
const Data&data= thepEnv->data();
myTerminalTime= ts.back().time();



TimeSteps::const_reverse_iterator rb,re,ri,rlag;
rb= ts.rbegin();
re= ts.rend();
for(State s= 0u;s<model().nStates();++s){
if(model().isAbsorbing(s)){
myFirstTime[s]= ts.front().time();
rlag= rb;
for(ri= rb;ri!=re;++ri){
if(!(ri->matchesObservation()&&
data.hasObservedState(ri->iObservation()))){
rlag= ri;
continue;
}
if(!model().isPossibleObservation(s,
data.state(ri->iObservation()))){
if(ri!=rb)
myFirstTime[s]= rlag->time();
else
myFirstTime[s]= ri->time()+static_cast<Time> (1);
break;
}

rlag= ri;
}
}
}

}

/* :231 */
#line 8282 "mspath.web"

/* 229: */
#line 8331 "mspath.web"


bool
mspath::StateTimeClassifier::isTerminal(const StatePoint&sp,
Environment*pEnv)const{
return sp.time()>=myTerminalTime;
}

/* :229 */
#line 8283 "mspath.web"

/* 230: */
#line 8354 "mspath.web"




bool
mspath::StateTimeClassifier::isOK(
const StatePoint&sp,const
TimePoint&tp,
Environment*pEnv)const{
assert(sp.time()==tp.time());
if(model().isAbsorbing(sp.state()))
return sp.time()>=myFirstTime[sp.state()];
return!tp.matchesObservation()||
model().isPossibleObservation(sp.state(),
pEnv->data().state(tp.iObservation()));
}


/* :230 */
#line 8284 "mspath.web"


/* 228: */
#line 8302 "mspath.web"

mspath::PickyStateTimeClassifier::PickyStateTimeClassifier(Model*pModel)
throw(mspath::InconsistentModel):
StateTimeClassifier(pModel){
for(State s= 0u;s<pModel->nStates();++s){
if(!pModel->isAbsorbing(s))
continue;
for(State j= 0u;j<pModel->nStates();++j){
if(s==j)
continue;
if(pModel->isPossibleObservation(s,j)||
pModel->isPossibleObservation(j,s))
throw InconsistentModel("Absorbing States with exact"
" times must be measured exactly.");
}
}
}


/* :228 */
#line 8286 "mspath.web"

/* 232: */
#line 8416 "mspath.web"


void
mspath::PickyStateTimeClassifier::startTree(Environment*thepEnv){
const TimeSteps&ts= thepEnv->timeSteps();
const Data&data= thepEnv->data();
myTerminalTime= ts.back().time();



TimeSteps::const_reverse_iterator rb,re,ri,rlag;
rb= ts.rbegin();
re= ts.rend();
for(State s= 0u;s<model().nStates();++s){
if(model().isAbsorbing(s)){
myFirstTime[s]= ts.front().time();
rlag= rb;
for(ri= rb;ri!=re;++ri){
if(!data.hasObservedState(ri->iObservation())){
continue;
}
if(!model().isPossibleObservation(s,
data.state(ri->iObservation()))){
if(ri!=rb)
myFirstTime[s]= rlag->time();
else
myFirstTime[s]= ri->time()+static_cast<Time> (1);
break;
}

rlag= ri;
}
}
}

}

/* :232 */
#line 8287 "mspath.web"



/* :226 */
#line 13441 "mspath.web"

/* 46: */
#line 2823 "mspath.web"

