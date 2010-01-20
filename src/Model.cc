
#include "Model.h"
#include <iostream> 

/* 234: */
#line 8472 "mspath.web"

void
mspath::Model::fillModelData(Node&theNode,Environment&theEnv){
if(mypComputerContainer.get()==0)
return;
for(size_t i= 0u;i<mypComputerContainer->size();++i){
(*mypComputerContainer)[i].evaluate(theEnv,theNode);
}
}


/* :234 */
#line 8459 "mspath.web"

/* 235: */
#line 8494 "mspath.web"


void
mspath::Model::evaluate(Node&theNode,Environment&theEnv)
throw(DataModelInconsistency){

if(theNode.isRoot()){
if(theNode.state()!=myInitialState)
throw DataModelInconsistency(theEnv.id(),"Initial Path State Inconsistent with"
"Model");
theEnv.activeEvaluationData(theNode)= 1.0;
return;
}else{
Node&lastNode= *(theNode.previous());
double lastLik= theEnv.activeEvaluationData(lastNode);
const Double2D&trans= mypTransition->evaluate(theEnv);
theEnv.activeEvaluationData(theNode)= lastLik*
trans(lastNode.state(),theNode.state());
}

if(mypMisclassification.get()==0||
!theEnv.hasObservedState(theNode))
return;

const Double2D&misc= mypMisclassification->evaluate(theEnv);
double obs= misc(theNode.state(),static_cast<State> (theEnv.observedState(theNode)));
if(obs==0.0)

throw DataModelInconsistency(theEnv.id(),
"Generated path has an impossible observation.");
theEnv.activeEvaluationData(theNode)*= 
misc(theNode.state(),static_cast<State> (theEnv.observedState(theNode)));
}

/* :235 */
#line 8460 "mspath.web"

/* 236: */
#line 8533 "mspath.web"

void
mspath::Model::validate()const
throw(InconsistentModel,BadInitialProbs,OneInitialState){
if(mypTransition.get()==0)
throw InconsistentModel("Model requires at least a Transition Specification");
if(myInitialState>=nStates())
throw InconsistentModel("Initial State is too high");
if(mypMisclassification.get()!=0){
if(mypMisclassification->nStates()!=mypTransition->nStates())
throw InconsistentModel("Model transition and misclassification"
" specifications use different number of states");



}
if(mypComputerContainer.get()){
const TComputerContainer&cc= *mypComputerContainer;
const size_t n= cc.size();
for(size_t i= 0u;i<n;++i){
if(cc[i].isCovariate())
if(cc[i].covariateIndex()>=n)
throw InconsistentModel("Path-Dependent Variable thinks it"
" lives at an impossibly high index");
}
}


}

/* :236 */
#line 8461 "mspath.web"

/* 237: */
#line 8564 "mspath.web"

std::ostream&mspath::operator<<(std::ostream&ostr,const mspath::Model&
model){
ostr<<"A model with "<<model.nStates()
<<" states, initial state of "
<<model.myInitialState<<std::endl
<<"Transitions governed by "<<*(model.mypTransition);
if(model.mypMisclassification.get()==0)
ostr<<"No misclassification."<<std::endl;
else
ostr<<"Misclassification governed by "
<<*(model.mypMisclassification);
if(model.nPathDependentVariables()> 0u){
ostr<<model.nPathDependentVariables()
<<" path-dependent variables:"<<std::endl;
mspath::Model::TComputerContainer::const_iterator i;
for(i= model.mypComputerContainer->begin();
i!=model.mypComputerContainer->end();
++i)
ostr<<(*i)<<std::endl;
}
return ostr;
}

/* :237 */
#line 8462 "mspath.web"

/* 238: */
#line 8603 "mspath.web"

void
mspath::Model::simulatePath(Environment&env){
const Double2D&trans= mypTransition->evaluate(env);
State s= env.randomDraw(trans.row(env.currentNode().state()));
env.currentNode().setState(s);
}

/* :238 */
#line 8463 "mspath.web"

/* 239: */
#line 8614 "mspath.web"

mspath::State
mspath::Model::simulateObservation(Environment&env){
State s= env.currentNode().state();
if(mypMisclassification.get()==0)
return s;
const Double2D&misc= mypMisclassification->evaluate(env);

s= env.randomDraw(misc.row(s));
return s;
}

/* :239 */
#line 8464 "mspath.web"



/* :233 */
#line 13417 "mspath.web"

/* 174: */
#line 7019 "mspath.web"

