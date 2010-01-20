
#include "Manager.h"

#include <R.h> 
#include <sstream> 

namespace mspath{
/* 206: */
#line 7823 "mspath.web"

void Manager::go(double*results,int do_what){

if(do_what==0)
setupCount();
else
setupLikelihood();


mypPathGenerator.reset(new PathGenerator(&environment(),&recorder(),
&stateTimeClassifier(),&successorGenerator()));

mainOperation();
getResults(results);
}

/* :206 */
#line 7813 "mspath.web"

/* 207: */
#line 7840 "mspath.web"

void Manager::setupCount(){
mypRecorder.reset(new SimpleRecorder(&(environment())));
}

/* :207 */
#line 7814 "mspath.web"

/* 208: */
#line 7846 "mspath.web"

void Manager::setupLikelihood(){
mypEvaluator.reset(new Evaluator(&(model())));
mypRecorder.reset(new EvaluatorRecorder(mypEvaluator.get(),
&(environment())));
}

/* :208 */
#line 7815 "mspath.web"

/* 209: */
#line 7854 "mspath.web"

void Manager::mainOperation(){
environment().startSession();
pathGenerator().startSession();
while(environment().next()){

timeStepsGenerator().makeStepsFor(&(environment()));
pathGenerator().startCase();


for(State s= 0;s<model().nStates();s++){
double p= model().initialProbabilities()[s];
if(p<=0.0)
continue;
StatePoint sp0(s,environment().timeSteps().front().time());
pathGenerator().startTree(sp0,p);
}
pathGenerator().finishCase();
}
pathGenerator().finishSession();



}

/* :209 */
#line 7816 "mspath.web"

/* 210: */
#line 7880 "mspath.web"

void Manager::getResults(double*results){
if(hasLikelihood())
*results++= dynamic_cast<EvaluatorRecorder
*> (mypRecorder.get())->logLikelihood();
else
*results++= 0.0;
*results++= recorder().cases();
*results++= recorder().goodPaths();
*results++= recorder().goodNodes();
*results++= recorder().badNodes();
*results++= recorder().goodPathNodes();
}

/* :210 */
#line 7817 "mspath.web"

/* 211: */
#line 7895 "mspath.web"

std::auto_ptr<RandomPathGenerator::Results> 
Manager::simulate(){
std::auto_ptr<RandomPathGenerator::Results> pResults(
new RandomPathGenerator::Results);
mypPathGenerator.reset(new RandomPathGenerator(&environment(),
&stateTimeClassifier(),&model(),myIsExactTimeAbsorb,
pResults.get()));
environment().startSession();
pathGenerator().startSession();
/* 212: */
#line 7916 "mspath.web"

State initialState= environment().randomDraw(
model().initialProbabilities());

/* :212 */
#line 7905 "mspath.web"

/* 213: */
#line 7921 "mspath.web"

while(environment().next()){

timeStepsGenerator().makeStepsFor(&(environment()));
pathGenerator().startCase();
StatePoint sp0(initialState,environment().timeSteps().front().time());
pathGenerator().startTree(sp0,1.0);
pathGenerator().finishCase();
}

/* :213 */
#line 7906 "mspath.web"

pathGenerator().finishSession();
return pResults;
}

/* :211 */
#line 7818 "mspath.web"

}


/* :205 */
#line 13447 "mspath.web"

/* 182: */
#line 7330 "mspath.web"

