
#ifndef Manager_h
#define Manager_h 1

#include <memory> 

#include "basic.h"
#include "Data.h"
#include "Model.h"
#include "Path.h"
#include "TimeSteps.h"
#include "Node.h"
#include "PathGenerator.h"
#include "Environment.h"
#include "SimpleRecorder.h"
#include "EvaluatorRecorder.h"
#include "Evaluator.h"
#include "FixedTimeStepsGenerator.h"

namespace mspath{
class Manager{
public:
/* 33: */
#line 2402 "mspath.web"

typedef Data TData;
typedef TimeSteps TTimeSteps;
typedef Model TModel;
typedef Node TNode;
typedef Path TPath;
typedef StateTimeClassifier TStateTimeClassifier;
typedef SuccessorGenerator TSuccessorGenerator;
typedef Environment TEnvironment;

/* :33 */
#line 2389 "mspath.web"

/* 34: */
#line 2414 "mspath.web"

Manager(TData*pData,
TModel*pModel,
int stepNumerator= 1,
int stepDenominator= 1,
bool isExactTimeAbsorb= false):
mypData(pData),
myEnvironment(pData,new Path(pModel->nPathDependentVariables())),
mypModel(pModel),
myTimeStepsGenerator(stepNumerator,stepDenominator),
mySuccessorGenerator(pModel),
mypStateTimeClassifier(0),
myIsExactTimeAbsorb(isExactTimeAbsorb),
mypRecorder(0),
mypEvaluator(0)
{
if(isExactTimeAbsorb)
mypStateTimeClassifier.reset(new
PickyStateTimeClassifier(pModel));
else
mypStateTimeClassifier.reset(new
StateTimeClassifier(pModel));
}

/* :34 */
#line 2390 "mspath.web"

/* 35: */
#line 2473 "mspath.web"


void go(double*results,int do_what= 0);





void setSubset(SubsetDataIterator::IDList&pSubset){
environment().setSubset(pSubset);
}


void setAll(void){
environment().setAll();
}




void setModel(std::auto_ptr<Model> pm){




#if 0
if(mypModel.get())
mypModel->release(&(environment()));
#endif

mypModel= pm;


stateTimeClassifier().setModel(&(model()));
successorGenerator().setModel(&(model()));
environment().clear();
}

/* :35 */
#line 2391 "mspath.web"

/* 39: */
#line 2559 "mspath.web"

std::auto_ptr<RandomPathGenerator::Results> 
simulate();

/* :39 */
#line 2392 "mspath.web"

protected:
/* 38: */
#line 2548 "mspath.web"

void setupCount();
void setupLikelihood();
void mainOperation();
void getResults(double*results);

/* :38 */
#line 2394 "mspath.web"

/* 37: */
#line 2533 "mspath.web"

TEnvironment&environment(){return myEnvironment;}
Model&model(){return*mypModel;}
FixedTimeStepsGenerator&timeStepsGenerator(){return myTimeStepsGenerator;}
AbstractPathGenerator&pathGenerator(){return*mypPathGenerator;}
SimpleRecorder&recorder(){return*mypRecorder;}
Evaluator&evaluator(){return*mypEvaluator;}
TStateTimeClassifier&stateTimeClassifier(){return
*mypStateTimeClassifier;}
TSuccessorGenerator&successorGenerator(){return
mySuccessorGenerator;}

bool hasLikelihood()const{return mypEvaluator.get()!=0;}

/* :37 */
#line 2395 "mspath.web"

/* 36: */
#line 2518 "mspath.web"

std::auto_ptr<Data> mypData;
TEnvironment myEnvironment;
std::auto_ptr<Model> mypModel;
FixedTimeStepsGenerator myTimeStepsGenerator;
TSuccessorGenerator mySuccessorGenerator;
std::auto_ptr<TStateTimeClassifier> mypStateTimeClassifier;
bool myIsExactTimeAbsorb;
std::auto_ptr<SimpleRecorder> mypRecorder;
std::auto_ptr<Evaluator> mypEvaluator;
std::auto_ptr<AbstractPathGenerator> mypPathGenerator;


/* :36 */
#line 2396 "mspath.web"

};
}
#endif 

/* :32 */
#line 13446 "mspath.web"

/* 205: */
#line 7805 "mspath.web"

