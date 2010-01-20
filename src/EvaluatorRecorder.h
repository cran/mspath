
#ifndef EvaluatorRecorder_h
#define EvaluatorRecorder_h 1

#include <cmath> 

#include "SimpleRecorder.h"
#include "Evaluator.h"
#include "Environment.h"

#if 0

#include "test/main_test.h"
#endif

namespace mspath{

class EvaluatorRecorder:public SimpleRecorder{
public:
EvaluatorRecorder(Evaluator*thepEvaluator,Environment*const
thepEnv):
SimpleRecorder(thepEnv),mypEvaluator(thepEvaluator){}
typedef SimpleRecorder Super;

/* 169: */
#line 6829 "mspath.web"

virtual const EvaluationData&logLikelihood()const{
return myTotalLogLikelihood;}

/* :169 */
#line 6818 "mspath.web"

/* 170: */
#line 6838 "mspath.web"


virtual void startSession(){
Super::startSession();
myTotalLogLikelihood= 0.0;
}

virtual void startCase(){
Super::startCase();
myTotalPathProbability= 0.0;
}

virtual void startTree(double initialProbability){
Super::startTree(initialProbability);

environment().currentNode().evaluationData()= 1.0;
}

virtual void goodPath(TPath&path){
Super::goodPath(path);
#if 0


std::cout<<environment().id()<<" "<<
environment().path()<<
environment().currentNode().evaluationData()<<
std::endl;

#endif
myTotalPathProbability+= 
environment().currentNode().evaluationData();
}

virtual void finishCase()throw(DataModelInconsistency){
Super::finishCase();
myTotalLogLikelihood+= std::log(myTotalPathProbability);
}

protected:



virtual void evaluateAnyNode(Node&node){
Super::evaluateAnyNode(node);
evaluator().evaluate(node,environment());
}
public:

/* :170 */
#line 6819 "mspath.web"

protected:
/* 171: */
#line 6889 "mspath.web"

Evaluator*mypEvaluator;
EvaluationData myTotalLogLikelihood;
EvaluationData myTotalPathProbability;

/* :171 */
#line 6821 "mspath.web"

/* 172: */
#line 6895 "mspath.web"

Evaluator&evaluator(){return*mypEvaluator;}
EvaluationData&totalPathProbability(){
return myTotalPathProbability;}

/* :172 */
#line 6822 "mspath.web"

};
}
#endif 


/* :168 */
#line 13415 "mspath.web"

/* 50: */
#line 3093 "mspath.web"

