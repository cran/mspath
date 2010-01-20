
#ifndef StateTimeClassifier_h
#define StateTimeClassifier_h 1
#include "basic.h"

#include "Environment.h"
#include "Model.h"
#include "TimeSteps.h"
#include "MSPathError.h"

namespace mspath{
/* 48: */
#line 2932 "mspath.web"


class StateTimeClassifier{
public:

StateTimeClassifier(Model*pModel)
throw(InconsistentModel);

virtual~StateTimeClassifier(){}


virtual void startTree(Environment*pEnv);
void finishTree(Environment*pEnv){}











bool isOK(const StatePoint&sp,const TimePoint&tp,Environment*pEnv)const;



bool isTerminal(const StatePoint&sp,Environment*pEnv)const;


const Model&model()const{return*mypModel;}
Model&model(){return*mypModel;}




void setModel(Model*pModel){
mypModel= pModel;}



protected:
Model*mypModel;
Time myTerminalTime;



std::vector<Time> myFirstTime;

};

/* :48 */
#line 2926 "mspath.web"

/* 49: */
#line 3007 "mspath.web"

class PickyStateTimeClassifier:public StateTimeClassifier{
public:

PickyStateTimeClassifier(Model*pModel)
throw(InconsistentModel);

virtual~PickyStateTimeClassifier(){}


virtual void startTree(Environment*pEnv);
};

/* :49 */
#line 2927 "mspath.web"

}
#endif 

/* :47 */
#line 13440 "mspath.web"

/* 226: */
#line 8272 "mspath.web"

