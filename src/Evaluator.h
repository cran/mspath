
#ifndef Evaluator_h
#define Evaluator_h 1

#include "Model.h"
#include "Node.h"

namespace mspath{

class Environment;

class Evaluator{
public:

Evaluator(Model*pModel):
mypModel(pModel){}


void evaluate(Node&theNode,Environment&theEnv);
protected:
Model*mypModel;
Model&model(){return*mypModel;}

};
}

#endif  


/* :173 */
#line 13410 "mspath.web"

/* 268: */
#line 9449 "mspath.web"

