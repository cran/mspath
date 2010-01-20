
#include "Evaluator.h"

namespace mspath{
void Evaluator::evaluate(Node&theNode,Environment&theEnv){
model().fillModelData(theNode,theEnv);
model().evaluate(theNode,theEnv);
}
}

/* :268 */
#line 13411 "mspath.web"

/* 155: */
#line 6497 "mspath.web"

