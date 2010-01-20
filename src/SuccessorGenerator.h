
#ifndef SuccessorGenerator_h
#define SuccessorGenerator_h 1
#include "basic.h"

#include <vector> 

#include "Model.h"
#include "Node.h"

namespace mspath{
class SuccessorGenerator{
public:
SuccessorGenerator(Model*pModel):
myPModel(pModel){}



void nextStates(std::vector<State> &next,
const Node&baseNode,
Time newTime);


State firstState()const{return 0u;}
State lastState()const{return myPModel->nStates()-1u;}
bool isPossibleTransition(State start,State end)const{
return myPModel->isPossibleTransition(start,end);
}




void setModel(Model*pModel){
myPModel= pModel;}

protected:
Model*model(){return(myPModel);}

Model*myPModel;
};
}
#endif 

/* :46 */
#line 13442 "mspath.web"

/* 225: */
#line 8236 "mspath.web"

