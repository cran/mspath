

#include "SuccessorGenerator.h"
namespace mspath{


void SuccessorGenerator::nextStates(std::vector<State> &next,
const Node&baseNode,
Time newTime){
next.clear();
size_t n= model()->nStates();
const State startState= baseNode.state();
State endState;
for(endState= 0u;endState<static_cast<State> (n);endState++)
if(startState==endState||
model()->isPossibleTransition(startState,endState))
next.push_back(endState);
}



}

/* :225 */
#line 13443 "mspath.web"

/* 40: */
#line 2572 "mspath.web"

