
#include "CompositeHistoryComputer.h"
#include <cmath> 
#include "Environment.h"

void mspath::LnHistoryComputer::evaluate(Environment&theEnv,
Node&theNode)
throw(std::out_of_range)
{
if(theNode.modelData()[target().dataIndex()]<=0.0)
throw std::out_of_range(name()+
" attempts ln of value <= 0");

theNode.modelData()[dataIndex()]= std::log(
theNode.modelData()[target().dataIndex()]);
}

/* :266 */
#line 13453 "mspath.web"

/* 192: */
#line 7496 "mspath.web"

