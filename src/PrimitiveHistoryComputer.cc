
#include "PrimitiveHistoryComputer.h"
#include "Environment.h"

void mspath::TimeInStateComputer::evaluate(Environment&theEnv,
Node&theNode)
throw(std::out_of_range)
{
Node*pprior= theNode.previous();

if(pprior){
if(pprior->state()==theNode.state())
theNode.modelData()[dataIndex()]= 
pprior->modelData()[dataIndex()]+
(theNode.time()-pprior->time());
else
theNode.modelData()[dataIndex()]= 
myInitialTime;
}else
theNode.modelData()[dataIndex()]= myInitialTime;
}

void mspath::TimeInPreviousStatesComputer::evaluate(Environment&theEnv,
Node&theNode)
throw(std::out_of_range)
{
Node*pprior= theNode.previous();

if(pprior){
if(pprior->state()!=theNode.state())
theNode.modelData()[dataIndex()]= 
myInitialTime+theNode.time()-theEnv.path().front().time();
else

theNode.modelData()[dataIndex()]= 
pprior->modelData()[dataIndex()];
}else
theNode.modelData()[dataIndex()]= myInitialTime;
}

void mspath::TimeSinceOriginComputer::evaluate(Environment&theEnv,Node&theNode)
throw(std::out_of_range)
{
Node*pprior= theNode.previous();

if(pprior){
theNode.modelData()[dataIndex()]= 
theNode.time()-theEnv.path().front().time()+
theEnv.path().front().modelData()[dataIndex()];

}else
theNode.modelData()[dataIndex()]= myInitialTime;
}

/* :265 */
#line 13451 "mspath.web"

/* 124: */
#line 5461 "mspath.web"

