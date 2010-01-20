
#include "TimeStepsGenerator.h"
#include "Data.h"

#include <cmath> 

namespace mspath{

void TimeStepsGenerator::makeStepsFor(Environment*pEnvironment){
Data&data= pEnvironment->data();
pEnvironment->clearTimeSteps();
Data::TIObs b,e,i,j;
Time t0,t1;
Time delta;
size_t n;
size_t ni;
b= pEnvironment->begin();
e= pEnvironment->end();
if(b==e)

return;
i= b;
t0= data.time(i);
t1= t0;


for(j= b+1;j<e;j++){
t1= data.time(j);
delta= t1-t0;
n= static_cast<size_t> (std::ceil(delta/myStepSize));
delta/= n;
for(ni= 0;ni<n;ni++)
makeStep(t0+delta*ni,ni==0,i,pEnvironment);

t0= t1;
i= j;
}
makeStep(t1,true,i,pEnvironment);

}

}



/* :264 */
#line 13439 "mspath.web"

/* 47: */
#line 2914 "mspath.web"

