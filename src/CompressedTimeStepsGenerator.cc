
#include "CompressedTimeStepsGenerator.h"
#include "Data.h"

#include <cmath> 

namespace mspath{

void
CompressedTimeStepsGenerator::makeStepsFor(Environment*pEnvironment){
Data&data= pEnvironment->data();
pEnvironment->clearTimeSteps();
myobnext= pEnvironment->begin();
myobend= pEnvironment->end();
if(myobnext==myobend)

return;
myitnext= integerTime(data.time(myobnext));
makeStep(timeFromInteger(myitnext),true,myobnext,pEnvironment);
lastObs(data);
while(myobnext!=myobend){
int itprev= myitlast;
Data::TIObs obprev= myoblast;
lastObs(data);
for(int it= itprev+1;it<myitlast;it++){
makeStep(timeFromInteger(it),false,obprev,pEnvironment);
}
makeStep(timeFromInteger(myitlast),true,myoblast,pEnvironment);
}
}
/* 263: */
#line 9261 "mspath.web"

void CompressedTimeStepsGenerator::lastObs(const Data&data){
myitlast= myitnext;
while(++myobnext!=myobend){
myitnext= integerTime(data.time(myobnext));
if(myitnext> myitlast){
myoblast= myobnext-1;
return;
}

myDuplicates.push_back(myitnext);
}

myoblast= myobnext-1;
}

/* :263 */
#line 9240 "mspath.web"

}

/* :262 */
#line 13437 "mspath.web"

/* 131: */
#line 5697 "mspath.web"

