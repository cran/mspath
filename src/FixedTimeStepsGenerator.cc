
#include "FixedTimeStepsGenerator.h"

#include <cmath> 
#include <sstream> 

#include "Data.h"

namespace mspath{

void
FixedTimeStepsGenerator::makeStepsFor(Environment*pEnvironment){
Data&data= pEnvironment->data();
pEnvironment->clearTimeSteps();
TIObs obnext= pEnvironment->begin();
TIObs obend= pEnvironment->end();
TInt iti;
if(obnext==obend)

return;
TInt itnext= integerTime(data.time(obnext));
makeStep(timeFromInteger(itnext),true,obnext,pEnvironment);
TInt itlast= itnext;
obnext++;
while(obnext!=obend){
itnext= integerTime(data.time(obnext));
if(itnext==itlast){
std::stringstream s;
s<<"Observation "<<obnext<<" (case "<<pEnvironment->id()
<<") overlaps in rounded time"
" with previous observation. Aborting.";
throw ExtraDataError(s.str());
}
for(iti= itlast+1;iti<itnext;iti++)
makeStep(timeFromInteger(iti),false,obnext-1,pEnvironment);
makeStep(timeFromInteger(itnext),true,obnext,pEnvironment);
itlast= itnext;
obnext++;
}
}
/* 260: */
#line 9140 "mspath.web"

FixedTimeStepsGenerator::TInt
FixedTimeStepsGenerator::integerTime(Time t){
return static_cast<TInt> (std::floor(t/myStepAsDouble+0.5));
}

Time
FixedTimeStepsGenerator::timeFromInteger(
FixedTimeStepsGenerator::TInt i){

return boost::rational_cast<Time> (i*myStepSize);
}

/* :260 */
#line 9195 "mspath.web"


}

/* :261 */
#line 13435 "mspath.web"

/* 129: */
#line 5636 "mspath.web"

