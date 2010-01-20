
#include "AbstractTimeStepsGenerator.h"
namespace mspath{


void AbstractTimeStepsGenerator::makeStep(
Time t,
bool trueObs,
TimePoint::TIObservation i,
Environment*pEnv)
{
TimePoint*p= new TimePoint(t,trueObs,i);
pEnv->timeSteps().push_back(p);
}

}

/* :259 */
#line 13433 "mspath.web"

/* 128: */
#line 5575 "mspath.web"

