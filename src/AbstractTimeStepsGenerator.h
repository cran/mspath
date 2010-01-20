
#ifndef AbstractTimeStepsGenerator_h
#define AbstractTimeStepsGenerator_h 1

#include "basic.h"
#include "Environment.h"
#include "TimePoint.h"

namespace mspath{

class AbstractTimeStepsGenerator{
public:
virtual void makeStepsFor(Environment*pEnvironment)= 0;
virtual~AbstractTimeStepsGenerator(){};

protected:

virtual void makeStep(Time t,bool trueObs,TimePoint::TIObservation i,
Environment*pEnv);
};

}
#endif

/* :127 */
#line 13432 "mspath.web"

/* 259: */
#line 9107 "mspath.web"

