
#ifndef TimeStepsGenerator_h
#define TimeStepsGenerator_h 1

#include "AbstractTimeStepsGenerator.h"

namespace mspath{
class TimeStepsGenerator:public AbstractTimeStepsGenerator{
public:

TimeStepsGenerator(double stepSize):
myStepSize(stepSize){};



virtual void makeStepsFor(Environment*pEnvironment);
protected:
double myStepSize;
};
}
#endif 


/* :131 */
#line 13438 "mspath.web"

/* 264: */
#line 9290 "mspath.web"

