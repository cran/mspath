
#ifndef CompressedTimeStepsGenerator_h
#define CompressedTimeStepsGenerator_h 1

#include <vector> 

#include "FixedTimeStepsGenerator.h"

namespace mspath{
class CompressedTimeStepsGenerator:public FixedTimeStepsGenerator{
public:

typedef std::vector<TIObs> TDuplicates;


CompressedTimeStepsGenerator(TInt stepNumerator= 1,
TInt stepDenominator= 1):
FixedTimeStepsGenerator(stepNumerator,stepDenominator){};
virtual~CompressedTimeStepsGenerator(){};



virtual void makeStepsFor(Environment*pEnvironment);

virtual void clear(){myDuplicates.clear();}



virtual const TDuplicates&duplicates()const{
return myDuplicates;}

protected:
TDuplicates myDuplicates;

/* 130: */
#line 5680 "mspath.web"


TIObs myoblast,myobnext,myobend;
TInt myitlast,myitnext;

void lastObs(const Data&);

/* :130 */
#line 5671 "mspath.web"

};
}
#endif 


/* :129 */
#line 13436 "mspath.web"

/* 262: */
#line 9209 "mspath.web"

