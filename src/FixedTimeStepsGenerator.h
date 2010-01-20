
#ifndef FixedTimeStepsGenerator_h
#define FixedTimeStepsGenerator_h 1

#include <boost/rational.hpp> 
#include <stdexcept> 

#include "AbstractTimeStepsGenerator.h"

namespace mspath{
class FixedTimeStepsGenerator:public AbstractTimeStepsGenerator{
public:

typedef Data::TIObs TIObs;
typedef std::vector<TIObs> TDuplicates;
typedef int TInt;
typedef boost::rational<TInt> TRational;


FixedTimeStepsGenerator(TInt stepNumerator= 1,
TInt stepDenominator= 1):
myStepSize(TRational(stepNumerator,stepDenominator)),
myStepAsDouble(boost::rational_cast<double> (myStepSize)){}

virtual~FixedTimeStepsGenerator(){};




virtual void makeStepsFor(Environment*pEnvironment);


class ExtraDataError:public std::invalid_argument{
public:
ExtraDataError(const std::string&msg):
std::invalid_argument(msg){};
};

protected:
TRational myStepSize;
double myStepAsDouble;


TInt integerTime(Time t);

Time timeFromInteger(TInt i);

};
}
#endif 

/* :128 */
#line 13434 "mspath.web"

/* 261: */
#line 9154 "mspath.web"

