
#ifndef PrimitiveHistoryComputer_h
#define PrimitiveHistoryComputer_h 1

#include "HistoryComputer.h"

namespace mspath{
/* 120: */
#line 5390 "mspath.web"

class PrimitiveHistoryComputer:public HistoryComputer{
public:
PrimitiveHistoryComputer(const std::string&name,
double theInitialTime= 0.0):
HistoryComputer(),
myName(name),myInitialTime(theInitialTime){}

virtual~PrimitiveHistoryComputer(){}



virtual const std::string&name()const{return myName;}


virtual bool matches(const std::string&term)const{
return term==myName;}

protected:
const std::string myName;
double myInitialTime;

};


/* :120 */
#line 5380 "mspath.web"

/* 121: */
#line 5416 "mspath.web"

class TimeInStateComputer:public PrimitiveHistoryComputer{
public:
TimeInStateComputer(const std::string&theName,
double theInitialTime= 0.0):
PrimitiveHistoryComputer(theName,theInitialTime){}

virtual~TimeInStateComputer(){}
virtual void evaluate(Environment&theEnv,Node&theNode)
throw(std::out_of_range);
};


/* :121 */
#line 5381 "mspath.web"

/* 122: */
#line 5430 "mspath.web"

class TimeInPreviousStatesComputer:public PrimitiveHistoryComputer{
public:
TimeInPreviousStatesComputer(const std::string&theName,
double theInitialTime= 0.0):
PrimitiveHistoryComputer(theName,theInitialTime){}

virtual~TimeInPreviousStatesComputer(){}
virtual void evaluate(Environment&theEnv,Node&theNode)
throw(std::out_of_range);
};


/* :122 */
#line 5382 "mspath.web"

/* 123: */
#line 5444 "mspath.web"

class TimeSinceOriginComputer:public PrimitiveHistoryComputer{
public:
TimeSinceOriginComputer(const std::string&theName,
double theInitialTime= 0.0):
PrimitiveHistoryComputer(theName,theInitialTime){}

virtual~TimeSinceOriginComputer(){}
virtual void evaluate(Environment&theEnv,Node&theNode)
throw(std::out_of_range);

};

/* :123 */
#line 5383 "mspath.web"


}
#endif


/* :119 */
#line 13450 "mspath.web"

/* 265: */
#line 9338 "mspath.web"

