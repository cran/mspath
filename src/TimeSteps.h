
#ifndef TimeSteps_h
#define TimeSteps_h 1

#include <boost/ptr_container/ptr_vector.hpp> 
#include "basic.h"
#include "TimePoint.h"

namespace mspath{

class TimeSteps:
public boost::ptr_vector<TimePoint> {
public:
typedef TimePoint TTimePoint;
typedef iterator TiTimePoint;

static const TimePoint&timePoint(const TiTimePoint&i){
return*i;}

};
}

#endif 


/* :132 */
#line 13400 "mspath.web"

/* 104: */
#line 5005 "mspath.web"

