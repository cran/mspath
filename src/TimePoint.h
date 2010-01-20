
#ifndef TimePoint_h
#define TimePoint_h 1

#include "basic.h"
#include <ostream> 

namespace mspath{
class TimePoint{
public:
/* 134: */
#line 5798 "mspath.web"

typedef IObservation TIObservation;

/* :134 */
#line 5785 "mspath.web"

/* 135: */
#line 5806 "mspath.web"

TimePoint(){}
TimePoint(Time t,bool isObservationTime,const IObservation&iobs):
myTime(t),
myMatchObservation(isObservationTime),
myIObservation(iobs){}
/* :135 */
#line 5786 "mspath.web"

/* 136: */
#line 5813 "mspath.web"

Time time()const{return myTime;}
bool matchesObservation()const{return myMatchObservation;}
IObservation iObservation()const{return myIObservation;}

/* :136 */
#line 5787 "mspath.web"

/* 138: */
#line 5835 "mspath.web"

friend bool operator<(const TimePoint&lhs,const TimePoint&rhs)
{
return lhs.myTime<rhs.myTime;
}


friend bool operator==(const TimePoint&lhs,const TimePoint&rhs)
{
return lhs.myTime==rhs.myTime&&
lhs.myMatchObservation==rhs.myMatchObservation&&
lhs.myIObservation==rhs.myIObservation;
}

/* :138 */
#line 5788 "mspath.web"

protected:
/* 137: */
#line 5827 "mspath.web"

Time myTime;
bool myMatchObservation;
IObservation myIObservation;

/* :137 */
#line 5790 "mspath.web"

};
/* 139: */
#line 5850 "mspath.web"

std::ostream&
operator<<(std::ostream&ostr,const TimePoint&tp);

/* :139 */
#line 5792 "mspath.web"


}
#endif 

/* :133 */
#line 13398 "mspath.web"

/* 276: */
#line 9690 "mspath.web"

