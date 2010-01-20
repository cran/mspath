
#include "TimePoint.h"
namespace mspath{

std::ostream&
operator<<(std::ostream&ostr,const TimePoint&tp){
ostr<<"TimePoint("<<tp.time()
<<", "<<tp.matchesObservation()
<<", "<<tp.iObservation()<<")";
return ostr;
}
}


/* :276 */
#line 13399 "mspath.web"

/* 132: */
#line 5728 "mspath.web"

