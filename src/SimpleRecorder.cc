
#include "SimpleRecorder.h"
namespace mspath{

void SimpleRecorder::startSession(){
myNCases= 0;
myNPaths= 0;
myNNodes= 0;
myNBads= 0;
myNPathNodes= 0;
}

std::ostream&operator<<(std::ostream&s,const SimpleRecorder&r){
std::streamsize prec= s.precision();
s.precision(3);
s<<r.cases()<<" cases with "<<r.goodPaths()
<<" good paths and "<<r.goodNodes()<<" good nodes."<<std::endl
<<"The paths had "<<r.goodPathNodes()
<<" total nodes, so we would calculate only "
<<100.0*r.goodNodes()/r.goodPathNodes()<<"%."<<std::endl
<<"Average path length = "<<r.averagePathLength()
<<".  "<<r.badNodes()<<" bad nodes."<<std::endl;
s.precision(prec);
return s;
}

}


/* :267 */
#line 13414 "mspath.web"

/* 168: */
#line 6793 "mspath.web"

