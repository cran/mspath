
#ifndef Recorder_h
#define Recorder_h 1

#include "Path.h"

namespace mspath{

class Recorder{
public:
/* 156: */
#line 6521 "mspath.web"


typedef Path TPath;
typedef unsigned long int TCount;
/* :156 */
#line 6508 "mspath.web"

/* 157: */
#line 6526 "mspath.web"

virtual~Recorder(){}
/* :157 */
#line 6509 "mspath.web"

/* 158: */
#line 6529 "mspath.web"

#if 0
Id id()const{return myCurrentId;}
#endif
/* :158 */
#line 6510 "mspath.web"

/* 159: */
#line 6559 "mspath.web"



virtual void startSession(){}


virtual void startCase()= 0;


virtual void startTree(double initialProbability){}



virtual void goodNode()= 0;







virtual void goodPath(TPath&path)= 0;



virtual void impermissible()= 0;


virtual void finishTree(){}


virtual void finishCase()= 0;


virtual void finishSession()= 0;
/* :159 */
#line 6511 "mspath.web"

private:
/* 160: */
#line 6595 "mspath.web"




/* :160 */
#line 6513 "mspath.web"

};

}

#endif 

/* :155 */
#line 13412 "mspath.web"

/* 161: */
#line 6614 "mspath.web"

