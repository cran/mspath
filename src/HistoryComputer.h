
#ifndef HistoryComputer_h
#define HistoryComputer_h 1

#include "basic.h"
#include "MSPathError.h"

#include <iostream>   
#include <stdexcept> 
#include <string> 

namespace mspath{
class Environment;
class Node;

class HistoryComputer{
public:
/* 114: */
#line 5284 "mspath.web"

HistoryComputer():
myIsRequired(false),myIsCovariate(false),
myCovariateIndex(0),myDataIndex(0){}

virtual~HistoryComputer(){}


/* :114 */
#line 5268 "mspath.web"

/* 115: */
#line 5296 "mspath.web"




virtual const std::string&name()const= 0;


virtual bool matches(const std::string&term)const= 0;




virtual void makeRequired(){myIsRequired= true;}


virtual void makeCovariate(size_t index)throw(DuplicateTerm){
if(isCovariate())
throw DuplicateTerm();
makeRequired();
myIsCovariate= true;
myCovariateIndex= index;
}





bool isRequired()const{return myIsRequired;}
bool isCovariate()const{return myIsCovariate;}

size_t covariateIndex()const{return myCovariateIndex;}



virtual HistoryComputer*requires()const{return 0;}

/* :115 */
#line 5269 "mspath.web"

/* 116: */
#line 5340 "mspath.web"


void setDataIndex(size_t index){myDataIndex= index;}
size_t dataIndex()const{return myDataIndex;}


/* :116 */
#line 5270 "mspath.web"

/* 118: */
#line 5355 "mspath.web"

friend std::ostream&operator<<(std::ostream&ostr,const
HistoryComputer&history){
ostr<<history.name()<<" saved at index "<<history.dataIndex();
return ostr;
}

/* :118 */
#line 5271 "mspath.web"




virtual void evaluate(Environment&theEnv,Node&theNode)
throw(std::out_of_range)= 0;

protected:
/* 117: */
#line 5347 "mspath.web"

bool myIsRequired;
bool myIsCovariate;
size_t myCovariateIndex;
size_t myDataIndex;


/* :117 */
#line 5279 "mspath.web"

};
}
#endif
/* :113 */
#line 13449 "mspath.web"

/* 119: */
#line 5372 "mspath.web"

