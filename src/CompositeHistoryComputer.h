
#ifndef CompositeHistoryComputer_h
#define CompositeHistoryComputer_h 1
#include "HistoryComputer.h"
namespace mspath{
/* 125: */
#line 5472 "mspath.web"

class CompositeHistoryComputer:public HistoryComputer{
public:

CompositeHistoryComputer(const std::string&theFunctionName,
HistoryComputer&theTarget):
HistoryComputer(),
myName(theFunctionName+"("+theTarget.name()+")"),
myTarget(theTarget){}

virtual~CompositeHistoryComputer(){}



virtual const std::string&name()const{return myName;}


virtual bool matches(const std::string&term)const{
return term==myName;}


virtual void makeRequired(){
HistoryComputer::makeRequired();
target().makeRequired();
}

HistoryComputer&target(){return myTarget;}

virtual HistoryComputer*requires()const{return&myTarget;}

protected:
const std::string myName;
HistoryComputer&myTarget;
};


/* :125 */
#line 5467 "mspath.web"

/* 126: */
#line 5510 "mspath.web"

class LnHistoryComputer:public CompositeHistoryComputer{
public:
LnHistoryComputer(const std::string&theFunctionName,
HistoryComputer&theTarget):
CompositeHistoryComputer(theFunctionName,theTarget){}

virtual~LnHistoryComputer(){}
virtual void evaluate(Environment&theEnv,Node&theNode)
throw(std::out_of_range);
};

#endif
/* :126 */
#line 5468 "mspath.web"

}

/* :124 */
#line 13452 "mspath.web"

/* 266: */
#line 9399 "mspath.web"

