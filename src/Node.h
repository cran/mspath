
#ifndef Node_h
#define Node_h 1

#include <ostream> 

#include "basic.h"
#include "TimePoint.h"

namespace mspath{

class Path;
class NodeFactoryTester;

class Node{
public:
/* 105: */
#line 5054 "mspath.web"

typedef Node TNode;
typedef ModelData TModelData;
typedef EvaluationData TEvaluationData;


/* :105 */
#line 5022 "mspath.web"

/* 106: */
#line 5084 "mspath.web"

Node(const StatePoint&sp,const TimePoint&tp,
size_t theNModelData= 0u):
mySP(sp),myMD(theNModelData),myED(0),
myPriorp(0),mypTP(&tp),mySeenGood(false){}

Node&recreate(const StatePoint&sp,const TimePoint&tp){
mySP= sp;
mypTP= &tp;
myPriorp= 0;
mySeenGood= false;
return*this;
}

void setState(State s){mySP.setState(s);}


/* :106 */
#line 5023 "mspath.web"

/* 107: */
#line 5108 "mspath.web"



const StatePoint&statePoint()const{return mySP;}
const Time time()const{return timePoint().time();}
const State&state()const{return mySP.state();}
StatePoint&statePoint(){return mySP;}
Time time(){return timePoint().time();}
State state(){return mySP.state();}
bool alreadyCountedAsGood()const{return mySeenGood;}

TNode*previous()const{
return myPriorp;}

const TimePoint&timePoint()const{
return*mypTP;}

ModelData&modelData(){return myMD;}
EvaluationData&evaluationData(){return myED;}




/* :107 */
#line 5024 "mspath.web"

/* 108: */
#line 5132 "mspath.web"

bool isRoot(){return myPriorp==0;}

/* :108 */
#line 5025 "mspath.web"

/* 110: */
#line 5148 "mspath.web"

void countAsGood(){mySeenGood= true;}

/* :110 */
#line 5026 "mspath.web"

/* 111: */
#line 5152 "mspath.web"

friend bool operator<(const Node&lhs,const Node&rhs){
if(lhs.statePoint()<rhs.statePoint())
return true;
if(!(lhs.statePoint()==rhs.statePoint()))
return false;
return lhs.timePoint()<rhs.timePoint();
}

friend bool operator==(const Node&lhs,const Node&rhs){
if(!(lhs.statePoint()==rhs.statePoint()))
return false;
return lhs.timePoint()==rhs.timePoint();
}

friend std::ostream&operator<<(std::ostream&ostr,const Node&n){
ostr<<"Node("<<n.state()<<", "<<n.time();
if(!n.alreadyCountedAsGood())
ostr<<" not";
ostr<<" already counted good, md "<<n.myMD
<<", ed "<<n.myED<<")";
return ostr;
}

/* :111 */
#line 5027 "mspath.web"

friend class Path;
friend class NodeFactoryTester;
protected:
/* 112: */
#line 5177 "mspath.web"

void setNoPrevious(){myPriorp= static_cast<Node*> (0);}
void setPrevious(Node*const p){myPriorp= p;}


/* :112 */
#line 5031 "mspath.web"

/* 109: */
#line 5139 "mspath.web"

StatePoint mySP;
ModelData myMD;
EvaluationData myED;
TNode*myPriorp;
const TimePoint*mypTP;
bool mySeenGood;

/* :109 */
#line 5032 "mspath.web"


};

}
#endif  


/* :104 */
#line 13401 "mspath.web"

/* 98: */
#line 4849 "mspath.web"

