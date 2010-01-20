
#ifndef SimpleRecorder_h
#define SimpleRecorder_h 1
#include "Recorder.h"

#include <ostream> 

#include "Environment.h"
#include "MSPathError.h"
#include "Path.h"

namespace mspath{

class SimpleRecorder:public Recorder{
public:
/* 162: */
#line 6647 "mspath.web"

SimpleRecorder(Environment*const thepEnv):
myNCases(0),myNPaths(0),myNNodes(0),myNBads(0),
myNPathNodes(0),mypEnv(thepEnv){}

/* :162 */
#line 6630 "mspath.web"

/* 163: */
#line 6653 "mspath.web"

TCount goodNodes()const{return myNNodes;}
TCount goodPaths()const{return myNPaths;}
TCount cases()const{return myNCases;}
TCount badNodes()const{return myNBads;}
double averagePathLength()const{
return static_cast<double> (myNPathNodes)/myNPaths;}
TCount goodPathNodes()const{return myNPathNodes;}
Environment&environment(){return*mypEnv;}

/* :163 */
#line 6631 "mspath.web"

/* 164: */
#line 6675 "mspath.web"


virtual void startSession();


virtual void startCase(){
++myNCases;
myNCasePaths= 0;
}

virtual void startTree(double p){




}



virtual void goodNode(){
}


virtual void goodPath(TPath&path){
evaluateNode(environment().currentNode());
++myNCasePaths;
myNPathNodes+= path.size();
}

/* 165: */
#line 6723 "mspath.web"

protected:



virtual void evaluateNode(Node&node){
if(node.alreadyCountedAsGood())
return;
if(!node.isRoot()){
Node*pn= node.previous();
environment().setCurrentNode(pn);
evaluateNode(*pn);
environment().setCurrentNode(node);
}
evaluateAnyNode(node);
}




virtual void evaluateAnyNode(Node&node){
myNNodes++;
node.countAsGood();
}

public:


/* :165 */
#line 6704 "mspath.web"




virtual void impermissible(){++myNBads;}

virtual void finishCase()throw(DataModelInconsistency){
if(myNCasePaths<=0)

throw DataModelInconsistency(environment().id(),
" Observed path can't come from Model.");
myNPaths+= myNCasePaths;
}
virtual void finishSession(){}
virtual void finishTree(){}

/* :164 */
#line 6632 "mspath.web"

protected:
/* 166: */
#line 6761 "mspath.web"

TCount myNCases;
TCount myNPaths;
TCount myNNodes;
TCount myNBads;
TCount myNPathNodes;
Environment*mypEnv;

TCount myNCasePaths;

/* :166 */
#line 6634 "mspath.web"

};

/* 167: */
#line 6772 "mspath.web"

std::ostream&operator<<(std::ostream&,const SimpleRecorder&);

/* :167 */
#line 6637 "mspath.web"

}
#endif

/* :161 */
#line 13413 "mspath.web"

/* 267: */
#line 9418 "mspath.web"

