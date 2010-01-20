
#ifndef Environment_h
#define Environment_h 1

#include <memory> 
#include <valarray>   

#include "basic.h"
#include "Covariates.h"
#include "Data.h"
#include "MSPathError.h"
#include "Path.h"
#include "ScratchPad.h"
#include "TimeSteps.h"

namespace mspath{
class Environment:public ScratchPad{
public:
/* 141: */
#line 5977 "mspath.web"

typedef Node TNode;
typedef TimePoint TTimePoint;

/* :141 */
#line 5964 "mspath.web"

/* 142: */
#line 5982 "mspath.web"

Environment(Data*pData,Path*pPath= new Path(static_cast<size_t> (0u))):
mypPath(pPath),mypData(pData),
mypDataIterator(new DataIterator(pData)){}

virtual~Environment(){
delete mypPath;
}

/* :142 */
#line 5965 "mspath.web"

/* 143: */
#line 6004 "mspath.web"

Id id()const{return dataIterator().subject();}
Path&path(){return*mypPath;}
TimeSteps&timeSteps(){return myTimeSteps;}
Data&data(){return*mypData;}


const Path&path()const{return*mypPath;}
const TimeSteps&timeSteps()const{return myTimeSteps;}
const Data&data()const{return*mypData;}
const TNode&currentNode()const{return*mypCurrentNode;}

Data::TIObs begin()const{return dataIterator().begin();}
Data::TIObs end()const{return dataIterator().end();}

/* 144: */
#line 6041 "mspath.web"


TNode&currentNode(){return*mypCurrentNode;}

const TimePoint&timePoint()const{
return timePoint(currentNode());
}



IObservation iObservation()const{
return iObservation(currentNode());
}



bool matchesObservation()const{
return matchesObservation(currentNode());
}


bool hasObservedState()const{
return hasObservedState(currentNode());
}



ObsState observedState()const{
return observedState(currentNode());
}




/* :144 */
#line 6019 "mspath.web"

/* 145: */
#line 6078 "mspath.web"


const TimePoint&timePoint(const Node&node)const{
return node.timePoint();
}



IObservation iObservation(const Node&node)const{
return timePoint(node).iObservation();
}




bool matchesObservation(const Node&node)const{
return timePoint(node).matchesObservation();
}


bool hasObservedState(const Node&node)const{
return matchesObservation(node)&&
data().hasObservedState(iObservation(node));
}




ObsState observedState(const Node&node)const{
return data().state(iObservation(node));
}





ModelData&activeModelData(Node&node){
return node.modelData();
}


double&activeEvaluationData(Node&node){
return node.evaluationData();
}


/* :145 */
#line 6020 "mspath.web"

/* 146: */
#line 6126 "mspath.web"








static TimePoint&timePoint(
const TimeSteps::iterator&i){
return*i;}

static TNode&node(
const Path::iterator&i){
return*i;}


/* :146 */
#line 6021 "mspath.web"




TNode&lastNode(){
return path().back();
}
const TNode&lastNode()const{
return path().back();
}



/* :143 */
#line 5966 "mspath.web"

/* 147: */
#line 6149 "mspath.web"

void pathPush(const StatePoint&theSP,const TimePoint&theTP){
Node*pn= path().pathPush(theSP,theTP);
setCurrentNode(pn);
}

void pathPop(){
Node*p= lastNode().previous();
path().pathPop();
setCurrentNode(p);
}

void pathClear(){
path().clear();
setCurrentNode(0);
}

void setCurrentNode(Node&theNode){
mypCurrentNode= &theNode;}



void setCurrentNode(Node*thepNode){
mypCurrentNode= thepNode;}

/* :147 */
#line 5967 "mspath.web"

/* 148: */
#line 6190 "mspath.web"

void clearTimeSteps(){timeSteps().clear();}



bool next()throw(DataIteratorError)
{return dataIterator().next();}


void startSession(){
dataIterator().startSession();
}




void setSubset(SubsetDataIterator::IDList&pSubset){
mypDataIterator.reset(
new SubsetDataIterator(&(data()),pSubset));
newModel();
}


void setAll(void){
mypDataIterator.reset(
new DataIterator(&(data())));
newModel();
}




void newModel(void){
clear();}


/* :148 */
#line 5968 "mspath.web"

/* 150: */
#line 6286 "mspath.web"




template<typename Prob> 
State randomDraw(const std::valarray<Prob> &theDensity);


/* :150 */
#line 5969 "mspath.web"

protected:
/* 149: */
#line 6233 "mspath.web"

AbstractDataIterator&dataIterator(){
return*mypDataIterator;};

const AbstractDataIterator&dataIterator()const{
return*mypDataIterator;};

Path*mypPath;
TimeSteps myTimeSteps;
Data*mypData;
TNode*mypCurrentNode;
std::auto_ptr<AbstractDataIterator> mypDataIterator;

/* :149 */
#line 5971 "mspath.web"

};
}
#endif 

/* :140 */
#line 13405 "mspath.web"

/* 277: */
#line 9793 "mspath.web"

