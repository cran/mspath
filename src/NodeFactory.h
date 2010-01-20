
#ifndef NodeFactory_h
#define NodeFactory_h 1
#include "basic.h"
#include "Node.h"
#include <vector> 
#include <cassert> 


namespace mspath{

class TimePoint;
class Path;
class NodeFactoryTester;

template<class Node> class NodeFactory{
public:
/* 99: */
#line 4883 "mspath.web"


NodeFactory(size_t nPath= 0u):
myNPath(nPath),myNext(0){}


NodeFactory*clone()const{
return new NodeFactory<Node> (myNPath);
}

~NodeFactory(){
for(size_t i= 0u;i<myNodeps.size();i++)
delete myNodeps[i];
}

/* :99 */
#line 4867 "mspath.web"

/* 100: */
#line 4904 "mspath.web"

Node*createNode(const StatePoint&theSP,const TimePoint&theTP){
Node*pNode;
if(myNext+1<=myNodeps.size()){
pNode= myNodeps[myNext];
pNode->recreate(theSP,theTP);
}else{
assert(myNext==myNodeps.size());
pNode= new Node(theSP,theTP,myNPath);
myNodeps.push_back(pNode);
}
myNext++;
return pNode;
}

Node*createNode(const Node&theNode){
Node*pNode;
if(myNext+1<=myNodeps.size()){
pNode= myNodeps[myNext];
(*pNode)= theNode;
}else{
assert(myNext==myNodeps.size());
pNode= new Node(theNode);
myNodeps.push_back(pNode);
}
myNext++;
return pNode;
}

/* :100 */
#line 4868 "mspath.web"

/* 101: */
#line 4934 "mspath.web"

void destroyNode(Node*thepNode){
assert(myNext> 0);
myNext--;
assert(thepNode==myNodeps[myNext]);
}

/* :101 */
#line 4869 "mspath.web"

protected:
/* 102: */
#line 4946 "mspath.web"

friend class Path;
friend class NodeFactoryTester;
void reset(){
myNext= 0u;
}

/* :102 */
#line 4871 "mspath.web"

/* 103: */
#line 4958 "mspath.web"

size_t myNPath;
std::vector<Node*> myNodeps;
size_t myNext;

/* :103 */
#line 4872 "mspath.web"

};
}
#endif

/* :98 */
#line 13402 "mspath.web"

/* 97: */
#line 4747 "mspath.web"

