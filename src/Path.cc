
#include <algorithm> 
#include "Path.h"
namespace mspath{


Path::Path(const Path&p):
mypNF(p.mypNF->clone()){
innerAssign(p);
}

Path&Path::operator= (const Path&p){
if(this!=&p){
clear();
mypNF->reset();
innerAssign(p);
}
return*this;
}

Node*
Path::pathPush(const StatePoint&theSP,const TimePoint&theTP){
Node*pn= mypNF->createNode(theSP,theTP);
push_back(pn);
return pn;
}

void
Path::pathPop(){
mypNF->destroyNode(pop_back().release());

}



void Path::push_back(Node*pn){
if(empty())
pn->setNoPrevious();
else

pn->setPrevious(&(this->back()));
Super::push_back(pn);
}

Path&Path::innerAssign(const Path&p){
Path::const_iterator i= p.begin();
while(i!=p.end()){
push_back(mypNF->createNode(*i));
++i;
}
return*this;
}


std::ostream&operator<<(std::ostream&s,const Path&p){
Path::const_iterator i,e(p.end());
for(i= p.begin();i!=e;++i){
s<<i->state()<<" ";
};
return s;
}


}


/* :257 */
#line 13404 "mspath.web"

/* 140: */
#line 5945 "mspath.web"

