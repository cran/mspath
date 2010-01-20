
#ifndef Path_h
#define Path_h

#include <iostream> 

#include <boost/ptr_container/ptr_vector.hpp> 
#include <memory> 
#include "basic.h"
#include "Node.h"
#include "NodeFactory.h"

namespace mspath{

class Path:public boost::ptr_vector<Node,boost::view_clone_allocator> {
public:
Path(size_t nPathVars= 0u):
mypNF(new NodeFactory<Node> (nPathVars)){}

Path(const Path&p);
Path&operator= (const Path&p);
Path*clone()const{return new Path(*this);}

Node*pathPush(const StatePoint&theSP,
const TimePoint&theTP);
void pathPop();


void clear(){


while(!empty())
pathPop();
}

protected:





void push_back(Node*);

Path&innerAssign(const Path&p);
typedef boost::ptr_vector<Node,boost::view_clone_allocator> Super;
std::auto_ptr<NodeFactory<Node> > mypNF;
};

std::ostream&
operator<<(std::ostream&s,const Path&p);

}
#endif 


/* :97 */
#line 13403 "mspath.web"

/* 257: */
#line 9036 "mspath.web"

