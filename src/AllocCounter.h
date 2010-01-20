
#ifndef AllocCounter_h
#define AllocCounter_h 1

#include <cstddef>   
#include <ostream> 
#include <typeinfo> 

using std::size_t;

namespace mspath{
template<class C> class AllocCounter:public C{
public:
/* 329: */
#line 11839 "mspath.web"

AllocCounter():myAllocID(myAlloc++),C(){}
template<typename A> 
AllocCounter(const A&a):
C(a),myAllocID(myAlloc++){}

template<typename A1,typename A2> 
AllocCounter(const A1&a1,const A2&a2):
C(a1,a2),myAllocID(myAlloc++){}

template<typename A1,typename A2,typename A3> 
AllocCounter(const A1&a1,const A2&a2,const A3&a3):
C(a1,a2,a3),myAllocID(myAlloc++){}


AllocCounter(const AllocCounter&x):
C(x),myAllocID(myAlloc++){}

virtual~AllocCounter(){myFree++;}

/* :329 */
#line 11790 "mspath.web"

/* 330: */
#line 11861 "mspath.web"

static size_t nAlloc(){return myAlloc;}
static size_t nFree(){return myFree;}
static size_t nInUse(){return myAlloc-myFree;}

/* :330 */
#line 11791 "mspath.web"

/* 331: */
#line 11868 "mspath.web"

static void resetAllocCounts(){
myAlloc= 0u;
myFree= 0u;
}


static std::ostream&statsPrint(std::ostream&str){
str<<typeid(C).name()<<" has "
<<nInUse()<<" instances after "<<nAlloc()
<<" constructions and "<<nFree()<<" destructions.";
return str;
}
/* :331 */
#line 11792 "mspath.web"

/* 333: */
#line 11889 "mspath.web"


AllocCounter&operator= (const AllocCounter&rhs){
this->C::operator= (rhs);
return*this;
}

size_t allocID()const{return myAllocID;}

/* :333 */
#line 11793 "mspath.web"

protected:
/* 332: */
#line 11883 "mspath.web"

static size_t myAlloc;
static size_t myFree;
size_t myAllocID;

/* :332 */
#line 11795 "mspath.web"

};

/* 334: */
#line 11900 "mspath.web"

template<class C> 
std::ostream&operator<<(std::ostream&str,const AllocCounter<C> &c){
str<<"Instance "<<c.allocID()<<":"<<std::endl
<<static_cast<const C&> (c)<<std::endl
<<c.nAlloc()<<" class instances constructed and "
<<c.nFree()<<" freed."<<std::endl;
return str;
}


/* :334 */
#line 11798 "mspath.web"

/* 335: */
#line 11912 "mspath.web"

template<class C> size_t AllocCounter<C> ::myAlloc= 0u;
template<class C> size_t AllocCounter<C> ::myFree= 0u;


/* :335 */
#line 11799 "mspath.web"

}

#endif


/* :328 */
#line 11548 "mspath.web"

/* 337: */
#line 11981 "mspath.web"

