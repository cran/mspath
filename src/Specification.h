
#ifndef Specification_h
#define Specification_h 1

#include <iosfwd> 

#include "basic.h"
#include "LinearProduct.h"
#include "MSPathError.h"
#include "ScratchDataProducer.h"

namespace mspath{
class Environment;
/* 64: */
#line 3606 "mspath.web"


class AbstractSpecification:public ScratchDataProducer{
public:
/* 65: */
#line 3620 "mspath.web"

AbstractSpecification(Bool2D*thepPermissible):
mypPermissible(thepPermissible){}

virtual~AbstractSpecification(){
delete mypPermissible;
}

/* :65 */
#line 3610 "mspath.web"

/* 66: */
#line 3632 "mspath.web"


virtual const Double2D&evaluate(Environment&theEnv)const= 0;

virtual bool
isChanged(Environment&theEnv,ScratchData*thepMemento)= 0;

virtual void memento(Environment&theEnv,ScratchData**
theppMemento)const= 0;

friend std::ostream&operator<<(
std::ostream&ostr,
const mspath::AbstractSpecification&spec);



/* :66 */
#line 3611 "mspath.web"

/* 67: */
#line 3653 "mspath.web"




size_t nStates()const{
return permissible().ncols();
}




bool isPermissible(State f,State t)const{
return permissible()(f,t);
}

/* :67 */
#line 3612 "mspath.web"

protected:
/* 68: */
#line 3670 "mspath.web"

Bool2D&permissible()const{
return*mypPermissible;
}

/* :68 */
#line 3614 "mspath.web"

/* 69: */
#line 3681 "mspath.web"

Bool2D*mypPermissible;



/* :69 */
#line 3615 "mspath.web"

};


/* :64 */
#line 3589 "mspath.web"

/* 70: */
#line 3702 "mspath.web"


class SimpleSpecification:public AbstractSpecification{
public:
/* 71: */
#line 3714 "mspath.web"

SimpleSpecification(ConstantLinearProduct*thepLP,
size_t thenStates,
Bool2D*thepPermissible)throw(InconsistentModel);

virtual~SimpleSpecification(){
delete mypLP;
}


virtual void release(ScratchPad*thePad)const{}
/* :71 */
#line 3706 "mspath.web"

/* 72: */
#line 3728 "mspath.web"


virtual const Double2D&evaluate(Environment&theEnv)const{
return myResult;
}

virtual bool
isChanged(Environment&theEnv,ScratchData*thepMemento){
return thepMemento==0;
}

virtual void
memento(Environment&theEnv,ScratchData**theppMemento)const
{
if(*theppMemento==0)
*theppMemento= new Memento();
}

friend std::ostream&operator<<(
std::ostream&ostr,
const mspath::SimpleSpecification&spec);


/* :72 */
#line 3707 "mspath.web"

protected:
/* 73: */
#line 3752 "mspath.web"

ConstantLinearProduct&linearProduct()const{
return*mypLP;
}

/* :73 */
#line 3709 "mspath.web"

/* 75: */
#line 3769 "mspath.web"

struct Memento:public ScratchData{
};

/* :75 */
#line 3710 "mspath.web"

/* 74: */
#line 3761 "mspath.web"

ConstantLinearProduct*mypLP;
Double2D myResult;

/* :74 */
#line 3711 "mspath.web"

};
/* :70 */
#line 3590 "mspath.web"

/* 76: */
#line 3783 "mspath.web"


class Specification:public AbstractSpecification{
public:
/* 77: */
#line 3800 "mspath.web"


Specification(AbstractLinearProduct*thepLP,
Bool2D*thepPermissible):
AbstractSpecification(thepPermissible),
mypLP(thepLP){}

~Specification(){
delete mypLP;
}

virtual void release(ScratchPad*thePad)const{
ScratchDataProducer::release(thePad);
linearProduct().release(thePad);
}

/* :77 */
#line 3787 "mspath.web"

/* 78: */
#line 3817 "mspath.web"


virtual const Double2D&evaluate(Environment&theEnv)const;

virtual bool
isChanged(Environment&theEnv,ScratchData*thepMemento){
return linearProduct().isChanged(theEnv,thepMemento);
}

virtual void
memento(Environment&theEnv,ScratchData**theppMemento)const{
linearProduct().memento(theEnv,theppMemento);
}

friend std::ostream&operator<<(
std::ostream&ostr,
const mspath::Specification&spec);

/* :78 */
#line 3788 "mspath.web"


protected:
/* 79: */
#line 3837 "mspath.web"

AbstractLinearProduct&linearProduct()const{
return*mypLP;
}

/* :79 */
#line 3791 "mspath.web"

/* 80: */
#line 3843 "mspath.web"


void computeResult(Double2D&theResult,
const Double1D&theLinearResult)const;


/* :80 */
#line 3792 "mspath.web"

/* 81: */
#line 3850 "mspath.web"

struct Scratch:public ScratchData{
Scratch(const Specification&theSpec):
result(0.0,
theSpec.permissible().nrows(),
theSpec.permissible().ncols()),
pMemento(0)
{}

~Scratch(){
delete pMemento;
}

Double2D result;
ScratchData*pMemento;
};

/* :81 */
#line 3793 "mspath.web"

/* 82: */
#line 3868 "mspath.web"

AbstractLinearProduct*mypLP;

/* :82 */
#line 3794 "mspath.web"

};


/* :76 */
#line 3591 "mspath.web"

}
#endif 


/* :63 */
#line 13430 "mspath.web"

/* 240: */
#line 8628 "mspath.web"

