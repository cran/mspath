
#ifndef LinearProduct_h
#define LinearProduct_h

#include <iosfwd> 

#include "basic.h"

#include "Coefficients.h"
#include "Covariates.h"
#include "Environment.h"
#include "ScratchDataProducer.h"


#include <boost/ptr_container/ptr_vector.hpp> 

namespace mspath{

class AbstractLinearProduct:public ScratchDataProducer{
public:
virtual~AbstractLinearProduct(){}




virtual bool isChanged(Environment&theEnv,
ScratchData*thepMemento)const= 0;



virtual void memento(Environment&theEnv,
ScratchData**theppMemento)= 0;


virtual size_t size()const= 0;




virtual const Double1D&
evaluate(Environment&theEnv)const= 0;


virtual std::ostream&printOn(std::ostream&ostr)const{
ostr<<"AbstractLinearProduct";
return ostr;
}




friend std::ostream&operator<<(std::ostream&ostr,
const AbstractLinearProduct&lp){
lp.printOn(ostr);
return ostr;};
};

/* 84: */
#line 3962 "mspath.web"

class ConstantLinearProduct:public AbstractLinearProduct{
public:

ConstantLinearProduct(InterceptCoefficients*
thepCoefficients):
mypCoefficients(thepCoefficients){}

virtual~ConstantLinearProduct(){
delete mypCoefficients;
}


virtual void release(ScratchPad*thePad)const{}

virtual bool isChanged(Environment&theEnv,ScratchData*thepMemento)const{
return thepMemento==0;
}

virtual void memento(Environment&theEnv,
ScratchData**theppMemento){
if(*theppMemento==0)
*theppMemento= new TScratchData();
}


virtual size_t size()const{
return mypCoefficients->nTotal();
}

virtual const Double1D&
evaluate(Environment&theEnv)const{
return mypCoefficients->multiply(theEnv);
}



const Double1D&evaluate()const{
return mypCoefficients->multiply();
}

virtual std::ostream&printOn(std::ostream&ostr)const;

protected:


InterceptCoefficients*const mypCoefficients;


struct TScratchData:public ScratchData{

TScratchData(){}

};
};

/* :84 */
#line 3945 "mspath.web"

/* 85: */
#line 4025 "mspath.web"


class DataLinearProduct:public AbstractLinearProduct{
public:
DataLinearProduct(const SlopeCoefficients*pSlope,
bool theUseMisclassificationData= false):
mypCoefficients(pSlope),
myUseMisclassificationData(theUseMisclassificationData){}

virtual~DataLinearProduct(){
delete mypCoefficients;
}

virtual void release(ScratchPad*thePad)const{
ScratchDataProducer::release(thePad);
mypCoefficients->release(thePad);
}

virtual bool isChanged(Environment&theEnv,
ScratchData*thepMemento)const;

virtual void memento(Environment&theEnv,
ScratchData**theppMemento);


virtual size_t size()const{
return mypCoefficients->nTotal();
}

virtual const Double1D&
evaluate(Environment&theEnv)const;

virtual std::ostream&printOn(std::ostream&ostr)const;

protected:
const SlopeCoefficients*const mypCoefficients;
const bool myUseMisclassificationData;

struct ScratchDataLinearProduct:public ScratchData{
ScratchDataLinearProduct(const Environment&theEnv,
const DataLinearProduct&theLP):
covariates(theLP.data(theEnv)),
results(theLP.size()),
pMemento(0){}

~ScratchDataLinearProduct(){
delete pMemento;
}

MatrixCovariates covariates;
Double1D results;
ScratchData*pMemento;
};

ScratchDataLinearProduct&scratch(Environment&theEnv)const;

const Double2D&data(const Environment&theEnv)const{
if(myUseMisclassificationData)
return theEnv.data().miscCovs();
return theEnv.data().covs();
}

};

/* :85 */
#line 3946 "mspath.web"

/* 86: */
#line 4093 "mspath.web"

class PathDependentLinearProduct:public AbstractLinearProduct{
public:
PathDependentLinearProduct(const SlopeCoefficients*pSlope,
const TIndirect1D&theIndices):
mypCoefficients(pSlope),
myIndices(theIndices){}

virtual~PathDependentLinearProduct(){
delete mypCoefficients;
}

virtual void release(ScratchPad*thePad)const{
ScratchDataProducer::release(thePad);
mypCoefficients->release(thePad);
}

virtual bool isChanged(Environment&theEnv,
ScratchData*thepMemento)const;

virtual void memento(Environment&theEnv,
ScratchData**theppMemento);


virtual size_t size()const{
return mypCoefficients->nTotal();
}

virtual const Double1D&
evaluate(Environment&theEnv)const;

virtual std::ostream&printOn(std::ostream&ostr)const;


protected:
const SlopeCoefficients*const mypCoefficients;
const TIndirect1D myIndices;

struct ScratchPathDependentLinearProduct:public ScratchData{
ScratchPathDependentLinearProduct(const TIndirect1D&theIndices):
covariates(theIndices){}

PathCovariates covariates;
};
};

/* :86 */
#line 3947 "mspath.web"

/* 87: */
#line 4158 "mspath.web"

class SumLinearProducts:public AbstractLinearProduct{
public:
SumLinearProducts(){}




void insert(AbstractLinearProduct*thepLinearProduct);

virtual~SumLinearProducts(){}

virtual void release(ScratchPad*thePad)const{
ScratchDataProducer::release(thePad);







for(size_t i= 0;i<myProducts.size();++i)
myProducts[i].release(thePad);
}



virtual bool isChanged(Environment&theEnv,
ScratchData*thepMemento)const;



virtual void memento(Environment&theEnv,
ScratchData**theppMemento);



virtual size_t size()const{
return myProducts.front().size();
}




virtual const Double1D&
evaluate(Environment&theEnv)const;


virtual std::ostream&printOn(std::ostream&ostr)const;


protected:
typedef boost::ptr_vector<AbstractLinearProduct> TProducts;
TProducts myProducts;
/* 88: */
#line 4232 "mspath.web"

struct Memento:public ScratchData{
Memento(size_t n):
mementos(n,0){}

virtual~Memento(){
for(size_t i= 0;i<mementos.size();++i){
delete mementos[i];
mementos[i]= 0;
}
}

ScratchData*&operator[](size_t i){
return mementos[i];
}

std::vector<ScratchData*> mementos;
};


/* :88 */
#line 4212 "mspath.web"

/* 89: */
#line 4253 "mspath.web"

class State:public ScratchData{
public:
State(const SumLinearProducts&theProd):
myResult(theProd.size()),
myMemento(theProd.myProducts.size()){};

ScratchData*&memento(size_t i){
return myMemento[i];
}

Memento&memento(){
return myMemento;
}

size_t size()const{return myResult.size();}

Double1D&result(){
return myResult;
}

protected:
Double1D myResult;
Memento myMemento;
};

/* :89 */
#line 4213 "mspath.web"

};


/* :87 */
#line 3948 "mspath.web"

}
#endif

/* :83 */
#line 13426 "mspath.web"

/* 248: */
#line 8795 "mspath.web"

