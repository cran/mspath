
#include "LinearProduct.h"
#include <iostream> 

/* 249: */
#line 8807 "mspath.web"

std::ostream&
mspath::ConstantLinearProduct::printOn(std::ostream&ostr)const{
ostr<<"Constant Terms "<<*mypCoefficients<<
std::endl;
return ostr;
}


/* :249 */
#line 8800 "mspath.web"

/* 250: */
#line 8820 "mspath.web"

bool
mspath::DataLinearProduct::isChanged(Environment&theEnv,
ScratchData*thepMemento)const{
return scratch(theEnv).covariates.isChanged(theEnv,thepMemento);
}

void
mspath::DataLinearProduct::memento(Environment&theEnv,
ScratchData**theppMemento){
scratch(theEnv).covariates.memento(theppMemento);

}

const mspath::Double1D&
mspath::DataLinearProduct::evaluate(Environment&theEnv)const{
ScratchDataLinearProduct&s= scratch(theEnv);
if(s.covariates.isChanged(theEnv,s.pMemento)){
s.results= mypCoefficients->multiply(theEnv,
s.covariates);
s.covariates.memento(&(s.pMemento));
}
return s.results;

}

mspath::DataLinearProduct::ScratchDataLinearProduct&
mspath::DataLinearProduct::scratch(Environment&theEnv)const{
if(!theEnv.hasScratchData(scratchKey()))
theEnv.setScratchData(scratchKey(),

new ScratchDataLinearProduct(theEnv,*this));
return static_cast<ScratchDataLinearProduct&> (
theEnv.getScratchData(scratchKey()));
}

std::ostream&
mspath::DataLinearProduct::printOn(std::ostream&ostr)const{
ostr<<" Linear Product of ";
if(myUseMisclassificationData)
ostr<<"Misclassification Covariates";
else
ostr<<"Regular Covariates";
ostr<<" and "<<*mypCoefficients<<std::endl;
return ostr;
}

/* :250 */
#line 8801 "mspath.web"

/* 251: */
#line 8868 "mspath.web"


bool
mspath::PathDependentLinearProduct::isChanged(Environment&theEnv,
ScratchData*thepMemento)
const{
ScratchPathDependentLinearProduct&scratch= 
getScratchData<ScratchPathDependentLinearProduct> (theEnv,myIndices);
return scratch.covariates.isChanged(theEnv,thepMemento);
}

void
mspath::PathDependentLinearProduct::memento(Environment&theEnv,
ScratchData**theppMemento){
ScratchPathDependentLinearProduct&scratch= 
getScratchData<ScratchPathDependentLinearProduct> (theEnv,myIndices);
scratch.covariates.memento(theppMemento);
}



const mspath::Double1D&
mspath::PathDependentLinearProduct::evaluate(Environment&theEnv)
const{
ScratchPathDependentLinearProduct&scratch= 
getScratchData<ScratchPathDependentLinearProduct> (theEnv,myIndices);
return mypCoefficients->multiply(theEnv,scratch.covariates);
}


std::ostream&
mspath::PathDependentLinearProduct::printOn(std::ostream&ostr)const{
ostr<<" Linear Product of Path-Dependent Variables at indices "
<<myIndices<<" and "
<<*mypCoefficients<<std::endl;
return ostr;
}



/* :251 */
#line 8802 "mspath.web"

/* 252: */
#line 8909 "mspath.web"






void
mspath::SumLinearProducts::insert(AbstractLinearProduct
*thepLinearProduct){

myProducts.push_back(thepLinearProduct);
}




bool
mspath::SumLinearProducts::isChanged(Environment&theEnv,
ScratchData*thepMemento)const{
Memento*pMemento= dynamic_cast<Memento*> (thepMemento);
if(pMemento==0)
return true;

for(size_t i= myProducts.size()-1;;--i){
if(myProducts[i].isChanged(theEnv,(*pMemento)[i]))
return true;
if(i==0u)
return false;
}
}



void
mspath::SumLinearProducts::memento(Environment&theEnv,
ScratchData**theppMemento){
if(*theppMemento==0)
*theppMemento= new Memento(myProducts.size());
for(size_t i= 0;i<myProducts.size();++i)
myProducts[i].memento(theEnv,
&(dynamic_cast<Memento&> (**theppMemento)[i]));
}





const mspath::Double1D&
mspath::SumLinearProducts::evaluate(Environment&theEnv)const{
State&s= getScratchData<State> (theEnv,*this);
Double1D&r= s.result();
r= myProducts[0].evaluate(theEnv);
for(size_t i= 1;i<myProducts.size();++i)
r+= myProducts[i].evaluate(theEnv);
return r;
}


std::ostream&
mspath::SumLinearProducts::printOn(std::ostream&ostr)const{
const size_t n= myProducts.size();
ostr<<" Sum of "<<n<<" Linear Products"<<std::endl;
for(size_t i= 0u;i<n;i++){
ostr<<"Linear Product "<<i<<" is ";
myProducts[i].printOn(ostr);
}
return ostr;
}



/* :252 */
#line 8803 "mspath.web"


/* :248 */
#line 13427 "mspath.web"

/* 90: */
#line 4318 "mspath.web"

