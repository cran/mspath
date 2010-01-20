
#include <cmath>   

#include <iostream> 

#include "Specification.h"


#include "Environment.h"
#include "LinearProduct.h"

/* 241: */
#line 8653 "mspath.web"

mspath::SimpleSpecification::SimpleSpecification(
ConstantLinearProduct*thepLP,size_t thenStates,
Bool2D*thepPermissible)throw(InconsistentModel)
:
AbstractSpecification(thepPermissible),
mypLP(thepLP),myResult(0.0,thenStates,thenStates)
{
const Double1D&r0= linearProduct().evaluate();
if(r0.max()> 1.0)
throw InconsistentModel("SIMPLE Measurement error"
" probabilities can not exceed 1.");
if(r0.min()<0.0)
throw InconsistentModel("SIMPLE Measurement error"
" probabilities can't be under 0.");

size_t k= 0u;
size_t i,j;
const size_t n= thenStates;
double sum;
for(i= 0u;i<n;i++){
sum= 1.0;
for(j= 0u;j<n;j++){

if(i!=j){
if(permissible()(i,j)){
myResult(i,j)= r0[k];
sum-= r0[k++];
}
}
if(sum<=0.0)
throw InconsistentModel("SIMPLE Measurement error"
" off-diagonal row probabilities sum to => 1.");
myResult(i,i)= sum;
}
}
}


/* :241 */
#line 8640 "mspath.web"

/* 242: */
#line 8693 "mspath.web"

const mspath::Double2D&
mspath::Specification::evaluate(Environment&theEnv)const{
Scratch&s= getScratchData<Scratch> (theEnv,*this);
if(linearProduct().isChanged(theEnv,s.pMemento)){
computeResult(s.result,linearProduct().evaluate(theEnv));
linearProduct().memento(theEnv,&(s.pMemento));
}
return s.result;
}

/* :242 */
#line 8641 "mspath.web"

/* 243: */
#line 8706 "mspath.web"

void
mspath::Specification::computeResult(Double2D&theResult,
const Double1D&theLinearResult)const{

#ifdef DEBUG
std::cout<<"Total Linear Product:"<<std::endl
<<theLinearResult<<std::endl;
#endif
Double1D r0(std::exp(theLinearResult));
size_t k= 0u;
size_t i,j;
size_t n= theResult.nrows();
double sum;
for(i= 0u;i<n;i++){
sum= 1.0;
for(j= 0u;j<n;j++){

if(i!=j){
if(permissible()(i,j)){
theResult(i,j)= r0[k];
sum+= r0[k++];
}
}
}



for(j= 0;j<n;j++){
if(i!=j){
if(permissible()(i,j))
theResult(i,j)/= sum;


}else{

theResult(i,j)= 1.0/sum;
}
}
}

#ifdef DEBUG
std::cout<<"Final outputs:"<<std::endl
<<theResult<<std::endl;
#endif
}

/* :243 */
#line 8642 "mspath.web"

/* 244: */
#line 8754 "mspath.web"

/* 245: */
#line 8761 "mspath.web"

std::ostream&mspath::operator<<(std::ostream&ostr,
const mspath::AbstractSpecification&spec){
ostr<<spec.permissible()<<std::endl;
return ostr;
}


/* :245 */
#line 8755 "mspath.web"

/* 247: */
#line 8781 "mspath.web"

std::ostream&mspath::operator<<(std::ostream&ostr,
const mspath::SimpleSpecification&spec){
ostr<<"Simple Specification:"<<std::endl
<<spec.myResult<<std::endl;
return ostr;
}

/* :247 */
#line 8756 "mspath.web"

/* 246: */
#line 8770 "mspath.web"

std::ostream&mspath::operator<<(std::ostream&ostr,
const mspath::Specification&spec){
ostr<<"Multinomial Logit Specification for "<<std::endl
<<spec.permissible()<<std::endl;
spec.linearProduct().printOn(ostr)<<std::endl;
return ostr;
}


/* :246 */
#line 8757 "mspath.web"


/* :244 */
#line 8643 "mspath.web"



/* :240 */
#line 13431 "mspath.web"

/* 127: */
#line 5534 "mspath.web"

