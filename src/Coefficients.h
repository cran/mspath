
#ifndef Coefficients_h
#define Coefficients_h 1
#include "basic.h"
#include "ScratchDataProducer.h"
#include "ScratchPad.h"

#include <iostream> 

namespace mspath{
class AbstractCovariates;




class AbstractCoefficients:public ScratchDataProducer{
public:
virtual~AbstractCoefficients(){}
};





/* 91: */
#line 4356 "mspath.web"

class InterceptCoefficients:virtual public AbstractCoefficients{
public:
InterceptCoefficients(const Double1D&theIntercepts,
const TIndirect1D&theConstraints):
myEffectiveIntercepts(theIntercepts),
myInterceptConstraints(theConstraints),
myTotalIntercepts(theIntercepts[theConstraints]){}

virtual~InterceptCoefficients(){}




size_t nTotal()const{
return interceptConstraints().size();
}




const Double1D&multiply(ScratchPad&thePad)const{
return totalIntercepts();}



const Double1D&multiply()const{
return totalIntercepts();}

friend std::ostream&operator<<(std::ostream&theStream,
const InterceptCoefficients&theCoeff);


protected:

double operator[](size_t i)const{
return totalIntercepts()[i];
}

const Double1D&totalIntercepts()const{
return myTotalIntercepts;
}

const Double1D&effectiveIntercepts()const{
return myEffectiveIntercepts;
}

const TIndirect1D&interceptConstraints()const{
return myInterceptConstraints;
}


const Double1D myEffectiveIntercepts;
const TIndirect1D myInterceptConstraints;
const Double1D myTotalIntercepts;
};


/* :91 */
#line 4343 "mspath.web"

/* 92: */
#line 4426 "mspath.web"

class Environment;

class SlopeCoefficients:public virtual AbstractCoefficients{
public:
SlopeCoefficients(const Double1D&theCoefficients,
const TIndirect2D&theConstraints):
myEffectiveSlopes(theCoefficients),
mySlopeConstraints(theConstraints),
myTotalSlopes(theCoefficients[theConstraints]){}

virtual~SlopeCoefficients(){}



size_t nCovs()const{
return totalSlopes().nrows();
}


size_t nTotal()const{
return totalSlopes().ncols();
}


const Double1D&multiply(Environment&theEnv,
AbstractCovariates&theCovs)const;


friend std::ostream&operator<<(std::ostream&theStream,
const SlopeCoefficients&theCoeff);


protected:

double operator()(size_t i,size_t j)const{
return totalSlopes()(i,j);
}

const Double2D&totalSlopes()const{
return myTotalSlopes;
}

const Double1D&effectiveSlopes()const{
return myEffectiveSlopes;
}

const TIndirect2D&slopeConstraints()const{
return mySlopeConstraints;
}


const Double1D myEffectiveSlopes;
const TIndirect2D mySlopeConstraints;
const Double2D myTotalSlopes;


class WorkData:public ScratchData{
public:
WorkData(size_t n):
results(n),pCovariateMemento(0){}

virtual~WorkData(){
delete pCovariateMemento;
}

Double1D results;
ScratchData*pCovariateMemento;

};
};



/* :92 */
#line 4344 "mspath.web"



}
#endif

/* :90 */
#line 13428 "mspath.web"

/* 253: */
#line 8981 "mspath.web"

