
#include "Coefficients.h"
#include "Covariates.h"
#include "Environment.h"

/* 255: */
#line 9012 "mspath.web"

std::ostream&mspath::operator<<(std::ostream&theStream,
const mspath::InterceptCoefficients&theC){
theStream<<"Intercept Only Coefficients"<<std::endl
<<" "<<theC.totalIntercepts()<<std::endl
<<" from effective coefficients "<<theC.effectiveIntercepts()<<std::endl
<<" and constraints "<<theC.interceptConstraints()<<std::endl;
return theStream;
}

/* :255 */
#line 8987 "mspath.web"

/* 256: */
#line 9023 "mspath.web"

std::ostream&mspath::operator<<(std::ostream&theStream,
const mspath::SlopeCoefficients&theC){
theStream<<"Slope Only Coefficients"<<std::endl
<<" "<<theC.totalSlopes()<<std::endl
<<" from effective coefficients "<<theC.effectiveSlopes()<<std::endl
<<" and constraints "<<theC.slopeConstraints()<<std::endl;
return theStream;
}


/* :256 */
#line 8988 "mspath.web"

/* 254: */
#line 8992 "mspath.web"

const mspath::Double1D&
mspath::SlopeCoefficients::multiply(Environment&theEnv,
AbstractCovariates&theCovs)const{
WorkData&w= getScratchData<WorkData> (static_cast<ScratchPad&> (theEnv),nTotal());
if(!theCovs.isChanged(theEnv,w.pCovariateMemento))
return w.results;
const Double1D&cov= theCovs.values(theEnv);
theCovs.memento(&(w.pCovariateMemento));



for(size_t i= 0;i<w.results.size();++i){
w.results[i]= (cov*(totalSlopes().col(i))).sum();
}
return w.results;
}


/* :254 */
#line 8989 "mspath.web"


/* :253 */
#line 13429 "mspath.web"
/* 63: */
#line 3575 "mspath.web"

