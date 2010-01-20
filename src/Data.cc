
#include "Data.h"

#include <sstream> 
#include <Rinternals.h> 

namespace mspath{
Data::TTimes Data::observationTimes(const AbstractDataIterator&theI){
return myTime[std::slice(theI.begin(),
theI.size(),
1)];
}

/* 270: */
#line 9480 "mspath.web"


bool DataIterator::next()throw(DataIteratorError){
if(myBegunIterating)
return advanceToNext(myCurrentLast+1);
else{
myBegunIterating= true;
return advanceToNext(0);
}
}


bool DataIterator::advanceToNext(Data::TIObs i)
throw(DataIteratorError)
{
if(i+1>=data().nObs())

return false;
myCurrentId= data().subject(i);
myCurrentFirst= i;
do++i;while(i<data().nObs()&&data().subject(i)==myCurrentId);
myCurrentLast= i-1;
myCurrentSize= i-myCurrentFirst;

#ifdef DEBUG
std::ostringstream buf;
buf<<"DataIterator::advanceToNext advanced to "<<
myCurrentId<<std::endl;
Rprintf("%s",buf.str().c_str());
#endif

return true;
}

/* :270 */
#line 9475 "mspath.web"

/* 271: */
#line 9515 "mspath.web"


bool SubsetDataIterator::next()throw(DataIteratorError){
if(myNextInSubset>=myEndSubset)
return false;
if(myNextInSubset==0u)
return advanceToNext(0u);
else
return advanceToNext(myCurrentLast+1u);
}



bool SubsetDataIterator::advanceToNext(Data::TIObs i)
throw(DataIteratorError){
myCurrentId= subsetID(myNextInSubset);

#ifdef DEBUG
std::ostringstream buf;
buf<<"SubsetDataIterator::advanceToNext looking for id "<<
myCurrentId<<std::endl;
Rprintf("%s",buf.str().c_str());
#endif 

size_t nAll= data().nObs();
while(i<nAll){
if(data().subject(i)==myCurrentId)
break;
++i;
}
if(i>=nAll)
throw SubsetDataIteratorError(myCurrentId);


myCurrentFirst= i;
++myNextInSubset;
do++i;while(i<nAll&&data().subject(i)==myCurrentId);
myCurrentLast= i-1;
myCurrentSize= i-myCurrentFirst;
#ifdef DEBUG
std::ostringstream buf2;
buf2<<"SubsetDataIterator::advanceToNext moved to id "<<myCurrentId<<std::endl;
Rprintf("%s",buf2.str().c_str());
#endif
return true;
}

/* :271 */
#line 9476 "mspath.web"

}

/* :269 */
#line 13421 "mspath.web"

/* 301: */
#line 10722 "mspath.web"

