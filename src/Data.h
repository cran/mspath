
#ifndef Data_h
#define Data_h 1
#include "basic.h"
#include <valarray> 
#include "MSPathError.h"
#include <R.h> 



#include <memory> 

namespace mspath{
class TimeStepsGenerator;
class FixedTimeStepsGenerator;
class CompressedTimeStepsGenerator;
class AbstractDataIterator;

class Data{
public:
friend class FixedTimeStepsGenerator;
friend class TimeStepsGenerator;
friend class CompressedTimeStepsGenerator;
friend class DataIterator;

typedef Double2D::TCol TCovariates;
typedef size_t TIObs;
typedef std::valarray<double> TTimes;
/* 175: */
#line 7063 "mspath.web"

Data(int*pSubject,size_t nobs,size_t theNpts,double*pTime,
ObsState*pState,
double*pCov,size_t ncovs,
double*pMiscCov,size_t nmisccovs):

mySubject(pSubject,nobs),
myTime(pTime,nobs),
myState(pState,nobs),
myCov(pCov,ncovs,nobs),
myMisccov(pMiscCov,nmisccovs,nobs),
myNpts(theNpts)
{}


Data(){}
/* :175 */
#line 7048 "mspath.web"

/* 176: */
#line 7081 "mspath.web"



Id subject(TIObs i)const{return mySubject[i];}
ObsState state(TIObs i)const{return myState[i];}
double time(TIObs i)const{return myTime[i];}
bool hasObservedState(TIObs i)const{return state(i)>=0;}



TCovariates covs(TIObs i)const{return myCov.col(i);}
TCovariates miscCovs(TIObs i)const{return myMisccov.col(i);}


Double2D&covs(){return myCov;}
Double2D&miscCovs(){return myMisccov;}
const Double2D&covs()const{return myCov;}
const Double2D&miscCovs()const{return myMisccov;}




size_t nObs()const{return mySubject.size();}


size_t nPersons()const{return myNpts;}


size_t nCovs()const{return myCov.nrows();}


size_t nMiscCovs()const{return myMisccov.nrows();}


TTimes observationTimes(const AbstractDataIterator&iterator);

/* :176 */
#line 7049 "mspath.web"


protected:
/* 177: */
#line 7118 "mspath.web"

Int1D mySubject;
Double1D myTime;
Array1D<ObsState> myState;
Double2D myCov;
Double2D myMisccov;
size_t myNpts;

/* :177 */
#line 7052 "mspath.web"

};

/* 178: */
#line 7158 "mspath.web"


/* 179: */
#line 7171 "mspath.web"

class AbstractDataIterator{
public:

AbstractDataIterator(Data*pData)throw(DataIteratorError):
mypData(pData){};

virtual~AbstractDataIterator(){};


virtual void startSession()= 0;
virtual bool next()throw(DataIteratorError)= 0;



virtual Data::TIObs begin()const= 0;
virtual Data::TIObs end()const= 0;
virtual Id subject()const= 0;
virtual size_t size()const= 0;

protected:

const Data&data()const{return*mypData;};

const Data*const mypData;
};

/* :179 */
#line 7160 "mspath.web"

/* 180: */
#line 7205 "mspath.web"

class DataIterator:public AbstractDataIterator{
public:
DataIterator(Data*pData)throw(DataIteratorError):
AbstractDataIterator(pData),
myBegunIterating(false){};
virtual~DataIterator(){};


virtual void startSession(){
myBegunIterating= false;}
virtual bool next()throw(DataIteratorError);



virtual Data::TIObs begin()const{return myCurrentFirst;}
virtual Data::TIObs end()const{return myCurrentLast+1;}
virtual Id subject()const{return myCurrentId;}
virtual size_t size()const{return myCurrentSize;}

protected:

bool advanceToNext(Data::TIObs i)throw(DataIteratorError);


Id myCurrentId;
Data::TIObs myCurrentFirst;
Data::TIObs myCurrentLast;
size_t myCurrentSize;
bool myBegunIterating;

};

/* :180 */
#line 7161 "mspath.web"

/* 181: */
#line 7270 "mspath.web"


class SubsetDataIterator:public AbstractDataIterator{
public:
SubsetDataIterator(Data*pData,std::auto_ptr<Int1D> pIDs)
throw(DataIteratorError):
AbstractDataIterator(pData),
mypIDs(pIDs),
myNextInSubset(0u),
myEndSubset(mypIDs->size()){}

virtual~SubsetDataIterator(){}

typedef std::auto_ptr<Int1D> IDList;


virtual void startSession(){
myNextInSubset= 0u;}
virtual bool next()throw(DataIteratorError);



virtual Data::TIObs begin()const{return myCurrentFirst;}
virtual Data::TIObs end()const{return myCurrentLast+1;}
virtual Id subject()const{return myCurrentId;}
virtual size_t size()const{return myCurrentSize;};

protected:

bool advanceToNext(Data::TIObs i)throw(DataIteratorError);

const Id&subsetID(size_t i)const{
return(*mypIDs)[i];
}


Id myCurrentId;
Data::TIObs myCurrentFirst;
Data::TIObs myCurrentLast;
size_t myCurrentSize;

IDList mypIDs;
size_t myNextInSubset;
size_t myEndSubset;

};


/* :181 */
#line 7162 "mspath.web"


/* :178 */
#line 7055 "mspath.web"


};
#endif

/* :174 */
#line 13418 "mspath.web"

/* 93: */
#line 4564 "mspath.web"

