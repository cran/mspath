

#ifndef basic_h
#define basic_h 1


#include <valarray> 


#include <functional> 


#include <vector> 

#include <iostream> 
#include <ostream> 
using std::cout;
using std::endl;

namespace mspath{

/* 25: */
#line 2046 "mspath.web"



/* 28: */
#line 2248 "mspath.web"


template<typename T> 
class Array2D;

template<typename T> 
class Array1D;


template<typename T> 
class Indirect1Dto2D{
public:
Indirect1Dto2D(const Array1D<T> &theBase,const Array2D<size_t> &
theIndirect):
myBase(theBase),myIndirect(theIndirect){};

const Array1D<T> &base()const{return myBase;}
const Array2D<size_t> &indirect()const{return myIndirect;}

protected:
const Array1D<T> &myBase;
const Array2D<size_t> &myIndirect;
};


/* :28 */
#line 2049 "mspath.web"

/* 26: */
#line 2056 "mspath.web"


template<typename T> 
class Array1D:public std::valarray<T> {
public:

using std::valarray<T> ::operator[];

Array1D(const T*d,size_t n):
std::valarray<T> (d,n){}

Array1D(){}

Array1D(size_t n):
std::valarray<T> (n){}

Array1D(const T&v,size_t n):
std::valarray<T> (v,n){}

Array1D(const std::valarray<T> &theVals):
std::valarray<T> (theVals){}

Array1D&operator= (const std::valarray<T> &rhs){
std::valarray<T> ::operator= (rhs);
return*this;
}


Indirect1Dto2D<T> operator[](const Array2D<size_t> &theIndirect)
const{
return Indirect1Dto2D<T> (*this,theIndirect);
}

template<typename U> 
const Array1D&setRaw(U*p,size_t n);
};


template<typename T> 
template<typename U> 
const Array1D<T> &Array1D<T> ::setRaw(U*p,size_t n){
this->resize(n);
for(size_t i= 0;i<n;i++)
(*this)[i]= static_cast<T> (p[i]);
return*this;

}

typedef Array1D<int> Int1D;
typedef Array1D<bool> Bool1D;
typedef Array1D<double> Double1D;

typedef Array1D<size_t> TIndirect1D;

template<typename T> 
std::ostream&operator<<(std::ostream&s,
const Array1D<T> &a);


/* :26 */
#line 2050 "mspath.web"

/* 27: */
#line 2126 "mspath.web"


template<typename T> 
class Array2D{
public:

typedef std::valarray<T> TRow;
typedef std::valarray<T> TCol;


Array2D(){}
Array2D(const T*p,size_t rows,size_t cols):
myCols(cols),myData(p,cols*rows){}
Array2D(size_t rows,size_t cols):
myCols(cols),myData(cols*rows){}
Array2D(const T&initial,size_t rows,size_t cols):
myCols(cols),myData(initial,cols*rows){}

Array2D(const Indirect1Dto2D<T> &theI):
myCols(theI.indirect().ncols()),
myData(theI.base()[theI.indirect().rawData()]){}


template<typename U> 
const Array2D&setRaw(U*vector,size_t rows,size_t cols);



T&operator()(size_t row,size_t col){
return myData[row*myCols+col];}
T operator()(size_t row,size_t col)const{
return myData[row*myCols+col];}
size_t nrows()const{if(myCols> 0)
return myData.size()/myCols;
else return 0;}
size_t ncols()const{return myCols;}

TRow row(size_t r){
return myData[std::slice(ncols()*r,ncols(),1)];
}

TCol col(size_t c){
return myData[std::slice(c,nrows(),ncols())];
}

const TRow row(size_t r)const{
return myData[std::slice(ncols()*r,ncols(),1)];
}

const TCol col(size_t c)const{
return myData[std::slice(c,nrows(),ncols())];
}


size_t size()const{return myData.size();}
void resize(size_t r,size_t c){
myData.resize(r*c);
myCols= c;
}



Array2D&operator-= (const T&x){
myData-= x;
return*this;
}

Array2D operator-(const T&x)const{
Array2D a(*this);
a-= x;
return a;
}

#if 0


template<typename U> 
friend class Array2D<U> ;
#endif



const std::valarray<T> &rawData()const{
return myData;
}

protected:
size_t myCols;
std::valarray<T> myData;
};

typedef Array2D<int> Int2D;
typedef Array2D<bool> Bool2D;
typedef Array2D<double> Double2D;

typedef Array2D<size_t> TIndirect2D;

template<typename T> 
template<typename U> 
const Array2D<T> &Array2D<T> ::setRaw(U*vector,size_t rows,size_t cols){
myCols= cols;
size_t nElements;
nElements= cols*rows;
myData.resize(nElements);
for(size_t i= 0;i<nElements;i++){
myData[i]= static_cast<T> (vector[i]);
}
return*this;
};

template<typename T> 
std::ostream&operator<<(std::ostream&s,
const Array2D<T> &a);

/* :27 */
#line 2051 "mspath.web"


/* :25 */
#line 1886 "mspath.web"

/* 23: */
#line 1947 "mspath.web"


using std::size_t;

typedef unsigned int State;
typedef int ObsState;

typedef double Time;
typedef int Id;










typedef double EvaluationData;



typedef Double1D ModelData;


typedef size_t IObservation;



/* :23 */
#line 1887 "mspath.web"

/* 24: */
#line 1990 "mspath.web"


class StatePoint{
public:
StatePoint(const State&s,const Time&t):
myState(s),myTime(t){}
const Time&time()const{return myTime;}
Time time(){return myTime;}
const State&state()const{return myState;}
State state(){return myState;}


void setState(State s){myState= s;}


friend bool operator==(const StatePoint&lhs,const StatePoint&rhs){
return lhs.myState==rhs.myState&&lhs.myTime==rhs.myTime;}

friend bool operator<(const StatePoint&lhs,const StatePoint&rhs){
if(lhs.myState<rhs.myState)
return true;
else if(lhs.myState> rhs.myState)
return false;
return lhs.myTime<rhs.myTime;}

friend std::ostream&
operator<<(std::ostream&ostr,const StatePoint&sp){
ostr<<"StatePoint("<<sp.state()<<", "
<<sp.time()<<")";
return ostr;
}
protected:
State myState;
Time myTime;
};

/* :24 */
#line 1888 "mspath.web"

/* 29: */
#line 2275 "mspath.web"


#ifdef DEBUG
#define TRACE(stuff) std::cout << stuff
#else
#define TRACE(stuff)
#endif

/* :29 */
#line 1889 "mspath.web"

/* 30: */
#line 2318 "mspath.web"



#include <boost/version.hpp> 
#if BOOST_VERSION/100 == 1031
#define BOOST_1_31 1
#elif BOOST_VERSION /100 == 1032
#define BOOST_1_32 1
#elif BOOST_VERSION/100 == 1033
#define BOOST_1_33 1
#elif BOOST_VERSION/100 == 1034
#define BOOST_1_34 1
#define BOOST_AUTO_UNIT_TEST(x) BOOST_AUTO_TEST_CASE(x)
#elif BOOST_VERSION/100 >  1034
#define BOOST_1_35PLUS
#define BOOST_AUTO_UNIT_TEST(x) BOOST_AUTO_TEST_CASE(x)
#else
#error mspath requires Boost 1.31 or later
#endif



/* :30 */
#line 1890 "mspath.web"

}


/* 278: */
#line 9842 "mspath.web"

namespace mspath{
/* 279: */
#line 9849 "mspath.web"

template<typename T> 
std::ostream&operator<<(std::ostream&s,const Array1D<T> &
a){
for(std::size_t i= 0;i<a.size();i++){
if((i%5)==0)
s<<std::endl<<"["<<i<<"]";
s<<" "<<a[i];
}
s<<std::endl;
return s;
}

/* :279 */
#line 9844 "mspath.web"

/* 280: */
#line 9863 "mspath.web"

template<typename T> 
std::ostream&operator<<(std::ostream&s,
const Array2D<T> &a){
size_t nr= a.nrows();
size_t nc= a.ncols();
size_t r,c;
for(r= 0;r<nr;r++){
s<<std::endl<<r<<":";
for(c= 0;c<nc;c++)
s<<" "<<a(r,c);
}
s<<std::endl;
return s;
}




/* :280 */
#line 9845 "mspath.web"


}
/* :278 */
#line 1894 "mspath.web"


#endif 

/* :22 */
#line 13397 "mspath.web"

/* 133: */
#line 5774 "mspath.web"

