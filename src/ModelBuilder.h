
#ifndef ModelBuilder_h
#define ModelBuiler_h 1

#include <memory> 
#include <string> 
#include <vector> 

#include <boost/ptr_container/ptr_list.hpp> 
#include <boost/bind.hpp> 

#include "basic.h"
#include "Coefficients.h"
#include "HistoryComputer.h"
#include "LinearProduct.h"
#include "Model.h"
#include "MSPathError.h"
#include "Specification.h"

namespace mspath{

class ModelBuilder{
public:
/* 199: */
#line 7665 "mspath.web"

ModelBuilder(
double*params,

double*allinits,
int np,

int nst,
int nfix,
int*fixedpars
):
myparams(params),myallinits(allinits),
myp(np),mynst(nst),mynfix(nfix),myfixedpars(fixedpars),
myifix(0),myiopt(0),myiall(0){}

/* :199 */
#line 7520 "mspath.web"

/* 193: */
#line 7539 "mspath.web"


std::auto_ptr<Model> makeModel(/* 194: */
#line 7562 "mspath.web"


int*misc,


int*qvector,
int*evector,
int*constraint,
int*miscconstraint,
int*baseconstraint,
int*basemiscconstraint,
int*pathconstraint,
int*pathmiscconstraint,
double*initprobs,
int*nms,
int*nintens,
int*nintenseffs,
int*nmisc,
int*nmisceffs,
int*ncovs,
int*ncoveffs,
int*nmisccovs,
int*nmisccoveffs,
int*nhistory,
const char**history,
double*initialOffset,
int*npatheffs,
int*npathmisceffs


/* :194 */
#line 7541 "mspath.web"
)
throw(InconsistentModel,BadInitialProbs,OneInitialState,UnknownTerm)
;


/* :193 */
#line 7521 "mspath.web"

protected:
/* 203: */
#line 7766 "mspath.web"

State makeInitial(size_t n,double*p)const
throw(OneInitialState,BadInitialProbs);

/* :203 */
#line 7523 "mspath.web"

/* 200: */
#line 7693 "mspath.web"



typedef boost::ptr_list<HistoryComputer> THistoryVector;
typedef std::vector<std::string> TStringVector;


std::auto_ptr<Model::TComputerContainer> 
makeHistory(
const char**history,
int nhistory,
double initialOffset
)throw(UnknownTerm,TangledDependencies);



TStringVector makeRequests(
const char**history,
int nhistory
);


std::auto_ptr<THistoryVector> allComputers(double offset= 0.0);



void makeHistoryStage1(THistoryVector&theComputers,
const TStringVector&theRequests)throw(UnknownTerm);

/* 201: */
#line 7736 "mspath.web"


struct Mark{
Mark(THistoryVector&theComputers):
myComputers(theComputers),myIndex(0){}


void operator()(const std::string&theRequest){
THistoryVector::iterator i= std::find_if(
myComputers.begin(),
myComputers.end(),
boost::bind(&HistoryComputer::matches,_1,theRequest));
if(i==myComputers.end())
throw(UnknownTerm(theRequest));
i->makeCovariate(myIndex++);
}

THistoryVector&myComputers;
size_t myIndex;
};

/* :201 */
#line 7722 "mspath.web"



std::auto_ptr<Model::TComputerContainer> 
makeHistoryStage2(THistoryVector&theComputers)throw(TangledDependencies);

/* :200 */
#line 7524 "mspath.web"

/* 202: */
#line 7759 "mspath.web"


std::auto_ptr<TIndirect1D> makeHistoryIndirection(
const Model::TComputerContainer&theComputers)const;

/* :202 */
#line 7525 "mspath.web"

/* 195: */
#line 7600 "mspath.web"


std::auto_ptr<Specification> makeSpecification(
int ninterceptEff,
int nintercept,
int*interceptConstraints,
int ncovEff,
int ncov,
int*covConstraints,
int npathEff,
int npath,
int*pathConstraints,
int*permissible,
const TIndirect1D*pathIndirect,
bool useMisclassification= false);



std::auto_ptr<SlopeCoefficients> makeSlope(
Double1D&effective,
int neffective,
int ncov,
int nterms,
int*constraints);

/* :195 */
#line 7526 "mspath.web"

/* 196: */
#line 7627 "mspath.web"

std::auto_ptr<SimpleSpecification> makeSimpleSpecification(
int ninterceptEff,
int nintercept,
int*interceptConstraints,
int*permissible);

/* :196 */
#line 7527 "mspath.web"

/* 197: */
#line 7638 "mspath.web"


void fillparvec(
Double1D&parvec,
int ni
);


/* :197 */
#line 7528 "mspath.web"

/* 198: */
#line 7648 "mspath.web"

const double*myparams;

const double*myallinits;
const size_t myp;

const size_t mynst;
const size_t mynfix;
int*myfixedpars;


size_t myifix;
size_t myiopt;
size_t myiall;

/* :198 */
#line 7529 "mspath.web"


};

}
#endif


/* :192 */
#line 13454 "mspath.web"

/* 310: */
#line 10953 "mspath.web"

