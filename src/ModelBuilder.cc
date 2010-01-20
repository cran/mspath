
#include "ModelBuilder.h"

#include <algorithm> 
#include <map> 
#include <functional> 
#include <sstream> 

#include "CompositeHistoryComputer.h"
#include "PrimitiveHistoryComputer.h"
#include "Specification.h"

/* 311: */
#line 10980 "mspath.web"

std::auto_ptr<mspath::Model> 
mspath::ModelBuilder::makeModel(
/* 194: */
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
#line 10983 "mspath.web"
)
throw(InconsistentModel,BadInitialProbs,OneInitialState,UnknownTerm)

{

std::auto_ptr<Model::TComputerContainer> pHistory= 
makeHistory(history,*nhistory,*initialOffset);
std::auto_ptr<TIndirect1D> pHistoryIndirect(0);
if(pHistory.get())
pHistoryIndirect= makeHistoryIndirection(*pHistory);
std::auto_ptr<Specification> pTransitions= 
makeSpecification(*nintenseffs,*nintens,baseconstraint,
*ncoveffs,*ncovs,constraint,
*npatheffs,*nhistory,pathconstraint,
qvector,pHistoryIndirect.get());
State initialState= makeInitial(pTransitions->nStates(),
initprobs);

std::auto_ptr<AbstractSpecification> pError(0);
if(*misc){
if(*misc==2)
pError.reset(makeSimpleSpecification(*nmisceffs,*nmisc,
basemiscconstraint,evector).release());
else
pError.reset(makeSpecification(
*nmisceffs,*nmisc,basemiscconstraint,
*nmisccoveffs,*nmisccovs,miscconstraint,
*npathmisceffs,*nhistory,pathmiscconstraint,
evector,pHistoryIndirect.get(),true).release());
}
return std::auto_ptr<Model> (new Model(pTransitions.release(),
pError.release(),
pHistory.release(),
initialState));
}

/* :311 */
#line 10966 "mspath.web"

/* 312: */
#line 11021 "mspath.web"

mspath::State mspath::ModelBuilder::makeInitial(size_t n,double*p)const
throw(OneInitialState,BadInitialProbs){

if(p==0)
return 0u;
double total= 0;
State pick= 0u;
for(size_t i= 0u;i<n;++i){
if(p[i]==0.0)
continue;
if(p[i]<0.0||p[i]> 1.0){
std::ostringstream ostr;
ostr<<"State "<<i<<" has impossible probability "<<p[i];
throw BadInitialProbs(ostr.str());
}
if(total> 0.0)
throw OneInitialState();
pick= i;
total+= p[i];
}
if(total!=1.0)
throw BadInitialProbs(std::string(" must sum to 1"));
return pick;
}

/* :312 */
#line 10967 "mspath.web"

/* 313: */
#line 11064 "mspath.web"


std::auto_ptr<mspath::Specification> 
mspath::ModelBuilder::makeSpecification(
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
bool useMisclassification){

Double1D effective;


fillparvec(effective,ninterceptEff);

TIndirect1D constraints(static_cast<size_t> (nintercept));
for(size_t i= 0;i<static_cast<size_t> (nintercept);++i)
constraints[i]= static_cast<size_t> (interceptConstraints[i]-1u);

std::auto_ptr<InterceptCoefficients> pIntercepts(
new InterceptCoefficients(effective,constraints));

std::auto_ptr<AbstractLinearProduct> pLP(new
ConstantLinearProduct(pIntercepts.release()));


if(ncov> 0||npath> 0){
std::auto_ptr<SumLinearProducts> pSum(0);
pSum.reset(new SumLinearProducts());
pSum->insert(pLP.release());



if(ncov> 0){
std::auto_ptr<SlopeCoefficients> pSlopes= 
makeSlope(effective,ncovEff,ncov,nintercept,covConstraints);
std::auto_ptr<DataLinearProduct> pDLP(
new DataLinearProduct(pSlopes.release(),useMisclassification));
pSum->insert(pDLP.release());
}


if(npathEff> 0&&npath> 0){
std::auto_ptr<SlopeCoefficients> pSlopes= 
makeSlope(effective,npathEff,npath,nintercept,pathConstraints);
std::auto_ptr<PathDependentLinearProduct> pPLP(
new PathDependentLinearProduct(pSlopes.release(),*pathIndirect));
pSum->insert(pPLP.release());
}

pLP= pSum;
}


std::auto_ptr<Bool2D> pPermissible(new Bool2D());
pPermissible->setRaw(permissible,mynst,mynst);
std::auto_ptr<Specification> pSpec(
new Specification(pLP.release(),pPermissible.release()));
return pSpec;
}

/* 314: */
#line 11135 "mspath.web"

std::auto_ptr<mspath::SlopeCoefficients> 
mspath::ModelBuilder::makeSlope(
Double1D&effective,
int neffective,
int ncov,
int nterms,
int*constraints){
fillparvec(effective,neffective);
TIndirect2D c2(ncov,nterms);
for(size_t i= 0u;i<static_cast<size_t> (ncov);++i)
for(size_t j= 0u;j<static_cast<size_t> (nterms);++j)
c2(i,j)= static_cast<size_t> (constraints[i*nterms+j]-1u);
return std::auto_ptr<SlopeCoefficients> (new SlopeCoefficients(effective,c2));
}

/* :314 */
#line 11132 "mspath.web"


/* :313 */
#line 10968 "mspath.web"

/* 315: */
#line 11152 "mspath.web"

std::auto_ptr<mspath::SimpleSpecification> 
mspath::ModelBuilder::makeSimpleSpecification(
int ninterceptEff,
int nintercept,
int*interceptConstraints,
int*permissible){

Double1D effective;


fillparvec(effective,ninterceptEff);

TIndirect1D constraints(static_cast<size_t> (nintercept));
for(size_t i= 0;i<static_cast<size_t> (nintercept);++i)
constraints[i]= static_cast<size_t> (interceptConstraints[i]-1u);

std::auto_ptr<InterceptCoefficients> pIntercepts(
new InterceptCoefficients(effective,constraints));

std::auto_ptr<ConstantLinearProduct> pLP(new
ConstantLinearProduct(pIntercepts.release()));

std::auto_ptr<Bool2D> pPermissible(new Bool2D());
pPermissible->setRaw(permissible,mynst,mynst);
std::auto_ptr<SimpleSpecification> pSpec(
new SimpleSpecification(pLP.release(),
mynst,
pPermissible.release()));
return pSpec;
}


/* :315 */
#line 10969 "mspath.web"

/* 316: */
#line 11194 "mspath.web"


void
mspath::ModelBuilder::fillparvec(
Double1D&parvec,
int ni
)
{
int i;
parvec.resize(ni);
for(i= 0;i<ni;++i,++(myiall)){
if((myifix<mynfix)&&(myiall==myfixedpars[myifix])){
parvec[i]= myallinits[myiall];
++(myifix);
}
else if(myiopt<myp){
parvec[i]= myparams[myiopt];
++(myiopt);
}
}
}


/* :316 */
#line 10970 "mspath.web"

/* 317: */
#line 11219 "mspath.web"


std::auto_ptr<mspath::Model::TComputerContainer> 
mspath::ModelBuilder::makeHistory(
const char**history,
int nhistory,
double initialOffset
)throw(mspath::UnknownTerm,mspath::TangledDependencies){
if(nhistory<=0)
return std::auto_ptr<Model::TComputerContainer> (0);
TStringVector requests= makeRequests(history,nhistory);
std::auto_ptr<THistoryVector> allHistoryComputers= 
allComputers(initialOffset);
makeHistoryStage1(*allHistoryComputers,requests);
return makeHistoryStage2(*allHistoryComputers);
};

/* 319: */
#line 11268 "mspath.web"

mspath::ModelBuilder::TStringVector
mspath::ModelBuilder::makeRequests(
const char**history,
int nhistory
){
TStringVector requests;
for(int i= 0;i<nhistory;++i)
requests.push_back(std::string(history[i]));
return requests;
}

/* :319 */
#line 11236 "mspath.web"

/* 318: */
#line 11252 "mspath.web"

std::auto_ptr<mspath::ModelBuilder::THistoryVector> 
mspath::ModelBuilder::allComputers(double offset){
THistoryVector all;
all.push_back(new TimeInStateComputer("TIS",offset));
all.push_back(new TimeInPreviousStatesComputer("TIP",offset));
all.push_back(new TimeSinceOriginComputer("TSO",offset));
THistoryVector::iterator p= all.begin();
all.push_back(new LnHistoryComputer("LN",*p++));
all.push_back(new LnHistoryComputer("LN",*p++));
all.push_back(new LnHistoryComputer("LN",*p++));
return all.release();
}

/* :318 */
#line 11237 "mspath.web"

/* 320: */
#line 11283 "mspath.web"

void
mspath::ModelBuilder::makeHistoryStage1(THistoryVector&theComputers,
const TStringVector&theRequests)throw(mspath::UnknownTerm){
Mark m(theComputers);
std::for_each(theRequests.begin(),
theRequests.end(),
m);
}

/* :320 */
#line 11238 "mspath.web"

/* 321: */
#line 11309 "mspath.web"

std::auto_ptr<mspath::Model::TComputerContainer> 
mspath::ModelBuilder::makeHistoryStage2(THistoryVector&theComputers)
throw(mspath::TangledDependencies){
Model::TComputerContainer final;

typedef std::map<HistoryComputer*,THistoryVector::iterator> TTransfer;
TTransfer untransferred;


for(THistoryVector::iterator i= theComputers.begin();
i!=theComputers.end();++i){
if(i->isRequired())
untransferred[&*i]= i;
}

/* 322: */
#line 11338 "mspath.web"

size_t modelDataIndex= 0u;

size_t n= untransferred.size();
while(n> 0u){
for(TTransfer::iterator i= untransferred.begin();
i!=untransferred.end();
){
TTransfer::mapped_type&val= i->second;
HistoryComputer*p= val->requires();
if(p==0||untransferred.count(p)==0){


val->setDataIndex(modelDataIndex++);
final.push_back(theComputers.release(val).release());

TTransfer::iterator zap(i);
++i;
untransferred.erase(zap);
}else{
++i;
}
}
size_t nold= n;
n= untransferred.size();
if(n==nold)

throw(TangledDependencies("There seem to be circular"
" dependencies in path computation"));
}

/* :322 */
#line 11325 "mspath.web"


return final.release();
}

/* :321 */
#line 11239 "mspath.web"


/* :317 */
#line 10971 "mspath.web"

/* 323: */
#line 11380 "mspath.web"


std::auto_ptr<mspath::TIndirect1D> 
mspath::ModelBuilder::makeHistoryIndirection(
const Model::TComputerContainer&theComputers)const{
std::auto_ptr<TIndirect1D> pIndirect(new TIndirect1D(theComputers.size()));
size_t i;
size_t nUser= 0u;
for(i= 0;i<theComputers.size();++i)
if(theComputers[i].isCovariate()){
pIndirect->operator[](theComputers[i].covariateIndex())= i;
nUser++;
}
if(nUser==pIndirect->size())
return pIndirect;

std::auto_ptr<TIndirect1D> s(new TIndirect1D(nUser));
for(i= 0;i<nUser;++i)
(*s)[i]= (*pIndirect)[i];
return s;
}

/* :323 */
#line 10972 "mspath.web"


/* :310 */
#line 13455 "mspath.web"

/* 326: */
#line 11547 "mspath.web"

/* 328: */
#line 11776 "mspath.web"

