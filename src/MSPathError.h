
#ifndef MSPathError_h
#define MSPathError_h 1

#include <sstream> 
#include <stdexcept> 
#include <string> 

#include "basic.h"

namespace mspath{

/* 183: */
#line 7359 "mspath.web"


struct DataModelInconsistency:public std::domain_error{
DataModelInconsistency(Id theId,const std::string&msg):
std::domain_error(msg),myId(theId){
std::stringstream myStream;
myStream<<"Model is inconsistent with data at case "
<<theId<<": "<<msg;
myMsg= myStream.str();
}

virtual~DataModelInconsistency()throw(){}
virtual const char*what()const throw(){
return myMsg.c_str();
}

Id id()const{return myId;}

protected:
Id myId;
std::string myMsg;
};

/* :183 */
#line 7343 "mspath.web"

/* 185: */
#line 7399 "mspath.web"


struct OneInitialState:public std::invalid_argument{
OneInitialState():
std::invalid_argument(
"Sorry, I can only handle a single initial state for now")
{}

virtual~OneInitialState()throw(){}
};

/* :185 */
#line 7344 "mspath.web"

/* 186: */
#line 7414 "mspath.web"

struct BadInitialProbs:public std::invalid_argument{
BadInitialProbs(const std::string&msg):
std::invalid_argument(
std::string("Impossible initial probabilities: ")+
msg){}

virtual~BadInitialProbs()throw(){};
};

/* :186 */
#line 7345 "mspath.web"

/* 184: */
#line 7390 "mspath.web"

struct InconsistentModel:public std::domain_error{
InconsistentModel(const std::string&msg):
std::domain_error(msg){}
virtual~InconsistentModel()throw(){}
};

/* :184 */
#line 7346 "mspath.web"

/* 187: */
#line 7432 "mspath.web"

struct DuplicateTerm:public std::invalid_argument{
DuplicateTerm():
std::invalid_argument(
std::string("Duplicate terms specified: ")){}

virtual~DuplicateTerm()throw(){};
};

/* :187 */
#line 7347 "mspath.web"

/* 188: */
#line 7443 "mspath.web"

struct UnknownTerm:public std::invalid_argument{
UnknownTerm(const std::string&term):
std::invalid_argument(
std::string("Unrecognized term: ")+term){}
};

/* :188 */
#line 7348 "mspath.web"

/* 189: */
#line 7453 "mspath.web"


struct TangledDependencies:public std::logic_error{
TangledDependencies(const std::string&msg):
std::logic_error(msg){}
};

/* :189 */
#line 7349 "mspath.web"

/* 190: */
#line 7470 "mspath.web"


struct DataIteratorError:public std::logic_error{
DataIteratorError(const std::string&msg):
std::logic_error(msg){}
};

struct SubsetDataIteratorError:public DataIteratorError{

SubsetDataIteratorError(int id):
DataIteratorError(
static_cast<std::ostringstream&> ((std::ostringstream()<<
"SubsetDataIterator failed at subset case id "<<id<<
".  Likely it is out of order, duplicate, or invalid.")).str()),
badid(id){}
int badid;
};

/* :190 */
#line 7350 "mspath.web"


}
#endif

/* :182 */
#line 13448 "mspath.web"

/* 113: */
#line 5250 "mspath.web"

