
#ifndef ScratchPad_h
#define ScratchPad_h 1

#include "ScratchData.h"

#include <boost/ptr_container/ptr_map.hpp> 

namespace mspath{
class ScratchDataProducer;
class ScratchPad{
public:
virtual~ScratchPad(){}


ScratchData&getScratchData(const ScratchDataProducer*key)
throw(boost::bad_ptr_container_operation){
return myState.at(key);}


void setScratchData(const ScratchDataProducer*key,ScratchData*pState){
myState.insert(key,pState);
};

bool hasScratchData(const ScratchDataProducer*key)const{
return myState.find(key)!=myState.end();
}


void release(const ScratchDataProducer*key){

TStore::iterator i(myState.find(key));
if(i!=myState.end())
myState.erase(i);
}


virtual void clear(){
myState.clear();
}

protected:

typedef boost::ptr_map<const ScratchDataProducer*,ScratchData> TStore;
TStore myState;
};
}
#endif





/* :152 */
#line 13407 "mspath.web"

/* 153: */
#line 6386 "mspath.web"

