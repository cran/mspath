
#ifndef ScratchDataProducer_h
#define ScratchDataProducer_h
#include "ScratchData.h"
#include "ScratchPad.h"
namespace mspath{

class ScratchDataProducer{
public:


virtual~ScratchDataProducer(){};

const ScratchDataProducer*scratchKey()const{
return this;}


virtual void release(ScratchPad*thePad)const{
thePad->release(scratchKey());
}

protected:

template<class TD> TD&getScratchData(ScratchPad&theTS)const{
if(!theTS.hasScratchData(scratchKey()))
theTS.setScratchData(scratchKey(),makeScratchData<TD> ());
return static_cast<TD&> (theTS.getScratchData(scratchKey()));
}

template<class TD> TD*makeScratchData()const{
return new TD;}






template<class TD,class Arg> TD&getScratchData(ScratchPad&theTS,
const Arg&theArg)const{
if(!theTS.hasScratchData(scratchKey()))
theTS.setScratchData(scratchKey(),makeScratchData<TD> (theArg));
return static_cast<TD&> (theTS.getScratchData(scratchKey()));
}

template<class TD,class Arg> TD*makeScratchData(const Arg&theArg)const{
return new TD(theArg);
}

};
}
#endif
/* :154 */
#line 13409 "mspath.web"

/* 173: */
#line 6967 "mspath.web"

