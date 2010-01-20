
#include "Environment.h"
#include <cstdlib> 

extern "C"{
#define STRICT_R_HEADERS 1
#include <R.h> 
}

namespace mspath{

template<typename Prob> 
State
Environment::randomDraw(const std::valarray<Prob> &theDensity){
Prob cum= static_cast<Prob> (unif_rand());
Prob sofar= 0.0;
State lastState;
for(size_t i= 0u;i<theDensity.size();i++){
if(theDensity[i]> 0){
sofar+= theDensity[i];
if(sofar>=cum)
return i;
lastState= i;
}
}



return lastState;
}



template State
Environment::randomDraw<double> (const std::valarray<double> &);

}

/* :277 */
#line 13406 "mspath.web"

/* 152: */
#line 6327 "mspath.web"

