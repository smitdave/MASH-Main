//
//  VectorControl.cpp
//  MASH
//

#include "VectorControl.hpp"


VectorControl::VectorControl(){
    //@ Initialises a vector control object with parameters all equal to zero and false activation
    parameters = NON_PARMS;
}
vectorControlParametersSet VectorControl::getParametersSet(){
    //@ Accessor for vector control parameters
    return parameters;
}
