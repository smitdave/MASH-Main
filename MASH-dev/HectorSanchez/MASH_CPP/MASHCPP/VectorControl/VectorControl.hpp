//  ***************************************************************************
//  VectorControl.hpp
//  MASH
//  Vector control intervention generic class definition.
//  ***************************************************************************


#ifndef VectorControl_hpp
#define VectorControl_hpp

#include <stdio.h>
#include "./VectorControlParameters.hpp"

class VectorControl{
protected:
    vectorControlParametersSet parameters;
public:
    //Constructors
    VectorControl();
    //Accessors
    vectorControlParametersSet getParametersSet();
};

#endif /* VectorControl_hpp */
