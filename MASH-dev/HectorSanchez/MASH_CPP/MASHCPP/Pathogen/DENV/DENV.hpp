//  ***************************************************************************
//  DENV.hpp
//  MASHCPP
//  Class definition for Dengue virus
//  ***************************************************************************

#ifndef DENV_hpp
#define DENV_hpp

#include <stdio.h>
#include "../Pathogen.hpp"

class DENV: public Pathogen {
protected:
    char serotype;      //@@ Serotype of the instance of the virus (defined as char to save memory)
public:
};


#endif /* DENV_hpp */
