//  ***************************************************************************
//  SiteSugar.hpp
//  MASH
//  Sugar source site definition. Inherits from Site class.
//  ***************************************************************************

#ifndef SiteSugar_hpp
#define SiteSugar_hpp

#include <stdio.h>
#import "../Site.hpp"
#include "../../VectorControl/ATSB/ATSB.hpp"

//Inherits from Site
class SiteSugar: public Site {
private:
    float sugarQuantity;        //@@ Sugar available in the site
    ATSB    atsb;               //@@ Attractive sugar bait object in site
public:
    //Constructors
    SiteSugar();
    //Accessors
    float getSugar();
    ATSB getATSB();
    //Mutators
    void setSugar(float sugarQuantityIn);
    //Actions
    void increaseSugar(float sugarIncrease);
    void decreaseSugar(float sugarDecrease);
};

#endif /* SiteSugar_hpp */
