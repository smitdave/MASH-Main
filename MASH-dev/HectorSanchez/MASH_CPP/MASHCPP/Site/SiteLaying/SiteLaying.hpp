//  ***************************************************************************
//  SiteLaying.hpp
//  MASH
//  Egg-laying site definition. Inherits from Site class.
//  ***************************************************************************

#ifndef SiteLaying_hpp
#define SiteLaying_hpp

#include <stdio.h>
#import "../Site.hpp"
#include "../../VectorControl/SourceReduction/SourceReduction.hpp"
#include "../../VectorControl/BiologicalControl/BiologicalControl.hpp"
#include "../../VectorControl/Larviciding/Larviciding.hpp"
#include "../../VectorControl/Ovitrap/Ovitrap.hpp"
#include "../../VectorControl/SourceReduction/SourceReduction.hpp"

//Inherits from Site
class SiteLaying: public Site {
private:
    float size;
    Ovitrap             ovitrap;            //@@ Ovitrap object in site
    Larviciding         larviciding;        //@@ Larviciding object in site
    SourceReduction     sourceReduction;    //@@ Source reduction object in site
    BiologicalControl   biologicalControl;  //@@ Biological Control object in site
public:
    //Constructors
    SiteLaying();
    //Accessors
    Ovitrap             getOvitrap();
    Larviciding         getLarviciding();
    SourceReduction     getSourceReduction();
    BiologicalControl   getBiologicalControl();
    //Mutators
};

#endif /* SiteLaying_hpp */
