//
//  DataExport.hpp
//  MASHCPP
//
//  Created by Hector Manuel Sanchez Castellanos on 12/16/16.
//  Copyright © 2016 MASH. All rights reserved.
//

#ifndef DataExport_hpp
#define DataExport_hpp

#include <stdio.h>

class DataExport{
protected:
public:
    void exportMosquitoPopulationDynamics(/*fileStream,mosquitoPopulation*/);
    void exportHumanPopulationSpecifications();
};

#endif /* DataExport_hpp */
