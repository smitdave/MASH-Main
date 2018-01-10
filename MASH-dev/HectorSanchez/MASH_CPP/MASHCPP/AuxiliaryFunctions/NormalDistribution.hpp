//
//  NormalDistribution.hpp
//  MASHCPP
//
//  Created by Hector Manuel Sanchez Castellanos on 1/19/17.
//  Copyright © 2017 MASH. All rights reserved.
//

#ifndef NormalDistribution_hpp
#define NormalDistribution_hpp

#include <stdio.h>
#include <boost/random.hpp>
#include <boost/random/normal_distribution.hpp>
class NormalDistribution
{
public:
    //http://stackoverflow.com/questions/10831003/efficient-boost-distribution-usage
    NormalDistribution();
    double sample(void);
private:
    // Use the boost random number generator
    boost::mt19937 rng;
    // Make a variate_generator OBJECT.
    boost::variate_generator<boost::mt19937&,boost::normal_distribution<> > var_nor;
};

#endif /* NormalDistribution_hpp */
