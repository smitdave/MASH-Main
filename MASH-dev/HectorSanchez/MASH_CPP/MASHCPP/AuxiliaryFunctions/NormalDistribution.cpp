//
//  NormalDistribution.cpp
//  MASHCPP
//
//  Created by Hector Manuel Sanchez Castellanos on 1/19/17.
//  Copyright © 2017 MASH. All rights reserved.
//

#include "NormalDistribution.hpp"

NormalDistribution::NormalDistribution():
rng(), var_nor(rng, boost::normal_distribution<>(0.0, 1.0))
{
    //std::cout << "Called normal distribution constructor, passing up var_nor" << std::endl;
}

double NormalDistribution::sample(void) {
    double x = var_nor();
    return x;
}
