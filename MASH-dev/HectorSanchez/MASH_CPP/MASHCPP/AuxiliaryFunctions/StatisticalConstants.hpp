//
//  StatisticalConstants.hpp
//  MASHCPP
//
//  Created by Hector Manuel Sanchez Castellanos on 12/29/16.
//  Copyright © 2016 MASH. All rights reserved.
//

#ifndef StatisticalConstants_hpp
#define StatisticalConstants_hpp

#include <stdio.h>
#include "boost/random.hpp"
#include <boost/math/distributions.hpp>

typedef boost::random::mt19937 mt_generator;
typedef boost::random::bernoulli_distribution<> bernoulli_distribution;
typedef boost::random::normal_distribution<> normal_distribution;
typedef boost::random::exponential_distribution<> exponential_distribution;
typedef boost::uniform_int<> uniform_int_distribution;
typedef boost::uniform_real<> uniform_float_distribution;
typedef boost::math::beta_distribution<> beta_distribution;

#endif /* StatisticalConstants_hpp */
