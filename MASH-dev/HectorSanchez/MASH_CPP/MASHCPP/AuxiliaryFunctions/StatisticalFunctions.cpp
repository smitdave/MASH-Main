//
//  StatisticalFunctions.cpp
//  MASHCPP
//
//  Created by Hector Manuel Sanchez Castellanos on 12/28/16.
//  Copyright © 2016 MASH. All rights reserved.
//

#include "StatisticalFunctions.hpp"

int randomUniformInt(mt_generator generator, int min, int max){
    //@ Generates a random integer sampled from a uniform distribution
    boost::random::uniform_int_distribution<> dist(min,max);
    return dist(generator);
}
float randomFloatZeroToOne(mt_generator generator){
    //@ Generates a random float from zero to one sampled from a uniform distribution
    boost::uniform_01<double> u01;
    return u01(generator);
}
bool randomBinomial(float probability){
    //@ This function generates a binomial event with a given probability but should be superseeded with BOOST
    bool var=false;
    if(rand()<probability){var=true;}
    return var;
}
float randomBeta(mt_generator generator, float alpha, float beta){
    //@ Generates a random beta distribution and samples a number from it (this scheme should be improved for memory efficiency)
    float randFromUnif=randomFloatZeroToOne(generator);
    boost::math::beta_distribution<> dist(alpha, beta);
    double randFromDist = boost::math::quantile(dist, randFromUnif);
    return randFromDist;
}
float randomNormal(mt_generator generator, float mean, float standardDeviation){
    //@ Generates a normal distribution and samples from it (this scheme should be improved for memory efficiency)
    boost::normal_distribution<> normalDistribution(mean, standardDeviation);
    boost::variate_generator<boost::mt19937&,boost::normal_distribution<>> var_nor(generator, normalDistribution);
    float randomSample=var_nor();
    return randomSample;
}


void runBoostExamples(){
    //@ Temporary function intended to be used as a template for future use of BOOST-related functions
    using std::cout;
    using std::cin;
    using std::endl;
    
    cout << "--------------------------------------------" << endl;
    cout << "BOOST Random Numbers Examples" << endl;
    cout << "--------------------------------------------" << endl;
    //**** Random number generator seeded ++++++++++++++++++++++++
    mt_generator generator(static_cast<unsigned int>(std::time(0)));
    //gen.seed(static_cast<unsigned int>(std::time(0)));
    //**** uniform_int_distribution ++++++++++++++++++++++++++++++
    int runiform = randomUniformInt(generator,0,5);
    cout << "uniform_int_distribution: " << runiform <<endl;
    //**** uniform_01 ++++++++++++++++++++++++++++++++++++++++++++
    float rfloat = randomFloatZeroToOne(generator);
    cout << "uniform_01: "<< rfloat << endl;
    //**** uniform_int --------------------------------------------
    const int rangeMin = 1;
    const int rangeMax = 10;
    //typedef boost::uniform_real<> NumberDistribution;
    typedef boost::variate_generator<mt_generator&,uniform_int_distribution> variate_generator_int;
    uniform_int_distribution distribution(rangeMin, rangeMax);
    variate_generator_int numberGenerator(generator, distribution);
    cout << "uniform_int: " << numberGenerator() << std::endl;
    //**** uniform_float -----------------------------------------
    typedef boost::variate_generator<mt_generator&,uniform_float_distribution> variate_generator_float;
    uniform_float_distribution distribution2(rangeMin, rangeMax);
    variate_generator_float numberGenerator2(generator, distribution2);
    cout << "uniform_float: " << numberGenerator() << std::endl;
    //**** bernoulli_distribution---------------------------------
    //https://theboostcpplibraries.com/boost.random
    bernoulli_distribution bernoulliSample(.5);
    cout << "bernoulli_distribution: " << bernoulliSample(generator) << endl;
    //**** normal_distribution---------------------------------
    //https://theboostcpplibraries.com/boost.random
    boost::random::normal_distribution<> normalSample(0,1);
    cout << "normal_distribution: " << normalSample(generator) << endl;
    //**** normal_distribution---------------------------------
    //https://theboostcpplibraries.com/boost.random
    boost::random::exponential_distribution<> exponentialSample(1);
    cout << "exponential_distribution: " << exponentialSample(generator) << endl;
}
