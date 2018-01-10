//##################################################################
//##################################################################
//##
//##  The Distributed Hazard Model Version-0.9.0 August 4, 2016
//##
//##  This version was designed and written by David L. Smith (aka.
//##  Dave). Please send bug reports, comments, and suggestions to
//##  <smitdave@gmail.com>.
//##
//##  Robert C. Reiner, Jr. (aka Bobby) <bcreiner@uw.edu>, Hector
//##  Manuel Sanchez Castellanos <sanchez.hmsc@itesm.mx> Sean Wu
//##  <slwu89@berkeley.edu>, and Amit Verma <amit.verma13@gmail.com>
//##  helped with development, debugging and documentation of
//##  version 1.0.
//##
//##  The DHM was conceived of by David Smith, and it was inspired
//##  by discussions with many people, including Bobby, Hector,
//##  Sean, Amit, Arnaud Le Menach, Nick Ruktanonchai, Samson
//##  Kiware, Gerry Killeen, Tom Scott, Ellis McKenzie, Steven W.
//##  Lindsay, Willem Takken, Philip Eckhoff, Nicole Achee, Chris
//##  Barker, Nakul Chitnis, Justin Cohen, Su Yun Kang, Audrey
//##  Lenhart, John Marshall, Phil McCall, Catherine Moyes, Doug
//##  Norris, Alex Perkins, Chris Stone, Edward Wenger, and Anne
//##  Wilson.
//##
//##################################################################
//##################################################################


#include <stdio.h>
#include <random>
#include "boost/random.hpp"
#include <math.h>
#include <iostream>

using std::cout;
using std::cin;
using std::endl;
boost::random::mt19937 MT_GENERATOR(static_cast<unsigned int>(std::time(0)));
#include "Imported.hpp"


int main(int argc, const char * argv[]) {
    cout << "Hello, MASH!" << endl;
    //Seed random generator
    srand (static_cast <unsigned> (time(0)));
    //**************** UNIT TESTS *********************************************
    if(PERFORM_UNIT_TESTS){
        int unitTestErrors=runTests();
        if(unitTestErrors>0){
            //Runs and exits with status "1" if an error on the unit tests was detected
            cout << unitTestErrors << " :: Unit Test Errors!!!" << endl << endl;
            cout << "***** Set 'GlobalParameters/Globals.hpp/PERFORM_UNIT_TESTS' to false to bypass unit tests *****"<< endl;
            cout << endl;
            return 1;
        }
    }
    //runBoostExamples();
    cout << "Goodbye, MASH!" << endl;
    
    NormalDistribution *nd = new NormalDistribution();
    for (int i=0; i < 10; ++i)
    {
        double d = nd->sample();
        std::cout << d << std::endl;
    }

    //cout<<randomBeta(MT_GENERATOR,.5,.5);
    //cout << randomBinomial(MT_GENERATOR);
    //*************************************************************************
    //Mosquito testMosquito;
    //std::cout << "Test Mosquito Constructor State: "
    //  << testMosquito.getState() << "\n";
    //
    //MosquitoFemale testFemale;
    //std::cout << "Test Female Constructor: " << testFemale.getIsFemale() << "\n";
    //
    //MosquitoMale testMale;
    //std::cout << "Test Male Constructor: " << testMale.getIsFemale() << "\n";
    //
    //coordinate coord={10,1};
    //std::cout << coord.latitude << "\n";
    //
    //MosquitoLifeParameters mosquitoLifeParameters=MosquitoLifeParameters();
    //cout << mosquitoLifeParameters.getBloodFeedAttemptProbability() << endl;
    //*************************************************************************

    //**************************************************************************
    //**************************************************************************
    
    cout << endl;
    return 0;
}
