//
//  UnitTest.c
//  MASHCPP
//
//  Created by Hector Manuel Sanchez Castellanos on 11/18/16.
//  Copyright © 2016 MASH. All rights reserved.
//

#include "UnitTest.hpp"

int runTests(){
    //@ Wrapper of the unit functions files to test life, the universe and everything
    std::cout << "************ UNIT TESTS SUMMARY ************\n";
    int totalErrorCount=0;
    //*************************************************************************
    //**** Auxiliary functions ************************************************
    int errorCountAuxiliaryFunctions=runTestsAuxiliaryFunctions();
    totalErrorCount=totalErrorCount+errorCountAuxiliaryFunctions;
    std::cout << "Auxiliary functions errors: " << errorCountAuxiliaryFunctions << "\n";
    //**** Mosquito functions *************************************************
    int errorCountMosquito=runTestsMosquito();
    totalErrorCount=totalErrorCount+errorCountMosquito;
    std::cout << "Mosquito functions errors: " << errorCountMosquito << "\n";
    //**** Vector Control functions *******************************************
    int errorsCountCattle=runTestsCattle();
    totalErrorCount=totalErrorCount+errorsCountCattle;
    std::cout << "Cattle functions errors: " << errorsCountCattle << "\n";
    //**** Site functions *****************************************************
    int errorCountSite=runTestsSite();
    totalErrorCount=totalErrorCount+errorCountSite;
    std::cout << "Site errors: " << errorCountSite << "\n";
    //**** Vector Control functions *******************************************
    int errorCountVectorControl=runTestsVectorControl();
    totalErrorCount=totalErrorCount+errorCountVectorControl;
    std::cout << "Vector Control errors: " << errorCountVectorControl << "\n";
    //**** Human functions ****************************************************
    int errorCountHuman=runTestsHuman();
    totalErrorCount=totalErrorCount+errorCountHuman;
    std::cout << "Human errors: " << errorCountHuman << "\n";
    //**** Mosquito Population functions **************************************
    int errorCountMosquitoPopulation=runtTestsMosquitoPopulation();
    totalErrorCount=totalErrorCount+errorCountMosquitoPopulation;
    std::cout << "Mosquito Population errors: " << errorCountMosquitoPopulation << "\n";
    //*************************************************************************
    std::cout << "Number of errors: " << totalErrorCount << "\n";
    std::cout << "********************************************\n";
    return totalErrorCount;
}

int runTestsAuxiliaryFunctions(){
    //@ Routine to test functions contained in "AuxiliaryFunctions.hpp"
    int errorCount=0;
    //
    coordinate posA = {0,0,0};
    coordinate posB = {5,10,10};
    double distance = calculateEuclideanDistance(posA, posB);
    if(distance!=15){errorCount++;}
    //SigmoidA---------------
    float sigmoidAResult = sigmoidA(0,10,5);
    if(sigmoidAResult>.17){errorCount++;}
    //SigmoidB---------------
    float sigmoidBResult = sigmoidB(0,5,2);
    if(sigmoidBResult!=1){errorCount++;}
    //RandomBinomial---------
    int trueCount = 0;
    for(int i=0;i<=1000;i++){if(randomBinomial(.25)){trueCount++;}}
    if(trueCount>300){errorCount++;}
    //Return Errors Count----
    return errorCount;
}

int runTestsMosquito(){
    //@ Routine to test functionsc containeed in "Mosquito.hpp"
    int errorCount=0;
    Mosquito mosquitoTest;
    MosquitoGenericFemale* mosquitoFemaleTest=new MosquitoGenericFemale();
    //Set state--------------
    mosquitoTest.setState(M);
    if(mosquitoTest.getState()!=M){errorCount++;}
    //std::cout << mosquitoTest.getState();
    //Die--------------------
    mosquitoTest.boutD();
    if(mosquitoTest.getState()!=D){errorCount++;}
    //std::cout << mosquitoTest.getState();
    //Estivate---------------
    mosquitoTest.boutE();
    if(mosquitoTest.getState()!=F){errorCount++;}
    //DHM--------------------
    mosquitoFemaleTest->setState('X');
    mosquitoFemaleTest->MBITES();
    //std::cout << mosquitoFemaleTest->getState();
    if(mosquitoFemaleTest->getState()!=D){errorCount++;}
    //Flight-----------------
    if(getFlightSurvivalProbability('O')!=MBITES_OP){errorCount++;};
    //Return Errors Count----
    delete mosquitoFemaleTest;
    return errorCount;
}

int runTestsSite(){
    //@ Routine to test functionsc containeed in "Site.hpp"
    int errorCount=0;
    Site siteA=Site(0,{0,0,0});
    Site siteB=Site(1,{5,10,10});
    //Test distance calculation between sites
    double distanceBetweenSites=siteA.distanceToSite(siteB);
    if(distanceBetweenSites!=15){errorCount++;}
    //Testing sugar sites definitions
    SiteSugar sugarSiteTest = SiteSugar();
    ATSB declaredATSB = sugarSiteTest.getATSB();
    if(declaredATSB.getParametersSet().killProbability != ASB_PARMS.killProbability){errorCount++;}
    //Testing mating sites definitions
    SiteMating matingSiteTest = SiteMating();
    SwarmSpray declaredSwarm = matingSiteTest.getSwarmSpray();
    if(declaredSwarm.getParametersSet().killProbability != SWS_PARMS.killProbability){errorCount++;}
    //Return Errors Count----
    return errorCount;
}

int runTestsVectorControl(){
    //@ Routine to test functions in "VectorControl.hpp"
    int errorCount=0;
    //Testing VC definition
    VectorControl vcTest = VectorControl();
    //Testing IRS definition
    IndoorResidualSpray testIRS = IndoorResidualSpray();
    vectorControlParametersSet paramsTest = testIRS.getParametersSet();
    if(paramsTest.repelProbability!=IRS_PARMS.repelProbability){errorCount++;}
    //Return Errors Count----
    return errorCount;
}

int runTestsCattle(){
    //@ Routine to tests functions in "Cattle.hpp"
    int errorCount=0;
    //Cattle attractiveness test
    Cattle cattleTest=Cattle(10);
    if(cattleTest.getAttractiveness()!=cattleTest.getQuantity()){errorCount++;}
    //Return Errors Count----
    return errorCount;
}

int runTestsHuman(){
    //@ Routine to tests functions in "Human.hpp"
    int errorCount=0;
    //Cattle attractiveness test
    Human humanTest=Human();
    InsecticideTreatedNet humanITN=humanTest.getInsecticideTreatedNet();
    if(humanITN.getParametersSet().repelProbability!=ITN_PARMS.repelProbability){errorCount++;}
    //Return Errors Count----
    return errorCount;
}

int runtTestsMosquitoPopulation(){
    //@ Routine to tests functions in "MosquitoPopulation.hpp"
    int errorCount=0;
    //Constructor test
    int size=10;
    MosquitoPopulation populationTest=MosquitoPopulation(size,D,size);
    std::vector<Mosquito> testPop=populationTest.getAdultPopulation();
    if(testPop.at(1).getState()!=D){errorCount++;}
    if(populationTest.countMosquitosInBout(D)!=size){errorCount++;}
    //Replacement test
    populationTest.printAdultMosquitoStates();
    populationTest.insertImagosIntoAdultPopulation();
    populationTest.printAdultMosquitoStates();
    //Return Errors Count----
    return errorCount;
}
