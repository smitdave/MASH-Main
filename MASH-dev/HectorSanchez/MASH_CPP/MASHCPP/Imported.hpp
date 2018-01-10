//  ***************************************************************************
//  includeed.h
//  MASHCPP
//  Header that loads all the files required for the main to work correctly.
//  ***************************************************************************

#ifndef includeed_hpp
#define includeed_hpp


//Loading needed files into work stream
#include "./GlobalParameters/Globals.hpp"
#include "./Mosquito/Mosquito.hpp"
#include "./Mosquito/MosquitoGeneric/MosquitoGeneric.hpp"
#include "./Mosquito/MosquitoGeneric/MosquitoGenericMale/MosquitoGenericMale.hpp"
#include "./Mosquito/MosquitoGeneric/MosquitoGenericFemale/MosquitoGenericFemale.hpp"
#include "./Mosquito/AnophelesGambiae/AnophelesGambiae.hpp"
#include "./Mosquito/AnophelesGambiae/AnophelesGambiaeMale/AnophelesGambiaeMale.hpp"
#include "./Mosquito/AnophelesGambiae/AnophelesGambiaeFemale/AnophelesGambiaeFemale.hpp"
#include "./Site/Site.hpp"
#include "./Site/SiteFeeding/SiteFeeding.hpp"
#include "./Site/SiteMating/SiteMating.hpp"
#include "./Site/SiteSugar/SiteSugar.hpp"
#include "./Site/SiteLaying/SiteLaying.hpp"
#include "./VectorControl/VectorControl.hpp"
#include "./UnitTests/UnitTest.hpp"
#include "./AuxiliaryFunctions/AuxiliaryFunctions.hpp"
#include "./AuxiliaryFunctions/StatisticalFunctions.hpp"
#include "./AuxiliaryFunctions/StatisticalConstants.hpp"
#include "./AuxiliaryFunctions/NormalDistribution.hpp"
#include "./Mosquito/MosquitoLifeParameters.hpp"
#include "./Mosquito/MBITES/Flight.hpp"
#include "./Human/Human.hpp"
#include "./Libraries/CBLAS/include/cblas.h"
#include "./DataFlow/DataExport/DataExport.hpp"
#include "./DataFlow/DataImport/DataImport.hpp"
#include "./Cattle/Cattle.hpp"
#include "./Mosquito/MosquitoPopulation.hpp"
#include "./Mosquito/MosquitoAquatic.hpp"

#endif
