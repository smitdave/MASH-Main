//
//  VectorControlParameters.hpp
//  MASHCPP
//

#ifndef VectorControlParameters_hpp
#define VectorControlParameters_hpp

#include <stdio.h>
#include "../GlobalParameters/Globals.hpp"

// ****** Vector Control Keys *********************//
// ARR: Area Repel
// ARS: Area Spray
// ASB: Sugar Bait
// BAI: Baited Trap
// BIO: Biological Control
// EAV: Eave Tubes
// GEM: Genetic Modification
// IRS: Indoor Residual Spray
// ITN: Insecticide Treated Net
// IVM: Ivermectin
// LAR: Larvaciding
// OVI: Ovitraps
// PEP: Personal Protection
// PER: Personal Repellant
// SOU: Source Control
// SWS: Swarm Spraying
// ZOO: Zoospraying
// ****** Defining Vector Control Activation Bools *//
const bool ARR_ACTIVE=false;
const bool ARS_ACTIVE=false;
const bool ASB_ACTIVE=false;
const bool BAI_ACTIVE=false;
const bool BIO_ACTIVE=false;
const bool EAV_ACTIVE=false;
const bool GEM_ACTIVE=false;
const bool IRS_ACTIVE=false;
const bool ITN_ACTIVE=false;
const bool IVM_ACTIVE=false;
const bool LAR_ACTIVE=false;
const bool OVI_ACTIVE=false;
const bool PEP_ACTIVE=false;
const bool PER_ACTIVE=false;
const bool SOU_ACTIVE=false;
const bool SWS_ACTIVE=false;
const bool ZOO_ACTIVE=false;
// ****** Defining Vector Control Bases *************//
// These should be viewed as the initializators but vector control interventions should be implemented in a flexible way to allow variations such as efficacy decay, different effectiveness depending on how they were applied, etcetera.
typedef struct {
    bool active;
    float repelProbability;
    float killProbability;
    float probeBlockProbability;
    float probeKillProbability;
    float feedBlockProbability;
} vectorControlParametersSet;

const vectorControlParametersSet NON_PARMS = {false,0,0,0,0,0};
const vectorControlParametersSet ARR_PARMS = {false,0,0,0,0,0};
const vectorControlParametersSet ARS_PARMS = {false,0,0,0,0,0};
const vectorControlParametersSet ASB_PARMS = {false,0,.95,0,0,0};
const vectorControlParametersSet BAI_PARMS = {false,0,0,0,0,0};
const vectorControlParametersSet BIO_PARMS = {false,0,0,0,0,0};
const vectorControlParametersSet EAV_PARMS = {false,0,0,0,0,0};
const vectorControlParametersSet GEM_PARMS = {false,0,0,0,0,0};
const vectorControlParametersSet IRS_PARMS = {false,.5,.75,0,0,0};
const vectorControlParametersSet ITN_PARMS = {false,.9,0,0,0,0};
const vectorControlParametersSet IVM_PARMS = {false,0,0,0,0,0};
const vectorControlParametersSet LAR_PARMS = {false,0,0,0,0,0};
const vectorControlParametersSet OVI_PARMS = {false,0,0,0,0,0};
const vectorControlParametersSet PEP_PARMS = {false,0,0,0,0,0};
const vectorControlParametersSet PER_PARMS = {false,0,0,0,0,0};
const vectorControlParametersSet SOU_PARMS = {false,0,0,0,0,0};
const vectorControlParametersSet SWS_PARMS = {false,0,0,0,0,0};
const vectorControlParametersSet ZOO_PARMS = {false,0,0,0,0,0};

#endif /* VectorControlParameters_hpp */
