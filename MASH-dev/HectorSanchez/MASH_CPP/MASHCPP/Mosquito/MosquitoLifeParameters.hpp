//  ***************************************************************************
//  MosquitoLifeParameters.hpp
//  MASHCPP
//  This file contains the declaration of the globals used to setup mosquitoes
//  life cycle variables.
//  ***************************************************************************


#ifndef MosquitoLifeParameters_hpp
#define MosquitoLifeParameters_hpp

#include <stdio.h>


// ** Globals ********************************************
const float BFA_P= .95;                 //@@ (bfa.p)
const float BFA_S=.3;                   //@@ (bfa.s)
const float BFA_T=.95;                  //@@ (bfa.t)
// ** Blood-Feed *****************************************
const float BMA=7.5;                    //@@ (bm.a)
const float BMB=2.5;                    //@@ (bm.b)
// ** Sugar-Feed *****************************************
// ** Estivate *******************************************
// ** Mate ***********************************************
// ** Egg-Laying *****************************************
const float BSM=30;                     //@@ (bs.m)
const float BSV=5;                      //@@ (bs.v)
// ** Survive Flight *************************************
const float MBITES_FP = .98;            //@@ (dhmP$F.p)
const float MBITES_FD = 1-MBITES_FP;    //@@ (dhmP$F.d)
const float MBITES_BP = .98;            //@@ (dhmP$B.p)
const float MBITES_BD = 1-MBITES_BP;    //@@ (dhmP$B.d)
const float MBITES_RP = .98;            //@@ (dhmP$R.p)
const float MBITES_RD = 1-MBITES_RP;    //@@ (dhmP$R.d)
const float MBITES_LP = .98;            //@@ (dhmP$L.p)
const float MBITES_LD = 1-MBITES_LP;    //@@ (dhmP$L.d)
const float MBITES_OP = .98;            //@@ (dhmP$O.p)
const float MBITES_OD = 1-MBITES_OP;    //@@ (dhmP$O.d)
const float MBITES_MP = .98;            //@@ (dhmP$M.p)
const float MBITES_MD = 1-MBITES_MP;    //@@ (dhmP$M.d)
const float MBITES_SP = .98;            //@@ (dhmP$S.p)
const float MBITES_SD = 1-MBITES_SP;    //@@ (dhmP$S.d)
const float MBITES_EP = 1;              //@@ (NA)
const float MBITES_ED = 1-MBITES_EP;    //@@ (NA)
const float MBITES_SNSA = .01;          //@@ (dhmP$sns.a)
const float MBITES_SNSB = 100;          //@@ (dhmP$sns.b)
const float MBITES_TTSZA = 5;           //@@ (dhmP$ttsz.a)
const float MBITES_TTSZB = 95;          //@@ (dhmP$ttsz.b)
const float MBITES_TTSZP = .5;          //@@ (dhmP$ttsz.p)
const float MBITES_TTRA = 10;           //@@ (dhmP$ttr.a)
const float MBITES_TTRB = 500;          //@@ (dhmP$ttr.b)
// ** Swarm **********************************************
const float MBITES_MALE_MS = .99;       //@@ (dhmP$maleM.s)
const float MBITES_MALE_MP = .95;       //@@ (dhmP$maleM.p)
// ** Time ***********************************************

#endif /* MosquitoLifeParameters_hpp */
