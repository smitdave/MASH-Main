///////////////////////////////////////////////////////////////////////////////
//      ____  ________           ___
//     / __ \/ __/ __ \___  ____/ (_)___ _________  ___
//    / /_/ / /_/ /_/ / _ \/ __  / / __ `/ ___/ _ \/ _ \
//   / ____/ __/ ____/  __/ /_/ / / /_/ / /  /  __/  __/
//  /_/   /_/ /_/    \___/\__,_/_/\__, /_/   \___/\___/
//                               /____/
//
//  PfPedigree Class Definition
//  MASH Team
//  December 2017
//
///////////////////////////////////////////////////////////////////////////////

#ifndef _MASHCPP_PFPEDIGREE_HPP_
#define _MASHCPP_PFPEDIGREE_HPP_

#include <Rcpp.h>

#include "MASHcpp/DEBUG.hpp"



namespace MASHcpp {

class PfPedigree {

public:

private:


};


//   PfPedigree <- R6Class("PfPedigree",
//
//                       public = list(
//                         initialize = function(){
//                           private$nAntigenLoci = 9
//                           private$nptypes = c(3,5,4,3,6,3,2,7,9)
//                           private$PedLength = 0
//                         },
//
//                         add2Pedigree = function(pf){
//                           pfid = pf$get_pfid()
//                           private$gtype[[pfid]] = pf$get_gtype()
//                           private$ptype[[pfid]] = pf$get_ptype()
//                           private$mic[[pfid]] = pf$get_mic()
//                           private$mac[[pfid]] = pf$get_mac()
//                           private$PedLength = private$PedLength +1
//                         },
//
//                         get_PedLength = function(){
//                           private$PedLength
//                         },
//
//                         get_gtype = function(pfid){
//                           private$gtype[[pfid]]
//                         },
//
//                         get_ptype = function(pfid){
//                           private$ptype[[pfid]]
//                         },
//
//                         get_mic = function(pfid){
//                           private$mic[[pfid]]
//                         },
//
//                         get_mac = function(pfid){
//                           private$mac[[pfid]]
//                         },
//
//                         get_th = function(pfid){
//                           private$th[[pfid]]
//                         },
//
//                         set_th = function(pfid,thnew){
//                           private$th[[pfid]] = thnew
//                         },
//
//                         get_thEnd = function(pfid){
//                           private$thEnd[[pfid]]
//                         },
//
//                         set_thEnd = function(pfid,thEndnew){
//                           private$thEnd[[pfid]] = thEndnew
//                         },
//
//                         get_sib = function(pfid){
//                           private$sib[[pfid]]
//                         },
//
//                         get_nAntigenLoci = function(){
//                           private$nAntigenLoci
//                         },
//
//                         get_nptypes = function(){
//                           private$nptypes
//                         }
//                       ),
//
//                       private = list(
//
//                         PedLength = integer(0),
//                         gtype = list(),
//                         ptype = list(),
//                         mic = list(),
//                         mac = list(),
//                         th = list(),
//                         thEnd = list(),
//                         ixh = list(),
//                         tm = list(),
//                         tmEnd = list(),
//                         ixm = list(),
//                         sib = list(),
//                         nAntigenLoci = integer(0),
//                         nptypes = NULL
//
//                       )
// )




}

#endif
