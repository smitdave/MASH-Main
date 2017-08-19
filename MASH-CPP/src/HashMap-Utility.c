///////////////////////////////////////////////////////////////////////////////
//       __  __           __    __  ___
//      / / / /___ ______/ /_  /  |/  /___ _____
//     / /_/ / __ `/ ___/ __ \/ /|_/ / __ `/ __ \
//    / __  / /_/ (__  ) / / / /  / / /_/ / /_/ /
//   /_/ /_/\__,_/____/_/ /_/_/  /_/\__,_/ .___/
//                                      /_/
//
//   MASH-CPP
//   HashMap C Helper Functions
//   Sean Wu
//   August 18, 2017
//
///////////////////////////////////////////////////////////////////////////////

// include headers
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <stdlib.h>
#include <stdint.h>
#include <Rmath.h>
#include <stdio.h>
#include <R_ext/Rdynload.h>

// eapply functionality with no output
SEXP R_eapplyInvisible(SEXP envir){
  if(!isEnvironment(envir)){
    error("envir should be an environment");
  }

  return R_NilValue;
}
