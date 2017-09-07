#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <stdlib.h>
#include <stdint.h>
#include <Rmath.h>
#include <stdio.h>


// #define ISNULL(x) ((x) == R_NilValue)
// #define HSIZE	  49157	/* The size of the hash table for symbols */
//
// // # define extern0 attribute_hidden
// # define extern0 extern
//
// #define CHECK_HASH_TABLE(table) do {		\
// if (TYPEOF(table) != VECSXP)		        \
//   error("bad hash table contents");	  \
// } while (0)                                                    \
//
// #define NONEMPTY_(_FRAME_) \
// CHAR(PRINTNAME(TAG(_FRAME_)))[0] != '.' && CAR(_FRAME_) != R_UnboundValue
//
// extern0 SEXP*	R_SymbolTable;	    /* The symbol table */


// rho is the environment passed to evaluate
SEXP R_eapply_noOut(SEXP rho){

  if(!isEnvironment(rho)){
    error("rho should be an environment");
  }

  return(R_NilValue);
}

// /*
//  *
//  * trying to get eapply to do what i want it to do.
//  *
//  */
//
//
// /* size defs */
// static int BuiltinSize(int all, int intern){
//   int count = 0;
//   SEXP s;
//   int j;
//   for (j = 0; j < HSIZE; j++) {
//     for (s = R_SymbolTable[j]; s != R_NilValue; s = CDR(s)) {
//       if (intern) {
//         if (INTERNAL(CAR(s)) != R_NilValue)
//           count++;
//       }
//       else {
//         if ((all || CHAR(PRINTNAME(CAR(s)))[0] != '.')
//               && SYMVALUE(CAR(s)) != R_UnboundValue)
//           count++;
//       }
//     }
//   }
//   return count;
// }
//
// static int FrameSize(SEXP frame, int all)
// {
//   int count = 0;
//   if (all) {
//     while (frame != R_NilValue) {
//       count += 1;
//       frame = CDR(frame);
//     }
//   } else {
//     while (frame != R_NilValue) {
//       if (NONEMPTY_(frame))
//         count += 1;
//       frame = CDR(frame);
//     }
//   }
//   return count;
// }
//
// static int HashTableSize(SEXP table, int all){
//   CHECK_HASH_TABLE(table);
//   int count = 0;
//   int n = length(table);
//   int i;
//   for (i = 0; i < n; i++)
//     count += FrameSize(VECTOR_ELT(table, i), all);
//   return count;
// }
//
//
// /* values defs */
// static void BuiltinValues(int all, int intern, SEXP values, int *indx)
//   {
//     SEXP s, vl;
//     int j;
//     for (j = 0; j < HSIZE; j++) {
//       for (s = R_SymbolTable[j]; s != R_NilValue; s = CDR(s)) {
//         if (intern) {
//           if (INTERNAL(CAR(s)) != R_NilValue) {
//             vl = SYMVALUE(CAR(s));
//             if (TYPEOF(vl) == PROMSXP) {
//               PROTECT(vl);
//               vl = eval(vl, R_BaseEnv);
//               UNPROTECT(1);
//             }
//             SET_VECTOR_ELT(values, (*indx)++, lazy_duplicate(vl));
//           }
//         }
//         else {
//           if ((all || CHAR(PRINTNAME(CAR(s)))[0] != '.')
//                 && SYMVALUE(CAR(s)) != R_UnboundValue) {
//             vl = SYMVALUE(CAR(s));
//             if (TYPEOF(vl) == PROMSXP) {
//               PROTECT(vl);
//               vl = eval(vl, R_BaseEnv);
//               UNPROTECT(1);
//             }
//             SET_VECTOR_ELT(values, (*indx)++, lazy_duplicate(vl));
//           }
//         }
//       }
//     }
//   }
//
// static void FrameValues(SEXP frame, int all, SEXP values, int *indx)
// {
//   if (all) {
//     while (frame != R_NilValue) {
// #         define DO_FrameValues						                       \
//       SEXP value = CAR(frame);					                         \
//       if (TYPEOF(value) == PROMSXP) {				                   \
//         PROTECT(value);						                               \
//         value = eval(value, R_GlobalEnv);			                \
//         UNPROTECT(1);						                                 \
//       }								                                             \
//       SET_VECTOR_ELT(values, *indx, lazy_duplicate(value));	\
//       (*indx)++
//
//     DO_FrameValues;
//       frame = CDR(frame);
//     }
//   } else {
//     while (frame != R_NilValue) {
//       if (NONEMPTY_(frame)) {
//         DO_FrameValues;
//       }
//       frame = CDR(frame);
//     }
//   }
// }
// #undef DO_FrameValues
//
//
// static void HashTableValues(SEXP table, int all, SEXP values, int *indx)
// {
//   CHECK_HASH_TABLE(table);
//   int n = length(table);
//   int i;
//   for (i = 0; i < n; i++)
//     FrameValues(VECTOR_ELT(table, i), all, values, indx);
// }
//
//
// /* actual function defs */
// SEXP R_hashApply(SEXP call, SEXP op, SEXP args, SEXP rho)
// {
//     SEXP env, R_fcall, FUN, tmp, tmp2, ind;
//     int i, k, k2;
//     int  all;
//
//     PROTECT(env = eval(CAR(args), rho));
//     if(ISNULL(env)){
//       error("use of NULL environment is defunct");
//     }
//     if(!isEnvironment(env)){
//       error("argument must be an environment");
//     }
//
//     FUN = CADR(args);
//     if(!isSymbol(FUN)){
//       error("arguments must be symbolic");
//     }
//
//     /* 'all.names' : */
//     all = asLogical(eval(CADDR(args), rho));
//     if (all == NA_LOGICAL) all = 0;
//
//
//     if(env == R_BaseEnv || env == R_BaseNamespace){
//       k = BuiltinSize(all, 0);
//     } else if(HASHTAB(env) != R_NilValue){
//       k = HashTableSize(HASHTAB(env), all);
//     } else {
//       k = FrameSize(FRAME(env), all);
//     }
//
//     PROTECT(tmp2 = allocVector(VECSXP, k));
//
//     k2 = 0;
//     if(env == R_BaseEnv || env == R_BaseNamespace){
//       BuiltinValues(all, 0, tmp2, &k2);
//     } else if(HASHTAB(env) != R_NilValue){
//       HashTableValues(HASHTAB(env), all, tmp2, &k2);
//     } else {
//       FrameValues(FRAME(env), all, tmp2, &k2);
//     }
//
//     SEXP Xsym = install("X");
//     SEXP isym = install("i");
//     PROTECT(ind = allocVector(INTSXP, 1));
//     /* tmp :=  `[`(<elist>, i) */
//     PROTECT(tmp = LCONS(R_Bracket2Symbol,
// 		LCONS(Xsym, LCONS(isym, R_NilValue))));
//     /* fcall :=  <FUN>( tmp, ... ) */
//     PROTECT(R_fcall = LCONS(FUN, LCONS(tmp, LCONS(R_DotsSymbol, R_NilValue))));
//
//     defineVar(Xsym, tmp2, rho);
//     SET_NAMED(tmp2, 1);
//     defineVar(isym, ind, rho);
//     SET_NAMED(ind, 1);
//
//     for(i = 0; i < k2; i++) {
// 	     INTEGER(ind)[0] = i+1;
// 	      // SEXP tmp = R_forceAndCall(R_fcall, 1, rho);
// 	      R_forceAndCall(R_fcall, 1, rho);
//         // if(MAYBE_REFERENCED(tmp)){
//         //   tmp = lazy_duplicate(tmp);
//         // }
//         // SET_VECTOR_ELT(ans, i, tmp);
//     }
//
//
//     UNPROTECT(5);
//     return(R_NilValue);
// }
