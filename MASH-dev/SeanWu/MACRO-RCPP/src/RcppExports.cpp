// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// rcpp_hello_world
List rcpp_hello_world();
RcppExport SEXP _MACRO_rcpp_hello_world() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(rcpp_hello_world());
    return rcpp_result_gen;
END_RCPP
}
// testHumanPop
void testHumanPop(const Rcpp::IntegerVector pop);
RcppExport SEXP _MACRO_testHumanPop(SEXP popSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Rcpp::IntegerVector >::type pop(popSEXP);
    testHumanPop(pop);
    return R_NilValue;
END_RCPP
}
// testEventQueue
void testEventQueue();
RcppExport SEXP _MACRO_testEventQueue() {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    testEventQueue();
    return R_NilValue;
END_RCPP
}
// testHuman
void testHuman();
RcppExport SEXP _MACRO_testHuman() {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    testHuman();
    return R_NilValue;
END_RCPP
}
// testHumanQ
void testHumanQ();
RcppExport SEXP _MACRO_testHumanQ() {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    testHumanQ();
    return R_NilValue;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_MACRO_rcpp_hello_world", (DL_FUNC) &_MACRO_rcpp_hello_world, 0},
    {"_MACRO_testHumanPop", (DL_FUNC) &_MACRO_testHumanPop, 1},
    {"_MACRO_testEventQueue", (DL_FUNC) &_MACRO_testEventQueue, 0},
    {"_MACRO_testHuman", (DL_FUNC) &_MACRO_testHuman, 0},
    {"_MACRO_testHumanQ", (DL_FUNC) &_MACRO_testHumanQ, 0},
    {NULL, NULL, 0}
};

RcppExport void R_init_MACRO(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}