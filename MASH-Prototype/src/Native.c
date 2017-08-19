#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <stdlib.h>

extern SEXP R_eapply_noOut(SEXP envir);

static const R_CallMethodDef CallEntries[] = {
  {"R_eapply_noOut", (DL_FUNC) &R_eapply_noOut, 1},
  {NULL, NULL, 0}
};
void R_init_MASHprototype(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
