#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <stdlib.h>

extern SEXP R_eapply_noOut(SEXP envir);

// ELP model
extern void init_ELP(void (* odeparms)(int *, double *));
extern void ode_ELP(int *neq, double *t, double *y, double *ydot, double *yout, int*ip);


// for .C interface
static const R_CMethodDef CEntries[] = {
      {"init_ELP",     (DL_FUNC) &init_ELP,     1},
      {"ode_ELP",      (DL_FUNC) &ode_ELP,      6},
      {NULL, NULL, 0}
};


// for .Call interface
static const R_CallMethodDef CallEntries[] = {
   {"R_eapply_noOut", (DL_FUNC) &R_eapply_noOut, 1},
   {NULL, NULL, 0}
 };

void R_init_MASHprototype(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
