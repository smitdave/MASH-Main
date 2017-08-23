#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

#ifndef TESTC
#define TESTC

static double parms[3];

#define k1 parms[0]
#define k2 parms[1]
#define k3 parms[2]
/* initializer  */
void initmod(void (* odeparms)(int *, double *))
{
int N=3;
    odeparms(&N, parms);
}
/* Derivatives and 1 output variable */
void derivs (int *neq, double *t, double *y, double *ydot,
             double *yout, int *ip)
{
    if (ip[0] <1) error("nout should be at least 1");
    ydot[0] = -k1*y[0] + k2*y[1]*y[2];
    ydot[2] = k3 * y[1]*y[1];
    ydot[1] = -ydot[0]-ydot[2];
    yout[0] = y[0]+y[1]+y[2];
}
/* The Jacobian matrix */
void jac(int *neq, double *t, double *y, int *ml, int *mu,
{
double *pd, int *nrowpd, double *yout, int *ip)
pd[0]
pd[1]
pd[2]
pd[(*nrowpd)]
pd[(*nrowpd) + 1]
pd[(*nrowpd) + 2]
pd[(*nrowpd)*2]
pd[2*(*nrowpd) + 1] = -k2 * y[1];
pd[2*(*nrowpd) + 2] = 0.0;
= -k1;
= k1;
= 0.0;
= k2*y[2];
= -k2*y[2] - 2*k3*y[1];
= 2*k3*y[1];
= k2*y[1];
}
