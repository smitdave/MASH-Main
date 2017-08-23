///////////////////////////////////////////////////////////////////////////////
//
//      ___   ____  __  _____
//     /   | / __ \/ / / /   |
//    / /| |/ / / / / / / /| |
//   / ___ / /_/ / /_/ / ___ |
//  /_/  |_\___\_\____/_/  |_|
//
//  MASH-CPP
//  AQUATIC ECOLOGY
//  ELP ODE definition
//  Sean Wu
//  August 23, 2017
//
///////////////////////////////////////////////////////////////////////////////

#include <R.h>

/* parms array of double to hold parameters */
static double parms[6];

/* function parameters */
#define f          parms[0]
#define v          parms[1]
#define alpha      parms[2]
#define gamma      parms[3]
#define psi        parms[4]
#define g          parms[5]


/* initializer: only two state variables */
void init_ELP(void (* odeparms)(int *, double *))
{
    int N = 2;
    odeparms(&N, parms);
}

/* derivatives */
void ode_ELP(int *neq, double *t, double *y, double *ydot, double *yout, int*ip)
{
    if (ip[0] < 2) error("nout should be >= 2");
    ydot[0] = f*v*y[1] - (alpha + gamma + psi*y[1])*y[0];
    ydot[1] = alpha*y[0] - g*y[1];

}
