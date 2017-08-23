# serious stuff going on here

library(MASHprototype)
library(deSolve)
parms <- c(k1 = 0.04, k2 = 1e4, k3=3e7)
Y     <- c(y1 = 1.0, y2 = 0.0, y3 = 0.0)
times <- c(0, 0.4*10^(0:11) )

out <- ode(Y, times, func = "derivs", parms = parms,
           jacfunc = "jac", dllname = "deSolveModel",
           initfunc = "initmod", nout = 1, outnames = "Sum")
