###############################################################################
#       ______      ____ 
#      /_  __/___ _/ __ \
#       / / / __ `/ /_/ /
#      / / / /_/ / _, _/ 
#     /_/  \__,_/_/ |_|  
#  
#     Test TaR for Bioko
#     Daniel Citron, David Smith, Sean Wu
#     August 2018
#
###############################################################################


###############################################################################
# load data
###############################################################################

rm(list=ls());gc()
library(MASS)

directory <- "/Users/slwu89/Desktop/git/MASH-Main/MASH-dev/DanielCitron/TaR_FOI/"

# psi
psi <- as.matrix(read.csv(paste0(directory,"pij.csv"),
                  col.names = c("x","ban.area", "mal.area", "bas.area", "off", "ban", "lub", "mal", "mok", "ria", "ure"),
                  row.names = c("ban.area", "mal.area", "bas.area", "off", "ban", "lub", "mal", "mok", "ria", "ure")))
psi <- psi[,2:11]

# X
X_est <- c(0.17300000, 0.15575000, 0.20550000, # example areas
           0.50000000, 0.10624307, 0.10887395, 0.15729471, 0.01386838, 0.12828326, 0.17975000 # regions
)
X_est <- as.matrix(X_est)


###############################################################################
# TaR model objective function, constraints function, gradients and jacobians
###############################################################################

# recovery rate
r = 1/200

# original solution
g <- r*X_est/(1-X_est)
h <- ginv(psi) %*% g
h

# nlopt library
library(nloptr)

# data is PfPR on odds scale
pr <- (r*X_est) / (1 - X_est)

# objective function (minimize SSE)
make_eval_f <- function(pr,psi){
  
  pr <- pr
  psi <- psi
  
  f <- function(theta){
    psi_hat <- psi
    # psi matrix
    psi_hat[2,1] <- theta[11]
    psi_hat[2,2] <- psi_hat[2,2] - theta[11] - theta[12]
    psi_hat[2,3] <- theta[12]
    # predicted PfPR
    pr_hat  <- psi_hat %*% matrix(theta[1:10],ncol=1)
    # minimize sum of squared errors
    sum((pr - pr_hat)^2)
  }
  
  return(f)
}


eval_f <- make_eval_f(pr,psi)

lb <- rep(0,12)
ub <- rep(Inf,12)

# inequality constraints
make_eval_g_ineq <- function(pr,psi){
  
  pr <- pr
  psi <- psi
  
  f <- function(theta){
    rbind(
      # FOI constraint 1
      -((theta[1]/theta[2]) - 100),
      # FOI constraint 2
      -((theta[3]/theta[2]) - 100),
      # FOI constraint 3
      -((sum(theta[c(5,6,8,9,10)])/theta[7]) - 500),
      # psi constraint 1
      -(psi[2,2] - theta[11] - theta[12]),
      # psi constraint 2
      -theta[11],
      # psi constraint 3
      -theta[12]
    )
  }
  
  return(f)
}

eval_g_ineq <- make_eval_g_ineq(pr,psi)

eval_f_grad <- function(theta){
  nloptr::nl.grad(theta,eval_f)
}

eval_g_ineq_jac <- function(theta){
  nloptr::nl.jacobian(theta,eval_g_ineq)
}

theta_init <- c(
  # FOI on h1,h2,h3
  5e-1,1e-3,5e-1,
  # FOI in h4 (no constraint)
  1e-3,
  # FOI in h5, ... ,h10 (strict constraint)
  rep(1e-1,2),5e-4,rep(1e-1,3),
  # deltas
  1e-1,1e-1)

if(any(eval_g_ineq(theta_init) >= 0)){
  cat("warning! initial values for theta not in feasible region\n")
}


###############################################################################
# COBYLA: derivative-free local optimization
###############################################################################

nlopt_opts <- list(
  "algorithm" = "NLOPT_LN_COBYLA",
  "xtol_rel"=1.0e-10,
  "maxeval"=1e5L,
  "print_level" = 1,
  "ranseed" = 66412L
)

# call out to nlopt
opt_cobyla <- nloptr::nloptr(x0 = theta_init,
               eval_f = eval_f,
               lb = lb,
               ub = ub,
               eval_g_ineq = eval_g_ineq,
               opts = nlopt_opts)

with(opt_cobyla,{
  h_hat <<- solution[1:10]
  psi_hat <<- psi
  psi_hat[2,1] <<- solution
  psi_hat[2,2] <<- psi_hat[2,2] - solution[11] - solution[12]
  psi_hat[2,3] <<- solution[12]
})

# results
psi_hat %*% h_hat

# results vs data
cbind(psi_hat %*% h_hat,pr)


###############################################################################
# Augmented Lagrangian Algorithm with numerical gradients and jacobians
###############################################################################

opt_ala <- nloptr::auglag(x0 = theta_init,
                          fn = eval_f,
                          gr = eval_f_grad,
                          lower = lb,upper = ub,
                          hin = eval_g_ineq,
                          hinjac = eval_g_ineq_jac,
                          localsolver = "LBFGS",
                          localtol = 1e-7,
                          control = list(
                            "xtol_rel"=1.0e-10,
                            "maxeval"=1e5L
                          ),
                          nl.info = TRUE)

with(opt_ala,{
  h_hat <<- par[1:10]
  psi_hat <<- psi
  psi_hat[2,1] <<- par[11]
  psi_hat[2,2] <<- psi_hat[2,2] - par[11] - par[12]
  psi_hat[2,3] <<- par[12]
})

# results vs data
cbind(psi_hat %*% h_hat,pr)


###############################################################################
# SQP: Sequential Quadratic Programming
###############################################################################

make_eval_g_ineq_g0 <- function(pr,psi){
  
  pr <- pr
  psi <- psi
  
  f <- function(theta){
    rbind(
      # FOI constraint 1
      (theta[1]/theta[2]) - 100,
      # FOI constraint 2
      (theta[3]/theta[2]) - 100,
      # FOI constraint 3
      (sum(theta[c(5,6,8,9,10)])/theta[7]) - 500,
      # psi constraint 1
      psi[2,2] - theta[11] - theta[12],
      # psi constraint 2
      theta[11],
      # psi constraint 3
      theta[12]
    )
  }
  
  return(f)
}

eval_g_ineq_g0 <- make_eval_g_ineq_g0(pr,psi)

eval_g_ineq_jac_g0 <- function(theta){
  nloptr::nl.jacobian(theta,eval_g_ineq_g0)
}

opt_slsqp <- nloptr::slsqp(x0 = theta_init,
                    fn = eval_f,
                    gr = eval_f_grad,
                    lower = lb,
                    upper = ub,
                    hin = eval_g_ineq_g0,
                    hinjac = eval_g_ineq_jac_g0,
                    nl.info = TRUE,
                    control = list(
                      "maxeval" = 1e4
                    ))

with(opt_slsqp,{
  h_hat <<- par[1:10]
  psi_hat <<- psi
  psi_hat[2,1] <<- par[11]
  psi_hat[2,2] <<- psi_hat[2,2] - par[11] - par[12]
  psi_hat[2,3] <<- par[12]
})

# results vs data
cbind(psi_hat %*% h_hat,pr)




