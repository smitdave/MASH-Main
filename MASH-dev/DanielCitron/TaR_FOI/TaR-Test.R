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
# TaR model
###############################################################################

# recovery rate
r = 1/200

# original solution
g <- r*X_est/(1-X_est)
h <- ginv(psi) %*% g
h


library(nloptr)

# 
pr <- (r*X_est) / (1 - X_est)

eval_f <- function(theta,pr,psi){
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

lb <- rep(0,12)
ub <- rep(Inf,12)

# inequality constraints
eval_g_ineq <- function(theta,pr,psi){
  rbind(
    # FOI constraint 1
    -((theta[2]/theta[2]) - 100),
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

theta_init <- c(rep(1e-2,10),1e-2,1e-2)

nlopt_opts <- list(
  # global methods
  # "algorithm" = "NLOPT_GN_ORIG_DIRECT_L",
  # # local gradient-free methods
  "algorithm" = "NLOPT_LN_COBYLA",
  "xtol_rel"=1.0e-10,
  "maxeval"=1e5L,
  "print_level" = 2
)

# call out to nlopt
opt <- nloptr::nloptr(x0 = theta_init,
               eval_f = eval_f,
               lb = lb,
               ub = ub,
               eval_g_ineq = eval_g_ineq,
               opts = nlopt_opts,
               pr = pr, psi = psi)

opt$solution

h_hat <- opt$solution[1:10]
psi_hat <- psi
psi_hat[2,1] <- opt$solution[11]
psi_hat[2,2] <- psi_hat[2,2] - opt$solution[11] - opt$solution[12]
psi_hat[2,3] <- opt$solution[12]

# results
psi_hat %*% h_hat

# results vs data
cbind(psi_hat %*% h_hat,pr)
