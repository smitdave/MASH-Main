################################################################################
#       __  ______   __________  ____
#      /  |/  /   | / ____/ __ \/ __ \
#     / /|_/ / /| |/ /   / /_/ / / / /
#    / /  / / ___ / /___/ _, _/ /_/ /
#   /_/  /_/_/  |_\____/_/ |_|\____/
#
#   Testing PfSI in MACRO package
#
#   Sean Wu
#   December 2018
#
################################################################################

library(MACRO)

# test a very simple 2-patch system
# unlike a normal stochastic matrix, p_{ii} = 0, even though sum_{j} p_{ij} = 1 (this is conditional on going *somewhere*)
n <- 2
move <- matrix(c(0,1,1,0),byrow = T,n,n)

patch_pars <- patches_parameters(move = move,bWeightZoo = rep(0,n),bWeightZootox = rep(0,n),reservoir = rep(F,n),res_EIR = rep(0,n))
pfsi_pars <- pfsi_parameters()
mosy_pars <- mosquito_rm_conpars(N = n,lambda = matrix(1,nrow = 365,ncol = n),
                                 psi = diag(n),rep(11,365),M = rep(500,n),Y = rep(10,n),Z = rep(2,n))
