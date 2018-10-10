# test it out on the TaR problem
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

G <- matrix(c(
  
))

constr <- function(theta,psi){
  # if any are FALSE, the constraints are violated.
  pass <- rep(T,6)
  # evaluate linear constraints
  pass[1] <- (psi[2,2] - theta[11] - theta[12] >= 0)
  pass[2] <- (theta[11] >= 0)
  pass[3] <- (theta[12] >= 0)
  # evaluate non-linear constraints
  pass[4] <- (theta[1]/theta[2] >= 100)
  pass[5] <- (theta[3]/theta[2] >= 100)
  pass[6] <- ((sum(theta[c(5,6,8,9,10)])/theta[7]) >= 500)
  # if any FALSE, this doesn't pass
  if(any(!pass)){
    return(FALSE)
  } else {
    return(TRUE)
  }
}

# make the theta object
theta_obj <- make_theta(d = 12)
# the h bits
for(i in 1:10){
  # 'basically' uniform on the part of the reals we care about
  theta_obj[[i]]$invCDF <- "qunif"
  theta_obj[[i]]$ARGS$min <- .Machine$double.eps
  theta_obj[[i]]$ARGS$max <- 2e16
}
for(i in c(1,3,7)){
  # 'basically' uniform on the part of the reals we care about
  theta_obj[[i]]$ARGS$min <- 100
}
# the delta bits
for(i in 11:12){
  # should be uniform on [0,1]
  theta_obj[[i]]$invCDF <- "qunif"
  theta_obj[[i]]$ARGS$min <- 0
  theta_obj[[i]]$ARGS$max <- 0.2
}

# ask for some points satisfying constraints
theta_grid <- constrained_lhs(n = 1000,theta = theta_obj,constraints = constr,max = 20,psi = psi)

# create_set <- function(input_values, input_names, sample_count, constraints){
#   
#   input.sets <- create_sample(input_values, input_names, sample_count)
#   
#   if(constraints == "none") {
#     constrained <- rep(TRUE, nrow(input.sets))
#   } else {
#     constrained <- with(input.sets, eval(parse(text=constraints)))
#   }
#   input.sets <- keep_satisfied(input.sets, constrained)
#   
#   while(nrow(input.sets) < sample_count) { 
#     # Create input factor sets by latin hypercube sampling:
#     input.sets <- rbind(input.sets,
#                         create_sample(input_values, input_names, sample_count))  
#     # Discard input factor sets that violate constraints:
#     constrained <- with(input.sets, eval(parse(text=constraints)))
#     input.sets <- keep_satisfied(input.sets, constrained)
#   }
#   
#   input.sets
# }
# 
# 
# create_sample <- function(input_values, input_names, sample_count) {
#   # will create values from 0 to 1 and must be transformed afterwards, if need be.
#   
#   # create a random sample of input factor sets with Latin Hypercube Sampling
#   lhs_design <- lhs::improvedLHS(sample_count, length(input_values))
#   
#   # transform the standardized random values to the real input value range (if need be)
#   # and apply the desired random distribution
#   lhs_design <- lapply(seq(1,length(input_values)), function(i) {
#     input_values[[i]]$ARGS$p <- as.vector(lhs_design[ ,i])
#     do.call(input_values[[i]]$random_function, input_values[[i]]$ARGS) # input_values[[i]]$min, input_values[[i]]$max
#   })
#   
#   names(lhs_design) <- input_names
#   data.frame(lhs_design)
# }
# 
# 
# 
# keep_satisfied <- function(sampled, constraints){
#   result <- data.frame(sampled[constraints, , drop=FALSE])
#   stopifnot(nrow(result) <= nrow(sampled))
#   result
# }