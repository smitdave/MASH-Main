####################################################################################################
# Vector control routines
# HMSC
####################################################################################################
UniformProbabilityIntervention=function(InterventionParameters){
  #. UniformProbabilityIntervention: Determines binomially if an intervention is present or absent in a site according to its coverage
  # UniformProbabilityIntervention(INTERVENTIONS_PARAMETERS$BIOLOGICAL.CONTROL)
  if(InterventionParameters$ACTIVE){
    binomialEvent(InterventionParameters$CoverageLevel)
  }else{
    FALSE
  }
}

# rnormInterventionEffect = function(mean,sd){
#   # Generates a random, normally distributed sample
#   clipIntervention(rnorm(1,mean,sd))
# }
# clipIntervention = function(value){if(value>1){1}else{if(value<0){0}else{value}}}
# binomialEvent <- function(probability){runif(1) < probability}
# cluster_coverage <- function(xy,cov){
#   # Assignment of intervention sites by distance based clustering
#   #example: xy = LANDSCAPE$f[,c("x","y")]
#   distMat <- dist(xy,method="euclidean")
#   clust <- hclust(distMat,method="average")
#   clustCut <- cutree(clust,h=max(clust$height)/1.90)
#   n_sites <- dim(xy)[[1]]
#   n_cov <- floor(n_sites*cov)
#   n_prob <- as.vector(table(clustCut)) / n_sites
#   n_samp <- floor(n_cov * n_prob)
#   cov_out <- rep(0,n_sites)
#   for(i in 1:length(n_samp)){
#     sites_i <- which(clustCut == i)
#     index_i <- sample(x=sites_i,size=n_samp[i],replace=FALSE)
#     cov_out[index_i] <- 1
#   }
#   return(cov_out)
# }
# createClusterCoverageVector = function(LANDSCAPE_HABITAT,coverage){
#   # Cluster coverage vector wrapper for simplicity
#   # Example: createClusterCoverageVector(LANDSCAPE$s,.25)
#   cluster_coverage(LANDSCAPE_HABITAT[,c("x","y")],coverage)
# }
# createRandomUniformCoverageVector = function(coverage,length){
#   # Creates a binary vector with a given probability so that it can be appended to the LANDSCAPE
#   rbinom(length,1,coverage)
# }
