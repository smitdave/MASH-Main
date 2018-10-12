



#

process_dispersion <- function(job){

  run <- as.character(job$id)

  out <- list()

  # read in mosquito data
  mosquitos_df <- jsonlite::fromJSON(job$mosy_json, flatten = TRUE)
  null_m <- which(sapply(mosquitos_df$id,is.null))
  if(length(null_m)>0){
    mosquitos_df <- mosquitos_df[-null_m,]
  }

  # calculate dispersion metrics
  out$cum_disperse <- Bionomics_cumulativeDisperse(mosquitos_df)
  out$abs_disperse <- Bionomics_absoluteDisperse(mosquitos_df)

  # calculate cumulative dispersion distances
  out$cum_disperse_v <- lapply(X = out$cum_disperse,FUN = function(x){
    d <- 0
    for(i in 1:(length(x)-1)){
      d <- d + job$dmat[x[i],x[i+1]]
    }
    return(unname(d))
  })
  out$cum_disperse_v <- unlist(out$cum_disperse_v)

  # calculate absolute dispersion distances
  out$abs_disperse_v <- lapply(X = out$abs_disperse,FUN = function(x){
    unname(job$dmat[x[1],x[2]])
  })
  out$abs_disperse_v <- unlist(out$abs_disperse_v)

}

# any(sapply(out$cum_disperse[which(sapply(out$cum_disperse,length)==2)],function(x){x[1]==x[2]}))


plot(density(out$cum_disperse_v),main=paste0("mean: ",round(mean(out$cum_disperse_v),3)))
plot(density(out$abs_disperse_v),main=paste0("mean: ",round(mean(out$abs_disperse_v),3)))


eip2_dir <- "/Users/slwu89/Downloads/peridom_qsub_analysis-2"

eip2_1 <- readRDS(file = paste0(eip2_dir,"/run1VC2.rds"))
eip2_13 <- readRDS(file = paste0(eip2_dir,"/run13VC2.rds"))
eip2_26 <- readRDS(file = paste0(eip2_dir,"/run26VC2.rds"))

l1_svc<- unlist(lapply(eip2_1$vc2,FUN = function(x){
    if(length(x$spatialVC)>0){

      unlist(lapply(x$spatialVC,function(y){
        dist <- numeric(length(y$dest))
        for(i in 1:length(y$dest)){
          dist[i] <- eip2_1$dmat[y$origin,y$dest[i]]
        }
        return(dist)
      }))

    }
  }
))

hist(l1_svc)

l26_svc<- unlist(lapply(eip2_26$vc2,FUN = function(x){
  if(length(x$spatialVC)>0){

    unlist(lapply(x$spatialVC,function(y){
      dist <- numeric(length(y$dest))
      for(i in 1:length(y$dest)){
        dist[i] <- eip2_1$dmat[y$origin,y$dest[i]]
      }
      return(dist)
    }))

  }
}
))

hist(l26_svc)
