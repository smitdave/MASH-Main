###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     Simulations of Peri-domestic breeding resource-scapes
#     MBITES Team
#     May 2018
#
###############################################################################

rm(list=ls());gc()
library(truncdist) # for zero-inflation of distributions (we dont do movement from i->i)
library(viridis)

###############################################################################
# load point sets
###############################################################################


dir_dev <- "/Users/slwu89/Desktop/git/MASH-Main/MASH-dev/"

xy_f_files <- list.files(path = paste0(dir_dev,"DavidSmith/MBITES-Demo/"),pattern = "[[:alpha:]]+\\.f[0-9]{3}\\.xyw$")
xy_l_files <- list.files(path = paste0(dir_dev,"DavidSmith/MBITES-Demo/"),pattern = "[[:alpha:]]+\\.l[0-9]{3}\\.xyw$")

n <- length(xy_f_files)

xy_points <- vector(mode = "list",length = n)

for(i in 1:n){
  xy_points[[i]]$f <- as.matrix(read.csv(paste0(dir_dev,"DavidSmith/MBITES-Demo/",xy_f_files[i]),header = TRUE,sep = ","))
  xy_points[[i]]$l <- as.matrix(read.csv(paste0(dir_dev,"DavidSmith/MBITES-Demo/",xy_l_files[i]),header = TRUE,sep = ","))
}

n_pts <- nrow(xy_points[[1]]$f)


###############################################################################
# sites by xy-coords
###############################################################################

# approximate equality for floats
fnear <- function (x, y, eps = .Machine$double.eps^0.5) {
  abs(x - y) < eps
}

xynear <- function(x,y){
  fnear(x[1],y[1]) & fnear(x[2],y[2])
}

# list of sites by xy coordinate and their type
xy_sites <- vector(mode = "list",length = n)

# type (0 = only feeding, 1 = only laying, 2 = both)
for(i in 1:n){
  
  # check which points are same
  same <- fnear(xy_points[[i]]$f[,c("x")],xy_points[[i]]$l[,c("x")]) & fnear(xy_points[[i]]$f[,c("y")],xy_points[[i]]$l[,c("y")])
  same_ind <- which(unname(same))
  
  if(length(same_ind)==0){
    
    xy_sites[[i]]$sites <- rbind(cbind(xy_points[[i]]$f,type=rep(0,n_pts)),cbind(xy_points[[i]]$l,type=rep(1,n_pts)))
    
  } else if(length(same_ind)==n_pts){
    
    xy_sites[[i]]$sites <- cbind(xy_points[[i]]$f,type=rep(2,n_pts))
    
  } else {
   
    xy_sites[[i]]$sites <- cbind(xy_points[[i]]$f[same_ind,],type=rep(2,length(same_ind)))
    xy_sites[[i]]$sites <- rbind(xy_sites[[i]]$sites,
                                 cbind(xy_points[[i]]$f[-same_ind,],type=rep(0,n_pts-length(same_ind))),
                                 cbind(xy_points[[i]]$l[-same_ind,],type=rep(1,n_pts-length(same_ind))))
    
  }
  
  rownames(xy_sites[[i]]$sites) <- 1:nrow(xy_sites[[i]]$sites)
}

# movement kernel
exp_fit <- function(d,q,up=1){
  f_opt = function(x){
    abs(d - qexp(p = q,rate = x))
  }
  sol = optimise(f = f_opt,lower = 0,upper = up,maximum = FALSE)
  return(sol$minimum)
}

# generate movement kernel between sites
pb <- txtProgressBar(min = 0,max = n)
for(i in 1:n){
  
  dist <- as.matrix(dist(xy_sites[[i]]$sites[,c("x","y")],diag = TRUE,upper = TRUE))
  half_d <- max(dist)/2
  exp_kern <- exp_fit(d = half_d,q = 0.95)
  xy_sites[[i]]$distance <- dist
  xy_sites[[i]]$movement <- apply(X = dist,MARGIN = 1,FUN = function(x){dtrunc(x = x,spec = "exp",a = 1e-12,b = Inf, rate = 1/exp_kern)})
  xy_sites[[i]]$movement <- xy_sites[[i]]$movement/rowSums(xy_sites[[i]]$movement)
  setTxtProgressBar(pb,i)
}


# color utility
cols <- function(n, alpha = 1) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100, alpha = alpha)[1:n]
}

# plot landscape
mar <- par()$mar

pch_map <- c(15,16,17)
col_map <- cols(3)

i=5

par(bg = grey(0.15))
par(mar=mar-2)
plot.new()
plot.window(xlim = c(
  floor(min(xy_sites[[i]]$sites[,"x"])),
  ceiling(max(xy_sites[[i]]$sites[,"x"]))
),ylim = c(
  floor(min(xy_sites[[i]]$sites[,"y"])),
  ceiling(max(xy_sites[[i]]$sites[,"y"]))
))

# plot(xy_sites[[i]]$sites[,c("x","y")],pch=21,cex=1.5,bg=grey(level = 0.95,alpha = 0.85),col="white",axes=FALSE,ylab="",xlab="")

movement_quantile = cut(as.vector(xy_sites[[i]]$movement),breaks=quantile(as.vector(xy_sites[[i]]$movement),probs=seq(0, 1, 0.2)),include.lowest = TRUE,labels = FALSE)
movement_color <- matrix(data = plasma(n = length(unique(movement_quantile)),alpha=0.05)[movement_quantile],nrow = nrow(xy_sites[[i]]$movement),ncol = ncol(xy_sites[[i]]$movement))
for(k in 1:ncol(xy_sites[[i]]$movement)){
  for(j in 1:nrow(xy_sites[[i]]$movement)){
    segments(x0 = xy_sites[[i]]$sites[k,"x"],y0 =xy_sites[[i]]$sites[k,"y"],
             x1 = xy_sites[[i]]$sites[j,"x"],y1 =xy_sites[[i]]$sites[j,"y"],
             col = movement_color[k,j],lty = 1.15,lwd = 1.15)
  }
}

points(xy_sites[[i]]$sites[,c("x","y")],pch=21,cex=1.5,bg=grey(level = 0.95,alpha = 0.85),col="white")
pt_pch <- unlist(Map(function(x){pch_map[x+1]},xy_sites[[i]]$sites[,"type"]))
pt_col <- unlist(Map(function(x){col_map[x+1]},xy_sites[[i]]$sites[,"type"]))
points(x = xy_sites[[i]]$sites[,c("x","y")],pch=pt_pch,
       cex=0.75,col=pt_col)

par(bg = "white")
par(mar=mar)


###############################################################################
# Make landscape initialization object
###############################################################################

landscapes <- vector(mode = "list",length = n)
pb <- txtProgressBar(min = 0,max = n)

n_aqua <- unique(sapply(xy_sites,function(x){
  sum(x$sites[,"type"]!=0)
}))

lambda <- 1

for(i in 1:n){
  
  n_site <- nrow(xy_sites[[i]]$sites)
  landscapes[[i]]$sites <- vector(mode="list",length=n_site)
  
  for(j in 1:n_site){
    landscapes[[i]]$sites[[j]]$id  <- j
    landscapes[[i]]$sites[[j]]$xy  <- xy_sites[[i]]$sites[j,c("x","y")]
    landscapes[[i]]$sites[[j]]$type <- 1L
    landscapes[[i]]$sites[[j]]$tileID <- 1L
    landscapes[[i]]$sites[[j]]$move <- xy_sites[[i]]$movement[j,-j]
    landscapes[[i]]$sites[[j]]$move_id <- as.integer(names(xy_sites[[i]]$movement[j,-j]))
    landscapes[[i]]$sites[[j]]$haz <- 0.001
    if(fnear(xy_sites[[i]]$sites[j,"type"],0)){
      landscapes[[i]]$sites[[j]]$feed[[1]] <- list(w=1,enterP=0.95)
      landscapes[[i]]$sites[[j]]$aqua <- NULL
    } else if(fnear(xy_sites[[i]]$sites[j,"type"],1)){
      landscapes[[i]]$sites[[j]]$feed <- NULL
      landscapes[[i]]$sites[[j]]$aqua[[1]] <- list(w=1,lambda=lambda)
    } else if(fnear(xy_sites[[i]]$sites[j,"type"],2)){
      landscapes[[i]]$sites[[j]]$feed[[1]] <- list(w=1,enterP=0.95)
      landscapes[[i]]$sites[[j]]$aqua[[1]] <- list(w=1,lambda=lambda)
    } else {
      cat("warning! unrecognized type detected at i: ",i," j: ",j,"\n")
    }
  }

  setTxtProgressBar(pb,i)  
}

saveRDS(object = landscapes,file = paste0(dir_dev,"DavidSmith/MBITES-Demo/periDomesticLandscapes.rds"))
saveRDS(object = xy_sites,file = paste0(dir_dev,"DavidSmith/MBITES-Demo/periDomesticRaw.rds"))


###############################################################################
# Make humans initialization object
###############################################################################

humans <- vector(mode = "list",length = n)
pb <- txtProgressBar(min = 0,max = n)

for(i in 1:n){
  
  humans[[i]]$siteID = which(sapply(landscapes[[i]]$sites,function(x){!is.null(x$feed)}))
  humans[[i]]$tileID = rep(1,length(humans[[i]]$siteID))
  humans[[i]]$feedingID = rep(1,length(humans[[i]]$siteID))
  humans[[i]]$w = rep(1,length(humans[[i]]$siteID))
    
  
  setTxtProgressBar(pb,i)
}

saveRDS(object = humans,file = paste0(dir_dev,"DavidSmith/MBITES-Demo/periDomesticHumans.rds"))


###############################################################################
# Make mosquito initialization object
###############################################################################

mosquitoes <- vector(mode = "list",length = n)
pb <- txtProgressBar(min = 0,max = n)

for(i in 1:n){
  
  mosquitoes[[i]]$siteID = which(sapply(landscapes[[i]]$sites,function(x){!is.null(x$aqua)}))
  mosquitoes[[i]]$tileID = rep(1,length(mosquitoes[[i]]$siteID))
  mosquitoes[[i]]$female = rep(TRUE,length(mosquitoes[[i]]$siteID))
  
  setTxtProgressBar(pb,i)
}

saveRDS(object = mosquitoes,file = paste0(dir_dev,"DavidSmith/MBITES-Demo/periDomesticMosquitoes.rds"))
