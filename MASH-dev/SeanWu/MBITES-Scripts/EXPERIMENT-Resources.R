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

lambda <- 5

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
      landscapes[[i]]$sites[[j]]$aqua[[1]] <- list(w=1,lambda=lambda/n_aqua)
    } else if(fnear(xy_sites[[i]]$sites[j,"type"],2)){
      landscapes[[i]]$sites[[j]]$feed[[1]] <- list(w=1,enterP=0.95)
      landscapes[[i]]$sites[[j]]$aqua[[1]] <- list(w=1,lambda=lambda/n_aqua)
    } else {
      cat("warning! unrecognized type detected at i: ",i," j: ",j,"\n")
    }
  }

  setTxtProgressBar(pb,i)  
}

saveRDS(object = landscapes,file = paste0(dir_dev,"DavidSmith/MBITES-Demo/periDomesticLandscapes.rds"))
saveRDS(object = xy_sites,file = paste0(dir_dev,"DavidSmith/MBITES-Demo/periDomesticRaw.rds"))


# try to make kernels
library(KernSmooth)
library(parallel)

numDiff <- function(x, y){
  sapply(3 : (length(x) - 2), function(ii)
    (-y[ii + 2] + 8 * y[ii + 1] - 8 * y[ii - 1] + y[ii - 2]) / (12 * (x[2] - x[1])))
}

normalize = function(x){
  return(x / sum(x))
} # end function normalize definition



# get a movement and distance matrix 

# dist_i <- as.matrix(dist(xy_sites[[i]]$sites[,c("x","y")],diag = TRUE,upper = TRUE))
# mvmt_i <- xy_sites[[i]]$movement
dist_i <- as.matrix(dist(cbind(runif(n = 20,min = 0,max = 10),runif(n = 20,min = 0,max = 10)),diag=T,upper=T))
half_d <- max(dist_i)/2
exp_kern <- exp_fit(d = half_d,q = 0.95)
mvmt_i <- apply(X = dist_i,MARGIN = 1,FUN = function(x){dtrunc(x = x,spec = "exp",a = 1e-12,b = Inf, rate = 1/exp_kern)})
mvmt_i <- mvmt_i/rowSums(mvmt_i)


dist_v <- as.vector(dist_i)
self <- which(dplyr::near(dist_v,0))

mvmt_v <- as.vector(mvmt_i)[-self]
dist_v <- dist_v[-self]
dist_bins <- unique(dist_v[order(dist_v)])

mvmt_v <- mvmt_v[order(dist_v)]
dist_v <- dist_v[order(dist_v)]

# empirical "pdf"; actually a pmf
# mvmt_pdf_emp <- normalize(sapply(dist_bins, function(dd){
#   sum(mvmt_v[which(dplyr::near(dist_v,dd))])
# }))

pmf_v <- function(dd,mvmt_v,dist_v){
  sum(mvmt_v[which(dplyr::near(dist_v,dd))])
}
pmf_v <- Vectorize(pmf_v,"dd")

mvmt_pdf_emp <- normalize(pvec(dist_bins,pmf_v,mvmt_v=mvmt_v,dist_v=dist_v))

cdf_v <- function(dd,mvmt_v,dist_v){
  sum(mvmt_v[which(dist_v <= dd)])
}
cdf_v <- Vectorize(cdf_v,"dd")

# mvmt_cdf_emp <- sapply(dist_bins,function(dd){
#   sum(mvmt_v[which(dist_v <= dd)])
# })
mvmt_cdf_emp <- pvec(dist_bins,cdf_v,mvmt_v=mvmt_v,dist_v=dist_v)

mvmt_cdf_emp <- mvmt_cdf_emp / max(mvmt_cdf_emp)

# smooth the empirical "cdf" (actually a cmf) into a cdf
# mvmt_cdf_sth <- ksmooth(dist_bins[2:length(dist_bins)],mvmt_cdf_emp[2:length(dist_bins)],kernel = "normal",bandwidth = dpill(dist_bins,mvmt_cdf_emp))

mvmt_cdf_sth1 <- loess(mvmt_cdf_emp~dist_bins)

# differentiate the smoothed cdf to obtain a smoothed pdf
mvmt_pdf_sth = list(x = mvmt_cdf_sth$x[3 : (length(mvmt_cdf_sth$x) - 2)],
                 y = normalize(numDiff(mvmt_cdf_sth$x, mvmt_cdf_sth$y)))

# redefine the smoothed cdf to be exactly consistent with the smoothed pdf
mvmt_cdf_sth$x = mvmt_pdf_sth$x
mvmt_cdf_sth$y = cumsum(mvmt_pdf_sth$y)

# # vectorize the matrix
# Q_vec = as.vector(Q)
# 
# # make bins according to unique distances
# bins = unique(as.vector(dist.F))
# 
# # empirical pdf and cdf
# Q_pdf.emp = normalize(sapply(bins, function(dd) sum(Q_vec[which(distVec.F == dd)])))
# Q_cdf.emp = sapply(bins, function(dd) sum(Q_vec[which(distVec.F <= dd)]))
# Q_cdf.emp = Q_cdf.emp / max(Q_cdf.emp)
# 
# # smoothed cdf
# Q_cdf.sth = ksmooth(bins[2 : length(bins)], Q_cdf.emp[2 : length(bins)], kernel = 'normal', bandwidth = 5 * dpill(bins, Q_cdf.emp))
# 
# # differentiate the smoothed cdf to obtain a smoothed pdf
# Q_pdf.sth = list(x = Q_cdf.sth$x[3 : (length(Q_cdf.sth$x) - 2)],
#                  y = normalize(numDiff(Q_cdf.sth$x, Q_cdf.sth$y)))

# # redefine the smoothed cdf to be exactly consistent with the smoothed pdf
# Q_cdf.sth$x = Q_pdf.sth$x
# Q_cdf.sth$y = cumsum(Q_pdf.sth$y)

plot(numDiff(mvmt_cdf_sth$x, mvmt_cdf_sth$y),type="l",main="centered finite difference approximation")

#find the average x between 2 points
avex<-mvmt_cdf_sth$x[-1]-diff(mvmt_cdf_sth$x)/2
#find the numerical approximation
#delta-y/delta-x
dydx<-diff(mvmt_cdf_sth$y)/diff(mvmt_cdf_sth$x)

#plot numeric approxiamtion
plot(x=avex, dydx,type="l")
