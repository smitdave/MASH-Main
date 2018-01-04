######################################################################
#
#    Algorithms to set up the patch structure
#
######################################################################

dist2P.i = function(i,O,P){
  sqrt((O$X[i] - P$X)^2 + (O$Y[i]-P$Y)^2)
}

d2P = function(i,O,P){
  which.min(dist2P.i(i,O,P)) 
}

# Assign each point in {f} or {l} to its patch
O2P = function(O, P){
  d = length(O$X)
  as.vector(unlist(sapply(1:d,d2P,O=O,P=P)))
}

# Make a list of the nearest patches, in order
ord.d2P = function(i,P){
  dd = sqrt((P$X[i] - P$X)^2 + (P$Y[i]-P$Y)^2)
  #NOTE: column 1 is 1:d 
  order(dd, decreasing = FALSE) 
}

ordP = function(P){
  d = length(P$X)
  t(matrix(sapply(1:d,ord.d2P,P=P), d, d))
}

##Deprecate? 
#jiggerPatches = function(P, eps){
#  d = length(P$X)
#  P$X = P$X + rnorm(d,eps,2*eps)
#  P$Y = P$Y + rnorm(d,eps,2*eps)
#  P
#}

remakeBorder = function(border){
  d = length(border[,1])
  P1 = border[1,border[1,]>0]
  P2 = border[2,border[2,]>0]
  B = list(P1,P2)
  for(i in 3:d){
    B[[i]] = border[i,border[i,]>0]
  }
  B
}


###########################################################
# Find all the neighbors within the radius "hood." 
#
# NOTE: Use for grids. deldir is not reliable at neighbor
#       finding for grids. Do not use for randomly generated
#       point sets. 
#
#   if 1 < hood < sqrt(2), then there are at most 4 neighbors
#   if sqrt(2) < hood < 2, then there are at most 8 neighbors
###########################################################
nbrP.hood = function(P, hood){
  
  d = length(P$X)
  nbr = matrix(0, d, d)
  
  for(i in 1:d){
    dst = dist2P.i(i,P,P)
    ix = which(dst>0 & dst<hood)
    nbr[i,ix] = 1
  }
  
  P$N = rowSums(nbr)
  border = matrix(-1,d, max(P$N))
  for(i in 1:d){
    border[i,1:P$N[i]] = which(nbr[i,]==TRUE) 
  }
  P$nbr = nbr
  #P$border = border 
  P$border = remakeBorder(border) 
  P
}

###########################################################
# Find all neighbors that share a border 
# NOTE: For grids, use nbrP.hood 
###########################################################
nbrP.adjacent = function(P, hood){
  d = length(P$X)
  nbr = matrix(0, d, d)
  jnt = deldir(P$X, P$Y)$delsgs
  i = jnt$ind1; j = jnt$ind2
  k = length(i)
  for(kk in 1:k){
    nbr[i[kk],j[kk]]=nbr[j[kk],i[kk]]=1
  }
  P$N = rowSums(nbr)
  border = matrix(-1,d, max(P$N))
  for(i in 1:d){
    border[i,1:P$N[i]] = which(nbr[i,]==TRUE) 
  }
  P$nbr = nbr
  #P$border = border 
  P$border = remakeBorder(border) 
  P
}

######################################################################
#    Patchmaking
######################################################################

######################################################################
# Find the weighted center of all the points in a patch.  
######################################################################
centerXY = function(F, L, P, trim){
  d = length(P$X)
  PP = list(X= 0*c(1:d), Y = 0*c(1:d))
  
  F$P = O2P(F,P)
  L$P = O2P(L,P)

  empty = c(-1)
  for(i in 1:d){
    ixL = which(L$P == i)
    ixF = which(F$P == i)
    if(length(c(ixL,ixF))>trim){
      PP$X[i] = weighted.mean(c(L$X[ixL], F$X[ixF]), c(L$w[ixL],F$w[ixF]))
      PP$Y[i] = weighted.mean(c(L$Y[ixL], F$Y[ixF]), c(L$w[ixL],F$w[ixF]))
    } else {
      empty = c(empty, i)
      PP$X[i] = 0
      PP$Y[i] = 0
    }
  }
  empty = empty[-1]
  if(length(empty > 0)){
    PP$X = PP$X[-empty]
    PP$Y = PP$Y[-empty]
  } 
  PP
}

adjustPatches = function(F,L,P, N.adj, trim){
  ######################################################################
  # Move the centers of points to the weighted centers of the
  # points currently in the patch. This is a way of creating a set
  # of patches that looks better. 
  ######################################################################
  if(N.adj>0){
    for(i in 1:N.adj){
      F$P = O2P(F,P)
      L$P = O2P(L,P)
      P = centerXY(F,L,P, trim)
    }
  }
  
  out = deldir(P$X, P$Y)
  P$X = out$summary$x
  P$Y = out$summary$y
  
  P 
}

Prand = function(F, L, Lobj, Pobj){with(Pobj,{
  ######################################################################
  # Establish a set of patches seeded by a random set of points,
  # then adjusted. 
  ######################################################################
  P = with(Lobj, unifXY(N.p, c, mn.x, mx.x, mn.y, mx.y)) 
  
  P = adjustPatches(F, L, P, N.adj, trim)

  #P$P = ordP(P)
  #P = nbrP.adjacent(P, hood)
  P
})}

Pgrid = function(F, L, Lobj, Pobj){with(c(Lobj, Pobj),{
  ######################################################################
  # Establish a set of patches that is a random grid.  
  # These can then be adjusted. 
  ######################################################################
  P = matrixXY(N.p, c, mn.x, mx.x, mn.y, mx.y)
  
  P = adjustPatches(F,L,P, N.adj, trim)
  
  #P = nbrP.hood(P, hood)
  P
})}

######################################################################
#  Movement Object :: Patch  
#  source ("Patches.R") 
#  be sure to run F$P = O2P(F,P); L$P = O2P(F,P) 
#  - Generate a patch structure
#  - near :: in the patch 
#  - around :: in patches including any one of the first N points
#  - moveFar :: to a patch and then in the patch  
######################################################################

dS2D.iP = function(i,S,D,Kobj){
  # Call pr.sort for movement from the ith point in S to D
  pr = dS2D.i(i, S, D, Kobj)
  ix = which(D$P == S$P[i])
  pr.sort(ix,pr)
}

dS2D.sP = function(i,set,S,D,Kobj){
  # call pr.sort for movement from the ith point in S 
  # any of the points in D falling in the set of patches
  # indexed by "set"
  pr = dS2D.i(i, S, D, Kobj)
  ix = which(D$P %in% set)
  pr.sort(ix,pr)
}

patchesMvOb.i = function(i, S, D, P, Kobj, N=1000){
  PR = dS2D.i(i, S, D, Kobj)
  herePR = PR[i]
 
  ot = order(-PR)
  
  # in my patch
  myP = S$P[i] 
  ixP = which(D$P %in% myP)
  ixHere = which(ixP == i) 
  near = pr.sort(ixP[-ixHere],PR[-ixHere])
  nearPR = sum(near$pr)

  myT = unique(D$P[ot][1:N])
  if(sum(near$pr) >0) myT = myT[-which(myT==myP)]
  ixT = which(D$P %in% myT)
  around = pr.sort(ixT,PR)
  aroundPR = sum(around$pr)
  
  list(PR = c(herePR,nearP, aroundP), near=near, around=around, myP = myP, nearP = c(myP, myT))   
}

patchesMvOb = function(S,D,P,Kobj,N=1000){
  P1 = patchesMvOb.i(1,S,D,P,Kobj,N)
  P2 = patchesMvOb.i(2,S,D,P,Kobj,N)
  MvOb = list(P1, P2) 
  for(i in 3:length(S$X)){
    MvOb[[i]] = patchesMvOb.i(i,S,D,P,Kobj,N)
  }
  MvOb
}

#############################################
#  Patch to Patch connectivity matrix 
#  from the weighted center of S  
#  DEPRECATE? 
#############################################
centP2Pi = function(D, P, Kobj){with(Kobj,{
  getPR.here.i = function(i,D,P,Kobj){
    ixd = which(D$P == i)
    if (length(ixd)>0){
      cX = weighted.mean(D$X[ixd], D$w[ixd])
      cY = weighted.mean(D$Y[ixd], D$w[ixd])
      pr.ixd = kernel(cX, D$X, cY, D$Y, D$w, Kobj)
      pr.sort(ixd, pr.ixd)
    } else {
      list(pr = 0, id = 0) 
    }
  }
  
  ob1 = getPR.here.i(1,D,P,Kobj)
  ob2 = getPR.here.i(2,D,P,Kobj)
  obb = list(ob1, ob2)
  
  d = length(P$X)
  for(i in 3:d)
    obb[[i]] = getPR.here.i(i,D,P,Kobj)
  obb
})}

#############################################
#  Patch to Patch connectivity matrix 
#  using the weighted center of S  
#  DEPRECATE? 
#############################################

centP2P = function(S, D, P, Kobj){with(Kobj,{
  sumIt = function(j,D,Pr){
    sum(Pr[which(D$P == j)])  
  }
  d = length(P$X)
  Pmatrix = matrix(0,d,d)
  for(i in 1:d){
    cix = which(S$P == i)
    if (length(cix)>0){
      ####################################
      # DNOTE: 
      # Are these the right weights? 
      # Maybe they are exactly wrong 
      # since more of the weight stays.
      ####################################
      cX = weighted.mean(S$X[cix], S$w[cix])
      cY = weighted.mean(S$Y[cix], S$w[cix])
    } else {
      cX=P$X[i]
      cY=P$Y[i]
    }
    Pr = kernel(cX, D$X, cY, D$Y, D$w, Kobj)
    Pmatrix[i,] = as.vector(unlist(sapply(1:d, sumIt, D=D, Pr=Pr)))
  }
  Pmatrix   
})}

#############################################
#  Patch to Patch connectivity matrix :: 
#  from the patch center to the points 
#  in D belonging to each patch  
#############################################

P2P = function(D, P, Kobj){with(Kobj,{
  sumIt = function(j,D,Pr){
    sum(Pr[which(D$P == j)])  
  }
  d = length(P$X)
  Pmatrix = matrix(0,d,d)
  for(i in 1:d){
    Pr = kernel(P$X[i], D$X, P$Y[i], D$Y, D$w, Kobj)
    Pmatrix[i,] = as.vector(unlist(sapply(1:d, sumIt, D=D, Pr=Pr)))
  }
  Pmatrix   
})}

DinP = function(D, P){
  d = length(P$X) 
  wts.i = function(i,D,P){
    ix = which(D$P == i)
    wts = D$w[ix]
    list(ix=ix, pr = wts/sum(wts)) 
  } 
  f1 = wts.i(1,D,P)  
  f2 = wts.i(2,D,P)  
  PRlist = list(f1, f2) 
  for(i in 3:d){
    PRlist[[i]] = wts.i(i,D,P)  
  } 
  PRlist 
}

patchesFAR.i = function(i, loc, pState, MvMat){
  if(pState == "F"){
    if(loc == "l") ix = L2f[[i]]$nearP
    if(loc == "f") ix = F2f[[i]]$nearP
    Pid = i.rmultinom(to.f[i,-ix])
    this = in.f[[Pid]]
  } 
  if(pState == "L"){
    if(loc == "l") ix = L2l[[i]]$nearP
    if(loc == "f") ix = F2l[[i]]$nearP
    Pid = i.rmultinom(to.l[i,-ix])
    this = in.l[[Pid]]
  } 
  this$id[i.rmultinom(this$pr)] 
}

patches.All = function(f, l, P, Fmv, Lmv){
  list(
    L2f = patchesMvOb(l,f,P,Fmv),
  	F2f = patchesMvOb(f,f,P,Fmv),
  	F2l = patchesMvOb(f,l,P,Lmv),
  	L2l = patchesMvOb(l,l,P,Lmv), 
    to.f = P2P(f,p,Fmv),
    to.l = P2P(l,p,Fmv),
    in.f = DinP(f,p), 
    in.l = DinP(l,p), 
    moveFar = patchesFAR.i 
  ) 
}
makePatch = function(F, L, Lobj=rect.Lobj(), Pobj = rect.Pobj()){
  P = patch$makeP(F, L, Lobj, Pobj) 
  F$P <<- O2P(F,P) 
  L$P <<- O2P(F,P)
  P
} 

rect.Pobj = function(
  N.p = 400,
  N.adj = 20,
  trim = 10,
  hood = 8,
  makeP = Prand
  #makeP = Pgrid 
){
  list(N.p = N.p, hood = hood, makeP = makeP, N.adj = N.adj, trim=trim)
}

#Example
#
#patch = rect.Pobj()
#P = makePatch(haunts,pools,lscape, patch) 
#visualizePoints(,L,P)
