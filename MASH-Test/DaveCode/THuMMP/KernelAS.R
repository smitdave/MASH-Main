
sigmoid = function(x, k=2.944445, x50=5){
  1-1/(1+exp(-k*(x-x50)))
}


routine = function(i,H,dd,D,PAR){with(PAR,{
  # Routine travel from here to everywhere
  # H is the vector of human H density
  # dst is the vector of distances
  #dk = (H*D^alpha.r*sigmoid(log10(dd[i,]),k=k,x50=x50))^delta.r
  wts = H^beta.r*sigmoid(log10(dd[i,]),k=k,x50=x50)
  wts[i] = 0 
  wts = wts/sum(wts)*(1-p0)
  wts[i] = p0
  wts
})}

travel = function(i,H,dd,D,PAR){with(PAR,{
  # Routine travel from here to everywhere
  # H is the vector of human H density
  # dst is the vector of distances
  dk = (H*(1-sigmoid(log10(dd[i,]),k=k,x50=x50)))^delta.r
  wts = H^beta.t*dk/dst^delta.t
  wts[i] = 0 
  wts/sum(wts)
})}
