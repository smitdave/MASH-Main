
redSmooth = function(Pt,gr){
  ix=which(diff(Pt)>gr)
  i=1
  while(length(ix)>0){
    i = i+1 
    if (i > 10) browser() 
    Pt[ix+1] = Pt[ix] + gr - 10^-6
    ix=which(diff(Pt)>gr)
  } 
  Pt} 

cumGam = function(iG1){
  GG = rep(NaN, 10)
  gg = NaN
  for(i in c(1:length(iG1))){
    gg = addPosLogs(c(gg-0.45,iG1[i]))
    GG = c(GG, gg)
  }
  if(!is.na(gg)){
    while(gg >0){
      gg = gg-0.45
      GG = c(GG, gg)  
    }
  } 
  GG
} 

Pt2Gt_simple = function(Pt, MZ2GZ){
  Gt = c(rep(NaN, 15), Pt-MZ2GZ) 
  Gt[which(Gt < 0)] = NaN
  Gt
}




makePt_tent = function(gr=1, MZ0=4.2, peakD=38, mxPD=10.5, tEnd=220){
  #FIX STUB 
  dr = mxPD/tEnd
  P1 = rep(NaN,6) 
  P2 = pmin(MZ0 + gr*c(1:(peakD-6)), mxPD)
  P3 = max(P2) - dr*c(1:tEnd)
  c(P1,P2,P3)
}

makeGt.tent = function(Pt){
  Gt = Pt-2 
  Gt[which(Gt <0)] = NaN 
  c(rep(-1,10), Gt)
}

PfID = 0