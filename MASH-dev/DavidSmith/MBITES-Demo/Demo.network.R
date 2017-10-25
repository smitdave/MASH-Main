
kerW = function(xy, XY, w, p=1){
  d = sqrt( (xy[,1] - XY[,1])^2 + (xy[,2] - XY[,2])^2) 
  exp(-d*p)*w
}

kerW.i = function(i, xy, XY, w, p=1){
  kerW(xy[i,], XY, w, p)
}


LF = sapply(X=c(1:N.f), FUN=kerW.i, xy = xy.f, XY=xy.l, w=w.f, simplify = "array")
FL = sapply(X=c(1:N.f), FUN=kerW.i, xy = xy.l, XY=xy.f,w=w.l, simplify = "array")

LF = t(LF)
FL = t(FL)

Q = LF%*%FL

Q = Q / rowSums(Q)

N = matrix(pmax(0, Q - t(Q)), 29, 29) 
C = Q-N

Q2 = (Q %*% Q)
Q2 = Q2 / rowSums(Q2)

N2= matrix(pmax(0, Q2 - t(Q2)), 29, 29) 
C2 = Q2-N2

plot(xf, yf, type = "n", pch = 16, col = "blue", ylim = c(-1,12), xlim = c(-1,12), xlab = "", xaxt = "n", ylab = "", yaxt = "n")

for(i in 1:29)
  for(j in i:29)
    segments(xl[i], yl[i], xl[j], yl[j], lwd = C2[i,j]*4)

plot(xl, yl, type = "n", pch = 16, col = "blue", ylim = c(-1,12), xlim = c(-1,12), xlab = "", xaxt = "n", ylab = "", yaxt = "n")

for(i in 1:29)
  for(j in 1:29)
    arrows(xl[i], yl[i], xl[j], yl[j], lwd = N2[i,j], length = .05)