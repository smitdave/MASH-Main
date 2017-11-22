
kerW = function(xy, XY, p=1.5){
  d = sqrt((xy[1] - XY[,1])^2 + (xy[2] - XY[,2])^2) 
  exp(-d*p)*XY[,3]
}

#xy = c(0,0)
#XY = cbind(1:20, 1:20, rep(1,20))
#plot(1:20, kerW(xy, XY))

kerW.i = function(i, xy, XY, w, p=1){
  kerW(xy[i,], XY, p)
}


FL = sapply(X=c(1:N.f), FUN=kerW.i, xy = xy.f, XY=xy.l, simplify = "array")
LF = sapply(X=c(1:N.l), FUN=kerW.i, xy = xy.l, XY=xy.f, simplify = "array")

LF = t(LF)
FL = t(FL)

Q = LF%*%FL

Q = Q / rowSums(Q)
N = matrix(pmax(0, Q - t(Q)), N.l, N.l) 
CC = Q-N

#par(mfrow = c(2,1), mar = c(0,0,0,0))
plot(xy.l[,1], xy.l[,2], pch = 3, col = "blue",  xlab = "", xaxt = "n", ylab = "", yaxt = "n", xlim = range(xy.f, xy.l), ylim = range(xy.f, xy.l))
points(xy.f[,1], xy.f[,2], pch = 4, col = "red")

for(i in 1:N.l)
  for(j in i:N.l)
    segments(xy.l[i,1], xy.l[i,2], xy.l[j,1], xy.l[j,2], lwd = CC[i,j]*2)

plot(xy.l[,1], xy.l[,2], pch = 16, col = "blue",  xlab = "", xaxt = "n", ylab = "", yaxt = "n", xlim = range(xy.f, xy.l), ylim = range(xy.f, xy.l))
points(xy.f[,1], xy.f[,2], pch = 16, col = "red")
for(i in 1:N.l)
  for(j in 1:N.l)
    arrows(xy.l[i,1], xy.l[i,2], xy.l[j,1], xy.l[j,2], lwd = N[i,j]*2, length =0.05)

#plot(xl, yl, type = "n", pch = 16, col = "blue", ylim = c(-1,12), xlim = c(-1,12), xlab = "", xaxt = "n", ylab = "", yaxt = "n")

#for(i in 1:29)
#  for(j in 1:29)
#    arrows(xl[i], yl[i], xl[j], yl[j], lwd = N2[i,j], length = .05)
