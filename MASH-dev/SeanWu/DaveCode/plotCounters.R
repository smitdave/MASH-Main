source ("Counters.R")
#plot(xx, waxwane(xx,6, kw=30, kn=30), type = "l", col = "red")
#segments(0,0,1,0, lty=2)

#P = seq(0, 13, by = .1)
#plot(P, xbar(P), type = "l")

par(mfrow = c(1,1), mar = c(5,4,1,1))
mx = 25*365
t = c(1:mx)
P = 13*(1+sin(1+2*pi*t/365))/2
pps = 5

plot(t/365,xbar(P, Ps=pps),type = "l", ylab = expression(bar(x)), xlab = "Time (Years)", ylim = c(0,1))
X = x = rep(0,25) 
wx = matrix(c(1/20, 1/60, 1/180, 1/540, 1/1500), 5,5)
wn = t(wx)
wx = as.vector(wx)
wn = as.vector(wn)
for(i in t[-1]){
  x = iterX(x, P[i], wax=wx, wane=wn, Ps=pps)
  X = rbind(X,x)
}

for(i in c(1:25)){
  lines(t/365, X[,i], col = i+11)
}
