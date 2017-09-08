
T = 365*5
d = seq(0,T,by=1)

# The Seasonal Signal 

lambda = function(d){ 
  moonDark = (2.5+sin(2*pi*(d+5)/28))/2.5
  biannual = (1.2 + sin(2*pi*d*2/365))
  annual = .2*(1.8 + sin(2*pi*(d-200)/365))^2
  year3 = .5*(2 + sin(2*pi*(d-200)/1035))^2
  long = (1.5 + sin(2*pi*(d-100)/2100))/2
  5*(biannual*annual*year3*long*moonDark)^1.5
} 

M = Mt = 50
Y = Yt = 5
Z = Zt = 1
H = 1000
X = Xt = 10

p = 0.9
EIP = 12
a = .3
c = .1
r = 1/200
b = 0.55

# Sd = rep(0.5,365)
# k = 0.5

for(i in 1:T){
 Mt = p*Mt + Sd[i]
 Yt = p*Yt + a*c*Xt/H*max(Mt-Yt,0)
 Zt = p*Zt + ifelse(i>EIP,p^EIP*Y[i-EIP],0)
 At = 1 - exp(-b*Zt/H)
 M.eq = Sd[i]/(1-p)
 k.eq = a*c*Xt/H
 Y.eq = k*M.eq/(1-p+k)
 # Z.eq = 
 Xt = rbinom(1, Xt, 1-r) + rbinom(1, H-Xt, At)
 M = c(M, Mt)
 Y = c(Y, Yt)
 Z = c(Z, Yt)
 X = c(X, Xt)
} 

par(mfrow = c(3,1))
ix = 1:length(M)
plot(d[ix]/365, Z, type = "l", ylab = expression(M(t)), xlab = "Time (Years)")
plot(d[ix]/365, Z/M, type = "l", col = "red")
plot(d[ix]/365, X/H, type = "l")


