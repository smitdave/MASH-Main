tt = c(1:365)
S = 2.5
hbr = 550/365
kappa = 0.01/S
plot(tt, cumsum(tt*0 + hbr*kappa), type = "l") 

