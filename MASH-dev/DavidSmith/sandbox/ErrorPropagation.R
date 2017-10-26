# Information Propagation / Relevant Detail

x = 1+rgamma(100, 1, 1)
f1 = function(x, n=2){x^n}

xx = seq(0,max(x), by = .01)

nn=4
par(mfrow = c(2,1))
plot(xx, f1(xx,nn), type = "l")
points(x,0*x, pch = 15, cex = .3)
points(0*x, f1(x,nn), pch = 15, cex = 0.3)
arrows(2,0,2,f1(2,nn), length = 0.05)
arrows(2,f1(2,nn),0,f1(2,nn), length = 0.05)

nn=0.5
plot(xx, f1(xx,nn), type = "l")
points(x,0*x, pch = 15, cex = .3)
points(0*x, f1(x,nn), pch = 15, cex = 0.3)
arrows(2,0,2,f1(2,nn), length = 0.05)
arrows(2,f1(2,nn),0,f1(2,nn), length = 0.05)