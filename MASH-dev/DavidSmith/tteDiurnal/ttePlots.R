tt = seq(0, 1, length.out=500)
plot(tt, cos(2*pi*tt), type = "l", col = "red")
lines(tt, sin(2*pi*tt))

lambda=.2
plot(tt, exp(-lambda*tt), type = "l")
lines(tt, exp(-lambda*tt)*exp(-lambda*tt*sin(2*pi*tt)), type = "l")

par(mfrow = c(3,1))
plot(tt, lambda*(1+cos(2*pi*tt)), type = "l")
lines(tt, lambda+0*tt, col = grey(0.5), lwd=2, lty=2)

#plot(tt, exp(-lambda*2*((tt*2*pi*sin(2*pi*tt) + cos(2*pi*tt)-1)/(2*pi)^2)), type = "l")

plot(tt, exp(-lambda*tt), type = "l", col = grey(0.5), lwd=2, lty=2)
#lines(tt, exp(-lambda*2*(tt^2/2 + (tt*2*pi*sin(2*pi*tt) + cos(2*pi*tt)-1)/(2*pi)^2)), type = "l")

lines(tt, exp(-lambda*(tt+sin(2*pi*tt)/2/pi)), type = "l", col = grey(0.5))

plot(exp(-lambda*tt), exp(-lambda*(tt+sin(2*pi*tt)/2/pi)), type = "l")
segments(0,0,1,1)
