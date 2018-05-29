
f.xy = read.csv('demo_data/peridom.f116.xyw', header=T)/2
plot(f.xy[,1], f.xy[,2], type = "n", pch = 3, col = "red", xlab = "East-West", ylab = "North-South")

l.xy = read.csv('demo_data/peridom.l117.xyw', header=T)/2

set.seed(21)
m.x = runif(10, -10, 10)/2
m.y = runif(10, -10, 10)/2
m.xy = cbind(x=m.x, y=m.y)

xx = unique(c(f.xy[,1], l.xy[,1]))
yy = unique(c(f.xy[,2], l.xy[,2])) 

lx = length(xx)
ix = sample(1:lx, 80)
s.x = c(xx[ix], runif(40, -10, 10)/2)
s.y = c(yy[ix], runif(40, -10, 10)/2)
s.xy = cbind(x=s.x, y=s.y)

f.xy[,3] = rgamma(250, 1, 1)
l.xy[,3] = rgamma(250,1,1)
m.xy = cbind(m.xy, w=rgamma(10,1,1))
s.xy = cbind(s.xy, w=rgamma(120,1,1))

points(m.xy, pch=15, col = "orange", cex = m.xy[,3])
points(f.xy, pch = 21, bg = "red", cex = f.xy[,3])
points(l.xy, pch = 4, col = "blue", cex = l.xy[,3])
points(s.xy, pch=6, col=grey(0.5), cex=s.xy[,3])

#points(s.xy, pch=15, col = "yellow")

write.csv(f.xy, "demo/demo_f.csv")
write.csv(l.xy, "demo/demo_l.csv")
write.csv(m.xy, "demo/demo_m.csv")
write.csv(s.xy, "demo/demo_s.csv")
