rsphere = function(n,d,r){
  t = matrix(rnorm(n*d,0,1),ncol=n,nrow=d)
  for(i in 1:n){
    t[,i] = r*t[,i]/norm(t[,i],type="2")
  }
  return(t)
}

p = (.05,.25)

t = c(0,7,14,21,30,37,44,51)
i = c(10,14,20,17,11,7,1,1)
fit = splinefun(t,i,method=c("natural"))
tt = seq(0,51,.01)
plot(tt,fit(tt),type="l")
points(t,i)
lines(t,i)
DSIR = 

p = p+rsphere(100,2,.1)