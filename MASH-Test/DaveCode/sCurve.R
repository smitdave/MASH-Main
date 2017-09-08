
Iscurve = function(X, k=2.944445, X50=5){
  log(1+exp(-k*(X-X50)))/ log(1 + exp(k*X50)) 
}

scurve = function(X, k=2.944445, X50=5){
  1-1/(1+exp(-k*(X-X50)))
}

dscurve = function(X, k=2.944445, X50=5){
  k * exp(-k*(X-X50))/ (1+ exp(-k*(X-X50)))^2
} 


par(mfrow = c(1,1))

plot(X, scurve(X), type ="l") 
#lines(X,Iscurve(X), type = "l")

X = seq(0,5, by = .05)
XX = exp(X)
plot(XX, scurve(X, X50 = 3, k=3), type = "l", xlab = "Distance (in km)")
lines(XX, scurve(X, X50 = 3, k=3), type = "l")
lines(XX, scurve(X, X50 = 3.5, k=3), type = "l")
lines(XX, scurve(X, X50 = 4, k=3), type = "l", xlab = "Distance (in km)")
lines(XX, scurve(X, X50 = 4.5, k=3), type = "l")
lines(XX, scurve(X, X50 = 3, k=4), type = "l")
lines(XX, scurve(X, X50 = 3.5, k=4), type = "l")
lines(XX, scurve(X, X50 = 4, k=4), type = "l", xlab = "Distance (in km)")
lines(XX, scurve(X, X50 = 4.5, k=4), type = "l")


