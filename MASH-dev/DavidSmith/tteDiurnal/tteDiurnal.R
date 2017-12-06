################################################
# To model mosquito diurnal activity, consider 
# the time dependent sinusoidal hazard rate function
#
# ff = lambda (1+amp*cos(2*pi*(t-peak))) 
# (0 <= amp <= 1)
################################################
par(mfrow = c(2,2))
ttt = seq(0,1, length.out=1000)
ff = function(t, peak=0){(1+cos(2*pi*(t-peak)))}
ff = function(t, peak=0, amp=1){(1+amp*cos(2*pi*(t-peak)))}

plot(ttt*24, ff(ttt), type = 'l', ylab = "Activity Level", xlab = "Time of Day", xaxt = "n") 
lines(ttt*24, ff(ttt, 0, 1), lty = 2) 
axis(1, c(0:3)*6, c("12 am", "6 am", "12 pm", "6 pm"))



################################################
# At the times t = peak+0, peak+1, peak+2, peak+3,...
# ff has the same cumulative hazard as the constant hazard
# function
#
# f = lambda 
# 
# A random number generator for this process has 
################################################


##################################################
#  lambda :: the average activity level
#  now    :: time when the waiting period starts
#  peak   :: peak activity level 
##################################################

rDiurnal = function(N, lambda, now=0, peak=0){
  t = rexp(N, lambda)
  ti = t-floor(t)
  ti - lambda*(ti*sin(2*pi*(ti+now-peak))  + cos(2*pi*(ti+now-peak)) -1) 
  #t + sin(2*pi*(ti+now-peak))
  #tr = t-ti 
  #ti+tr*sin(2*pi*(tr+now-peak))+cos(tr)
  #(t-ti)*(1+cos(2*pi*(t-ti+now-peak)))
  #acos(2*pi*(t-ti-now-peak)) 
  #ti + 
}

#rDiurnal = function(N, lambda, now=0, peak=0, amp=1){
#  t = rexp(N, lambda)
#  ti = floor(t)
#  ti + amp*(1+amp*sin(2*pi*(t-ti+now-peak)))/2
#}


wt=rnorm(10000,.75,1/24)
wt=0
#hist(wt*24, xlab = "Minimal Waiting Time, in Hours", main = "Histogram of Minimal Waiting Time")

tt =  wt + rDiurnal(10000, 1, now=wt, peak =0.5)
#tt =  wt + rDiurnal(10000, 1, wt, peak =12, amp=0.5)
hist(tt,40, xlab = "Time to Events", main = "Histogram of TTE")
tti = floor(tt)

hist(tt-tti, 40, xlab = "Time of Day", main = "Histogram of Time of Day")
lines(ttt, 150*(1+cos(2*pi*ttt)))
ttb = hist(tt, plot=FALSE)$breaks[-1]
lines(ttb, 150*(1+cos(2*pi*ttb))*hist(tt, plot=FALSE)$counts/800)

plot(ecdf(tt-tti))
segments(0,0,1,1)


