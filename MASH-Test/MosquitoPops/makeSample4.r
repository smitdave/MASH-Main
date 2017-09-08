setwd("C:/Users/bcreiner/Dropbox/Davis/ColoredNoise/2014_6_25")



source ("makeFigs.R") 

#out1 = replicate(100, os(p=.025)) 
#out2 = replicate(100, os(p=.05)) 
#out3 = replicate(100, os(p=.1)) 
#out4 = replicate(100, os(p=.2)) 

mZ = function(out){ 
  mean(c(out[1,], out[4,])) 
} 

dZ = function(out){ 
  (out[1,] +  out[4,])/2
} 

dz = function(out){ 
  out[3,]- out[6,]
} 

#dd1 = abs(dz(out1)) 
#dd2 = abs(dz(out2)) 
#dd3 = abs(dz(out3)) 
#dd4 = abs(dz(out4)) 

# n=12
# a=.3
# p=11/12

require(fArma)
Repetitions<-1000
HurstVals <- c(.01,0.5,.99)
fgnscaleVals <- c(.1,1,10)
ReductionVals <- c(.25,.5,.8)
for (R in 1:length(ReductionVals)){
for (H in 1:length(HurstVals)){
for (S in 1:length(fgnscaleVals)){
	reduction <- ReductionVals[R]
	Hurst <- HurstVals[H]
	fgnscale <- fgnscaleVals[S]
	out=vector("list",Repetitions)
	for (i in 1:Repetitions){
		out[[i]]<-os(Hurst=Hurst,fgnscale=fgnscale,reduction=reduction,kappaval=0.1)
	}
	assign(sprintf("out%i%i%i",H,S,R),out)
}
}
}

AllOutput4<-array(list(NULL),c(length(HurstVals),length(fgnscaleVals)))
for (R in 1:length(ReductionVals)){
for (H in 1:length(HurstVals)){
for (S in 1:length(fgnscaleVals)){
	AllOutput4[[R,H,S]]=get(sprintf("out%i%i%i",H,S,R))
}
}
}
	
save(AllOutput4,file="AllOutput4.RData")
