
P = 100
Years = 5
Fortnights = Years*26
aFOI = 2
lambda = aFOI/26
Pt = matrix(0,nrow=Fortnights,ncol=P)

start = proc.time()
for(j in 1:P){
  
  human = PDGHuman$new()

  for(i in 1:Fortnights){
  
    infection = rpois(1,lambda)
    if(infection>0){
      human$infect_Human(infection)
    }
  
    human$update_Human()
  }
  
  Pt[,j] = human$get_history()$Pt
}

time = (0:Fortnights)/26
PfPR = 1-rowSums(Pt==0|is.na(Pt))/P
PfPR = c(0,PfPR)
plot(time,PfPR,type="l",ylim=c(0,1),main="Prevalence in PDG",xlab="Time (in Years)",ylab="PfPR")

finish = proc.time()
SimTime = finish-start

# input is population size, years to simulate, number of infections to initiate at outset, and the outputs you want
PDGMOISim = function(P,Years,MOI=1,PfPR=T,Pt=T){
  
  output = list()
  if(Pt==T){
    output$Pt = matrix(0,nrow=,ncol=)
  }
  
  for(j in 1:P){
    human=PDGHuman$new()
    human$infect_Human(MOI)
  
    fortnights = 26*Years
    for(i in 1:fortnights){
      human$update_Human()
    }
  
  if(PfPR==T){
    output$
  }
  if(Pt==T){
    
  }
  }
}