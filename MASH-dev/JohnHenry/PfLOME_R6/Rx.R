
RxRegister = list()

# DRC :: Drug Resistance Class
#   Artemisinin
#   

RxRegister[[1]] = list(
  Duration = 23,
  PfPD = c(c(4,4,4), 2*(1-c(1:20)/20)), 
  DRC = c(1,2), 
  PfPDx = list(),
  name = "PfPrototype, Full"
)

RxRegister[[1]]$PfPDx[[1]] = c(c(4,4,4, rep(0,20))) 
RxRegister[[1]]$PfPDx[[2]] = c(c(rep(0,3), 2*(1-c(1:20)/20)))


RxRegister[[2]] = list(
  Duration = 23,
  PfPD = c(c(4,4,0), 1.5*(1-c(1:20)/20)), 
  DRC = c(1,2), 
  PfPDx = list(),
  name = "PfPrototype, Partial"
)

RxRegister[[2]]$PfPDx[[1]] = c(c(4,4,0, rep(0,20))) 
RxRegister[[2]]$PfPDx[[2]] = c(c(rep(0,3), 1.5*(1-c(1:20)/20)))

