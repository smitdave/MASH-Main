source('PDG.R')

human = PDGHuman$new()
human$infect_Human()


Nweeks = 50
for(t in seq(1:Nweeks)){
  human$update_Human()
#  if(rbinom(1,1,.01)==1){
#    human$infect_Human()
#  }
}

Pt = human$get_history()$Pt
Gt = human$get_history()$Gt
Imm = human$get_history()$Imm

plot(seq(1:Nweeks),Pt,type="l",ylim=c(0,14),xlab="weeks")
lines(Gt,lty=2)
abline(h=8)
lines(seq(1:Nweeks),Imm,type="l",ylim=c(0,1))
abline(h=1)

