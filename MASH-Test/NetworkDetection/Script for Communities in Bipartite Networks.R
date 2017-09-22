# The code relies on 2 libraries: igraph and clue, both of which will need to be installed to avoid errors

# General function which produces 3 communities in the first mode and 2 in the second
sim.two.mode<-function(a,b,den,p){
  # a is the length of the first mode
  # b is the length of the second mode
  # NOTE: a should be less than b (if not, take the transpose)
  # den is the (asymptotic) density of the simulated network
  # p is the probability of a within-community tie
  e11<-t(replicate((a/3),sample(1:b,(b*den),replace=FALSE,prob=c(rep(p,(b/2)),rep((1-p),(b/2))))))
  f11<-matrix(0,nc=b,nr=(a/3))
  for(i in 1:(a/3)){f11[i,][e11[i,]]<-1
  }
  e12<-t(replicate((a/3),sample(1:b,(b*den),replace=FALSE,prob=c(rep((1-p)/2,(b/3)),rep(p,(b/3)),rep((1-p)/2,(b/3))))))
  f12<-matrix(0,nc=b,nr=(a/3))
  for(i in 1:(a/3)){f12[i,][e12[i,]]<-1
  }    
  e14<-t(replicate((a/3),sample(1:b,(b*den),replace=FALSE,prob=c(rep((1-p),(b/2)),rep((p),(b/2))))))
  f14<-matrix(0,nc=b,nr=(a/3))
  for(i in 1:(a/3)){f14[i,][e14[i,]]<-1
  }
    f1<-rbind(f11,f12,f14)
    return(f1)}

# Function which computes the normalized mutual information between the a priori community structure and the discovered one; returns 4 NMIs and whether the graph was connected.
NMIs<-function(a,b,den,p){
  # a is the length of the first mode
  # b is the length of the second mode
  # NOTE: a should be less than b (if not, take the transpose)
  # den is the (asymptotic) density of the simulated network
  # p is the probability of a within-community tie
  library(igraph)
  library(clue)
  A<-sim.two.mode(a,b,den,p)
  B<-matrix(0,nr=a,nc=a)
  C<-matrix(0,nr=b,nc=b)
  A2<-cbind(B,A)
  A3<-cbind(t(A),C)
  X<-rbind(A2,A3)
  X2<-graph.adjacency(X,mode="undirected")
  k<-(is.connected(X2))*1
  # a priori clustering
  apriori1<-c(rep(1,a/3),rep(2,a/3),rep(3,a/3))
  apriori2<-c(rep(1,b/2),rep(2,b/2))
  A1<-as.cl_partition(apriori1)
  A2<-as.cl_partition(apriori2)
  if (k==1) u<-walktrap.community(X2)$membership else u<-0
  if (k==1) u1<-u[1:a] else u1<-0
  if (k==1) u1<-as.matrix(u1) else u1<-0
  if (k==1) u1<-as.numeric(u1) else u1<-0
  if (k==1) W1<-as.cl_partition(u1) else W1<-0
  if (k==1) x1<-cl_agreement(A1,W1,method="NMI") else x1<-0
  if (k==1) u2<-u[(a+1):(a+b)] else u2<-0
  if (k==1) u2<-as.matrix(u2) else u2<-0
  if (k==1) u2<-as.numeric(u2) else u2<-0
  if (k==1) W2<-as.cl_partition(u2) else W2<-0
  if (k==1) x2<-cl_agreement(A2,W2,method="NMI") else x2<-0
  C<-A%*%t(A)
  for(i in 1:dim(A)[1]){C[i,i]<-0}
  D<-t(A)%*%A
  for(i in 1:dim(A)[2]){D[i,i]<-0}
  if (k==1) y1<-walktrap.community(graph.adjacency(C,weighted=TRUE))$membership else y1<-0
  if (k==1) y1<-as.matrix(y1) else y1<-0
  if (k==1) y1<-as.numeric(y1) else y1<-0
  if (k==1) Y1<-as.cl_partition(y1) else Y1<-0
  if (k==1) Y1<-cl_agreement(A1,Y1,method="NMI")  else Y1<-0
  if (k==1) y2<-walktrap.community(graph.adjacency(D,weighted=TRUE))$membership  else y2<-0
  if (k==1) y2<-as.matrix(y2) else y2<-0
  if (k==1) y2<-as.numeric(y2) else y2<-0
  if (k==1) Y2<-as.cl_partition(y2) else Y2<-0
  if (k==1) Y2<-cl_agreement(A2,Y2,method="NMI") else Y2<-0
  # x1 is the NMI for walktrap on the first mode
  # x2 is the NMI for walktrap on the second mode
  #
  # Y1 is the NMI for walktrap on the first mode (dual projection)
  # Y2 is the NMI for walktrap on the second mode (dual projection)
  NMIs<-c(x1,x2,Y1,Y2,k)
  NMIs<-t(as.matrix(NMIs))
  names<-c('BiPart_Walk_First','Bipart_Walk_Second','DP_Walk_First','DP_Walk_Second','AllGraphsConnected')
  colnames(NMIs)<-names
  return(NMIs)}

# Function that replicates the NMI function as many times as you want. Relies on the above two functions.
Loop<-function(nobs,a,b,den,p){
  # nobs is the number of observations or number of times you want to run the simulation. 
  # In the paper, nobs is 1,000 for the small networks and 100 for the large networks
  q<-matrix(0,nr=nobs,nc=5)
  for(i in 1:nobs){q[i,]<-NMIs(a,b,den,p)}
  names<-c('BiPart_Walk_First','Bipart_Walk_Second','DP_Walk_First','DP_Walk_Second','AllGraphsConnected')
  colnames(q)<-names
  return(q)
}

# Results for small networks
set.seed(1982)
e<-Loop(1202,60,120,.125,.9)
sum(e[1:1202,9])
e<-e[1:1202,]
f<-1:1202
g<-e[,9]*f
# h is the results for the small networks when there are 3 communities in the first mode and 2 in the second mode.
h<-e[g,]
# End results for small networks

# Results for large networks
set.seed(1982)
e<-Loop(102,600,1200,.025,.9)
e<-e[1:102,]
f<-1:102
g<-e[,9]*f
# h is the results for the large networks when there are 3 communities in the first mode and 2 in the second mode.
h<-e[g,]
# End results for large networks


# "Control" networks with three equal-sized communities.
sim.two.mode<-function(a,b,den,p){
  e11<-t(replicate((a/3),sample(1:b,(b*den),replace=FALSE,prob=c(rep(p,(b/3)),rep((1-p)/2,(b/3)),rep((1-p)/2,(b/3))))))
  f11<-matrix(0,nc=b,nr=(a/3))
  for(i in 1:(a/3)){f11[i,][e11[i,]]<-1
  }
  e12<-t(replicate((a/3),sample(1:b,(b*den),replace=FALSE,prob=c(rep((1-p)/2,(b/3)),rep(p,(b/3)),rep((1-p)/2,(b/3))))))
  f12<-matrix(0,nc=b,nr=(a/3))
  for(i in 1:(a/3)){f12[i,][e12[i,]]<-1
  }  
  e13<-t(replicate((a/3),sample(1:b,(b*den),replace=FALSE,prob=c(rep((1-p)/2,(b/3)),rep((1-p)/2,(b/3)),rep(p,(b/3))))))
  f13<-matrix(0,nc=b,nr=(a/3))
  for(i in 1:(a/3)){f13[i,][e13[i,]]<-1
  }
  f1<-rbind(f11,f12,f13)
  return(f1)}


NMIs<-function(a,b,den,p){
  library(igraph)
  library(clue)
  A<-sim.two.mode(a,b,den,p)
  B<-matrix(0,nr=a,nc=a)
  C<-matrix(0,nr=b,nc=b)
  A2<-cbind(B,A)
  A3<-cbind(t(A),C)
  X<-rbind(A2,A3)
  X2<-graph.adjacency(X,mode="undirected")
  k<-(is.connected(X2))*1
  # a priori clustering
  apriori1<-c(rep(1,a/3),rep(2,a/3),rep(3,a/3))
  apriori2<-c(rep(1,b/3),rep(2,b/3),rep(3,b/3))
  A1<-as.cl_partition(apriori1)
  A2<-as.cl_partition(apriori2)
  if (k==1) u<-walktrap.community(X2)$membership else u<-0
  if (k==1) u1<-u[1:a] else u1<-0
  if (k==1) u1<-as.matrix(u1) else u1<-0
  if (k==1) u1<-as.numeric(u1) else u1<-0
  if (k==1) W1<-as.cl_partition(u1) else W1<-0
  if (k==1) x1<-cl_agreement(A1,W1,method="NMI") else x1<-0
  if (k==1) u2<-u[(a+1):(a+b)] else u2<-0
  if (k==1) u2<-as.matrix(u2) else u2<-0
  if (k==1) u2<-as.numeric(u2) else u2<-0
  if (k==1) W2<-as.cl_partition(u2) else W2<-0
  if (k==1) x2<-cl_agreement(A2,W2,method="NMI") else x2<-0
  C<-A%*%t(A)
  for(i in 1:dim(A)[1]){C[i,i]<-0}
  D<-t(A)%*%A
  for(i in 1:dim(A)[2]){D[i,i]<-0}
  if (k==1) y1<-walktrap.community(graph.adjacency(C,weighted=TRUE))$membership else y1<-0
  if (k==1) y1<-as.matrix(y1) else y1<-0
  if (k==1) y1<-as.numeric(y1) else y1<-0
  if (k==1) Y1<-as.cl_partition(y1) else Y1<-0
  if (k==1) Y1<-cl_agreement(A1,Y1,method="NMI")  else Y1<-0
  if (k==1) y2<-walktrap.community(graph.adjacency(D,weighted=TRUE))$membership  else y2<-0
  if (k==1) y2<-as.matrix(y2) else y2<-0
  if (k==1) y2<-as.numeric(y2) else y2<-0
  if (k==1) Y2<-as.cl_partition(y2) else Y2<-0
  if (k==1) Y2<-cl_agreement(A2,Y2,method="NMI") else Y2<-0
  # x1 is the NMI for walktrap on the first mode
  # x2 is the NMI for walktrap on the second mode
  #
  # Y1 is the NMI for walktrap on the first mode (dual projection)
  # Y2 is the NMI for walktrap on the second mode (dual projection)
  NMIs<-c(x1,x2,Y1,Y2,k)
  NMIs<-t(as.matrix(NMIs))
  names<-c('BiPart_Walk_First','Bipart_Walk_Second','DP_Walk_First','DP_Walk_Second','AllGraphsConnected')
  colnames(NMIs)<-names
  return(NMIs)}


Loop<-function(nobs,a,b,den,p){
  q<-matrix(0,nr=nobs,nc=9)
  for(i in 1:nobs){q[i,]<-NMIs(a,b,den,p)}
  names<-c('BiPart_Walk_First','Bipart_Walk_Second','DP_Walk_First','DP_Walk_Second','AllGraphsConnected')
  colnames(q)<-names
  return(q)
}


set.seed(1982)
# Small control networks
e<-Loop(1015,60,120,.125,.9)
# Large control networks
e<-Loop(200,600,1200,.0125,.9)













###########
# Second simulation: 2 communities in the first mode and 10 in the second
###########




##############################
# Small Networks
##############################

sim.two.mode<-function(a,b,p){
  # a is the length of the first mode
  # b is the length of the second mode
  # p is the probability of a within-community tie
  
  g1<-matrix(0,nr=3,nc=11)
  for (i in 1:3){g1[i,]<-sample(1:(b/10),round(p*b/10),replace=FALSE)}
  g2<-matrix(0,nr=3,nc=11)
  for (i in 1:3){g2[i,]<-sample(13:24,round(p*12),replace=FALSE)}
  g3<-matrix(0,nr=3,nc=11)
  for (i in 1:3){g3[i,]<-sample(25:36,round(p*12),replace=FALSE)}
  g4<-matrix(0,nr=3,nc=11)
  for (i in 1:3){g4[i,]<-sample(37:48,round(p*12),replace=FALSE)}
  g5<-matrix(0,nr=3,nc=11)
  for (i in 1:3){g5[i,]<-sample(49:60,round(p*12),replace=FALSE)}
  g6<-matrix(0,nr=3,nc=11)
  for (i in 1:3){g6[i,]<-c(sample((c((1:2),59:60)),round(p*4),replace=FALSE),rep(0,7))}
  g7<-matrix(0,nr=3,nc=11)
  for (i in 1:3){g7[i,]<-c(sample((11:14),round(p*4),replace=FALSE),rep(0,7))}
  g8<-matrix(0,nr=3,nc=11)
  for (i in 1:3){g8[i,]<-c(sample((23:26),round(p*4),replace=FALSE),rep(0,7))}
  g9<-matrix(0,nr=3,nc=11)
  for (i in 1:3){g9[i,]<-c(sample((35:38),round(p*4),replace=FALSE),rep(0,7))}
  g10<-matrix(0,nr=3,nc=11)
  for (i in 1:3){g10[i,]<-c(sample((47:50),round(p*4),replace=FALSE),rep(0,7))}
  g11<-rbind(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10)
  e11<-t(replicate((a/2),sample(1:(b),(a/10),replace=FALSE)))
  e11<-cbind(e11,g11)
  f11<-matrix(0,nc=(b),nr=(a/2))
  for(i in 1:(a/2)){f11[i,][e11[i,]]<-1
  }
  g1<-matrix(0,nr=3,nc=11)
  for (i in 1:3){g1[i,]<-sample(1:(b/10),round(p*b/10),replace=FALSE)}
  g2<-matrix(0,nr=3,nc=11)
  for (i in 1:3){g2[i,]<-sample(13:24,round(p*12),replace=FALSE)}
  g3<-matrix(0,nr=3,nc=11)
  for (i in 1:3){g3[i,]<-sample(25:36,round(p*12),replace=FALSE)}
  g4<-matrix(0,nr=3,nc=11)
  for (i in 1:3){g4[i,]<-sample(37:48,round(p*12),replace=FALSE)}
  g5<-matrix(0,nr=3,nc=11)
  for (i in 1:3){g5[i,]<-sample(49:60,round(p*12),replace=FALSE)}
  g6<-matrix(0,nr=3,nc=11)
  for (i in 1:3){g6[i,]<-c(sample((c((1:2),59:60)),round(p*4),replace=FALSE),rep(0,7))}
  g7<-matrix(0,nr=3,nc=11)
  for (i in 1:3){g7[i,]<-c(sample((11:14),round(p*4),replace=FALSE),rep(0,7))}
  g8<-matrix(0,nr=3,nc=11)
  for (i in 1:3){g8[i,]<-c(sample((23:26),round(p*4),replace=FALSE),rep(0,7))}
  g9<-matrix(0,nr=3,nc=11)
  for (i in 1:3){g9[i,]<-c(sample((35:38),round(p*4),replace=FALSE),rep(0,7))}
  g10<-matrix(0,nr=3,nc=11)
  for (i in 1:3){g10[i,]<-c(sample((47:50),round(p*4),replace=FALSE),rep(0,7))}
  g11<-rbind(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10)
  g11[1:15,]<-g11[1:15,]+60
  g11[16:30,1:4]<-g11[16:30,1:4]+60
  e11<-t(replicate((a/2),sample(1:(b),(a/10),replace=FALSE)))
  e11<-cbind(e11,g11)
  f15<-matrix(0,nc=(b),nr=(a/2))
  for(i in 1:(a/2)){f15[i,][e11[i,]]<-1
  }
  t3<-rbind(f11,f15)
  return(t3)}




NMIs<-function(a,b,p){
  library(igraph)
  library(clue)
  A<-sim.two.mode(a,b,p)
  B<-matrix(0,nr=a,nc=a)
  C<-matrix(0,nr=b,nc=b)
  A2<-cbind(B,A)
  A3<-cbind(t(A),C)
  X<-rbind(A2,A3)
  X2<-graph.adjacency(X,mode="undirected")
  # a priori clustering
  apriori1<-c(rep(1,a/2),rep(2,a/2))
  apriori2<-c(rep(1,b/10),rep(2,b/10),rep(3,b/10),rep(4,b/10),rep(5,b/10),rep(6,b/10),rep(7,b/10),rep(8,b/10),rep(9,b/10),rep(10,b/10))
  A1<-as.cl_partition(apriori1)
  A2<-as.cl_partition(apriori2)
  k<-(is.connected(X2))*1
  if (k==1) u<-walktrap.community(X2)$membership else u<-0
  if (k==1) u1<-u[1:a] else u1<-0
  if (k==1) u1<-as.matrix(u1) else u1<-0
  if (k==1) u1<-as.numeric(u1) else u1<-0
  if (k==1) W1<-as.cl_partition(u1) else W1<-0
  if (k==1) x1<-cl_agreement(A1,W1,method="NMI") else x1<-0
  if (k==1) u2<-u[(a+1):(a+b)] else u2<-0
  if (k==1) u2<-as.matrix(u2) else u2<-0
  if (k==1) u2<-as.numeric(u2) else u2<-0
  if (k==1) W2<-as.cl_partition(u2) else W2<-0
  if (k==1) x2<-cl_agreement(A2,W2,method="NMI") else x2<-0
  C<-A%*%t(A)
  for(i in 1:dim(A)[1]){C[i,i]<-0}
  D<-t(A)%*%A
  for(i in 1:dim(A)[2]){D[i,i]<-0}
  if (k==1) y1<-walktrap.community(graph.adjacency(C,weighted=TRUE))$membership else y1<-0
  if (k==1) y1<-as.matrix(y1) else y1<-0
  if (k==1) y1<-as.numeric(y1) else y1<-0
  if (k==1) Y1<-as.cl_partition(y1) else Y1<-0
  if (k==1) Y1<-cl_agreement(A1,Y1,method="NMI")  else Y1<-0
  if (k==1) y2<-walktrap.community(graph.adjacency(D,weighted=TRUE))$membership  else y2<-0
  if (k==1) y2<-as.matrix(y2) else y2<-0
  if (k==1) y2<-as.numeric(y2) else y2<-0
  if (k==1) Y2<-as.cl_partition(y2) else Y2<-0
  if (k==1) Y2<-cl_agreement(A2,Y2,method="NMI") else Y2<-0
  # x1 is the NMI for walktrap on the first mode
  # x2 is the NMI for walktrap on the second mode
  #
  # Y1 is the NMI for walktrap on the first mode (dual projection)
  # Y2 is the NMI for walktrap on the second mode (dual projection)
  NMIs<-c(x1,x2,Y1,Y2,k)
  NMIs<-t(as.matrix(NMIs))
  names<-c('BiPart_Walk_First','Bipart_Walk_Second','DP_Walk_First','DP_Walk_Second','AllGraphsConnected')
  colnames(NMIs)<-names
  return(NMIs)}


Loop<-function(nobs,a,b,p){
  q<-matrix(0,nr=nobs,nc=5)
  for(i in 1:nobs){q[i,]<-NMIs(a,b,p)}
  names<-c('BiPart_Walk_First','Bipart_Walk_Second','DP_Walk_First','DP_Walk_Second','AllGraphsConnected')
  colnames(q)<-names
  return(q)
}

set.seed(1982)
e<-Loop(1200,60,120,.9)
# e is the results for the small networks.
e<-e[1:1002,]






##############################
# Large Networks
##############################


sim.two.mode<-function(a,b,p){
  g1<-matrix(0,nr=30,nc=108)
  for (i in 1:30){g1[i,]<-sample(1:120,round(p*b/10),replace=FALSE)}
  g2<-matrix(0,nr=30,nc=108)
  for (i in 1:30){g2[i,]<-sample(121:240,round(p*b/10),replace=FALSE)}
  g3<-matrix(0,nr=30,nc=108)
  for (i in 1:30){g3[i,]<-sample(241:360,round(p*b/10),replace=FALSE)}
  g4<-matrix(0,nr=30,nc=108)
  for (i in 1:30){g4[i,]<-sample(361:480,round(p*b/10),replace=FALSE)}
  g5<-matrix(0,nr=30,nc=108)
  for (i in 1:30){g5[i,]<-sample(481:600,round(p*b/10),replace=FALSE)}
  g6<-matrix(0,nr=30,nc=108)
  for (i in 1:30){g6[i,]<-c(sample((c((1:20),581:600)),round(p*40),replace=FALSE),rep(0,72))}
  g7<-matrix(0,nr=30,nc=108)
  for (i in 1:30){g7[i,]<-c(sample(101:140,round(p*40),replace=FALSE),rep(0,72))}
  g8<-matrix(0,nr=30,nc=108)
  for (i in 1:30){g8[i,]<-c(sample(221:260,round(p*40),replace=FALSE),rep(0,72))}
  g9<-matrix(0,nr=30,nc=108)
  for (i in 1:30){g9[i,]<-c(sample(341:380,round(p*40),replace=FALSE),rep(0,72))}
  g10<-matrix(0,nr=30,nc=108)
  for (i in 1:30){g10[i,]<-c(sample(461:500,round(p*40),replace=FALSE),rep(0,72))}
  g11<-rbind(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10)
  e11<-t(replicate((a/2),sample(1:(b),(a/25),replace=FALSE)))
  e11<-cbind(e11,g11)
  f11<-matrix(0,nc=(b),nr=(a/2))
  for(i in 1:(a/2)){f11[i,][e11[i,]]<-1
  }
  g1<-matrix(0,nr=30,nc=108)
  for (i in 1:30){g1[i,]<-sample(1:120,round(p*b/10),replace=FALSE)}
  g2<-matrix(0,nr=30,nc=108)
  for (i in 1:30){g2[i,]<-sample(121:240,round(p*b/10),replace=FALSE)}
  g3<-matrix(0,nr=30,nc=108)
  for (i in 1:30){g3[i,]<-sample(241:360,round(p*b/10),replace=FALSE)}
  g4<-matrix(0,nr=30,nc=108)
  for (i in 1:30){g4[i,]<-sample(361:480,round(p*b/10),replace=FALSE)}
  g5<-matrix(0,nr=30,nc=108)
  for (i in 1:30){g5[i,]<-sample(481:600,round(p*b/10),replace=FALSE)}
  g6<-matrix(0,nr=30,nc=108)
  for (i in 1:30){g6[i,]<-c(sample((c((1:20),581:600)),round(p*40),replace=FALSE),rep(0,72))}
  g7<-matrix(0,nr=30,nc=108)
  for (i in 1:30){g7[i,]<-c(sample(101:140,round(p*40),replace=FALSE),rep(0,72))}
  g8<-matrix(0,nr=30,nc=108)
  for (i in 1:30){g8[i,]<-c(sample(221:260,round(p*40),replace=FALSE),rep(0,72))}
  g9<-matrix(0,nr=30,nc=108)
  for (i in 1:30){g9[i,]<-c(sample(341:380,round(p*40),replace=FALSE),rep(0,72))}
  g10<-matrix(0,nr=30,nc=108)
  for (i in 1:30){g10[i,]<-c(sample(461:500,round(p*40),replace=FALSE),rep(0,72))}
  g11<-rbind(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10)
  g11[1:150,]<-g11[1:150,]+600
  g11[160:300,1:36]<-g11[160:300,1:36]+600
  e11<-t(replicate((a/2),sample(1:(b),(a/25),replace=FALSE)))
  e11<-cbind(e11,g11)
  f15<-matrix(0,nc=(b),nr=(a/2))
  for(i in 1:(a/2)){f15[i,][e11[i,]]<-1
  }
  t3<-rbind(f11,f15)
  return(t3)}


NMIs<-function(a,b,p){
  library(igraph)
  library(clue)
  A<-sim.two.mode(a,b,p)
  B<-matrix(0,nr=a,nc=a)
  C<-matrix(0,nr=b,nc=b)
  A2<-cbind(B,A)
  A3<-cbind(t(A),C)
  X<-rbind(A2,A3)
  X2<-graph.adjacency(X,mode="undirected")
  # a priori clustering
  apriori1<-c(rep(1,a/2),rep(2,a/2))
  apriori2<-c(rep(1,b/10),rep(2,b/10),rep(3,b/10),rep(4,b/10),rep(5,b/10),rep(6,b/10),rep(7,b/10),rep(8,b/10),rep(9,b/10),rep(10,b/10))
  A1<-as.cl_partition(apriori1)
  A2<-as.cl_partition(apriori2)
  k<-(is.connected(X2))*1
  if (k==1) u<-walktrap.community(X2)$membership else u<-0
  if (k==1) u1<-u[1:a] else u1<-0
  if (k==1) u1<-as.matrix(u1) else u1<-0
  if (k==1) u1<-as.numeric(u1) else u1<-0
  if (k==1) W1<-as.cl_partition(u1) else W1<-0
  if (k==1) x1<-cl_agreement(A1,W1,method="NMI") else x1<-0
  if (k==1) u2<-u[(a+1):(a+b)] else u2<-0
  if (k==1) u2<-as.matrix(u2) else u2<-0
  if (k==1) u2<-as.numeric(u2) else u2<-0
  if (k==1) W2<-as.cl_partition(u2) else W2<-0
  if (k==1) x2<-cl_agreement(A2,W2,method="NMI") else x2<-0
  C<-A%*%t(A)
  for(i in 1:dim(A)[1]){C[i,i]<-0}
  D<-t(A)%*%A
  for(i in 1:dim(A)[2]){D[i,i]<-0}
  if (k==1) y1<-walktrap.community(graph.adjacency(C,weighted=TRUE))$membership else y1<-0
  if (k==1) y1<-as.matrix(y1) else y1<-0
  if (k==1) y1<-as.numeric(y1) else y1<-0
  if (k==1) Y1<-as.cl_partition(y1) else Y1<-0
  if (k==1) Y1<-cl_agreement(A1,Y1,method="NMI")  else Y1<-
  if (k==1) y2<-walktrap.community(graph.adjacency(D,weighted=TRUE))$membership  else y2<-0
  if (k==1) y2<-as.matrix(y2) else y2<-0
  if (k==1) y2<-as.numeric(y2) else y2<-0
  if (k==1) Y2<-as.cl_partition(y2) else Y2<-0
  if (k==1) Y2<-cl_agreement(A2,Y2,method="NMI") else Y2<-0
  # x1 is the NMI for walktrap on the first mode
  # x2 is the NMI for walktrap on the second mode
  #
  # Y1 is the NMI for walktrap on the first mode (dual projection)
  # Y2 is the NMI for walktrap on the second mode (dual projection)
  NMIs<-c(x1,x2,Y1,Y2,k)
  NMIs<-t(as.matrix(NMIs))
  names<-c('BiPart_Walk_First','Bipart_Walk_Second','DP_Walk_First','DP_Walk_Second','AllGraphsConnected')
  colnames(NMIs)<-names
  return(NMIs)}


Loop<-function(nobs,a,b,p){
  q<-matrix(0,nr=nobs,nc=5)
  for(i in 1:nobs){q[i,]<-NMIs(a,b,p)}
  names<-c('BiPart_Walk_First','Bipart_Walk_Second','DP_Walk_First','DP_Walk_Second','AllGraphsConnected')
  colnames(q)<-names
  return(q)
}

set.seed(1982)
e<-Loop(105,600,1200,.9)
e<-e[-37,]
# e is the results for the large network.
e<-e[1:100,]






