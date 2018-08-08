
BFAB_PAR = function(
  A=  .98,
  B0= 0.00, B1= 0.90,  B2= 0.10, #B3 = 1-B0-B1-B2
  C1= 0.01, C2= 0.88,            #C3 = 1-C1-D2
  C4= 0.01, C5= 0.85,            #C6 = 1-C4-C5
  C7= 0.50,                      #C8 = 1-C7
  D1= 0.015, D2= 0.86,            #D3 = 1-D1-D2
  E = 0.99,
  F1= 0.99, F2= 0.9,
  G = 0.20,
  H1= 0.80, H2= 0.80, H3= 0.25
){
list(A=A, B0=B0, B1=B1, B2=B2, B3=1-B0-B1-B2,
           C1=C1, C2=C2, C3=1-C1-C2, C4=C4,
           C5=C5, C6=1-C4-C5, C7=C7, C8=1-C7,
           D1=D1, D2=D2, D3=1-D1-D2, E=E,
           F1=F1, F2=F2, G=G, H1=H1, H2=H2, H3=H3)
}


# changed
BFAB_B2Y <- function(PAR){with(PAR,{

  Fail <- (1-A) + A*(B0 + B1*(C3 + C2*D3) + B2*C6 + B3*C8)

  B2F <- Fail*F2*(1-H3)
  B2B <- Fail*F2*H3
  B2R <- A*(B1*C2*D2 + B2*C5)*E
  B2D <- 1-B2R-B2F-B2B

  return(c(B2F, B2B, B2R, 0, 0, B2D))
})}

BFAB_B2Y(BFAB_PAR())

# changed
BFAB_R2Y <- function(PAR){with(PAR,{
  R2F = F1*G*(1-H1)
  R2B = F1*G*H1
  R2O = F1*(1-G)*H2
  R2L = F1*(1-G)*(1-H2)
  R2D = 1-R2B-R2O-R2L-R2F

  R2ALL = c(R2F,R2B,0,R2L,R2O,R2D)
  return(R2ALL)
})}


ELAB_PAR = function(
  A=  0.75,
  B0= 0.0,  B1= 1.00, #B2=1-B0-B1
  C1= 0.00, C2= 1, #C3=1-C1-C2
  C4= 0.01,           #C5=1-C4
  D1= 0.94,
  D2= 0.9,
  E = 0,
  F1= 0.53,
  F2= 0.4,
  F3=0.25
){
  list(A=A, B0=B0, B1=B1, B2=1-B0-B1,
       C1=C1, C2=C2, C3=1-C1-C2,
       C4=C4, C5=1-C4,
       D1=D1, D2=D2, E=E,
       F1=F1, F2=F2, F3=F3)
}

# changed
ELAB_O2Y <- function(PAR){with(PAR,{

  Fail = (1-A) + (A*B0) + (A*(1-B0)*B1*C3) + (A*(1-B0)*B2*C5)

  O2L <- (Fail*D2*(1-F3)) + (A*(1-B0)*B1*C2*D1*E*(1-F1))
  O2O <- (Fail*D2*F3) + (A*(1-B0)*B1*C2*D1*E*F1)
  O2F <- (A*(1-B0)*B1*C2*D1*(1-E)*(1-F2))
  O2B <- (A*(1-B0)*B1*C2*D1*(1-E)*F2)
  O2D <- 1-O2L-O2O-O2F-O2B
  return(c(O2F,O2B,0,O2L,O2O,O2D))
})}

ELAB_O2Y((ELAB_PAR()))

BFSB_PAR = function(A=.94,
                    B=0.893617){
  list(A=A,B=B)
}

# changed
BFSB_F2Y <- function(PAR){with(PAR,{
  F2F <- (A*D1*F1) + ((1-A)*D2)
  F2B <- A*D1*(1-F1)
  F2D <- 1-(F2F+F2B)

  return(c(F2F, F2B, 0, 0, 0, F2D))
})}

BFSB_F2Y(BFSB_PAR(A=.94, B=.893617))

ELSB_PAR = function(A=.94, B=.893617){
  list(A=A,B=B)
}

# changed
ELSB_L2Y <- function(PAR){with(PAR,{
  L2L <- (A*D1*F1) + ((1-A)*D2) # oviposition search
  L2O <- A*D1*(1-F1) # oviposition here
  L2D <- (A*(1-D1)) + ((1-A)*(1-D2)) # death

  return(c(0,0,0,L2L,L2O,L2D))
})}

Y2Y = rbind(
BFSB_F2Y(BFSB_PAR()),
BFAB_B2Y(BFAB_PAR()),
BFAB_R2Y(BFAB_PAR()),
ELAB_O2Y(ELAB_PAR()),
ELSB_L2Y(ELSB_PAR()),
c(rep(0, 5), 1)
)
rownames(Y2Y) = c("F","B","R","L","O", "D")
colnames(Y2Y) = c("F","B","R","L","O", "D")
round(1000*Y2Y)/1000
