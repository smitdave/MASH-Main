
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

BFAB_B2Y = function(PAR){with(PAR,{
  B2R = A*(B1*C2*D2 + B2*C5)*E

  Fail = (1-A) + A*(B0 + B1*(C3 + C2*D3) + B2*C6 + B3*C8)
  B2B = Fail*F2*H3
  B2F = Fail*F2*(1-H3)

  # additional mass on D from local hazards
  B2D = 1-B2R-B2F-B2B

  # normalize
  B2ALL = c(B2F=B2F, B2B=B2B, B2R=B2R, 0, 0, B2D=B2D)
  return(B2ALL)
})}

BFAB_B2Y(BFAB_PAR())

BFAB_R2Y = function(PAR){with(PAR,{
  R2B = F1*G*H1
  R2F = F1*G*(1-H1)
  R2O = F1*(1-G)*H2
  R2L = F1*(1-G)*(1-H2)
  R2D = 1-R2B-R2O-R2L

  R2ALL = c(R2F=R2F,R2B=R2B,0,R2L=R2L,R2O=R2O,R2D=R2D)
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
ELAB_O2Y = function(PAR){with(PAR,{

  Fail = (1-A) + A*(B0+ B1*C3 + B2*C5)

  O2L = Fail*D2*(1-F3) + A*B1*C2*D1*E*(1-F1)
  O2O = Fail*D2*F3 + A*B1*C2*D1*E*F1
  O2F = A*B1*C2*D1*(1-E)*(1-F2)
  O2B = A*B1*C2*D1*(1-E)*F2
  O2D = 1-O2L-O2O-O2F-O2B
  O2ALL = c(O2F=O2F,O2B=O2B,0,O2L=O2L,O2O=O2O,O2D)
  return(O2ALL)
})}

ELAB_O2Y((ELAB_PAR()))

BFSB_PAR = function(A=.94,
                    B=0.893617){
  list(A=A,B=B)
}

BFSB_F2Y= function(PAR){with(PAR,{
  F2B = A*B
  F2F = A*(1-B)
  F2D = 1-F2B-F2F
  F2ALL = c(F2F=F2F, F2B=F2B, 0,0,0, F2D=F2D)
  return(F2ALL)
})}

BFSB_F2Y(BFSB_PAR(A=.94, B=.893617))

ELSB_PAR = function(A=.94, B=.893617){
  list(A=A,B=B)
}

ELSB_L2Y= function(PAR){with(PAR,{
  L2O = A*B
  L2L = A*(1-B)
  L2D = 1-L2O-L2L
  L2ALL = c(0,0,0,L2L=L2L, L2O=L2O, L2D=L2D)
  return(L2ALL)
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
