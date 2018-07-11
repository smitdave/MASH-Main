
BFAB_PAR = function(
  A=  1.00, 
  B0= 0.00,
  B1= 0.90,
  B2= 0.10,
  B3= 0.00,
  C1= 0.01,
  C2= 0.85,
  C3= 0.14,
  C4= 0.01,
  C5= 0.85,
  C6= 0.14,
  C7= 0.5,
  C8= 0.5,
  D1= 0.01, 
  D2= 0.90,
  D3= 0.09, 
  E = 0.99,
  F1= 0.99,
  F2= 0.99,
  G = 0.2,
  H1= 0.8,
  H2= 0.8
){
list(A=A, B0=B0, B1=B1, B2=B2, B3=B3,
           C1=C1, C2=C2, C3=C3, C4=C4,
           C5=C5, C6=C6, C7=C7, C8=C8,
           D1=D1, D2=D2, D3=D3, E=E, 
           F1=F1, F2=F2, G=G, H1=H1, H2=H2)
} 

BFAB_B2X = function(PAR){with(PAR,{
  B2R = A*(B1*C2*D2 + B2*C5)*E
  
  Fail = (1-A) + A*(B0 + B1*(C3 + C2*D3) + B2*C6 + B3*C8)
  B2B = Fail*F2*H2
  B2F = Fail*F2*(1-H2)
  
  # additional mass on D from local hazards
  B2D = 1-B2R-B2F-B2B
  
  # normalize
  B2ALL = c(B2F=B2F, B2B=B2B, B2R=B2R, 0, 0, B2D=B2D)
  return(B2ALL)
})}

BFAB_R2X = function(PAR){with(PAR,{
  R2B = F1*G
  R2O = F1*(1-G)*H1
  R2L = F1*(1-G)*(1-H1)
  R2D = 1-R2B-R2O-R2L
  
  R2ALL = c(0,R2B=R2B,0,R2L=R2L,R2O=R2O,R2D=R2D)
  return(R2ALL)
})}


ELAB_PAR = function(
  A=  1.00, 
  B0= 0.00,
  B1= 0.90,
  B2= 0.10,
  C1= 0.01,
  C2= 0.85,
  C3= 0.14,
  C4= 0.01,
  C5= 0.85,
  D1= 0.01, 
  D2= 0.99,
  E = 0.99,
  F1= 0.99,
  F2= 0.99,
  F3=0.99
){
  list(A=A, B0=B0, B1=B1, B2=B2,
       C1=C1, C2=C2, C3=C3, C4=C4, C5=C5,
       D1=D1, D2=D2, E=E, 
       F1=F1, F2=F2, F3=F3) 
}
ELAB_O2X = function(PAR){with(PAR,{
  
  Fail = (1-A) + A*(B0+ B1*C3 + B2*C5)
  
  O2L = Fail*D2*(1-F3) + B1*C2*D1*E*(1-F1)
  O2O = Fail*D2*F3 + B1*C2*D1*E*F1
  O2F = A*B1*C2*D1*(1-E)*(1-F2)
  O2B = A*B1*C2*D1*(1-E)*F2
  O2D = 1-O2L-O2O-O2F-O2B
  O2ALL = c(O2F,O2B,0,O2L,O2O,O2D)
  return(O2ALL)
  
})}



BFAB_B2X(BFAB_PAR())
BFAB_R2X(BFAB_PAR())
ELAB_O2X(ELAB_PAR())

