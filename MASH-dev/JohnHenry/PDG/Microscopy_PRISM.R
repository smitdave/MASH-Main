PRISM <- read.csv('PRISM_Data.csv',header=TRUE)

## Parasite Density Detected
PD = PRISM$parsdens
## Detection by Microscopy (Binary)
BinMicro = PRISM$microDICH

which(!is.na(PD[1:100]))
## 10 individuals have some recorded PD in first 100 records
which(BinMicro[1:100]=='Positive')
## 2 individuals have positively detected parasitemia by microscopy
## Therefore, this can give some function positivity by PD!