MASHHOME = "/Users/smitdave/GitHub/Mash-Development/"
MASHHOME = "/Users/slwu89/Desktop/MASH-Development/"
LOME = paste(MASHHOME,"PATHOGEN/Plasmodium.falciparum/PfLOME/",sep="")
LOME = paste0(MASHHOME,"MAIN/Rdev/PfLOME/")
setwd(LOME) 

source(paste(LOME,"PfLOME.R", sep=""))
source(paste(LOME,"setup.PfLOME.R", sep=""))
source(paste(LOME,"PfLOME_ImmuneCounters.R", sep=""))
source(paste(LOME,"PfLOME_Infections.R", sep=""))
source(paste(LOME,"PfLOME_Objects.R", sep=""))
source(paste(LOME,"PfLOME_visualize.R", sep=""))
source(paste(LOME,"PfLOME_History.R", sep="")) 
source(paste(LOME,"PfLOME_History.R", sep=""))

# Make some humans
source (paste(MASHHOME, "HUMAN/Human.R", sep=""))
source(paste0(MASHHOME,"MAIN/HUMANS/Humans.R"))
HUMANS = list() 
for(i in 1:20){ 
  # HUMANS[[i]] = nullHuman() 
  HUMANS[[i]] = makeHuman(id = i,hhIx = i,w = rgamma(1,shape = 1,rate = 1))
  HUMANS$Pf = pathObject_PfLOME() 
} 
source (paste(LOME, "setup.PfLOME.R", sep=""))

MosqPf = function(
  tm=0, xm=0, ym=0,   
  ixH2M   = 9,           
  NOok  = 1,           
  dam  = 1,           
  sire  = 3            
)list(tm=tm,xm=xm,ym=ym,ixH2m=ixH2M,NOok=NOok,dam=dam,sire=sire) 

PfID = 1 

probeHost_PfLOME(1, 1, 20, 1, 2, MosqPf(dam=1,sire=2))
probeHost_PfLOME(1, 1, 120, 3, 4, MosqPf(dam=3,sire=4)) 

   
