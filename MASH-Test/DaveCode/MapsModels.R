
Transmission: 
Mosquito Habitat / Environmental Covariates
Mosquito Populations / EIR, HBR  / L_i, Rc
Human Population / H
Travel Model / CDR, questionnaires / psi
Transmission Model / dXdt
Human Malaria / PR, serology
Health Seeking / Clinical Cases 

We have data here, and we want to use it to design 
a set of interventions that works everywhere. 

Currently, we do this:
data + covariates -> PR map -> model -> Rc

...but really, we want to capture the uncertainty so 
we want to do this: 

IF:   
covariates -> Rc
Rc + psi + dXdt -> data 
THEN: 
data + covariates + psi -> Rc


