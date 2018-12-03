# # MACRO Calibration for Bioko Island  ----
# # Set up single-patch simulation
# # Make sure that we can calibrate to PR, that our mathematics were correct
# ####
#
# # Initialize libraries and directories ----
# rm(list=ls());gc()
# library(data.table)
# library(ggplot2)
# library(MASS)
# library(MASHmacro)
# set.seed(0)
# setwd("/Users/dtcitron/Documents/MASH/Bioko_Macro/TAG_2018")
# # Source analysis functions
# source("/Users/dtcitron/Documents/MASH/MACRO-multipatch-test/Multipatch_data_transform.R")
#
#
# ####
# # Read in data ----
# travel.model.data <- fread("Bioko_EIR_Surfaces/EIR_surfaces.csv")
# # TaR matrix 1
# TaR.1 <- read.csv(file = "/Users/dtcitron/Documents/MASH/Bioko_Macro/TAG_2018/Bioko_EIR_Surfaces/TAR1.csv")
# TaR.1 <- as.matrix(TaR.1[, 2:202])
# # Fitting to reservoir data, 201-row data.table that includes pfpr, pop, h.1, h.2...
# reservoir.data <- fread(file = "/Users/dtcitron/Documents/MASH/Bioko_Macro/TAG_2018/Bioko_EIR_Surfaces/pfpr_input.csv")
# #  parameters
# a = 0.3*0.9
# b = 0.55
# c = 0.15
# r = 1./200 # rate at which people become cured
# eta = 1./30 # rate at which prophylaxis wears off
# p = 0.9 # fraction of surviving mosquitoes
# g = 1 - p # fraction of dying mosquitoes
# peip = p^11 # fraction of mosquitoes who survive incubation period
# fever.pf = 0.1116336
# treat.pf = 0.6025641
# rho = fever.pf*treat.pf
#
# ####
# # Pick an area to simulate ----
# ####
# areaIds = sort(travel.model.data$areaId)
# patch_areaId = 273#398#339#735 # 2083 #152 #2087#735 #582 #2324#
# # output directory
# directory = paste0("BI_Macro_Calibration/patch_",patch_areaId,"_nomain/")
#
# ####
# # Some mathematical checks ahead of time ####
# ####
# #odds.vector <- r/(1-rho)*pfpr.input/(1-(1+rho*r/eta/(1-rho))*pfpr.input)
# #hist(abs(TaR.1 %*% reservoir.data$h.1 - odds.vector))
# #hist(abs(TaR.2 %*% reservoir.data$h.2 - odds.vector))
#
#
# ####
# # Define PfSI model parameters ----
# ####
# PfSI.Setup(
#   DurationPf = 200, mnChemoprophylaxisPf = 30, # setting duration of infectious period and Prophylaxis period
#   FeverPf = fever.pf, TreatPf = treat.pf, # setting fever/treatment parameters
#   PEProtectPf = 0, peBlockPf = 1., # probability that it will work, and decreased efficacy
#   mnPEPf = 270, vrPEPf = 1 # setting PE vaccination timeparameters
# )
# SimBitePfSI.Setup()
#
# ####
# # Set up Human Population
# ####
# patch_ix = which(areaIds == patch_areaId)
# patch.human.populations <- c(reservoir.data$pop.input[patch_ix], rep(0,7))
# MACRO.Human.Setup(pathogen = "PfSI", tripFrequency = 0, tripDuration = 1) # define tripFreq and tripDur below
#
#
# ####
# # Define Patch parameters ----
# # Number of patches
# n = 1 + 7 # four areas, with 7 regions for travel
# # Aquatic ecology parameters
# patch.lambda <- c(travel.model.data[areaId==patch_areaId]$lambda.1, rep(0,7)) ### Kluge for now - not sure where the discrepancy comes from...
# aquaPar = AquaPop_Emerge.Parameters(nPatch = n,lambda = patch.lambda, seasonality = FALSE)
# # Create the movement matrix ####
# # Needs to have zero diagonals, and all rows normalized
# moveMat <- diag(n)
# moveMat[1,] <- with(travel.model.data[areaId==patch_areaId], c(0, p.off, p.ban, p.lub, p.mal, p.mok, p.ria, p.ure))
# #rowSums(moveMat)
# # Patch Parameters ####
# patchPar = lapply(X = 1:n,FUN = function(i){
#   list(
#     bWeightZoo = 0,
#     bWeightZootox = 0,
#     travelWeight = moveMat[i,],
#     reservoir = FALSE,
#     resEIR = NULL
#   )
# })
# # Designate the last 7 patches as reservoirs ####
# eir.1 <- reservoir.data$h.1/b
# eir <- eir.1[c(patch_ix,195:201)]
# # And here's where the magic happens: we set mainland EIR -> EIR/2
# eir[2] <- eir[2]/10
# for(i in 2:n){
#   patchPar[[i]]$reservoir <- TRUE
#   patchPar[[i]]$resEIR <- eir[i]
# }
#
# # PfPR ----
# pfpr = reservoir.data$pfpr.input[c(patch_ix,195:201)]
#
#
# ####
# # Define Mosquito parameters ----
# # (Some optional math checks first)
# #EIR = h/b = maz
# #travel.model.data$m.1*a*travel.model.data$z.1 - (reservoir.data$h.1/b)[1:194]
# #travel.model.data$m.1*a*travel.model.data$z.1 - eir.1[1:194]
# #Z = z*M = z*lambda*p/(1-p) = EIR*H/a
# #eir.1[1:194]/a*reservoir.data$pop.input[1:194] - travel.model.data$z.1*travel.model.data$lambda.1*p/(1-p)
#
# Z.1 = eir.1/a*reservoir.data$pop.input/diag(TaR.1)
# Z <- c(Z.1[patch_ix], rep(0, 7) )
# psi = diag(n)
# mosquitoPar = list(model="RM", M=patch.lambda*p/(1-p),EIP = rep(11,365),
#                    Y=Z/peip, Z=Z,
#                    p=0.9, f=0.3, Q=0.9, v=20, psi = psi)
#
# ####
# # Define Human Parameters ----
# n_humans = sum(patch.human.populations)
# patch_id = rep(x = 1:n,patch.human.populations)
# home_id = rep(x = 1:n,patch.human.populations)
# human_ages = unlist(lapply(X = patch.human.populations[1],FUN = siteAges_HumanPop))
# # set biting weights
# human_bWeight = rep(1, n_humans)
# # Human Parameters
# humanPar = lapply(X = 1:n_humans,function(i){
#   list(
#     houseID = home_id[i],
#     patchID = patch_id[i],
#     homeHouseID = home_id[i],
#     homePatchID = patch_id[i],
#     age = human_ages[i],
#     bWeight = human_bWeight[i],
#     tripDuration = c(1,10,3,3,3,3,3,3),# this is if we use TaR matrix 1
#     tripFrequency = travel.model.data$freq.model.fit[patch_ix]
#   )
# })
#
#
# ####
# # Set up schedule of vaccinations ----
# humanIDs = as.character(1:n_humans)
# vaxPar = vector(mode = "list",length = n_humans)
# names(vaxPar) = humanIDs
# # Vaccinate all of the people in the first 2 clusters after 2 months, and also treat them
# for (i in 1:n_humans){
#   vaxPar[[i]] = list(tVax = c(365), tTreat = c(NA))
# }
#
#
# print(patch_areaId)
# print(patch.human.populations)
# print(TaR.1[patch_ix, c(patch_ix, 195:201)])
# print(patch.lambda)
# print(eir)
#
# pfpr[1]*n_humans
#
# ####
# # Create a tile! ----
# set.seed(1)
# tile = MacroTile$new(nPatch = n,
#                      AquaPar = aquaPar,
#                      PatchPar = patchPar,
#                      MosquitoPar = mosquitoPar,
#                      HumanPar = humanPar,
#                      directory = directory,
#                      runID = 1)
#
#
# ####
# # Run a single simulation ----
# tile$simMacro(tMax = 10*365, PfPAR = pfpr, PEVAXPAR = vaxPar)
#
#
#
# human.pathogen.path <- paste0(directory, "/HumanPathogen_Run1.csv")
# human.move.path <- paste0(directory, "/HumanMove_Run1.csv")
# t4 <- SIP.Conversion.Curves(human.pathogen.path, human.move.path, patch.human.populations,  10*365)
# ggplot(data = t4) +
#   geom_line(mapping = aes(x = time, y = N, color = status)) +
#   facet_wrap(~location, ncol = 3, labeller = label_parsed) +
#   scale_color_manual(name = "Status",
#                      values = c("#ff0000", "#000cff", "#00ff1d"),
#                      breaks = c("I", "S", "P"),
#                      labels = c("Infected (PR)", "Susceptible", "Protected")) +
#   xlim(0, 10*365) + ylim(0,300) +
#   xlab("Time") + ylab("N")
#
# #View(t4)
# # Rough check on occupancies
# holder <- dcast(t4[time > 200, sum(N), by = c("time", "location")], time ~ location)
# holder[is.na(holder)] <- 0
#
# c(mean(holder$'1')/n_humans, mean(holder$'2')/n_humans, mean(holder$'3')/n_humans, mean(holder$'4')/n_humans,
#   mean(holder$'5')/n_humans, mean(holder$'6')/n_humans, mean(holder$'7')/n_humans, mean(holder$'8')/n_humans)
#
# TaR.1[patch_ix,c(patch_ix, 195:201)]
#
# # Rough check on total pfpr - not too far away, well within margin of error
# mean(t4[time > 2000 & status == "I", sum(N), by = "time"]$V1)
# sd(t4[time > 2000 & status == "I", sum(N), by = "time"]$V1)
# pfpr[1]*n_humans
#
# pr.actual <- mean(t4[status == "I" & time > 200][, sum(N), by = c("time")]$V1)/n_humans
# pr.actual
#
# psi.h <- TaR.1[patch_ix, c(patch_ix, c(195:201))] %*% h.main#[c(patch_ix, c(195:201))]
# psi.h/(r/(1-rho) + (1+rho*r/eta/(1-rho))*psi.h)
# pfpr[1]
#
# # and looking at total FOI - pretty close
# r/(1-rho)*pr.actual/(1-(1+rho*r/eta/(1-rho))*pr.actual)
# TaR.1[patch_ix, c(patch_ix, c(195:201))] %*% h.main#[c(patch_ix, c(195:201))]
#
# ##
#
# # Making a histogram of overall change:
#
# h.adj <- reservoir.data$h.1.V1
# h.adj[195] <- h.adj[195]/10# 2
# psi.h.adj <- TaR.1 %*% h.adj
#
# hist((psi.h/(r/(1-rho) + (1+rho*r/eta/(1-rho))*psi.h))[1:194])
# hist((psi.h.adj/(r/(1-rho) + (1+rho*r/eta/(1-rho))*psi.h.adj))[1:194])
#
# hist((psi.h.adj/(r/(1-rho) + (1+rho*r/eta/(1-rho))*psi.h.adj) - reservoir.data$pfpr.input )[1:194])
# hist(100*((psi.h.adj/(r/(1-rho) + (1+rho*r/eta/(1-rho))*psi.h.adj) - reservoir.data$pfpr.input)/reservoir.data$pfpr.input )[1:194],
#      main = "% change in PR from Mainland EIR->EIR/10")
#
# # Note also that h will decrease on the island because there will be fewer
# # secondary infections transferred on the island if there are fewer importations
# hist(reservoir.data$pfpr.input[1:194])
# hist((psi.h/(r/(1-rho) + (1-(1+rho*r/eta/(1-rho))*psi.h)) - reservoir.data$pfpr.input)[1:194])
#
#
# # Making a map:
# travel.model.data$main10 <- 100*((psi.h.adj/(r/(1-rho) + (1+rho*r/eta/(1-rho))*psi.h.adj) - reservoir.data$pfpr.input)/reservoir.data$pfpr.input )[1:194]
# area.data = merge(areasf, travel.model.data, by.x = "id", by.y = "areaId", all=TRUE)
# plot.data<-area.data[order(area.data$order), ]
# p1 = ggplot(data = plot.data, aes(x=long, y=lat.x, group = group))
# p2 = p1 + geom_polygon(data = bioko, aes(x = long, y = lat, group = group), color = "black", fill="grey", size = 0.25)
# map <- p2 + geom_polygon(data = plot.data, aes(x = long, y = lat, group = group, fill = main10), color = NA, size = 0.25) +
#  scale_fill_gradient(name="% change PR", low="yellow", high="red", limits=c(-100,0)) +
#  geom_polygon(data = bioko, aes(x = long, y = lat, group = group), color = "black", fill=NA, size = 0.25) +
#  theme(axis.line=element_blank(),axis.text.x=element_blank(), axis.text.y=element_blank(),axis.ticks=element_blank(),
#        axis.title.x=element_blank(),
#        axis.title.y=element_blank(), panel.background=element_blank(), legend.position=c(0.2, 0.8))
# map
#
#
# travel.model.data$point <- 0
# travel.model.data[areaId == 336]$point <- 1 # Malabo
# travel.model.data[areaId == 1028]$point <- 1
# travel.model.data[areaId == 2694]$point <- 1 # Riaba
#
#
# area.data = merge(areasf, travel.model.data, by.x = "id", by.y = "areaId", all=TRUE)
# plot.data<-area.data[order(area.data$order), ]
# p1 = ggplot(data = plot.data, aes(x=long, y=lat.x, group = group))
# p2 = p1 + geom_polygon(data = bioko, aes(x = long, y = lat, group = group), color = "black", fill="grey", size = 0.25)
# map <- p2 + geom_polygon(data = plot.data, aes(x = long, y = lat, group = group, fill = point), color = NA, size = 0.25) +
#   scale_fill_gradient(name="% change PR", low="yellow", high="red", limits=c(0,1)) +
#   geom_polygon(data = bioko, aes(x = long, y = lat, group = group), color = "black", fill=NA, size = 0.25) +
#   theme(axis.line=element_blank(),axis.text.x=element_blank(), axis.text.y=element_blank(),axis.ticks=element_blank(),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank(), panel.background=element_blank(), legend.position=c(0.2, 0.8))
# map
