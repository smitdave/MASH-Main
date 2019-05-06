################################################################################
#       __  ______   __________  ____
#      /  |/  /   | / ____/ __ \/ __ \
#     / /|_/ / /| |/ /   / /_/ / / / /
#    / /  / / ___ / /___/ _, _/ /_/ /
#   /_/  /_/_/  |_\____/_/ |_|\____/
#
#   Sean Wu & Daniel Citron
#   Replication of results from Uganda PRISM Study
#   File to make plots
#   April 2019
#
################################################################################

rm(list=ls());gc()

library(here)

library(reshape2)
library(ggplot2)
library(gridExtra)
library(scales)


################################################################################
#   exposure vs. infection --- inefficiency of transmission (Fig 2D)
################################################################################

pfmoi_hom_ineff <- readRDS(file = here::here("sim/PfMOI_hom_ineff.rds"))
pfmoi_het_ineff <- readRDS(file = here::here("sim/PfMOI_het_ineff.rds"))

pfsi_hom_ineff <- readRDS(file = here::here("sim/PfSI_hom_ineff.rds"))
pfsi_het_ineff <- readRDS(file = here::here("sim/PfSI_het_ineff.rds"))

load(here::here("data/data_plot_dave.Rdata"))
data.plot$eir <- 10^data.plot$log10_eir
data.plot$aeff <- data.plot$eir/data.plot$yearly_rate


ylims <- c(0,1e3)
xlims <- c(0,1.2e3)

pfmoi_het

ggplot(data = pfmoi_het_ineff[is.finite(pfmoi_het_ineff$eff) & is.finite(pfmoi_het_ineff$aEIR),]) +
  geom_jitter(aes(x=aEIR,y=eff,color=site),alpha=0.025,width = 0.15, height = 0.15) +
  geom_point(aes(x=eir,y=aeff,color=site),alpha=0.5,data=data.plot,shape=17,size=2.5) +
  scale_y_continuous(trans = scales::log1p_trans(),breaks = c(1/2, 2, 10, 50, 1e2, 1e3),labels = c("1:2","2:1","10:1","50:1","100:1","1000:1"),limits = ylims) +
  scale_x_continuous(trans = scales::log1p_trans(),breaks = 10^(0:3), limits = xlims) +
  scale_color_manual(values = c(Tororo="darkred",Kanungu="darkblue",Jinja="darkgreen")) +
  guides(colour = FALSE) +
  ylab("Inefficiency (aEIR : aFOI)") +
  xlab("Annual EIR") +
  ggtitle("PfMOI (heterogeneous biting)") +
  theme_bw()

ggplot(data = pfsi_het_ineff[is.finite(pfsi_het_ineff$eff) & is.finite(pfsi_het_ineff$aEIR),]) +
  geom_jitter(aes(x=aEIR,y=eff,color=site),alpha=0.025,width = 0.15, height = 0.15) +
  geom_point(aes(x=eir,y=aeff,color=site),alpha=0.5,data=data.plot,shape=17,size=2.5) +
  scale_y_continuous(trans = scales::log1p_trans(),breaks = c(1/2, 2, 10, 50, 1e2, 1e3),labels = c("1:2","2:1","10:1","50:1","100:1","1000:1"),limits = ylims) +
  scale_x_continuous(trans = scales::log1p_trans(),breaks = 10^(0:3),limits = xlims) +
  scale_color_manual(values = c(Tororo="darkred",Kanungu="darkblue",Jinja="darkgreen")) +
  guides(colour = FALSE) +
  ylab("Inefficiency (aEIR : aFOI)") +
  xlab("Annual EIR") +
  ggtitle("PfSI (heterogeneous biting)") +
  theme_bw()

ggplot(data = pfmoi_hom_ineff[is.finite(pfmoi_hom_ineff$eff) & is.finite(pfmoi_hom_ineff$aEIR),]) +
  geom_jitter(aes(x=aEIR,y=eff,color=site),alpha=0.025,width = 0.15, height = 0.15) +
  geom_point(aes(x=eir,y=aeff,color=site),alpha=0.5,data=data.plot,shape=17,size=2.5) +
  scale_y_continuous(trans = scales::log1p_trans(),breaks = c(1/2, 2, 10, 50, 1e2, 1e3),labels = c("1:2","2:1","10:1","50:1","100:1","1000:1"),limits = ylims) +
  scale_x_continuous(trans = scales::log1p_trans(),breaks = 10^(0:3),limits = xlims) +
  scale_color_manual(values = c(Tororo="darkred",Kanungu="darkblue",Jinja="darkgreen")) +
  guides(colour = FALSE) +
  ylab("Inefficiency (aEIR : aFOI)") +
  xlab("Annual EIR") +
  ggtitle("PfMOI (homogeneous biting)") +
  theme_bw()

ggplot(data = pfsi_hom_ineff[is.finite(pfsi_hom_ineff$eff) & is.finite(pfsi_hom_ineff$aEIR),]) +
  geom_jitter(aes(x=aEIR,y=eff,color=site),alpha=0.025,width = 0.15, height = 0.15) +
  geom_point(aes(x=eir,y=aeff,color=site),alpha=0.5,data=data.plot,shape=17,size=2.5) +
  scale_y_continuous(trans = scales::log1p_trans(),breaks = c(1/2, 2, 10, 50, 1e2, 1e3),labels = c("1:2","2:1","10:1","50:1","100:1","1000:1"),limits = ylims) +
  scale_x_continuous(trans = scales::log1p_trans(),breaks = 10^(0:3),limits = xlims) +
  scale_color_manual(values = c(Tororo="darkred",Kanungu="darkblue",Jinja="darkgreen")) +
  guides(colour = FALSE) +
  ylab("Inefficiency (aEIR : aFOI)") +
  xlab("Annual EIR") +
  ggtitle("PfSI (homogeneous biting)") +
  theme_bw()
