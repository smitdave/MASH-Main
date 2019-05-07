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

plot_pfmoi_het_ineff <- ggplot(data = pfmoi_het_ineff[is.finite(pfmoi_het_ineff$eff) & is.finite(pfmoi_het_ineff$aEIR),]) +
  geom_jitter(aes(x=aEIR,y=eff,color=site),alpha=0.02,width = 0.15, height = 0.15) +
  geom_point(aes(x=eir,y=aeff,color=site),alpha=0.5,data=data.plot,shape=17,size=2.5) +
  scale_y_continuous(trans = scales::log1p_trans(),breaks = c(1/2, 2, 10, 50, 1e2, 1e3),labels = c("1:2","2:1","10:1","50:1","100:1","1000:1"),limits = ylims) +
  scale_x_continuous(trans = scales::log1p_trans(),breaks = 10^(0:3), limits = xlims) +
  scale_color_manual(values = c(Tororo="darkred",Kanungu="darkblue",Jinja="darkgreen")) +
  guides(colour = FALSE) +
  ylab("Inefficiency (aEIR : aFOI)") +
  xlab("Annual EIR") +
  ggtitle("PfMOI (heterogeneous biting)") +
  theme_bw()

plot_pfsi_het_ineff <- ggplot(data = pfsi_het_ineff[is.finite(pfsi_het_ineff$eff) & is.finite(pfsi_het_ineff$aEIR),]) +
  geom_jitter(aes(x=aEIR,y=eff,color=site),alpha=0.02,width = 0.15, height = 0.15) +
  geom_point(aes(x=eir,y=aeff,color=site),alpha=0.5,data=data.plot,shape=17,size=2.5) +
  scale_y_continuous(trans = scales::log1p_trans(),breaks = c(1/2, 2, 10, 50, 1e2, 1e3),labels = c("1:2","2:1","10:1","50:1","100:1","1000:1"),limits = ylims) +
  scale_x_continuous(trans = scales::log1p_trans(),breaks = 10^(0:3),limits = xlims) +
  scale_color_manual(values = c(Tororo="darkred",Kanungu="darkblue",Jinja="darkgreen")) +
  guides(colour = FALSE) +
  ylab("Inefficiency (aEIR : aFOI)") +
  xlab("Annual EIR") +
  ggtitle("PfSI (heterogeneous biting)") +
  theme_bw()

plot_pfmoi_hom_ineff <- ggplot(data = pfmoi_hom_ineff[is.finite(pfmoi_hom_ineff$eff) & is.finite(pfmoi_hom_ineff$aEIR),]) +
  geom_jitter(aes(x=aEIR,y=eff,color=site),alpha=0.02,width = 0.15, height = 0.15) +
  geom_point(aes(x=eir,y=aeff,color=site),alpha=0.5,data=data.plot,shape=17,size=2.5) +
  scale_y_continuous(trans = scales::log1p_trans(),breaks = c(1/2, 2, 10, 50, 1e2, 1e3),labels = c("1:2","2:1","10:1","50:1","100:1","1000:1"),limits = ylims) +
  scale_x_continuous(trans = scales::log1p_trans(),breaks = 10^(0:3),limits = xlims) +
  scale_color_manual(values = c(Tororo="darkred",Kanungu="darkblue",Jinja="darkgreen")) +
  guides(colour = FALSE) +
  ylab("Inefficiency (aEIR : aFOI)") +
  xlab("Annual EIR") +
  ggtitle("PfMOI (homogeneous biting)") +
  theme_bw()

plot_pfsi_hom_ineff <- ggplot(data = pfsi_hom_ineff[is.finite(pfsi_hom_ineff$eff) & is.finite(pfsi_hom_ineff$aEIR),]) +
  geom_jitter(aes(x=aEIR,y=eff,color=site),alpha=0.02,width = 0.15, height = 0.15) +
  geom_point(aes(x=eir,y=aeff,color=site),alpha=0.5,data=data.plot,shape=17,size=2.5) +
  scale_y_continuous(trans = scales::log1p_trans(),breaks = c(1/2, 2, 10, 50, 1e2, 1e3),labels = c("1:2","2:1","10:1","50:1","100:1","1000:1"),limits = ylims) +
  scale_x_continuous(trans = scales::log1p_trans(),breaks = 10^(0:3),limits = xlims) +
  scale_color_manual(values = c(Tororo="darkred",Kanungu="darkblue",Jinja="darkgreen")) +
  guides(colour = FALSE) +
  ylab("Inefficiency (aEIR : aFOI)") +
  xlab("Annual EIR") +
  ggtitle("PfSI (homogeneous biting)") +
  theme_bw()

plot_ineff <- grid.arrange(plot_pfmoi_het_ineff,plot_pfsi_het_ineff,plot_pfmoi_hom_ineff,plot_pfsi_hom_ineff,ncol=2,nrow=2)

ggsave(filename = here::here("figures/ineff_2d.pdf"),plot = plot_ineff,device = "pdf",width = 14,height = 12)


################################################################################
#   transmission efficiency vs. time -- can we estimate FOI from AR? (Fig 4)
################################################################################

pfmoi_hom_ts <- readRDS(file = here::here("sim/PfMOI_hom.rds"))
pfmoi_het_ts <- readRDS(file = here::here("sim/PfMOI_het.rds"))

pfsi_hom_ts <- readRDS(file = here::here("sim/PfSI_hom.rds"))
pfsi_het_ts <- readRDS(file = here::here("sim/PfSI_het.rds"))

# pfmoi
plot_pfmoi_EIR <- ggplot(data = pfmoi_het_ts) +
  geom_line(aes(x=time,y=EIR,color=site,group=interaction(site,iter)),alpha=0.05) +
  scale_x_continuous(breaks = (0:3)*26,labels = 0:3,name = "Time (Years)") +
  scale_y_continuous(trans = scales::log1p_trans()) +
  scale_color_manual(values = c(Tororo="darkred",Kanungu="darkblue",Jinja="darkgreen")) +
  guides(color = FALSE) +
  theme_bw()

plot_pfmoi_FOI <- ggplot(data = pfmoi_het_ts) +
  geom_line(aes(x=time,y=h_hat,color=site,group=interaction(site,iter)),alpha=0.05) +
  geom_line(aes(x=time,y=h_tilde,color=site,group=interaction(site,iter)),linetype=2,alpha=0.05) +
  scale_x_continuous(breaks = (0:3)*26,labels = 0:3,name = "Time (Years)") +
  scale_y_continuous(trans = scales::log1p_trans()) +
  scale_color_manual(values = c(Tororo="darkred",Kanungu="darkblue",Jinja="darkgreen")) +
  guides(color = FALSE) +
  ylab("FOI") +
  theme_bw()

plot_pfmoi_aeff <- ggplot(data = pfmoi_het_ts) +
  geom_line(aes(x=time,y=aeff,color=site,group=interaction(site,iter)),alpha=0.05) +
  scale_color_manual(values = c(Tororo="darkred",Kanungu="darkblue",Jinja="darkgreen")) +
  scale_y_log10(breaks = c(2.2, 4.4, 9.5),labels = c("1.7:1","2.7:1","7.4:1")) +
  scale_x_continuous(breaks = (0:3)*26,labels = as.character(0:3)) +
  guides(colour = FALSE) +
  ylab("Transmission Efficiency") +
  xlab("Time (Years)") +
  theme_bw()

plot_pfmoi_hom <- ggplot(data = pfmoi_hom_ts) +
  geom_line(aes(x=time,y=aeff,color=site,group=interaction(site,iter)),alpha=0.05) +
  scale_color_manual(values = c(Tororo="darkred",Kanungu="darkblue",Jinja="darkgreen")) +
  # scale_y_log10(breaks = c(2.2, 4.4, 9.5),labels = c("1.7:1","2.7:1","7.4:1")) +
  scale_x_continuous(breaks = (0:3)*26,labels = as.character(0:3)) +
  guides(colour = FALSE) +
  ylab("Transmission Efficiency") +
  xlab("Time (Years)") +
  theme_bw()

plot_pfmoi_ts <- grid.arrange(plot_pfmoi_EIR,plot_pfmoi_FOI,plot_pfmoi_aeff,plot_pfmoi_hom,nrow=4)

ggsave(filename = here::here("figures/ts_pfmoi_4.pdf"),plot = plot_pfmoi_ts,device = "pdf",width = 10,height = 14)

# pfsi
plot_pfsi_EIR <- ggplot(data = pfsi_het_ts) +
  geom_line(aes(x=time,y=EIR,color=site,group=interaction(site,iter)),alpha=0.05) +
  scale_x_continuous(breaks = (0:3)*26,labels = 0:3,name = "Time (Years)") +
  scale_y_continuous(trans = scales::log1p_trans()) +
  scale_color_manual(values = c(Tororo="darkred",Kanungu="darkblue",Jinja="darkgreen")) +
  guides(color = FALSE) +
  theme_bw()

plot_pfsi_FOI <- ggplot(data = pfsi_het_ts) +
  geom_line(aes(x=time,y=h_hat,color=site,group=interaction(site,iter)),alpha=0.05) +
  geom_line(aes(x=time,y=h_tilde,color=site,group=interaction(site,iter)),linetype=2,alpha=0.05) +
  scale_x_continuous(breaks = (0:3)*26,labels = 0:3,name = "Time (Years)") +
  scale_y_continuous(trans = scales::log1p_trans()) +
  scale_color_manual(values = c(Tororo="darkred",Kanungu="darkblue",Jinja="darkgreen")) +
  guides(color = FALSE) +
  ylab("FOI") +
  theme_bw()

plot_pfsi_aeff <- ggplot(data = pfsi_het_ts) +
  geom_line(aes(x=time,y=aeff,color=site,group=interaction(site,iter)),alpha=0.05) +
  scale_color_manual(values = c(Tororo="darkred",Kanungu="darkblue",Jinja="darkgreen")) +
  scale_y_log10(breaks = c(2.2, 4.4, 9.5),labels = c("1.7:1","2.7:1","7.4:1")) +
  scale_x_continuous(breaks = (0:3)*26,labels = as.character(0:3)) +
  guides(colour = FALSE) +
  ylab("Transmission Efficiency") +
  xlab("Time (Years)") +
  theme_bw()

plot_pfsi_hom <- ggplot(data = pfsi_hom_ts) +
  geom_line(aes(x=time,y=aeff,color=site,group=interaction(site,iter)),alpha=0.05) +
  scale_color_manual(values = c(Tororo="darkred",Kanungu="darkblue",Jinja="darkgreen")) +
  # scale_y_log10(breaks = c(2.2, 4.4, 9.5),labels = c("1.7:1","2.7:1","7.4:1")) +
  scale_x_continuous(breaks = (0:3)*26,labels = as.character(0:3)) +
  guides(colour = FALSE) +
  ylab("Transmission Efficiency") +
  xlab("Time (Years)") +
  theme_bw()

plot_pfsi_ts <- grid.arrange(plot_pfsi_EIR,plot_pfsi_FOI,plot_pfsi_aeff,plot_pfsi_hom,nrow=4)

ggsave(filename = here::here("figures/ts_pfsi_4.pdf"),plot = plot_pfsi_ts,device = "pdf",width = 10,height = 14)
