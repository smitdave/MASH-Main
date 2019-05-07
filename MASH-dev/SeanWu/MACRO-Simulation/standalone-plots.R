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




# fig4a <- ggplot(data = simout_t) +
#   geom_line(aes(x=time,y=h_hat,group=iter),color="darkred",alpha=0.15) +
#   geom_line(aes(x=time,y=h_tilde,group=iter),color="firebrick3",alpha=0.15) +
#   scale_x_continuous(breaks = (0:3)*26,labels = 0:3,name = "Time (Years)") +
#   ylab("daily FOI (simulated)") +
#   ggtitle("b) PfSI: Tororo") +
#   theme_bw()
#
# fig4b <- ggplot(data = simout_k) +
#   geom_line(aes(x=time,y=h_hat,group=iter),color="darkblue",alpha=0.15) +
#   geom_line(aes(x=time,y=h_tilde,group=iter),color="steelblue",alpha=0.15) +
#   scale_x_continuous(breaks = (0:3)*26,labels = 0:3,name = "Time (Years)") +
#   ylab("daily FOI (simulated)") +
#   ggtitle("b) PfSI: Kanungu") +
#   theme_bw()
#
# fig4c <- ggplot(data = simout_j) +
#   geom_line(aes(x=time,y=h_hat,group=iter),color="darkgreen",alpha=0.15) +
#   geom_line(aes(x=time,y=h_tilde,group=iter),color="forestgreen",alpha=0.15) +
#   scale_x_continuous(breaks = (0:3)*26,labels = 0:3,name = "Time (Years)") +
#   ylab("daily FOI (simulated)") +
#   ggtitle("c) PfSI: Jinja") +
#   theme_bw()
#
# # transmission efficiency (4D: FOI normalized by N
# fig4d <- ggplot(data = simout) +
#   geom_line(aes(x=time,y=aeff,color=site,group=interaction(iter,site)),alpha=0.15) +
#   scale_color_manual(values = c(Tororo="darkred",Kanungu="darkblue",Jinja="darkgreen")) +
#   # scale_color_manual(values = c(Tororo="firebrick3",Kanungu="steelblue",Jinja="darkorchid3")) +
#   # guides(colour = guide_legend(override.aes = list(alpha = 1,size = 2))) +
#   guides(colour = FALSE) +
#   scale_y_log10(breaks = c(2.2, 4.4, 9.5),labels = c("1.7:1","2.7:1","7.4:1")) +
#   scale_x_continuous(breaks = (0:3)*26,labels = as.character(0:3)) +
#   ylab("Transmission Efficiency") +
#   xlab("Time (Years)") +
#   ggtitle("d)") +
#   theme_bw()
#
# fig4 <- grid.arrange(fig4a,fig4b,fig4c,fig4d,nrow=2,ncol=2)
#
# ggsave(filename = here::here("figures/pfsi_4.pdf"),plot = fig4,device = "pdf",width = 12,height = 10)