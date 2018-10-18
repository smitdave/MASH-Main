setwd("/Users/dtcitron/Documents/MASH/Bioko_Macro/Travel_Model_Fitting/Importations - Carlos Paper")

deltahist <- hist(BI$delta.e,
                  main = TeX("Fraction of Infected Returning Travelers $(\\delta)$"), xlab = TeX("$\\delta$"),
                  probability=TRUE)
plot(deltahist,
     main = TeX("Rate of Returning Travelers Per Day $(\\delta)$"), xlab = TeX("$\\delta$"),
     cex.main = 1.3,
     cex.lab = 1.2)
dev.off()



pdf(file = "treatment_LR.pdf")
par(mar=c(5,6,4,1)+.1)
plot(with(BI, h.co.e/(h.co.e + 1/200)),
     with(BI, h.co.r/(h.co.r + 1/100)),
     main = TeX("Local Residual Transmission"),
     xlab = TeX("r = 1/200"),
     ylab = TeX("After Increased Treatement, r = 1/100"),
     ylim = c(0,.4),
     cex.main = 1.5,
     cex.lab = 1.4)
segments(0,0,1,1)
dev.off()

pdf(file = "treatment_TF.pdf")
par(mar=c(5,6,4,1)+.1)
plot(with(BI, tf.co.e),
     with(BI, tf.co.r),
     main = TeX("Travel Fraction"),
     xlab = TeX("r = 1/200"),
     ylab = TeX("After Increased Treatement, r = 1/100"),
     ylim = c(0,1),
     cex.main = 1.5,
     cex.lab = 1.4)
segments(0,0,1,1)
dev.off()

pdf(file = "travel_heterogeneity.pdf")
par(mar=c(5,6,4,1)+.1)
plot(with(BI, delta.e*eta.co.e/(delta.e*eta.co.e + r)),
     with(BI, delta2*eta.co.e/(delta2*eta.co.e + r)*fac),
     main = TeX("Comparing Uniform and Heterogeneous Travel"),
     xlab = TeX("$PR$, Uniform Travel"),
     ylab = TeX("$PR$, Heterogeneous Travel"),
     ylim = c(0,.3),
     xlim = c(0,.3),
     cex.main = 1.5,
     cex.lab = 1.3)
segments(0,0,1,1)
dev.off()