pr2eir = read.csv("PfEIR_to_PfPR (JUN_30).csv")
eir = pr2eir[,1]
pr2eir = as.matrix(pr2eir[,-1])
treat = seq(0,1,by =0.05)

filled.contour(log(eir[4:12]), treat[5:15],  pr2eir[4:12,5:15], plot.axes = {axis(1, log(c(1,4,16,64)), c(1,4,16,64) ) 
  axis(2,treat[5:15])}, main = "PfPR", xlab = "Annual EIR", ylab = "Proportion Treated")

image(pr2eir)
