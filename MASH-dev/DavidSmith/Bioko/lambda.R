data<-data.frame(cbind(deir=eirarea, X=PfPR, H=H, lambda=lambda))
write.csv(data, "sample.csv")

datlam<-data.frame(cbind(id=areadat$id, lambda=lambda, l=lambda*H/(1-p)))



#PARAMETERS:
f=1/3
Q=0.95
p=0.90
c=0.1
n=12
eir=eirarea/365
PfPR=areadat$PfPR/100
H=areadat$H

lambda=(eir*(1-p)*H)/(f*Q*c*PfPR)


##MAP FIGURES

arealamb = merge(areasf, datlam, by="id", all.x=TRUE)
final.plot<-arealamb[order(arealamb$order), ] 

b = ggplot(data = final.plot, aes(x=long, y=lat, group = group))
c = b + geom_polygon(data = bioko, aes(x = long, y = lat, group = group), color = "black", size = 0.25)

plot<- c + geom_polygon(data = final.plot, aes(x = long, y = lat, group = group, fill = lambda), color = NA, size = 0.25) +
scale_fill_gradient(name="lambda", limits=c(min(final.plot$lambda),max(final.plot$lambda)), low="lightyellow", high="red") +
#scale_fill_distiller(name="API", palette = "RdOrYl", trans="reverse", breaks = pretty_breaks(n = 5)) +
labs(title="estimated lambda values")

pdf("lambda_areas.pdf")
print(plot)
dev.off()


plot<- c + geom_polygon(data = final.plot, aes(x = long, y = lat, group = group, fill = l), color = NA, size = 0.25) +
scale_fill_gradient(name="lambda*H/(1-p)", limits=c(min(final.plot$l),max(final.plot$l)), low="lightyellow", high="red") +
#scale_fill_distiller(name="API", palette = "RdOrYl", trans="reverse", breaks = pretty_breaks(n = 5)) +
labs(title="lambda*H/(1-p)")

pdf("lambda2_areas.pdf")
print(plot)
dev.off()


#eir = (f^2*Q^2*(lambda/(1-p)^2)/H)*p^n*c*PfPR/(1 + (f*Q/(1-p))*c*PfPR)
#eir_simple=(lambda/(1-p)/H)*f*Q*c*PfPR

#lambda=3.596383e-07
#lambda=26.74401

(a*c*PfPR/(1-p))/((1 + a*c*PfPR/(1-p))*p^n)

l=(eir*H*p^n*c*PfPR*(1-p)^2)/f^2*Q^2*(((1-p)*c*PfPR)/(1+f*Q))

dem=f^2*Q^2*((1-p)*c*PfPR/(1+f*Q))

num=eir*H*p^n*c*PfPR*(1-p)^2

num2=f^2*Q^2*lambda/(1-p)^2

dem2=H*p^n*c*PfPR/(1 + f*Q/(1-p)*c*PfPR)

a=f*Q
m=((lambda/(1-p))/H)
z=((a*c*PfPR)/(1-p))/((1+a*c*PfPR)/((1-p)*p^n))
S=f*Q/(1-p)