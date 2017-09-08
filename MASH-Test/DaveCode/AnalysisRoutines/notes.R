MASHHOME = "/Users/chipdelmal/Documents/School/Research/MASH/"

sapply(MPop$MM, function(x) x$tnext)
names(MPop$MM[[1]])
#ggplot(dat, aes(x=rating)) + geom_histogram(binwidth=.5)
hist(sapply(MPop$MM, function(x) x$tnext))
d <- density(sapply(MPop$MM, function(x) x$tnext))
plot(d)

AquaPOP
names(AquaPOP)
names(AquaPOP$pop)

str(AquaPOP)
names(EggQ$batch[[1]])

names(ImagoQ[[1]])
sapply(ImagoQ, function(x) x$sire)

par(mfrow=c(4,4))
