setwd("~/Downloads/Mission01")
require(ggplot2)

dist = read.csv("lscape_dist_5.csv", header = FALSE)
kernel = read.csv("lscape_kernel_5.csv", header = FALSE)
kernel_list = unlist(as.data.frame(t(as.matrix(kernel))))
dist_list = unlist(as.data.frame(t(as.matrix(dist))))
d = data.frame(Distance = dist_list, Probability = kernel_list)

#Data Points plot
ggplot(d, aes(x=Distance, y=Probability)) + 
  geom_point(size=0.40, alpha = 0.1, color = 'firebrick3') +
  scale_y_continuous(breaks = seq(0, 1, by=0.2), limits=c(0,1)) +
  ggtitle("Data Points") +
  theme(plot.title = element_text(hjust = 0.5))


#PDF plot
x = ggplot()
#I've only gone up to 395 because for some reason, the axes start to disappear 
#and other weird things begin to happen past 400 (reached elasped time limit error)
for (i in seq(1, 460*459, 459)) {
  #grab subset of data corresponding to each site
  small = d[c(i:(i+459)), ]
  
  #order by distance
  small = small[order(small$Distance), ]
  
  #add geom_step plot to our ggplot
  x = x + geom_step(data = small,
                       mapping = aes(x=Distance, y=Probability),
                       direction="vh",
                       color = 'mediumblue',
                       alpha = 0.1, size = 0.2) 
}
#make everything pretty
x = x +  scale_y_continuous(breaks = seq(0, 1, by=0.2), limits=c(0,1)) +
  ggtitle("PDF") +
  theme(plot.title = element_text(hjust = 0.5))

#get median line
dist.sorted = data.frame((apply(dist,1,sort)))
dist_medians = apply(dist.sorted, 1, median)

#sort probs based on distances
B = as.matrix(kernel)
A = as.matrix(dist)
prob.sorted = as.data.frame((sapply(1:NROW(A), function(i) B[i,][order(A[i,])])))
prob_medians = apply(prob.sorted, 1, median)

#plot median
x = x + geom_step(data = data.frame(x = dist_medians, y = prob_medians),
                  mapping = aes(x=x, y=y),
                  direction="vh",
                  color = 'black',
                  alpha = 0.9, size = 1) 
x



#CDF plot
x = ggplot()
#I've only gone up to 395 because for some reason, the axes start to disappear 
#and other weird things begin to happen past 400 (reached elasped time limit error)
for (i in seq(1, 460*459, 459)) {
  #grab subset of data corresponding to each site
  small = d[c(i:(i+459)), ]
  
  #order by distance
  small = small[order(small$Distance), ]
  small['cumsum'] = cumsum(small$Probability)
  
  #precision errors in summing make the cumsum larger or smaller than 1 sometimes, so I need to 
  #add this condition
  #if (small$cumsum[460] <= 1 & small$cumsum[460] > 0.99){  
    x = x + geom_step(data = small,
                      mapping = aes(x=Distance, y=cumsum),
                      direction="vh",
                      color = 'purple',
                      alpha = 0.15, size = 0.2)
  #}
  #add geom_step plot to our ggplot
 
}
#make everything pretty
x = x + scale_y_continuous(breaks = seq(0, 1, by=0.2), limits=c(0,1)) +
  ggtitle("CDF") + ylab("Probability")+
  theme(plot.title = element_text(hjust = 0.5))

#get median line
dist.sorted = data.frame((apply(dist,1,sort)))
dist_medians = apply(dist.sorted, 1, mean)

#sort probs based on distances
B = as.matrix(kernel)
A = as.matrix(dist)
prob.sorted = as.data.frame((sapply(1:NROW(A), function(i) B[i,][order(A[i,])])))
prob_medians = apply(prob.sorted, 1, mean)
w = cumsum(prob_medians)
#plot median
x + geom_step(data = data.frame(x = dist_medians, y = w),
                  mapping = aes(x=x, y=y),
                  direction="vh",
                  color = 'black',
                  alpha = 0.9, size = 1) 
w

