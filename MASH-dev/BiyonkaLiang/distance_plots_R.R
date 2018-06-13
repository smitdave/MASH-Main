setwd("~/Downloads/Mission01")
require(ggplot2)

dist = read.csv("lscape_dist_5.csv", header = FALSE)
kernel = read.csv("lscape_kernel_5.csv", header = FALSE)
kernel_list = unlist(kernel)
dist_list = unlist(dist)
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
                       alpha = 0.2, size = 0.2) 
}
#make everything pretty
x +  scale_y_continuous(breaks = seq(0, 1, by=0.2), limits=c(0,1)) +
  ggtitle("PDF") +
  theme(plot.title = element_text(hjust = 0.5))

