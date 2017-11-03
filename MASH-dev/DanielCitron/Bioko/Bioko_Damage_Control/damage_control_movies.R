# Create stacks of images from damage_control:

library(sp)
library(raster)
library(maptools)
library(RColorBrewer)
library(MASS)
library(rgeos)
library(GISTools)
library(animation)

### Read in the necessary gis files from csv:
# Carlos - these are your pfpr documents that you sent us
setwd("/Users/dtcitron/Documents/MASH/MASH-Main/MASH-dev/DavidSmith/Bioko")
biokoPOP <- read.csv("bioko_areas.csv")
# load in PfPR for bioko island
biokoPFPR <- read.csv("pfprmap_area_CAGcorrectNA.csv")
# load in Lambda values for bioko island
biokoLambda <- read.csv("data_area.csv")
# Merge the two three sets on the areaIds
bioko <- merge(biokoPOP, biokoLambda, by = "areaId", all =FALSE)

### Read in the .rds files of all the pfpr vs. time
Bioko.baseline <- readRDS("../../DanielCitron/Bioko/Bioko_Damage_Control/Bioko_pevaxx90_PfPR_vs_t.rds")

# This function converts the pfpr 
convert_to_matrix <- function(x){
  # x = Bioko.pevaxx50
  holder <- matrix(0, nrow = nrow(x), ncol = (365*5 + 1))
  for (i in 1:nrow(x)){
    for (j in 1:(365*5 + 1)){
      holder[i,j] <- x[i,]$PfPR.t[[1]][j]
    }
  }
  return(holder)
}

baseline.h <- convert_to_matrix(Bioko.baseline)

### Create 1 test image
# Notice that I'm doing this using GISTools' choropleth() function - 
# You may want to use your own methods of plotting here, and adjust the color schemes to be
# as understandable as possible
c <- data.frame(areaId = bioko$areaId, x = bioko$utmx, y = bioko$utmy, data = baseline.h[,300])
coordinates(c) <- ~x+y
choropleth(c, c$data, shading = shades, pch=15, cex = .7)

# Define Shading scheme
shades <- auto.shading(c(0,.4), n = 8, cols = brewer.pal(8,'Reds'))
# Output a stack of images, from each month
s <- seq(0, 5*365, by = 30)
for (i in 1:length(s)){
  c <- data.frame(areaId = bioko$areaId, x = bioko$utmx, y = bioko$utmy, data = baseline.h[,s[i]])
  coordinates(c) <- ~x+y
  ### EDIT THE FILE PATH
  jpeg(filename = sprintf(paste("../../DanielCitron/Bioko/Bioko_Damage_Control/test_90_",i,".jpg",sep="")))
  choropleth(c, c$data, shading = shades, pch=15, cex = .7)
  dev.off()
}



# From here, it's a matter of using image magick -
# - I generated the gif you saw from the command line:
# `magick convert test_90*.jpg test_90.gif` # generates the gif
# `magick convert -delay 50 test_90.gif test_90.gif` # slows down the gif
# you can also do this using im.convert()
#


