library(sp)
library(raster)
library(maptools)
library(RColorBrewer)
library(MASS)
library(rgeos)
library(GISTools)

# Load in two .csv files
# One file will contain PfPR data, indexed by areaID.  Its coordinates are the UTM projection.
# One file will contain Population data, indexed by areaID.  Its coordinates can be written both as
# long/lat and the UTM projection.

# Set Working Directory to ~/MASH/MASH-Main/MASH-dev/DavidSmith/Bioko

# Results: read in the data and create plots of how the population and PfPR are distributed on Bioko


# This is the csv file of PfPR data
csvPfPr <- read.csv("pfprmap_area_CAGcorrectNA.csv")
head(csvPfPr)
# force this to be a SpatialPointsDataFrame
coordinates(csvPfPr) <- ~x+y

# This is the csv file of the populated areas
biokoPOP <- read.csv("bioko_areas.csv")
head(biokoPOP)
# force this to be a SpatialPointsDataFrame
coordinates(biokoPOP) <- ~utmx+utmy

# Create a plot of the inferred PfPR, and a plot of where people live
par(mfrow=c(1,2))
c <- csvPfPr[!is.na(csvPfPr$pfpr),]
shades <- auto.shading(c$pfpr, n = 8, cols = brewer.pal(8,'Reds'))
choropleth(c, c$pfpr, shading = shades)
title("PfPR from CSV")
choro.legend(434299.5,432728.3, shades, cex = 0.6)

c2 <- biokoPOP[!is.na(biokoPOP$pop),]
shades <- auto.shading(c2$pop, n = 8, cols = brewer.pal(8,'Blues'))
choropleth(c2, c2$pop, shading = shades)
title("Population from CSV")
choro.legend(438299.5,428728.3, shades, cex = 0.6)

