library(sp) # necessary for rgdal
library(rgdal)
library(raster)
library(rgeos)
library(maptools)

GBD <- readShapePoly("/Volumes/snfs/DATA/SHAPE_FILES/GBD_geographies/master/GBD_2016/master/shapefiles/GBD2016_analysis_final.shp")

# Load in A2 data
global <- readShapePoly("/Volumes/snfs/WORK/11_geospatial/06_original shapefiles/GAUL_admin/admin2/g2015_2014_2/g2015_2014_2_modified.shp")

# Find the subset of A2 areas that belong to Haiti
HTI <- global[global$ADM0_NAME=="Haiti",]
plot(HTI)

# These are the names of the A1 areas
unique(HTI$ADM1_NAME)
# And the names of the A2 areas in Grand Anse:

HTI[HTI$ADM1_Name == "Grande Anse",]

# The names of the A2 areas in Sud
HTI[HTI$ADM1_NAME == "Sud",]$ADM2_NAME

HTI.SUD <- HTI[HTI$ADM1_NAME=="Sud",]
ga <- unique(HTI$ADM1_NAME)[9]
HTI.GA <- HTI[HTI$ADM1_NAME==ga,]

# The names of the A2 areas in GA
unique(HTI.GA$ADM2_NAME)
# The names of the A2 areas in Sud
unique(HTI.SUD$ADM2_NAME)

# Outline of the two main A1 units
plot(HTI.SUD)
plot(HTI.GA, add = TRUE)

# Highlight the westernmost A2 units
plot(HTI.GA[HTI.GA$ADM2_NAME=="Anse-D'Ainault",], col = "Red", add = TRUE)
plot(HTI.GA[HTI.GA$ADM2_NAME=="Jeremie",], col = "Red", add = TRUE)
plot(HTI.SUD[HTI.SUD$ADM2_NAME=="Chardonnieres",], col = "Red", add = TRUE)
# Highlight some eastern A2 units nearby
plot(HTI.GA[HTI.GA$ADM2_NAME=="Corail",], col = "Green", add = TRUE)
plot(HTI.SUD[HTI.SUD$ADM2_NAME=="Coteaux",], col = "Green", add = TRUE)
plot(HTI.SUD[HTI.SUD$ADM2_NAME=="Port-Salut",], col = "Green", add = TRUE)
plot(HTI.SUD[HTI.SUD$ADM2_NAME=="Cayes",], col = "Green", add = TRUE)


# Label each of the A2 units
# Pulling out centroid Long/Lat - Anse-D'Ainault
AD.centroid <- gCentroid(HTI.GA[HTI.GA$ADM2_NAME=="Anse-D'Ainault",])
AD.long <- coordinates(AD.centroid)[1] # -74.39124
AD.lat  <- coordinates(AD.centroid)[2] #  18.46622
text(AD.long, AD.lat, "Anse-D'Ainault")

# Pulling out centroid Long/Lat - Jeremie
JE.centroid <- gCentroid(HTI.GA[HTI.GA$ADM2_NAME=="Jeremie",])
JE.long <- coordinates(JE.centroid)[1] # -74.22257
JE.lat  <- coordinates(JE.centroid)[2] # 18.534
text(JE.long, JE.lat, "Jeremie")

# Pulling out centroid Long/Lat - Corail
CO.centroid <- gCentroid(HTI.GA[HTI.GA$ADM2_NAME=="Corail",])
CO.long <- coordinates(CO.centroid)[1] # -73.92961
CO.lat  <- coordinates(CO.centroid)[2] # 18.49504
text(CO.long, CO.lat, "Corail")

# Pulling out centroid Long/Lat - Chardonnieres
CH.centroid <- gCentroid(HTI.SUD[HTI.SUD$ADM2_NAME=="Chardonnieres",])
CH.long <- coordinates(CH.centroid)[1] #
CH.lat  <- coordinates(CH.centroid)[2]
text(CH.long, CH.lat, "Chardonnieres")

# Pulling out centroid Long/Lat - Coteaux
CT.centroid <- gCentroid(HTI.SUD[HTI.SUD$ADM2_NAME=="Coteaux",])
CT.long <- coordinates(CT.centroid)[1] # -74.20935
CT.lat  <- coordinates(CT.centroid)[2] # 18.33986
text(CT.long, CT.lat, "Coteaux")

# Pulling out centroid Long/Lat - Port-Salut
PS.centroid <- gCentroid(HTI.SUD[HTI.SUD$ADM2_NAME=="Port-Salut",])
PS.long <- coordinates(PS.centroid)[1] # -73.87902
PS.lat  <- coordinates(PS.centroid)[2] # 18.09536
text(PS.long, PS.lat, "Port-Salut")

# Pulling out centroid Long/Lat - Cayes
CA.centroid <- gCentroid(HTI.SUD[HTI.SUD$ADM2_NAME=="Cayes",])
CA.long <- coordinates(CA.centroid)[1] # -73.83127
CA.lat  <- coordinates(CA.centroid)[2] # 18.28119
text(CA.long, CA.lat, "Cayes")

