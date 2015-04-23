#Rony Nedkov and Damiano Luzzi
#April 2015

#import libraries
library(sp)
library(rgdal)
library(gstat)
library(rgeos)

#Loading data
studarea <- readOGR("data/study_area.shp", "study_area")
mask <- readGDAL("data/mask100.asc")
defaultdem <- readGDAL("export/defaultDEM.asc")
dem1 <- readGDAL("export/dem/dem5.asc")
dem2 <- readGDAL("export/dem/dem23.asc")
dem3 <- readGDAL("export/dem/dem56.asc")
dem4 <- readGDAL("export/dem/dem74.asc")
dem5 <- readGDAL("export/dem/dem89.asc")

# Removing NA values from DEM realisations
defaultdem$band1 <- ifelse(is.na(defaultdem$band1), 0, defaultdem$band1) # set NA to 0
dem1$band1 <- ifelse(is.na(dem1$band1), 0, dem1$band1) # set NA to 0
dem2$band1 <- ifelse(is.na(dem2$band1), 0, dem2$band1) # set NA to 0
dem3$band1 <- ifelse(is.na(dem3$band1), 0, dem3$band1) # set NA to 0
dem4$band1 <- ifelse(is.na(dem4$band1), 0, dem4$band1) # set NA to 0
dem5$band1 <- ifelse(is.na(dem5$band1), 0, dem5$band1) # set NA to 0

#Calculating mean and sd for every DEM realisation
mean(defaultdem$band1)
mean(dem1$band1)
mean(dem2$band1)
mean(dem3$band1)
mean(dem4$band1)
mean(dem5$band1)

max(defaultdem$band1)
max(dem1$band1)
max(dem2$band1)
max(dem3$band1)
max(dem4$band1)
max(dem5$band1)


#plot dem
png(filename="export/defaultdem.png")
spplot(defaultdem, zcol = "band1", main = "Default DEM", sp.layout = list("sp.polygons", studarea, first = F), 
       col.regions = colorRampPalette(c("chartreuse", "darkgreen", "yellow", "white"))(30))
dev.off()

png(filename="export/dem1.png")
spplot(dem1, zcol = "band1", main = "DEM realisation 8", sp.layout = list("sp.polygons", studarea, first = F), 
       col.regions = colorRampPalette(c("chartreuse", "darkgreen", "yellow", "white"))(30))
dev.off()

png(filename="export/dem2.png")
spplot(dem2, zcol = "band1", main = "DEM realisation 40", sp.layout = list("sp.polygons", studarea, first = F), 
       col.regions = colorRampPalette(c("chartreuse", "darkgreen", "yellow", "white"))(30))
dev.off()

png(filename="export/dem3.png")
spplot(dem3, zcol = "band1", main = "DEM realisation 73", sp.layout = list("sp.polygons", studarea, first = F), 
       col.regions = colorRampPalette(c("chartreuse", "darkgreen", "yellow", "white"))(30))
dev.off()

png(filename="export/dem4.png")
spplot(dem4, zcol = "band1", main = "DEM realisation 85", sp.layout = list("sp.polygons", studarea, first = F), 
       col.regions = colorRampPalette(c("chartreuse", "darkgreen", "yellow", "white"))(30))
dev.off()

png(filename="export/dem5.png")
spplot(dem5, zcol = "band1", main = "DEM realisation 23", sp.layout = list("sp.polygons", studarea, first = F), 
       col.regions = colorRampPalette(c("chartreuse", "darkgreen", "yellow", "white"))(30))
dev.off()

#export maps
png(filename="export/defaultdem.png")


