#Rony Nedkov and Damiano Luzzi
#April 2015

#import libraries
library(sp)
library(rgdal)
library(gstat)
library(rgeos)

#Import relative paths and files
studarea <- readOGR("data/study_area.shp", "study_area")
mask <- readGDAL("data/mask100.asc")

inroot1 <- "D:/Geostat/FloodingCase/export/output/experiment1/" # change this
inroot2 <- "D:/Geostat/FloodingCase/export/output/experiment2/" # change this
inroot3 <- "D:/Geostat/FloodingCase/export/output/experiment3/" # change this

# Read all flooding model results into a matrix "fr"
flist <- list.files(inroot1) # all file names in directory
fr = numeric()
for(in_name in flist) {
  in_name <- paste(inroot1, in_name, sep = "")
  fr <- cbind(fr,(readGDAL(in_name, silent = TRUE))$band1)
}

fr <- ifelse(is.na(fr), 0, fr) # set NA to 0

# Compute probability of flooding
prob <- apply(fr, 1, function(x) mean(ifelse(x > 0, 1, 0)))
prob <- prob * mask$band1 # probs; get rid of NA area

# Convert prob to SpatialPixelsDataFrame
prob <- as.data.frame(prob)
coordinates(prob) <- coordinates(mask)
gridded(prob) <- TRUE

# plot using blue, pink, yellow colorramp
spplot(prob, zcol=1, col.regions = colorRampPalette(c("white", "green", "yellow", "red"))(30), 
       sp.layout = list("sp.polygons", studarea, first = F))

#Calculation and plotting of mean, sd and variance per grid cell
prob$Variance <- apply(fr, 1 , function(x) var(x))
prob$sd <- apply(fr, 1 , function(x) sd(x))
prob$mean <- apply(fr, 1 , function(x) mean(x))

spplot(prob, zcol = "mean", main = "Mean of flooding", col.regions = colorRampPalette(c("white", "blue"))(30), 
       sp.layout = list("sp.polygons", studarea, first = F))
       
spplot(prob, zcol = "Variance", main = "Variance of flooding", col.regions = colorRampPalette(c("white", "green", "yellow", "red"))(30), 
       sp.layout = list("sp.polygons", studarea, first = F))

spplot(prob, zcol = "sd", main = "Sd of flooding", col.regions = colorRampPalette(c("white", "green", "yellow", "red"))(30), 
       sp.layout = list("sp.polygons", studarea, first = F))

#Experiment 2

flist2 <- list.files(inroot2) # all file names in directory
exp2 = numeric()

for(in_name in flist2) {
  in_name <- paste(inroot2, in_name, sep = "")
  exp2 <- cbind(exp2,(readGDAL(in_name, silent = TRUE))$band1)
}

exp2 <- ifelse(is.na(exp2), 0, exp2) # set NA to 0

# Compute probability of flooding
prob2 <- apply(exp2, 1, function(x) mean(ifelse(x > 0, 1, 0)))
prob2 <- prob2 * mask$band1 # probs; get rid of NA area

# Convert prob to SpatialPixelsDataFrame
prob2 <- as.data.frame(prob2)
coordinates(prob2) <- coordinates(mask)
gridded(prob2) <- TRUE

# plot prob map
spplot(prob2, zcol=1, col.regions = colorRampPalette(c("white", "green", "yellow", "red"))(30), 
       sp.layout = list("sp.polygons", studarea, first = F))

#Calculation and plotting of mean, sd and variance per grid cell
prob2$variance <- apply(exp2, 1 , function(x) var(x))
prob2$sd <- apply(exp2, 1 , function(x) sd(x))
prob2$mean <- apply(exp2, 1 , function(x) mean(x))

spplot(prob2, zcol = "mean", main = "Mean of flooding", col.regions = colorRampPalette(c("white", "blue"))(30), 
       sp.layout = list("sp.polygons", studarea, first = F))

spplot(prob2, zcol = "variance", main = "Variance of flooding", col.regions = colorRampPalette(c("white", "green", "yellow", "red"))(30), 
       sp.layout = list("sp.polygons", studarea, first = F))

spplot(prob2, zcol = "sd", main = "Sdv of flooding", col.regions = colorRampPalette(c("white", "green", "yellow", "red"))(30), 
       sp.layout = list("sp.polygons", studarea, first = F))

#experiment 3


flist3 <- list.files(inroot3) # all file names in directory
exp3 = numeric()
for(in_name in flist3) {
  in_name <- paste(inroot3, in_name, sep = "")
  exp3 <- cbind(exp3,(readGDAL(in_name, silent = TRUE))$band1)
}

exp3 <- ifelse(is.na(exp3), 0, exp3) # set NA to 0

# Compute probability of flooding
prob3 <- apply(exp3, 1, function(x) mean(ifelse(x > 0, 1, 0)))
prob3 <- prob3 * mask$band1 # probs; get rid of NA area

# Convert prob to SpatialPixelsDataFrame
prob3 <- as.data.frame(prob3)
coordinates(prob3) <- coordinates(mask)
gridded(prob3) <- TRUE

# plot prob map
spplot(prob3, zcol=1, col.regions = colorRampPalette(c("white", "green", "yellow", "red"))(30), 
       sp.layout = list("sp.polygons", studarea, first = F))

#Calculation and plotting of mean, sd and variance per grid cell
prob3$variance <- apply(exp3, 1 , function(x) var(x))
prob3$sd <- apply(exp3, 1 , function(x) sd(x))
prob3$mean <- apply(exp3, 1 , function(x) mean(x))

spplot(prob3, zcol = "mean", main = "Mean of flooding", col.regions = colorRampPalette(c("white", "blue"))(30), 
       sp.layout = list("sp.polygons", studarea, first = F))

spplot(prob3, zcol = "variance", main = "Variance of flooding", col.regions = colorRampPalette(c("white", "green", "yellow", "red"))(30), 
       sp.layout = list("sp.polygons", studarea, first = F))

spplot(prob3, zcol = "sd", main = "Sdv of flooding", col.regions = colorRampPalette(c("white", "green", "yellow", "red"))(30), 
       sp.layout = list("sp.polygons", studarea, first = F))


