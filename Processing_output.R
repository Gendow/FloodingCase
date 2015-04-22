library(sp)
library(rgdal)
library(gstat)
library(rgeos)

studarea <- readOGR("data/study_area.shp", "study_area")


inroot <- "D:/Geostat/FloodingCase/export/output/experiment1/" # change this
mask <- readGDAL("data/mask100.asc")
# Read all flooding model results into a matrix "fr"
flist <- list.files(inroot) # all file names in directory
fr = numeric()
for(in_name in flist)
{
  in_name <- paste(inroot, in_name, sep = "")
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
spplot(prob, zcol=1, col.regions=bpy.colors())

#Calculation and plotting of mean and variance per grid cell
prob$Variance <- apply(fr, 1 , function(x) var(x))
prob$sd <- apply(fr, 1 , function(x) sd(x))
prob$mean <- apply(fr, 1 , function(x) mean(x))



spplot(prob, zcol = "mean", main = "Mean of flooding", col.regions = colorRampPalette(c("white", "blue"))(30), 
       sp.layout = list("sp.polygons", studarea, first = F))
       
spplot(prob, zcol = "Variance", main = "Variance of flooding", col.regions = colorRampPalette(c("white", "green", "red", "yellow"))(30), 
       sp.layout = list("sp.polygons", studarea, first = F))
spplot(prob, zcol = "sd", main = "Sdv of flooding", col.regions = colorRampPalette(c("white", "green", "red", "yellow"))(30), 
       sp.layout = list("sp.polygons", studarea, first = F))


