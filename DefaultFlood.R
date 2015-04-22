library(sp)
library(rgdal)
library(gstat)
library(rgeos)

# Load data
flood <- readGDAL("export/output/defaultflood.asc_corrected")
mask <- readGDAL("data/mask100.asc")
studarea <- readOGR("data/study_area.shp", "study_area")

# Mask flooding to area
flood$mask <- flood$band1 * mask$band1

# Read all flooding model results into a matrix "fr"
fr = numeric()
fr <- cbind(fr,(readGDAL("export/output/defaultflood.asc_corrected"))$band1)

fr <- ifelse(is.na(fr), 0, fr) # set NA to 0
# Compute probability of flooding
prob <- apply(fr, 1, function(x) mean(ifelse(x > 0, 1, 0)))
prob <- prob * mask$band1 # probs; get rid of NA area
# Convert prob to SpatialPixelsDataFrame
prob <- as.data.frame(prob)
coordinates(prob) <- coordinates(mask)
gridded(prob) <- TRUE

# Create safe and hazard areas
prob$safe <- factor(ifelse(prob$prob > 0, 1, 0), levels=c(0,1), labels=c("safe","hazard"))

# Plots safe and hazard areas, save to png
spplot(prob, zcol="safe", col.regions=(c("green", "red")), main = "Safe areas 6 hours after dike breach")
dev.print(png, file="Safeareas.png", width=500, height=300)
dev.off()

# Plot inundation depths
spplot(flood, zcol="mask", col.regions = bpy.colors(), 
       sp.layout = list("sp.polygons", studarea, first = F), 
       main = "Inundation depth 6 hours after dike breach", 
       legend = "test")
dev.print(png, file="DefaulInundationDepth.png", width=500, height=300)
dev.off()

