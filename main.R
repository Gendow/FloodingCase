

library(sp)
library(rgdal)
library(gstat)
library(rgeos)

#Import data
ahn <- readGDAL("data/ahn100_f.asc")
studyArea <- readGDAL("data/mask100.asc")
error <- read.table("data/Sample_error.txt", header = T)

MC <- 100

#set coordinates
coordinates(error) <- ~x+y

#bubble plot
bubble(error, zcol = "resid", main = "Residuals")

#Creating variogram
gError <- gstat(formula = resid~1, data = error)
vgmGError <- variogram(gError, cutoff = 5000)
vgmError <- vgm(nugget = 6000, psill = 3700, range = 1400, model = "Exp")
plot(vgmGError, vgmError, plot.nu = T)
vgmError <- fit.variogram(vgmGError, vgmError, fit.method=7)
plot(vgmGError, vgmError, plot.nu = T)

attr(vgmError, "SSErr")

# Kriging
resid.krig <- krige(resid~1, error, newdata = studyArea, vgmError, nmax = 24)
resid.sim <- krige(resid~1, error, newdata = studyArea, vgmError, nmax = 24, nsim = MC)

#cross validation
resid.xv <- krige.cv(resid~1, error, vgmError, nmax = 24)
str(resid.xv)
    
hist(resid.xv$var1.pred)
mean(resid.xv$zscore)
sd(resid.xv$zscore)
hist(resid.xv$zscore)

#Create defaultDem
# Add residuals to AHN and make SpatialPixelsDataFrame object
# it is assumed that predicted residuals are in "resid"
defaultdem <- as.data.frame(ahn$band1 + resid.krig$var1.pred)
names(defaultdem) <- "band1"
coordinates(defaultdem) <- coordinates(ahn)
gridded(defaultdem) <- TRUE
spplot(defaultdem, col.regions=bpy.colors())
write.asciigrid(defaultdem, "export/defaultDEM.asc") # write to disk

#uncertain DEM
outroot <- "export/dem/dem"

for(i in 1:100)
{
  outname <- paste(outroot, i, ".asc", sep = "")
  outdem <- as.data.frame(ahn$band1 + resid.sim[[i]])
  coordinates(outdem) <- coordinates(ahn)
  gridded(outdem) <- T
  write.asciigrid(outdem, outname)
}

#uncertain inflow
inflow.sim <- rnorm(n=MC, mean = 2700, sd = 120)

write.table(inflow.sim, file = "export/inflow.txt", col.names = "inflow")

