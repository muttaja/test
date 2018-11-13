# .las files

install.packages("rlas")
install.packages("lidR")
setwd("A:/MAKA/KARU/ALS/Kagu/")
require(rlas)
require(lidR)
test = read.las("378655.las")


t1 = catalog_apply("A:/MAKA/KARU/ALS/Kagu/2017", select = "xyz")
l1 = readLAS("A:/MAKA/KARU/ALS/Kagu/2017/378655.las", select = "xyz")


LASfile <- system.file("2017", "", package = "lidR")
project = catalog(LASfile)
plot(project)

###LASmetrics? rLiDAR?
install.packages("rLiDAR")
require(rLiDAR)
setwd("A:/MAKA/KARU/ALS/Kagu/2017")
las = readLAS("378655.las")
met = LASmetrics(las)

#
LASfile <- system.file("A:/MAKA/KARU/ALS/Kagu/2017", package="rLiDAR")
LASfile <- system.file("extdata", "LASexample1.las", package="rLiDAR")
# Reading LAS file
rLAS<-readLAS(LASfile,short=TRUE)
# Summary of the LAS file
summary(rLAS)

# Set the minht and above parameters
minht<-1.37 # meters or feet
above<-2.00 # meters or feet
# LiDAR metrics computation
LiDARmetrics<-LASmetrics(LASfile, minht, above)

########## kas saan oma LAS-iga tööle?
las = readLAS("378655.las", short = T)
met = LASmetrics("378655.las", minht, above)


##
# Importing LAS file:
LASfile <- system.file("extdata", "LASexample1.las", package="rLiDAR")
# Reading LAS file
LAS<-readLAS(LASfile,short=TRUE)
# Height subsetting the data
xyz<-subset(LAS[,1:3],LAS[,3] >= 1.37)
# Getting LiDAR clusters
set.seed(1)
clLAS<-kmeans(xyz, 32)
# Set the points id
id<-as.factor(clLAS$cluster)
# Set the xyid input
xyid<-cbind(xyz[,1:2],id)
# Compute the LiDAR convex hull of the clusters
chullTrees<-chullLiDAR2D(xyid)
# Plotting the LiDAR convex hull
library(sp)
plot(SpatialPoints(xyid[,1:2]),cex=0.5,col=xyid[,3])
plot(chullTrees$chullPolygon,add=TRUE, border='green')
# Get the ground-projected area of LiDAR convex hull
chullList<-chullTrees$chullArea
summary(chullList) # summary

###
las = readLAS("378656.las", short = T)
test1 = data.frame(las[,1:3])
summary(test1$Z)

#test = read.csv("ALS/pilved36m/1109_2017.xyz", header = F, sep = " ")
#las1 = las0[las0$V2 == 1,]
require(ggplot2)
m = ggplot(data = test1, aes(X,Y))
m + geom_point(aes(col = Z, size = 4, alpha = 0.4)) + scale_colour_gradient2(high = "darkgreen")

