#.dbf
setwd("A:/MAKA/MR")
require(foreign)
eraldis = read.dbf("eraldis_TY.dbf")
head(mr,30)
element = read.csv("element_TY.csv")
head(element)
eraldis[eraldis$id == 7225877,]
require(rgdal)
shape = readOGR(dsn = ".", layer = "SHAPEFILE")
