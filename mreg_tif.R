#metsaregister read TIF

require(raster)
setwd("A:/MAKA/d2_13.11.2018_esimene_andmekaust_nimi_korrastamata/naidised/mreg")
mreg = read.csv("eraldised2018.taks_SAT.koos.csv")
setwd("A:/MAKA/d2_13.11.2018_esimene_andmekaust_nimi_korrastamata/mreg")


options(stringsAsFactors = FALSE)

list.files()
all_tifs <- list.files(pattern = ".tif$",full.names = TRUE)

haab <- raster(all_tifs[1])
mand <- raster(all_tifs[9])

mand

xy = cbind(seq(640000,645000, by = 200),  rep(6385500,26))
result <- extract(mand, xy)
result
coordinates(mand)[result[,2],]

