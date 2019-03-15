#lidar tiff
require(raster)
setwd("A:/MAKA/d2_13.11.2018_esimene_andmekaust_nimi_korrastamata/ALS/koos")
options(stringsAsFactors = FALSE)

list.files()
all_tifs <- list.files(pattern = ".tif$",full.names = TRUE)
all_tifs

#kõikidel peegeldustel põhinev katvus 2017:
kk2017 <- raster(all_tifs[35])
plot(kk2017, col = gray(0:100 / 100))


