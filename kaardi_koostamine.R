#read shapefile?

require(rgdal)
setwd("A:/MAKA/KARU/pildid/2018/LC08_185019_20180512/maskid")
pilved <- readOGR(dsn = ".", layer = "pilved")
pilve.data = pilved[1][1]

#tifi lugemine failist tif:

#TIF
require(raster)
setwd("A:/MAKA/KARU/pildid/2018/LC08_185019_20180512")
options(stringsAsFactors = FALSE)

list.files()
all_landsat_bands <- list.files(pattern = ".TIF$",full.names = TRUE)

landsat_band2 <- raster(all_landsat_bands[1])

plot(landsat_band2, col = gray(0:100 / 100))


#############
landsat_stack_csf <- stack(all_landsat_bands)
# then turn it into a brick
landsat_csf_br <- brick(landsat_stack_csf)
# view stack attributes
landsat_csf_br

plot(landsat_csf_br,
     col = gray(20:100 / 100))

names(landsat_csf_br)
names(landsat_csf_br) <- gsub(pattern = "LC08_L1TP_185019_20180512_20180517_01_T1_", replacement = "", names(landsat_csf_br))
plot(landsat_csf_br,
     col = gray(20:100 / 100))


par(col.axis = "white", col.lab = "white", tck = 0)
plotRGB(landsat_csf_br,
        r = 3, g = 2, b = 1,
        stretch = "lin",
        axes = TRUE,
        main = "RGB composite image\n Landsat Bands 4, 3, 2")
box(col = "white")

ext = c(485385, 535385, 6436985, 6486985) #kagueesti nurk
crp = crop(landsat_csf_br, ext)
par(col.axis = "white", col.lab = "white", tck = 0)
plotRGB(crp,
        r = 3, g = 2, b = 1,
        stretch = NULL,
        axes = TRUE,
        main = "RGB composite image\n Landsat Bands 4, 3, 2")
box(col = "white")