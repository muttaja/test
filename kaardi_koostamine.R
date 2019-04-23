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
landsat_band3 <- raster(all_landsat_bands[2])
landsat_band4 <- raster(all_landsat_bands[3])

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



################# kuidas koordinaadid ja kanalid kätte saab!?
landsat_band2 #see on teise kanali info
ext = c(483800, 515000, 6440000, 6468800)
lb2 = crop(landsat_band2, ext)
par(mfrow = c(1,1))
plot(lb2, col = gray(0:100 / 100))

#parem tartu ümbrus? vasak ülemine nurk luunja sild, täpselt üle vooremäe jookseb kaart
ext = c(493800, 510000, 6455000, 6468800)
lb3 = crop(landsat_band3, ext)
par(mfrow = c(1,1))
plot(lb3, col = gray(0:100 / 100))

ext = c(493800, 510000, 6455000, 6468800)
lb4 = crop(landsat_band4, ext)
par(mfrow = c(1,1))
plot(lb4, col = gray(0:100 / 100))


crp = crop(landsat_csf_br, ext)
par(col.axis = "white", col.lab = "white", tck = 0)
plotRGB(crp,
        r = 3, g = 2, b = 1,
        stretch = "lin",
        axes = TRUE,
        main = "RGB composite image\n Landsat Bands 4, 3, 2")
box(col = "white")

#väike luunja nurk testiks
ext = c(493800, 500000, 6465000, 6468800)
crp = crop(landsat_csf_br, ext)
par(col.axis = "white", col.lab = "white", tck = 0)
plotRGB(crp,
        r = 3, g = 2, b = 1,
        stretch = "lin",
        axes = TRUE,
        main = "RGB composite image\n Landsat Bands 4, 3, 2")
box(col = "white")
require(raster)
rasmat = as.matrix(crp)
#töötab, aga ei ole koordinaate :/



spts <- rasterToPoints(crp, spatial = TRUE)
require(rgdal)
llprj <-  "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
llpts <- spTransform(spts, CRS(llprj))
print(head(llpts))

dfx <- as.data.frame(llpts)
#25959x9 selle üliväikse nurga korral. pilte on 24(?)
#suure kaardi korral: 248400. OK! See peaks olema hallatav
#nüid kust saada katastrikaart!?











