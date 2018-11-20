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
#mida strech teeb? NULL, "lin" või "hist"


###teine fail:
setwd("A:/MAKA/KARU/pildid/2018/LC08_187020_20180510")
#############
all_landsat_bands <- list.files(pattern = ".TIF$",full.names = TRUE)
landsat_stack_csf <- stack(all_landsat_bands)
# then turn it into a brick
landsat_csf_br <- brick(landsat_stack_csf)
# view stack attributes
#landsat_csf_br

plot(landsat_csf_br,
     col = gray(20:100 / 100))

names(landsat_csf_br)
names(landsat_csf_br) <- gsub(pattern = "LC08_L1TP_187020_20180510_20180517_01_T1_", replacement = "", names(landsat_csf_br))
plot(landsat_csf_br,
     col = gray(20:100 / 100))


par(col.axis = "white", col.lab = "white", tck = 0)
plotRGB(landsat_csf_br,
        r = 3, g = 2, b = 1,
        stretch = "lin",
        axes = TRUE,
        main = "RGB composite image\n Landsat Bands 4, 3, 2")
box(col = "white")



###üritame fenoloogiat tuvastada:
setwd("A:/MAKA/KARU/pildid/2018")
options(stringsAsFactors = FALSE)

list.files()
all_bands_LC08_185019_20180512 <- list.files("LC08_185019_20180512",pattern = ".TIF$",full.names = TRUE)
all_bands_LC08_185019_20180528 <- list.files("LC08_185019_20180528",pattern = ".TIF$",full.names = TRUE)
landsat_20180512_band2 <- raster(all_bands_LC08_185019_20180512[1])
landsat_20180528_band2 <- raster(all_bands_LC08_185019_20180528[1])
vahe2 = landsat_20180528_band2 - landsat_20180512_band2
extent(vahe2)[2] - extent(vahe2)[1]
ex = extent(vahe2) - c(0,200000,0,-100000)
ext = c(485385, 535385, 6436985, 6486985) #kagueesti nurk
v2 = crop(vahe2, ext)
plot(v2, col = gray(0:100 / 100))

landsat_20180512_band3 <- raster(all_bands_LC08_185019_20180512[2])
landsat_20180528_band3 <- raster(all_bands_LC08_185019_20180528[2])
vahe3 = landsat_20180528_band3 - landsat_20180512_band3
v3 = crop(vahe3, ext)
plot(v3, col = gray(0:100 / 100))

landsat_20180512_band4 <- raster(all_bands_LC08_185019_20180512[3])
landsat_20180528_band4 <- raster(all_bands_LC08_185019_20180528[3])
vahe4 = landsat_20180528_band4 - landsat_20180512_band4
v4 = crop(vahe4, ext)
plot(v4, col = gray(0:100 / 100))

v_stack = stack(c(v2,v3,v4))
v_brick = brick(v_stack)

par(col.axis = "white", col.lab = "white", tck = 0)
plotRGB(v_brick,
        r = 3, g = 2, b = 1,
        stretch = "hist",
        axes = TRUE,
        main = "RGB composite image\n Landsat Bands 4, 3, 2")
box(col = "white")

#kollased on vist tegelikult ka rapsipõllud: raps hakkab mais õitsema?
#vt näiteks suur rapsipõld põlva külje all googlemapsist

#tume roheline äkki oder (või nisu, rukis): vt järvselja kahe rabamassiivi vahel
#valge mingi teine teravili




#differences_all = raster(all_bands_LC08_185019_20180528) - raster(all_bands_LC08_185019_20180512)
#nii ei saa
#sügisel?
all_bands1 <- list.files("LC08_186019_20180823",pattern = ".TIF$",full.names = TRUE) #23. aug
all_bands2 <- list.files("LC08_186019_20180908",pattern = ".TIF$",full.names = TRUE) #8. sept
band11 = raster(all_bands1[1])
band12 = raster(all_bands1[2])
band13 = raster(all_bands1[3])
band21 = raster(all_bands2[1])
band22 = raster(all_bands2[2])
band23 = raster(all_bands2[3])

dif1 = band21-band11
dif2 = band22-band12
dif3 = band23-band13
dfs = brick(c(dif1,dif2,dif3))
dfs = crop(dfs,ext)

par(col.axis = "white", col.lab = "white", tck = 0)
plotRGB(dfs,
        r = 3, g = 2, b = 1,
        stretch = "hist",
        axes = TRUE,
        main = "RGB composite image\n Landsat Bands 4, 3, 2")
box(col = "white")
#pilved :/
