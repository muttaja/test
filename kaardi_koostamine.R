
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
        main = "RGB komposiitpilt. Melliste-Võnnu ümbrus")
box(col = "white")

#väike luunja nurk testiks
ext = c(493800, 500000, 6465000, 6468800)
crp = crop(landsat_csf_br, ext)
par(col.axis = "white", col.lab = "white", tck = 0)
plotRGB(crp,
        r = 3, g = 2, b = 1,
        stretch = "lin",
        axes = TRUE,
        main = "RGB komposiitpilt. Vana-Kastre ümbrus")
box(col = "white")
require(raster)
rasmat = as.matrix(crp)
#töötab, aga ei ole koordinaate :/



spts <- rasterToPoints(crp, spatial = TRUE)
require(rgdal)
#see sama on igal juhul LC08 jp2 fotodel...
llprj <-  "+proj=lcc +lat_1=59.33333333333334 +lat_2=58 +lat_0=57.51755393055556 +lon_0=24 +x_0=500000 +y_0=6375000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs "
llpts <- spTransform(spts, CRS(llprj))
print(head(llpts))

dfx <- as.data.frame(llpts)
#25959x9 selle üliväikse nurga korral. pilte on 24(?)
#suure kaardi korral: 248400. OK! See peaks olema hallatav
#nüid kust saada katastrikaart!?


#aga kuidas jp2 avab?
setwd("A:/MAKA/KARU/pildid/Kagu-Eesti/2018/LC08_185019_20180512")
all_landsat <- list.files(pattern = ".jp2$",full.names = TRUE)
land1 <- readGDAL(all_landsat[1])
land2 <- readGDAL(all_landsat[2])
land3 <- readGDAL(all_landsat[3])
land = cbind(land1,land2,land3)
rasland = brick(land1)
rasland2 = brick(land2)
crp = crop(rasland, c(669400,685000,6460000,6472500)) #exp ei kattud, kuna koord.sys erinev; määrame uue ext õigete koord põhjal

plot(crp, col = gray(0:100 / 100))
spts <- rasterToPoints(crp, spatial = TRUE)
llprj <-  "+proj=lcc +lat_1=59.33333333333334 +lat_2=58 +lat_0=57.51755393055556 +lon_0=24 +x_0=500000 +y_0=6375000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs "
llpts <- spTransform(spts, CRS(llprj))
print(head(llpts))
dfx <- as.data.frame(llpts)
test2 = head(dfx)
test = merge(test1, test2, by = c("x","y"))

dfx = data.frame(B2 = c(), B3 = c(), B4 = c(),B5 = c(), B6 = c(), B7 = c(), B9 = c())



#landsat 2018 kõik:
require(raster)
require(rgdal)
require(stringr)
setwd("A:/MAKA/KARU/pildid/Kagu-Eesti/2018")
options(stringsAsFactors = FALSE)
list.land = list.dirs(full.names = T, recursive = T)
list.land  = list.land[ grepl("LC08", list.land) ]
ext = c(669400,685000,6460000,6472500)
llprj <-  "+proj=lcc +lat_1=59.33333333333334 +lat_2=58 +lat_0=57.51755393055556 +lon_0=24 +x_0=500000 +y_0=6375000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs "
data.landsat = data.frame(band1 = c(), x = c(), y = c(), band = c(), kp = c())
bands = c("B2","B3", "B4", "B5", "B6", "B7")
for(land in list.land){
  setwd(file.path("A:/MAKA/KARU/pildid/Kagu-Eesti/2018", land))
  options(stringsAsFactors = FALSE)
  all_landsat = list.files(pattern = ".jp2$",full.names = TRUE)
  for(j in 1:length(all_landsat)){
    land.j = readGDAL(all_landsat[j])
    brick.j = brick(land.j)
    crp.j = crop(brick.j, ext)
    if(is.na(maxValue(crp.j))){
      bind = F
      next
    }
    spts.j = rasterToPoints(crp.j, spatial = TRUE)
    dfx.j = as.data.frame(spTransform(spts.j, CRS(llprj)))
    dfx.j$band = rep(bands[j],dim(dfx.j)[1])
    dfx.j$kp = str_sub(land, start = length(land)-9)
    print(head(dfx.j)); bind = T
    if(bind == T){data.landsat = rbind(data.landsat, dfx.j)}
  }
}

names(data.landsat) = c("value","x","y", "band", "kp")
#test = data.landsat[data.landsat$value == 0,]
#data.landsat[data.landsat$value == 0,]$value = NA
require(reshape2)
land.wide = dcast(data.landsat, x+y+kp ~ band)
land.wide$satel = "LC08"
hw = head(land.wide);hw


#sentinel
setwd("A:/MAKA/KARU/pildid/Kagu-Eesti/2018")
list.sent = list.dirs(full.names = T, recursive = T)
list.sent  = list.sent[ grepl("S2", list.sent) ]
ext = c(669400,685000,6460000,6472500)
llprj <-     "+proj=lcc +lat_1=59.33333333333334 +lat_2=58 +lat_0=57.51755393055556 +lon_0=24 +x_0=500000 +y_0=6375000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs "
#eraldistes: +proj=lcc +lat_1=59.33333333333334 +lat_2=58 +lat_0=57.51755393055556 +lon_0=24 +x_0=500000 +y_0=6375000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs
bands = c("B02","B03", "B04", "B05", "B06", "B07", "B08", "B11", "B12")
for(sent in list.sent){
  setwd(file.path("A:/MAKA/KARU/pildid/Kagu-Eesti/2018", sent))
  options(stringsAsFactors = FALSE)
  all_sentinel = list.files(pattern = ".jp2$",full.names = TRUE)
  for(j in 1:length(all_sentinel)){
    sent.j = readGDAL(all_sentinel[j])
    brick.j = brick(sent.j)
    crp.j = crop(brick.j, ext)
    if(is.na(maxValue(crp.j))){
      bind = F
      next
    }
    spts.j = rasterToPoints(crp.j, spatial = TRUE)
    dfx.j = as.data.frame(spTransform(spts.j, CRS(llprj)))
    dfx.j$band = rep(bands[j],dim(dfx.j)[1])
    dfx.j$kp = str_sub(sent, start = length(sent)-9)
    print(head(dfx.j)); bind = T
    if(bind == T){data.sentinel = rbind(data.sentinel, dfx.j)}
  }
}

names(data.sentinel) = c("value","x","y", "band", "kp")
test = data.sentinel[data.sentinel$value == 0,] #umbes kolmandik...
data.sentinel[data.sentinel$value == 0,]$value = NA
data.sentinel = na.omit(data.sentinel)

sent.wide = dcast(data.sentinel, x+y+kp ~ band)
sent.wide$satel = "S2"
hw = head(sent.wide)

table(data.sentinel$kp)
table(data.landsat$kp)
#iga piksli kohta vähemalt 9 pilti
#2017 ka siia juurde!
#ja siis veel mullakaarti vaja!

######
require(stringr)
load.pic = function(dir, sat){
  setwd(dir)
  list.sent = list.dirs(full.names = T, recursive = T)
  list.sent  = list.sent[ grepl(sat, list.sent) ]
  ext = c(669400,685000,6460000,6472500)
  llprj <-     "+proj=lcc +lat_1=59.33333333333334 +lat_2=58 +lat_0=57.51755393055556 +lon_0=24 +x_0=500000 +y_0=6375000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs "
  if(sat == "S2"){bands = c("B02","B03", "B04", "B05", "B06", "B07", "B08", "B11", "B12")}
  else{bands = c("B2","B3", "B4", "B5", "B6", "B7")}
  data.sentinel = data.frame(band1 = c(), x = c(), y = c(), band = c(), kp = c())
  for(sent in list.sent){
    setwd(file.path(dir, sent))
    options(stringsAsFactors = FALSE)
    all_sentinel = list.files(pattern = ".jp2$",full.names = TRUE)
    for(j in 1:length(all_sentinel)){
      sent.j = readGDAL(all_sentinel[j])
      brick.j = brick(sent.j)
      crp.j = crop(brick.j, ext)
      if(is.na(maxValue(crp.j)) | maxValue(crp.j) == 0){
        bind = F
        next
      }
      spts.j = rasterToPoints(crp.j, spatial = TRUE)
      dfx.j = as.data.frame(spTransform(spts.j, CRS(llprj)))
      dfx.j$band = rep(bands[j],dim(dfx.j)[1])
      dfx.j$kp = str_sub(sent, start = length(sent)-9)
      print(head(dfx.j)); bind = T
      if(bind == T){data.sentinel = rbind(data.sentinel, dfx.j)}
    }
  }
  names(data.sentinel) = c("value","x","y", "band", "kp")
  if(length(data.sentinel[data.sentinel$value == 0,]$value) > 0){
    data.sentinel[data.sentinel$value == 0,]$value = NA
  }
  data.sentinel = na.omit(data.sentinel)
  sent.wide = dcast(data.sentinel, x+y+kp ~ band)
  sent.wide$satel = sat
  sent.wide
}

sent.wide.17 = load.pic(dir = "A:/MAKA/KARU/pildid/Kagu-Eesti/2017", sat = "S2")
sent.wide.18 = load.pic(dir = "A:/MAKA/KARU/pildid/Kagu-Eesti/2018", sat = "S2")
land.wide.17 = load.pic(dir = "A:/MAKA/KARU/pildid/Kagu-Eesti/2017", sat = "LC08")
land.wide.18 = load.pic(dir = "A:/MAKA/KARU/pildid/Kagu-Eesti/2018", sat = "LC08")

table(sent.wide.17$kp)
table(sent.wide.18$kp)
table(land.wide.17$kp)
table(land.wide.18$kp)

#Vähemalt 12 pilti iga piksli kohta!
hw = head(sent.wide.18)

landsat_luunja_vonnu = rbind(land.wide.17, land.wide.18)
sentinel_luunja_vonnu = rbind(sent.wide.17, sent.wide.18)
#setwd("A:/MAKA/TEST/test/kaart")
#save(landsat_luunja_vonnu, file = "landsat_luuna_vonnu.RData")
#save(sentinel_luunja_vonnu, file = "sentinel_luuna_vonnu.RData")


#metsaregister
setwd("A:/MAKA/KARU/metsaregister")
eraldis_era = readOGR(dsn = ".", layer = "eraldis")
eraldis_riik = readOGR(dsn = ".", layer = "eraldis_riik")
eraldis = rbind(eraldis_era, eraldis_riik)

setwd("A:/MAKA/KARU/metsaregister/plva")
eraldis_era = readOGR(dsn = ".", layer = "eraldis")
eraldis_riik = readOGR(dsn = ".", layer = "eraldis_riik")
eraldis_plva = rbind(eraldis_era, eraldis_riik)

eraldis = rbind(eraldis, eraldis_plva)

# eraldis.raster = rasterize(eraldis)
# he = head(eraldis,1000)
# nms = names(he);nms[4] = "TUNNUS";names(he) = nms
# eraldis.1 = eraldis[1]
# eraldis.raster = rasterize(eraldis.1)

eraldis.points <- spTransform(eraldis, CRS(llprj)) #landsati coord. ref
#extent siia peale
eraldis.ext = crop(eraldis.points, ext)

#plotime punasega katastripiirid peale:
setwd("A:/MAKA/KARU/pildid/Kagu-Eesti/2018/LC08_185019_20180512")
all_landsat <- list.files(pattern = ".jp2$",full.names = TRUE)
land1 <- readGDAL(all_landsat[1])
land2 <- readGDAL(all_landsat[2])
land3 <- readGDAL(all_landsat[3])
land = cbind(land1,land2,land3)
#ext = c(493800, 500000, 6465000, 6468800) #mis see on? kust see tuli!?
ext = c(669400,685000,6460000,6472500) #kumb siis ikkagi õige on!?
land.br = brick(land)
crp = crop(land.br, ext)
par(col.axis = "white", col.lab = "white", tck = 0)
plotRGB(crp,
        r = 3, g = 2, b = 1,
        stretch = "lin",
        axes = TRUE,
        main = "RGB komposiitpilt. Melliste-Võnnu ümbrus")
box(col = "white")
par(new=TRUE)
plot(eraldis.ext, border= "red", lwd=.01, add = T)



require(rgeos)
inter = gIntersection(eraldis.ext, byid=T, drop_not_poly=T)
#tundub, et metsaregistris pole kõiki eraldisi

#plotime kevadise fenoloogilise erinevuse ka
#sent 10-05-2018 ja 30-05-2018; need sama trajektooriga ka!
require(rgdal)
options(stringsAsFactors = FALSE)
setwd("A:/MAKA/KARU/pildid/Kagu-Eesti/2018/S2AL1C_R036_20180510")
sent1 <- list.files(pattern = ".jp2$",full.names = TRUE)
sent11 = readGDAL(sent1[1]);sent12 = readGDAL(sent1[2]);sent13 = readGDAL(sent1[3])
setwd("A:/MAKA/KARU/pildid/Kagu-Eesti/2018/S2AL1C_R036_20180530")
sent2 <- list.files(pattern = ".jp2$",full.names = TRUE)
sent21 = readGDAL(sent2[1]);sent22 = readGDAL(sent2[2]);sent23 = readGDAL(sent2[3])
#sent11 = crop(sent11, ext);sent12 = crop(sent12, ext);sent13 = crop(sent13, ext)
#csent = cbind(sent21 - sent11, sent22 - sent12, sent23 - sent13)
#ext = c(493800, 500000, 6465000, 6468800) #no kust see...
ext = c(669400,685000,6460000,6472500)
brick.1 = brick(cbind(sent11,sent12,sent13)) #näikse, et sentinel ja landsatil on samad coord. ref
crp.1 = crop(brick.1, ext)
brick.2 = brick(cbind(sent21,sent22,sent23))
crp.2 = crop(brick.2, ext)
crp = crp.2-crp.1
par(col.axis = "white", col.lab = "white", tck = 0)
plotRGB(crp,
        r = 3, g = 2, b = 1,
        stretch = "lin",
        axes = TRUE,
        main = "Kevadine fenoloogia. RGB komposiitpilt. Melliste-Võnnu ümbrus")
box(col = "white")








#katastrikaart
setwd("A:/MAKA/KARU")
options(stringsAsFactors = FALSE)
x <- read.dbf("SHP_KATASTRIYKSUS.dbf")
xx = head(x,1000)
table(x$SIHT1)
xxx = xx[xx$SIHT1 == "Maatulundusmaa",]
#siin eraldi "mets" ja "metsata metsamaa" ei ole...
#kataster ja mr-register:
test = x[x$TUNNUS == "12601:003:0418",]
xxx = merge(he,x, all.x = T, by = "TUNNUS")



# MULLAKAART #


require(rgdal)
require(raster)
options(stringsAsFactors = FALSE)
setwd("A:/MAKA/muld")
muld = readOGR(dsn = ".", layer = "Mullakaart", encoding = "ISO-8859-15", use_iconv = FALSE) #4.1GB, foook
muld = spTransform(muld, CRS(llprj))
#ext = c(493800, 500000, 6465000, 6468800)#aga see ei pruugi korrektne olla; nimelt satikatel pärast projektsiooni muutmist:
#nhj, kust ma üldse selle c(493800, .... ) võtsin...
ext = c(669400,685000,6460000,6472500)
#ext = c(6460012, 6472487, 669412.5, 684987.5)
#landsati orig proj. oli:
proj.landsat = "+proj=lcc +lat_1=59.33333333333334 +lat_2=58 +lat_0=57.51755393055556 +lon_0=24 +x_0=500000
+y_0=6375000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
m1 = muld[700001:766627,];m1 = spTransform(m1, CRS(proj.landsat))
m1.crop = crop(m1,ext)
#tst = spTransform(m1.crop, CRS(llprj)) #testime, kas nüüd on sama ext, mis landsati datal lõpuks on
for(i in 1:7){
  i0 = (i-1)*100000 + 1
  muld.i = muld[i0:(i*100000),]
  muld.i = spTransform(muld.i, CRS(proj.landsat)) #äkki see on ka loopis mõistlikum teha väikeste tükkidena!?
  m.crop = crop(muld.i,ext)
  m1.crop = rbind(m1.crop,m.crop,makeUniqueIDs = TRUE)
  m1.crop = spTransform(m1.crop, CRS(llprj))
}

m1.crop #4020 ala
llprj <-  "+proj=lcc +lat_1=59.33333333333334 +lat_2=58 +lat_0=57.51755393055556 +lon_0=24 +x_0=500000 +y_0=6375000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs "
df.muld = as.data.frame(spTransform(m1.crop, CRS(llprj))) # ENNE CROPPI TULEB projektsioon õigeks muuta!????
setwd("A:/MAKA/TEST/test/kaart")
#save(df.muld, file = "muld_kaardi_jaoks.RData")
#save(m1.crop, file = "muld_kaardi_jaoks_shape.RData")

# for (col in colnames(mydataframe)){
#   Encoding(mydataframe[[col]]) <- "UTF-8"}


Encoding(df.muld$Siffer) = "UTF-8" #pekkis

table(muld$Sif1)
tst = muld[muld$Siffer == "AMÿÿ",]

#mulla kodeering:
m10 = c("Kh", "E3k", "Kr")
m11 = c("E2k", "K", "K(g)", "Kr(g)")
m13 = c("Kg", "Khg", "Krg")
m14 = c("L(k)", "Gk", "Gh", "Gh1", "Gkr", "Gr")
m16 = c("AMÿ", "Mÿ", "Ma\u0080\u0099")
m21 = c("G(o)", "Gk1", "Go", "Gor", "Ko", "Ko(g)", "Kog", "Kor", "Korg")
m31 = c("E2o", "E3o", "KI")
m37 = c("AMÿÿ", "M", "Mÿÿ","Ma\u0080\u0099a\u0080\u0099")
m42 = c("D", "D(g)", "KI(g)", "KIg", "LP", "LPe")
m43 = c("Dg", "LP(g)", "LPg")
m44 = c("DG", "LkG", "LPG")
m45 = c("GI1", "G1", "GI", "Go1", "GoI", "Pp")
m48 = c("Ag", "AG", "AG1", "AMÿÿ", "Ar", "ArG", "Mÿÿÿ","Ma\u0080\u0099a\u0080\u0099a\u0080\u0099","Ma\u0080\u0099a\u0080\u0099a\u0080\u0099a","Ma\u0080\u0099a\u0080\u0099a\u0080\u0099Aµ")
m51 = c("LkII", "E2I", "E3I", "Lk", "LkI", "LkI(g)", "LkII", "LkII(g)", "LkIII")
m53 = c("Lk(g)", "Lkg", "LkIg", "LkIIg", "LkIII(g)", "LkIIIg")
m57 = c("Rÿ", "S", "Sÿ", "Sÿÿ","Sa\u0080\u0099","Sa\u0080\u0099a\u0080\u0099")
m61 = c("L", "L(k)", "L(k)I", "L(k)II", "L(k)III", "LI", "LII", "LIII")
m63 = c("L(k)g", "L(k)Ig", "L(k)IIg", "L(k)IIIg")
m64 = c("LG", "LG1")
m73 = c("Lg", "LIg", "LIIg", "LIIIg")
m77 = c("R", "Rÿÿ", "Rÿÿÿ", "Sÿÿÿ","Sa\u0080\u0099a\u0080\u0099a\u0080\u0099","Ra\u0080\u0099a\u0080\u0099a\u0080\u0099","Sa\u0080\u0099a\u0080\u0099a\u0080\u0099Aµ")
m109 = c("Arv", "Av")
list.mullakood = list(m10,m11,m13,m14,m16,m21,m31,m37,m42,m43,m44,m45,m48,m51,m53,m57,m61,m63,m64,m73,m77,m109)
kood = c(10,11,13,14,16,21,31,37,42,43,44,45,48,51,53,57,61,63,64,73,77,109)
df.muld$kood = NA
#kas on vaja üldse data.frame'iks teha!?

for(i in 1:22){
  if(dim(df.muld[df.muld$Siffer %in% list.mullakood[[i]],])[1] > 0){
  df.muld[df.muld$Siffer %in% list.mullakood[[i]],]$kood = kood[i]}
}

muld.sp = spTransform(m1.crop, CRS(llprj)) #see ei teeju midagi...? või on siiski mullakaart teise projektsiooniga!!!???
muld.sp$kood = df.muld$kood

#save(muld.sp, file = "muld_koodiga.RData")
setwd("A:/MAKA/TEST/test/kaart")
load(file = "muld_koodiga.RData")

#
#https://gis.stackexchange.com/questions/133625/checking-if-points-fall-within-polygon-shapefile
library(rgeos)
library(sp)
library(rgdal)
df = land.wide.18[land.wide.18$kp == 20180510,]
coordinates(df) <- ~ x + y
#crs(df) = llprj

#

proj4string(df) <- proj4string(muld.sp) #llprj

#satikatel: 669412.5, 684987.5, 6460012, 6472487
#mullal: 493800, 500000, 6465000, 6468800

df.over = over(df,muld.sp)
#siit saab nüüd lõpuks mullakoodid kätte!!!
df$muld = df.over$kood

table(df$muld)
df = data.frame(df)
df[is.na(df$muld),]$muld = 999
table(df$muld) #krt kolmandikul ikka muld puudu :S :S :S
hdf = head(df)
dfm = df[,c("x","y","muld")]
save(dfm, file = "muld_ja_koordinaadid_luunja_vonnu.RData")

#minunurk:
summary(df$x);summary(df$y)
#võtaks x 675000, 684988; y 6468000, 6472487
land.luunja = landsat_luunja_vonnu[landsat_luunja_vonnu$x > 679988 & landsat_luunja_vonnu$y > 6467487,] #5000x5000; pikslites ~557*557?
land.luunja = merge(land.luunja, dfm, all.x = T, by = c("x","y"))

#siia veel üks mullateisendus peale, vastavalt sellele, mis mudelis oli!
setwd("A:/MAKA/TEST/test")
sent455 = read.csv("sentinel455.csv")
land455 = read.csv("landsat455.csv")
muld0 = unique(sent455$muld)
muld0

table(land.luunja$muld)
land.luunja[!(land.luunja$muld %in% c(45,77,21,42,43,51,48,37,44,61,64,53)),]$muld = 999
table(land.luunja$muld)

landsat_luunja_vonnu_muld = merge(landsat_luunja_vonnu, dfm, all.x = T, by = c("x","y"))
sentinel_luunja_vonnu_muld = merge(sentinel_luunja_vonnu, dfm, all.x = T, by = c("x","y"))
landsat_luunja_vonnu_muld[!(landsat_luunja_vonnu_muld$muld %in% c(45,77,21,42,43,51,48,37,44,61,64,53)),]$muld = 999
sentinel_luunja_vonnu_muld[!(sentinel_luunja_vonnu_muld$muld %in% c(45,77,21,42,43,51,48,37,44,61,64,53)),]$muld = 999

setwd("A:/MAKA/TEST/test/kaart")
save(landsat_luunja_vonnu_muld, file = "landsat_luuna_vonnu_muld.RData")
save(sentinel_luunja_vonnu_muld, file = "sentinel_luuna_vonnu_muld.RData")

dfm1 = dfm
dfm1[!(dfm1$muld %in% c(45,77,21,42,43,51,48,37,44,61,64,53)),]$muld = 999
muld.l = dcast(dfm1, x+y~muld,fun.aggregate = function(x){as.integer(length(x) > 0)})
landsat_luunja_vonnu_muld_dummy = merge(landsat_luunja_vonnu_muld, muld.l, all.x = T, by = c("x","y"))
sentinel_luunja_vonnu_muld_dummy = merge(sentinel_luunja_vonnu_muld, muld.l, all.x = T, by = c("x","y"))

landsat_luunja_vonnu_muld_dummy$kp = as.Date(landsat_luunja_vonnu_muld_dummy$kp, "%Y%m%d")
sentinel_luunja_vonnu_muld_dummy$kp = as.Date(sentinel_luunja_vonnu_muld_dummy$kp, "%Y%m%d")

as.character(unique(landsat_luunja_vonnu_muld_dummy$kp)) %in% unique(land455$kp)
as.character(unique(sentinel_luunja_vonnu_muld_dummy$kp)) %in% unique(sent455$kp)
#kokku saame 6 + 9 = 15 pilti... vähe :S

landsat_luunja_vonnu_muld_dummy$kp = as.character(landsat_luunja_vonnu_muld_dummy$kp)
sentinel_luunja_vonnu_muld_dummy$kp = as.character(sentinel_luunja_vonnu_muld_dummy$kp)

ld = landsat_luunja_vonnu_muld_dummy[landsat_luunja_vonnu_muld_dummy$kp %in% unique(land455$kp),]
sd = sentinel_luunja_vonnu_muld_dummy[sentinel_luunja_vonnu_muld_dummy$kp %in% unique(sent455$kp),]
ld$"37" = 0;ld$"77" = 0
sd$"37" = 0;sd$"77" = 0

write.csv(ld, "landsat_kaart.csv")
write.csv(sd, "sentinel_kaart.csv")
save(ld, file = "landsat_luuna_vonnu_muld_dummy_kp_oiged.RData")
save(sd, file = "sentinel_luuna_vonnu_muld_dummy_kp_oiged.RData")


write.csv(ld[1:1000,], "landsat_kaart1000.csv")

#kas kuupäevad klapivad!?
unique(land455$kp)
unique(landsat_luunja_vonnu_muld_dummy$kp)

as.character(unique(ld$kp)) %in% unique(land455$kp)
as.character(unique(sd$kp)) %in% unique(sent455$kp)


setwd("A:/MAKA/TEST/test/kaart")
land.cart = read.csv("KAART_landsat.csv")
#kuupäevad ja koordinaadid paika
kpcart = as.factor(unique(land.cart$kp))

ldx = ld;
ldx = ld[match(kpcart, ld$kp),]
require(gdata)
install.packages("gdata")
ldx$kp = as.factor(ldx$kp)
ldx$kp <- reorder.factor(ldx$kp, new.order=kpcart)
ldx = ldx %>% arrange(kp)

land.cart$x = ldx$x;land.cart$y = ldx$y

dfmean = land.cart[,c(1:4,6,7)] %>% group_by(.dots=c("x","y")) %>% summarise_all(funs(mean))
head(dfmean)
coordinates(dfmean) = ~ x + y
proj4string(dfmean) <- llprj
plot(dfmean$MA)

save(dfmean, file = "CART_prognoos_mean.RData")






























