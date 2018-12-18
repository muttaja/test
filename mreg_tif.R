#metsaregister read TIF

require(raster)
setwd("A:/MAKA/d2_13.11.2018_esimene_andmekaust_nimi_korrastamata/naidised/mreg")
mreg = read.csv("eraldised2018.taks_SAT.koos.csv")
setwd("A:/MAKA/d2_13.11.2018_esimene_andmekaust_nimi_korrastamata/mreg")


options(stringsAsFactors = FALSE)

list.files()
all_tifs <- list.files(pattern = ".tif$",full.names = TRUE)

haab <- raster(all_tifs[1])
kask <- raster(all_tifs[4])
kuusk <- raster(all_tifs[5])
mand <- raster(all_tifs[9])
muu <- raster(all_tifs[6])
sanglepp <- raster(all_tifs[7])
lepp <- raster(all_tifs[8])


# xy = cbind(seq(640000,645000, by = 200),  rep(6385500,26))
# result <- extract(mand, xy)
# result
# coordinates(mand)[result[,2],]

###

intsct = intersect(mets_raie$SID,koos$SID); intsct = intersect(intsct, SID_temp)
coords = koos[koos$SID %in% intsct, c("koord_e", "koord_n")]

mannid <- extract(mand, coords) / 100
kuused <- extract(kuusk, coords) / 100
kased <- extract(kask, coords) / 100
haavad <- extract(haab, coords) / 100
lepad <- extract(lepp, coords) / 100
sanglepad <- extract(sanglepp, coords) / 100
muud <- extract(muu, coords) / 100

HINNANG_TODE_MREG = data.frame(
  cbind(PROPS_HINNANG_JA_TODE, mannid, kuused, kased, haavad, lepad, sanglepad, muud)
)


names(HINNANG_TODE_MREG) = c("SID",paste(rep(c("MA", "KU", "KS", "HB", "LV", "LM", "KX"),3),
                                         rep(c("HINNANG","TODE","MREG"), each = 7), sep = "."))

dat = HINNANG_TODE_MREG



setwd("A:/MAKA/")
png(filename="MREG.png")
par(mfrow=c(3,3))
plot(dat$MA.MREG, dat$MA.TODE)
plot(dat$KU.MREG, dat$KU.TODE)
plot(dat$KS.MREG, dat$KS.TODE)
plot(dat$HB.MREG, dat$HB.TODE)
plot(dat$LV.MREG, dat$LV.TODE)
plot(dat$LM.MREG, dat$LM.TODE)
plot(dat$KX.MREG, dat$KX.TODE)
dev.off()

png(filename="KNN_HINNANG.png")
par(mfrow=c(3,3))
plot(dat$MA.HINNANG, dat$MA.TODE)
plot(dat$KU.HINNANG, dat$KU.TODE)
plot(dat$KS.HINNANG, dat$KS.TODE)
plot(dat$HB.HINNANG, dat$HB.TODE)
plot(dat$LV.HINNANG, dat$LV.TODE)
plot(dat$LM.HINNANG, dat$LM.TODE)
plot(dat$KX.HINNANG, dat$KX.TODE)
dev.off()

#võtaks praegu ainult need, kus tüvemahtu vähemalt 100. Raiesmikule pole mõtet liigilist koosseisu hinnata;
SID100 = koos[koos$arv_maht_es > 100, "SID"]
dat = HINNANG_TODE_MREG[HINNANG_TODE_MREG$SID %in% SID100,]

setwd("A:/MAKA/")
png(filename="MREG_100.png")
par(mfrow=c(3,3))
plot(dat$MA.MREG, dat$MA.TODE)
plot(dat$KU.MREG, dat$KU.TODE)
plot(dat$KS.MREG, dat$KS.TODE)
plot(dat$HB.MREG, dat$HB.TODE)
plot(dat$LV.MREG, dat$LV.TODE)
plot(dat$LM.MREG, dat$LM.TODE)
plot(dat$KX.MREG, dat$KX.TODE)
dev.off()

png(filename="KNN_HINNANG_100.png")
par(mfrow=c(3,3))
plot(dat$MA.HINNANG, dat$MA.TODE)
plot(dat$KU.HINNANG, dat$KU.TODE)
plot(dat$KS.HINNANG, dat$KS.TODE)
plot(dat$HB.HINNANG, dat$HB.TODE)
plot(dat$LV.HINNANG, dat$LV.TODE)
plot(dat$LM.HINNANG, dat$LM.TODE)
plot(dat$KX.HINNANG, dat$KX.TODE)
dev.off()

#väikseid osakaale peaks vähendama ja suuri suurendama

#Siia juurde ekspertvalik:
EKSPERTH_HINNANG_TODE_MREG = data.frame(
  cbind(PROPS_HINNANG_JA_TODE,EKSPERTHINNANG_RAIE_JA_TODE1, mannid, kuused, kased, haavad, lepad, sanglepad, muud)
)


names(EKSPERTH_HINNANG_TODE_MREG) = c("SID",paste(rep(c("MA", "KU", "KS", "HB", "LV", "LM", "KX"),4),
                                         rep(c("EKS_VALIK","HINNANG","TODE","MREG"), each = 7), sep = "."))

dat = EKSPERTH_HINNANG_TODE_MREG
png(filename="EKSPERT_KNN_HINNANG.png")
par(mfrow=c(3,3))
plot(dat$MA.EKS_VALIK, dat$MA.TODE)
plot(dat$KU.EKS_VALIK, dat$KU.TODE)
plot(dat$KS.EKS_VALIK, dat$KS.TODE)
plot(dat$HB.EKS_VALIK, dat$HB.TODE)
plot(dat$LV.EKS_VALIK, dat$LV.TODE)
plot(dat$LM.EKS_VALIK, dat$LM.TODE)
plot(dat$KX.EKS_VALIK, dat$KX.TODE)
dev.off()

dat = dat[dat$SID %in% SID100,]
png(filename="EKSPERT_KNN_HINNANG_100.png")
par(mfrow=c(3,3))
plot(dat$MA.EKS_VALIK, dat$MA.TODE)
plot(dat$KU.EKS_VALIK, dat$KU.TODE)
plot(dat$KS.EKS_VALIK, dat$KS.TODE)
plot(dat$HB.EKS_VALIK, dat$HB.TODE)
plot(dat$LV.EKS_VALIK, dat$LV.TODE)
plot(dat$LM.EKS_VALIK, dat$LM.TODE)
plot(dat$KX.EKS_VALIK, dat$KX.TODE)
dev.off()






