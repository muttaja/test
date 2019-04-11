#cart prognoosid vaid ¸he pildi pıhjal, seej‰rel keskmine
#sat_sep_koos failist andmestike_sisselugemine_koos

koos601 = sat_sep_koos[sat_sep_koos$aproovitykk_id %in% sidxx,] #test
koos601 = koos601[koos601$band == "B02",] #kui on ¸ks kanal, siis on kıik

compcases = rowSums(table(koos601$aproovitykk_id, koos601$kp)) - rowSums(table(na.omit(koos601)$aproovitykk_id, na.omit(koos601)$kp)) 
table(compcases)
compcases[compcases == 7]
#15 erinevat pilti
#47 juhul vaid 1 pilt, neeb vıibolla v‰lja vıtta?

#aga piltide kaupa?
comppilt = rowSums(table(na.omit(koos601)$kp, na.omit(koos601)$aproovitykk_id))
table(comppilt)
comppilt[comppilt == 601]
#2017-05-02 2017-09-24 2018-05-10 2018-09-19  neil kuup‰evadel K’IK pildid olemas!

#kui 2 kehvemat pilti v‰lja vıtta, siis palju vaatlusi j‰‰b?
k1 = koos601[!(koos601$kp %in% comppilt[comppilt %in% c(100,182)]),]

ck1 = rowSums(table(k1$aproovitykk_id, k1$kp)) - rowSums(table(na.omit(k1)$aproovitykk_id, na.omit(k1)$kp)) 
table(ck1)

ck1[ck1 == 7] #124915, siin 7

#227 + 135 + 30 + 14 +1
#407 juhul v‰hemalt 4 pilti!

#
koos_wide = sat_sep_koos %>% group_by(aproovitykk_id, band, kp) %>% dcast(aproovitykk_id + kp + satel ~ band, value.var="value")
kw601 = koos_wide[koos_wide$aproovitykk_id %in% sidxx,]
m601 = data.frame(muld = koos[koos$aproovitykk_id %in% sidxx,]$muld,aproovitykk_id = sidxx, MA = koos[koos$aproovitykk_id %in% sidxx,]$MA, KU = koos[koos$aproovitykk_id %in% sidxx,]$KU, KS = koos[koos$aproovitykk_id %in% sidxx,]$KS, MUU = koos[koos$aproovitykk_id %in% sidxx,]$KX1)
m601[m601$muld %in% names(table(m601$muld) < 10)[table(m601$muld) < 10],"muld"] = 999
m601[is.na(m601$muld),] = 999
muld2 = dcast(m601[,c("aproovitykk_id", "muld")],aproovitykk_id~muld,fun.aggregate = function(x){as.integer(length(x) > 0)})
kw601 = merge(kw601,m601, all.x = T)
kw601 = merge(kw601,muld2, by = "aproovitykk_id")
kw601 = kw601[,-11] #B10 on v‰‰rakas
sentinel601 = kw601[kw601$satel == "S2",]
save(kw601, file = "kw601.RData")
write.csv(kw601, file = "kw601.csv")
write.csv(sentinel601, file = "sentinel601.csv")

#599 j‰‰b p‰rast na.omit

#sent_id = na.omit(sentinel601)
#sent_id = unique(sent_id$aproo )

#numpsik pythonist :)

#require(RcppCNPy)
#numpsik <- npyLoad("numpsik.npy")

#NB! 599 vaatlust!

pbp = read.csv("cart_d5m8.csv")
pbp[,1:4] = pbp[,1:4] / 100
pbp[,1:4] = pbp[,1:4] / rowSums(pbp[,1:4])
nms = names(pbp); nms[5] = "aproovitykk_id"; names(pbp) = nms

dpp = cbind(data_puud_raie_props, sidxx)
nms = names(dpp); nms[5] = "aproovitykk_id"; names(dpp) = nms

ppp = merge(pbp, dpp, by = "aproovitykk_id", all.x = T)
#hist(pbp[,4])
dev.off()
par(mfrow = c(2,2))
plot(ppp[,6],ppp[,2])
plot(ppp[,7],ppp[,3])
plot(ppp[,8],ppp[,4])
plot(ppp[,9],ppp[,5])

#points(ppp[,6],ppp[,2], col = "dark red", cex  = .5)

#kui mitu korda sama pilt l‰bi jooksutada:

c1 = read.csv("cartx1.csv")
c2 = read.csv("cartx2.csv")
c3 = read.csv("cartx3.csv")
c1[1,];c2[1,];c3[1,]

cik = read.csv("cart_id_kp.csv")
#test = cbind(cik, cik1)
#mingi skoor avutada igale pildile selle pıhjal, palju parameetrite muutmine proportsioone muudab!?

#plotime iga kuup‰eva prognoosid

par(mfrow = c(4, 4))
length(unique(cik$KP)) #15


nms = names(dpp); nms[5] = "ID"; names(dpp) = nms

cik = read.csv("cart_id_kp_msl5_scaled.csv")
kpd = unique(cik$KP)
nms = names(cik);nms[5] = "aproovitykk_id";names(cik) = nms

dev.off()
par(mfrow = c(4, 4))
for (i in 1:15) {
  kp = kpd[i]
  kp_data = cik[cik$KP == kp,]
  kp_data = merge(kp_data, taks.info, all.x = T, by = "aproovitykk_id")
  print(dim(kp_data))
  plot(kp_data[,12],kp_data[,2], main = paste("M‰nd_", kp), xlim = c(0,1), ylim = c(0,100))
}

dev.off()
par(mfrow = c(4, 4))
for (i in 1:15) {
  kp = kpd[i]
  kp_data = cik[cik$KP == kp,]
  kp_data = merge(kp_data, taks.info, all.x = T, by = "aproovitykk_id")
  print(dim(kp_data))
  plot(kp_data[,13],kp_data[,3], main = paste("Kuusk_", kp), xlim = c(0,1), ylim = c(0,100))
}

dev.off()
par(mfrow = c(4, 4))
for (i in 1:15) {
  kp = kpd[i]
  kp_data = cik[cik$KP == kp,]
  kp_data = merge(kp_data, dpp, all.x = T)
  print(dim(kp_data))
  plot(kp_data[,9],kp_data[,4], main = paste("Kask_", kp), xlim = c(0,1), ylim = c(0,100))
}

dev.off()
par(mfrow = c(4, 4))
for (i in 1:15) {
  kp = kpd[i]
  kp_data = cik[cik$KP == kp,]
  kp_data = merge(kp_data, dpp, all.x = T)
  print(dim(kp_data))
  plot(kp_data[,10],kp_data[,5], main = paste("Muu_", kp), xlim = c(0,1), ylim = c(0,100))
}

rowSums(kp_data[,7:10])

#NO KURAT ƒKKI MINGID INDEKSID IKKA SASSIS? Vıi asi selles, et andmed pole skaleertitud?
#sentinel601sc = sentinel601
#sentinel601sc[,4:19] = scale(sentinel601sc[,4:19])
#write.csv(sentinel601sc, file = "sentinel601sc.csv")
#Ses osas l‰ks pilt paremaks, et n¸¸d on prognoosid v‰hemalt kuni 100%-ni

miin = cik %>% group_by(ID) %>% summarise_all(funs(mean)) %>% data.frame()

dev.off()
par(mfrow = c(2,2))
plot(ppp[,6],miin[,2])
plot(ppp[,7],miin[,3])
plot(ppp[,8],miin[,4])
plot(ppp[,9],miin[,5])

#SUUR T’EHETK: kas mu MƒƒRAMATUS tegi imet?
cik = read.csv("cart_uncertainty_pic_by_pic_msl100.csv")

dev.off()
par(mfrow = c(4, 4))
for (i in 1:15) {
  kp = kpd[i]
  kp_data = cik[cik$KP == kp,]
  kp_data = merge(kp_data, dpp, all.x = T)
  print(dim(kp_data))
  plot(kp_data[,7],kp_data[,2], main = paste("M‰nd_", kp), xlim = c(0,1), ylim = c(0,1))
}

dev.off()
par(mfrow = c(4, 4))
for (i in 1:15) {
  kp = kpd[i]
  kp_data = cik[cik$KP == kp,]
  kp_data = merge(kp_data, dpp, all.x = T)
  print(dim(kp_data))
  plot(kp_data[,8],kp_data[,3], main = paste("Kuusk_", kp), xlim = c(0,1), ylim = c(0,1))
}

dev.off()
par(mfrow = c(4, 4))
for (i in 1:15) {
  kp = kpd[i]
  kp_data = cik[cik$KP == kp,]
  kp_data = merge(kp_data, dpp, all.x = T)
  print(dim(kp_data))
  plot(kp_data[,9],kp_data[,4], main = paste("Kask_", kp), xlim = c(0,1), ylim = c(0,1))
}

dev.off()
par(mfrow = c(4, 4))
for (i in 1:15) {
  kp = kpd[i]
  kp_data = cik[cik$KP == kp,]
  kp_data = merge(kp_data, dpp, all.x = T)
  print(dim(kp_data))
  plot(kp_data[,10],kp_data[,5], main = paste("Muu_", kp), xlim = c(0,1), ylim = c(0,1))
}


miin = cik %>% group_by(ID) %>% summarise_all(funs(mean)) %>% data.frame()
rowSums(miin[,2:5])

dev.off()
par(mfrow = c(2,2))
plot(ppp[,6],miin[,2])
plot(ppp[,7],miin[,3])
plot(ppp[,8],miin[,4])
plot(ppp[,9],miin[,5])

#kui eeldada, et osakaalud on sisuliselt normaaljaotusega, ja selle eelduse pıhjal vıtta keskmise arvutamisel
#ekstreemsed v‰‰rtused v‰lja!?

#RANDOM FOREST!
setwd("A:/MAKA/TEST")
cik = read.csv("rf_24.csv") #vıi ‰kki oli 69 mitte 24?

dev.off()
par(mfrow = c(4, 4))
for (i in 1:15) {
  kp = kpd[i]
  kp_data = cik[cik$KP == kp,]
  kp_data = merge(kp_data, dpp, all.x = T)
  print(dim(kp_data))
  plot(kp_data[,7],kp_data[,2], main = paste("M‰nd_", kp), xlim = c(0,1), ylim = c(0,1))
}

dev.off()
par(mfrow = c(4, 4))
for (i in 1:15) {
  kp = kpd[i]
  kp_data = cik[cik$KP == kp,]
  kp_data = merge(kp_data, dpp, all.x = T)
  print(dim(kp_data))
  plot(kp_data[,8],kp_data[,3], main = paste("Kuusk_", kp), xlim = c(0,1), ylim = c(0,1))
}

dev.off()
par(mfrow = c(4, 4))
for (i in 1:15) {
  kp = kpd[i]
  kp_data = cik[cik$KP == kp,]
  kp_data = merge(kp_data, dpp, all.x = T)
  print(dim(kp_data))
  plot(kp_data[,9],kp_data[,4], main = paste("Kask_", kp), xlim = c(0,1), ylim = c(0,1))
}

dev.off()
par(mfrow = c(4, 4))
for (i in 1:15) {
  kp = kpd[i]
  kp_data = cik[cik$KP == kp,]
  kp_data = merge(kp_data, dpp, all.x = T)
  print(dim(kp_data))
  plot(kp_data[,10],kp_data[,5], main = paste("Muu_", kp), xlim = c(0,1), ylim = c(0,1))
}


miin = cik %>% group_by(ID) %>% summarise_all(funs(mean)) %>% data.frame()

dev.off()
par(mfrow = c(2,2))
plot(ppp[,6],miin[,2])
plot(ppp[,7],miin[,3])
plot(ppp[,8],miin[,4])
plot(ppp[,9],miin[,5])
