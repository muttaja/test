### kNN, distantsid ###

require(FNN)
#dists = knn.cv(train = data4all[,3:6], cl = data4all$ENAMUS, k = 10) #algne asi
#nüüd alõpliku andmestiku põhjal:
#dists = knn.cv(train = data4all[,3:6], cl = data4all$ENAMUS, k = 10)
dists = knn.cv(train = mets2[,1:8], cl = mets2$cl, k = 7)

#tore oleks kuskil mignit põlve näha
dist1 = attr(dists,"nn.dist")
index1 = attr(dists,"nn.index")

#https://stackoverflow.com/questions/2018178/finding-the-best-trade-off-point-on-a-curve
#põhjal 
require(nnet)
elbow <- function(arg){
  y_values = cumsum(arg)
  x_values = 1:length(arg)
  max_df = data.frame(x = c(1,10), y = c(y_values[1],y_values[10])) 
  fit <- lm(max_df$y ~ max_df$x)
  distances = c()
  for(i in 1:10){
    distances <- c(distances, abs(coef(fit)[2]*x_values[i] - y_values[i] + coef(fit)[1]) / sqrt(coef(fit)[2]^2 + 1^2))
  }
  which.is.max(distances)
}

maxpoint = apply(dist1, 1, elbow)
hist(maxpoint)

#nüüd vastavate kaaludega cumsumist

propcumsum <- function(arg){
  maxpoint = elbow(arg)
  data = arg
  if(maxpoint != length(arg)){
    data[(maxpoint+1):length(arg)] = data[maxpoint+1]
  }
  props = 1 - (data/data[maxpoint+1])
  props = props/(sum(props))
  props
}

props = apply(dist1, 1, propcumsum1)
props = data.frame(t(props))

#nüüd korrutame lihtsalt läbi :)
#dataw
sum (dataw[index1[1,],]$ARV_VMA * props[1,])
sum (dataw[index1[1,],]$ARV_VKU * props[1,])

colSums(dataw[index1[1,],c("ARV_VMA", "ARV_VKU")] * props[1,])
#1. sid mänd ja kuusk

(dataw[index1,c("ARV_VMA", "ARV_VKU", "ARV_VKS", "ARV_VHB", "ARV_VLM", "ARV_VLV", "ARV_VXX")] * props)

#agr_data = 
puud = c("ARV_VMA", "ARV_VKU", "ARV_VKS", "ARV_VHB", "ARV_VLM", "ARV_VLV", "ARV_VKX") #NB! uues andmestikus ARV_VKX!
indxprops = cbind(index1, props)
#07.12.2018: kasutame nüüd täielist andmestikku;
data_puud = taks_uus[,puud]

agre <- function(arg,k){
  #"arg"-ist osa indexid, osa neile vastavad propsid
  indx = arg[1:k]; props = arg[(k+1):(2*k)]
  colsums = colSums(data_puud[indx,]*props)
  colsums
}

agre1 <- function(arg, data1){
  #"arg"-ist osa indexid, osa neile vastavad propsid
  indx = arg[1:10]; props = arg[11:20]
  colsums = colSums(data1[indx, puud]*props) #väljastab ka midagi:O?
}

AGRETUD = data.frame(round(t(apply(indxprops, 1, agre)),0))

HINNANG_JA_TODE = data.frame(cbind(AGRETUD, round(data_puud,0)))
#save(HINNANG_JA_TODE, file = "KNN_HINNANG_JA_TODE.RData")
#väga suured probleemid vägasuurte? tüvemahuga aladega!




#vecs1 = apply(indxprops, 1, function(x) agre1(x, dataw)) #milleks ma selle tegin?

###sama asi, aga terve andmestiku peal?
#dataw1 see andmestik, kust pole midagi välja võetud

PCA100 = prcomp(dataw1[,c(2:31,56:60)])
summary(PCA100)
cumvar = cumsum(sat.all$sdev**2)/sum(sat.all$sdev**2)
cumvar

pca4_100 = PCA100$x[,1:4]
data100 = cbind(dataw1[,c("SID", "ENAMUS")], pca4_100)
data100 = merge(data100, muld1, by = "SID", all.x = T)



KNN100 = knn.cv(train = data100[,3:6], cl = data100$ENAMUS, k = 10)

indx100 = attr(KNN100, "nn.index"); props100 = t(apply(attr(KNN100, "nn.dist"), 1, propcumsum))
indx100; props100

indxprops_all = cbind(indx100, props100)

KOIK_VEKTORID = t(apply(indxprops_all, 1, function(x) agre1(x,dataw1)))

ERINEVUSED = abs(KOIK_VEKTORID - dataw1[,puud]) #mõttetu töö

#proovime erinevat prop vektorit?
#see annab liiga võrdsed kaalud!
propcumsum1 <- function(arg){
  maxpoint = elbow(arg)
  data = arg
  props = c()
  if(maxpoint != length(arg)){
    data[(maxpoint+1):length(arg)] = 0
  }
  props[1:maxpoint] = 1 - (data/ sum(data)); props[(maxpoint+1):length(arg)] = 0
  props = props/(sum(props))
  props
}
  
props100x = t(apply(attr(KNN100, "nn.dist"), 1, propcumsum1))
round(props100x,3)

#raie arvesse võtmine:
SID_raie = koos[koos$raie_aeg1 == -1,"SID"]
mets_raie = mets2[mets2$SID %in% SID_raie,]
dim(mets2) #371
dim(mets_raie) #340

fun_agre = function(data, data_puud, nrcomp = 8)
  #nrcomp - mitu peakomponenti; lisasin selle hiljem
  {
  dists = knn.cv(train = data[,1:nrcomp], cl = data$cl, k = 10)
  dist1 = attr(dists,"nn.dist")
  index1 = attr(dists,"nn.index")
  props = apply(dist1, 1, propcumsum1) #erinevad proportsioonide vektorid võimalikud
  props = t(props)
  indxprops = cbind(index1, props)
  data_puud = data_puud
  data.frame(round(t(apply(indxprops, 1, agre)),0))
}

data_puud = taks_uus[taks_uus$SID %in% SID_raie,puud]
SID_raie1 = taks_uus[taks_uus$SID %in% SID_raie,"SID"]
HINNANG_RAIE = fun_agre(mets_raie, data_puud)
data_puud_raie = taks_uus[taks_uus$SID %in% SID_raie,puud]
HINNANG_RAIE_JA_TODE = data.frame(cbind(SID_raie1, HINNANG_RAIE, round(data_puud_raie,0)))

par(mfrow=c(3,2))
for(puu in puud[1:6]){
  plot(HINNANG_RAIE_JA_TODE[,puu], HINNANG_RAIE_JA_TODE[,paste(puu, ".1", sep = "")])
}

#92. rida? äkki toimunud raie?
#järelikult vaja mitmetasemelist protsessi, mis esmalt tunneb ära raie olemasolu (kui seda pole andmebaasi märgitud)?
#vaatame erinevusi: kas jäävad silma väga suured erinevused, kus hinnang on nulllilähedane, aga takseeris metsa rohkelt;
difs = rowSums(HINNANG_RAIE_JA_TODE[,9:15]) - rowSums(HINNANG_RAIE_JA_TODE[,2:8])
difs = data.frame(SID = SID_raie1, dif = difs, tode = rowSums(HINNANG_RAIE_JA_TODE[,9:15]), hinnang = rowSums(HINNANG_RAIE_JA_TODE[,2:8]))
hist(difs$dif, breaks = 20) #on mõned suured erindid!
difs[difs$dif > 500,] #500 asemel hinnangu põhjal saadud 0.9 vms kvantiil, eeldades, et erinevused on normaaljaotusega;
SID_dif500 = difs[difs$dif > 500, "SID"] 
tst = koos[koos$SID %in% SID_dif500,]
#võibolla ka need välja võtta, kus hinnang tuleb nullilähedane, aga nö tõde on suur?
hist(difs[difs$hinnang < 50,]$dif, breaks = 10) #on mõned suured erindid!
SID_hinnang50 = difs[(difs$hinnang < 50)&(difs$dif > 200) , "SID"] 
SID_hinnang50
tst1 = koos[koos$SID %in% SID_hinnang50,] #Need küll kõik raiutud :)

SID_hinnang100 = difs[((50 < difs$hinnang) & (difs$hinnang< 100))&(difs$dif > 200) , "SID"] 
SID_hinnang100
tst2 = koos[koos$SID %in% SID_hinnang100,]
#1. ei ole tõenäoliselt raiet tehtud, aga maastik on väga liigendatud
#2. on harvendusraie kindlasti tehtud, kas ka midagi muud?

SID_hinnang100_100 = difs[((difs$hinnang< 100))&(difs$dif > 100) , "SID"] 
SID_hinnang100_100
tst3 = koos[koos$SID %in% SID_hinnang100_100,]
#34262: liigendatud maastik
#70030: suht põllu lähedal
#70216: raie
#7124: erineva vanusega metsa piiril
#75950: harvendusraie
#76466: raie
#78290: raie mõned aastad tagasi
#78305 raie
#82137: raie
#110903: imelik raie, väga tugev harvendus vms
#112497: kaardi järgi väga kena mets. Põld ca 100m
#114449: liigendatud, rauitud umbmääraselt läheduses
#114777: kaardi järgi täpselt raie piiril
#611001: liigendatud maastik, taluhoov alla 100m
valja = c(SID_dif500, 70216, 75950, 76466, 78290, 78305, 82137,110903,114777)


#kui need välja võtta:
SID_temp = SID_raie[!(SID_raie %in% valja)]
data_puud = taks_uus[taks_uus$SID %in% SID_temp, puud]
SID_raie1 = taks_uus[taks_uus$SID %in% SID_temp,"SID"]
mets_raie1 = mets_raie[mets_raie$SID %in% SID_temp,]
HINNANG_RAIE = fun_agre(mets_raie1, data_puud)

data_puud_raie = taks_uus[taks_uus$SID %in% SID_temp,puud]
HINNANG_RAIE_JA_TODE = data.frame(cbind(SID_raie1, HINNANG_RAIE, round(data_puud_raie,0)))
par(mfrow=c(1,1))
plot(HINNANG_RAIE_JA_TODE$ARV_VMA, HINNANG_RAIE_JA_TODE$ARV_VMA.1)
par(mfrow=c(3,2))
for(puu in puud[1:6]){
  plot(HINNANG_RAIE_JA_TODE[,puu], HINNANG_RAIE_JA_TODE[,paste(puu, ".1", sep = "")])
}



difs = rowSums(HINNANG_RAIE_JA_TODE[,9:15]) - rowSums(HINNANG_RAIE_JA_TODE[,2:8])
difs = data.frame(SID = SID_raie1, dif = difs, tode = rowSums(HINNANG_RAIE_JA_TODE[,9:15]), hinnang = rowSums(HINNANG_RAIE_JA_TODE[,2:8]))
hist(difs$dif, breaks = 20)
#vaatame uuesti suured erinevused üle
dif1 = difs[difs$dif > 200 & difs$tode > 4*difs$hinnang,]
dif1
temp1 = koos[koos$SID %in% dif1$SID,]
#34262: liigendatud
#35291: raie
#76146: raie
#109156: liigendatud, põld ja teed
#114405: võimalik harvendusraie. Raske uskuda, et seal 513m3 metsa hektarile on... Peaks olema must lepp.
#114699: täpselt raie piiril
valja1 = c(35291,76146,114699,82458,71539,645858,73783)
valja = c(valja, valja1)

SID_temp = SID_raie[!(SID_raie %in% valja)]
data_puud = taks_uus[taks_uus$SID %in% SID_temp, puud]
SID_raie1 = taks_uus[taks_uus$SID %in% SID_temp,"SID"]
mets_raie1 = mets_raie[mets_raie$SID %in% SID_temp,]
HINNANG_RAIE = fun_agre(mets_raie1, data_puud)

data_puud_raie = taks_uus[taks_uus$SID %in% SID_temp,puud]
HINNANG_RAIE_JA_TODE = data.frame(cbind(SID_raie1, HINNANG_RAIE, round(data_puud_raie,0)))
par(mfrow=c(1,1))
plot(HINNANG_RAIE_JA_TODE$ARV_VMA, HINNANG_RAIE_JA_TODE$ARV_VMA.1)
plot(HINNANG_RAIE_JA_TODE$ARV_VKU, HINNANG_RAIE_JA_TODE$ARV_VKU.1)
plot(HINNANG_RAIE_JA_TODE$ARV_VKS, HINNANG_RAIE_JA_TODE$ARV_VKS.1)

par(mfrow=c(3,2))
for(puu in puud[1:6]){
  plot(HINNANG_RAIE_JA_TODE[,puu], HINNANG_RAIE_JA_TODE[,paste(puu, ".1", sep = "")])
}

difs = rowSums(HINNANG_RAIE_JA_TODE[,9:15]) - rowSums(HINNANG_RAIE_JA_TODE[,2:8])
difs = data.frame(SID = SID_raie1, dif = difs, tode = rowSums(HINNANG_RAIE_JA_TODE[,9:15]), hinnang = rowSums(HINNANG_RAIE_JA_TODE[,2:8]))
hist(difs$dif, breaks = 20)
#vaatame uuesti suured erinevused üle
dif2 = difs[difs$dif > 200 & difs$tode > 3*difs$hinnang,] #muutsin 3x peale
dif2
temp3 = koos[koos$SID %in% dif2$SID,]
#82458: raie, lisan selle (oli eelmine samm)

#71539: raie
#645858: keset põldu :)
#lisan need 2

#73783: raie, lisan

############################################################################################
#vaatasin kõik käsitsi üle;
############################################################################################

SID_temp = kontrollitud_SID[!(kontrollitud_SID %in% c(114697,82493))]
data_puud = taks_uus[taks_uus$SID %in% SID_temp, puud]
SID_raie1 = taks_uus[taks_uus$SID %in% SID_temp,"SID"]
mets_raie1 = mets_raie[mets_raie$SID %in% SID_temp,]
HINNANG_RAIE = fun_agre(mets_raie1, data_puud)

data_puud_raie = taks_uus[taks_uus$SID %in% SID_temp,puud]
HINNANG_RAIE_JA_TODE = data.frame(cbind(SID_raie1, HINNANG_RAIE, round(data_puud_raie,0)))
par(mfrow=c(1,1))
plot(HINNANG_RAIE_JA_TODE$ARV_VMA, HINNANG_RAIE_JA_TODE$ARV_VMA.1)
plot(HINNANG_RAIE_JA_TODE$ARV_VKU, HINNANG_RAIE_JA_TODE$ARV_VKU.1)
plot(HINNANG_RAIE_JA_TODE$ARV_VKS, HINNANG_RAIE_JA_TODE$ARV_VKS.1)
plot(HINNANG_RAIE_JA_TODE$ARV_VHB, HINNANG_RAIE_JA_TODE$ARV_VHB.1)
#haava hinnangud kõik väga väiksed

par(mfrow=c(3,3))
for(puu in puud){
  plot(HINNANG_RAIE_JA_TODE[,puu], HINNANG_RAIE_JA_TODE[,paste(puu, ".1", sep = "")])
}


difs = rowSums(HINNANG_RAIE_JA_TODE[,9:15]) - rowSums(HINNANG_RAIE_JA_TODE[,2:8])
difs = data.frame(SID = SID_raie1, dif = difs, tode = rowSums(HINNANG_RAIE_JA_TODE[,9:15]), hinnang = rowSums(HINNANG_RAIE_JA_TODE[,2:8]))
hist(difs$dif, breaks = 20)
suurimad = head(sort(difs$dif, decreasing = T),3)
suurimad1 = difs[difs$dif %in% suurimad,]
suurimad3 = raied1[raied1$SID %in% suurimad1$SID,]

count = 0
count=count +1;browseURL(suurimad3$link[count], browser = getOption("browser"),encodeIfNeeded = FALSE)
#73757: ongi vist keeruline juht
#82493: raie koha kõrval, välja
#114697: lagedam ala keset võimast metsa, pole homogeenne



#jätka hommikul: epenechnikovi kaalud vms / mingid paremad kaalud
#proportsioonidele KNN?
#histrogrammid "tõest" ja metsaregistri andmetest

#teine prop vektor?
propcumsum1 <- function(arg){
  maxpoint = elbow(arg)
  data = arg
  if(maxpoint != length(arg)){
    data[(maxpoint+1):length(arg)] = data[maxpoint+1]
  }
  
  props = 1 / (data/data[1]) #kaugused kõige lähema suhtes
  props = props**2 #et esimesetele suurem kaal anda
  props = props/sum(props)
  props
}

#kui vaid nende 235 punkti peal teha PCA?
data235 = sat18_lid_muld[sat18_lid_muld$SID %in% SID_temp,] #238? 3 läheb hiljem veel kaotsi?
pcmuld1 = prcomp(data235[,-1])
cumsum(pcmuld1$sdev**2 / sum(pcmuld1$sdev**2)) #1. kirjeldab 80*%, esiemsed 10 kirjeldavad 99%
mets11 = data.frame(pcmuld1$x[,1:10])
mets11$SID = data235$SID
mets22 = mets11[mets11$SID %in% taks_uus$SID,] #nüüd 235
mets22$cl = "cl"

SID_temp = SID_temp #need 235
data_puud = taks_uus[taks_uus$SID %in% SID_temp, puud]
SID_raie1 = taks_uus[taks_uus$SID %in% SID_temp,"SID"]
mets_raie = mets22
mets_raie1 = mets_raie[mets_raie$SID %in% SID_temp,]
HINNANG_RAIE = fun_agre(mets_raie1, data_puud, nrcomp = 10)

data_puud_raie = taks_uus[taks_uus$SID %in% SID_temp,puud]
HINNANG_RAIE_JA_TODE = data.frame(cbind(SID_raie1, HINNANG_RAIE, round(data_puud_raie,0)))
par(mfrow=c(1,1))
plot(HINNANG_RAIE_JA_TODE$ARV_VMA, HINNANG_RAIE_JA_TODE$ARV_VMA.1)
plot(HINNANG_RAIE_JA_TODE$ARV_VKU, HINNANG_RAIE_JA_TODE$ARV_VKU.1)
plot(HINNANG_RAIE_JA_TODE$ARV_VKS, HINNANG_RAIE_JA_TODE$ARV_VKS.1)
plot(HINNANG_RAIE_JA_TODE$ARV_VHB, HINNANG_RAIE_JA_TODE$ARV_VHB.1)
#haava hinnangud kõik väga väiksed

par(mfrow=c(3,3))
for(puu in puud){
  plot(HINNANG_RAIE_JA_TODE[,puu], HINNANG_RAIE_JA_TODE[,paste(puu, ".1", sep = "")])
}

difs = rowSums(HINNANG_RAIE_JA_TODE[,9:15]) - rowSums(HINNANG_RAIE_JA_TODE[,2:8])
difs = data.frame(SID = SID_raie1, dif = difs, tode = rowSums(HINNANG_RAIE_JA_TODE[,9:15]), hinnang = rowSums(HINNANG_RAIE_JA_TODE[,2:8]))
hist(difs$dif, breaks = 20)
suurimad = head(sort(abs(difs$dif), decreasing = T),10)
suurimad1 = difs[abs(difs$dif) %in% suurimad,]
suurimad2 = raied1[raied1$SID %in% suurimad1$SID,]
count = 0
count=count +1;browseURL(suurimad2$link[count], browser = getOption("browser"),encodeIfNeeded = FALSE)
#35343: metsa-põllu piir;
#72259: kena mets
#73757: pole homogeenne
#73779: kena mets, vb mitte väga homogeenne
#82461: tode = 50, 20 aastane mets. Pildi järgi ei ole see nii
#109139: raie kõrval, välja?
#112469: raie kõrval, samas väike osa ümbrusest
#114405: sanglepik. ei taha uskuda, et tüvemaht seal nii kõrge on
#114725: tee ja metsalagendik, ehk pole homogeenne
#114771: väike välu metsas

valja1 = c(35343, 73757, 82461, 109139, 114405, 114725)
SID_temp = SID_temp[!(SID_temp %in% valja1)] #232

#82491: vanus 20? tüvemaht 39? väike raiesmiku/välu keset metsa. Pole homogeenne!
#124267: metsa-põllu piir
#124471: kõrvalt raiutud ja midagi seest ka!
valja1 = c(valja1, 82491, 124267, 124471)
SID_temp = SID_temp[!(SID_temp %in% valja1)] #229

#35345: võimalik harvendusraie, põld lähedal
valja1 = c(valja1, 35345)
SID_temp = SID_temp[!(SID_temp %in% valja1)] #228

#35345

#siit tagasi proportsioonide peale:

PROPS_HINNANG_JA_TODE = data.frame(
  cbind(
    HINNANG_RAIE_JA_TODE$SID_raie1,
    round(HINNANG_RAIE_JA_TODE[,2:8] / rowSums(HINNANG_RAIE_JA_TODE[,2:8]),2),
    round(HINNANG_RAIE_JA_TODE[,9:15] / rowSums(HINNANG_RAIE_JA_TODE[,9:15]),2)
  )
)

par(mfrow=c(1,1))
plot(PROPS_HINNANG_JA_TODE$ARV_VMA, PROPS_HINNANG_JA_TODE$ARV_VMA.1)
plot(PROPS_HINNANG_JA_TODE$ARV_VKU, PROPS_HINNANG_JA_TODE$ARV_VKU.1)
plot(PROPS_HINNANG_JA_TODE$ARV_VKS, PROPS_HINNANG_JA_TODE$ARV_VKS.1)
plot(PROPS_HINNANG_JA_TODE$ARV_VHB, PROPS_HINNANG_JA_TODE$ARV_VHB.1)



#kui ainult proportsioonid?
data_puud = koos[koos$SID %in% SID_temp,c("MA", "KS", "KU", "HB", "LM", "LV", "KX")]

SID_temp = SID_temp
SID_raie1 = taks_uus[taks_uus$SID %in% SID_temp,"SID"]
mets_raie = mets22
mets_raie1 = mets_raie[mets_raie$SID %in% SID_temp,]
HINNANG_RAIE = fun_agre(mets_raie1, data_puud, nrcomp = 10)

intsct = intersect(mets_raie$SID,koos$SID); intsct = intersect(intsct, SID_temp)
data_puud_raie = koos[koos$SID %in% intsct,c("MA", "KS", "KU", "HB", "LM", "LV", "KX")]

HINNANG_RAIE_JA_TODE = data.frame(cbind(SID_raie1, HINNANG_RAIE, round(data_puud_raie,0)))
par(mfrow=c(1,1))
plot(HINNANG_RAIE_JA_TODE$ARV_VMA, HINNANG_RAIE_JA_TODE$ARV_VMA.1)
plot(HINNANG_RAIE_JA_TODE$ARV_VKU, HINNANG_RAIE_JA_TODE$ARV_VKU.1)
plot(HINNANG_RAIE_JA_TODE$ARV_VKS, HINNANG_RAIE_JA_TODE$ARV_VKS.1)
plot(HINNANG_RAIE_JA_TODE$ARV_VHB, HINNANG_RAIE_JA_TODE$ARV_VHB.1)

