#
setwd("C:/Users/Mats/Documents/Kool/MAKAT��/loplik_andmestik/naidised/SMI")
#setwd("C:/Users/Mats/Documents/Kool/MAKAT��/loplik_andmestik/naidised/SMI")
katvus = read.csv("Kagu-Eesti_15m_katvus.csv")
korgus = read.csv("Kagu-Eesti_15m_korgus.csv")
koos = read.csv("SMI_13_17_taks_ALS_SAT_koos.csv")
sat = read.csv("smi_prt_13_17_pixphv-heledused.csv")

#v�tab hetkel aint need punktid, kus on feneoloogia m��detav (ehk kevadised ja s�gisesed j�rjest m��tmised olemas)
#aga selliseid polegi, kus k�ik oleks olemas...
sat[sat == 0] = NA #0 ja NA on sama
#sat = na.omit(sat)
muld0 = koos$muld
sat = subset(sat, select = -c(label,prtk,muld))


require(tidyr)
require(stringr)
satnames = names(sat); satnames[2] = "SID"; names(sat) = satnames;



band = str_sub(satnames[-(1:3)],-3,-1)
satel = str_sub(satnames[-(1:3)],1,2)
dates = as.Date(gsub("_","",str_sub(satnames[-(1:3)],-12,-4)), format="%Y%m%d")
# gsub("_","",str_sub(satnames[-(1:3)],-12,-4))

#Probleem: "LGN00"?
LGN = grep("LGN00", satnames)
sat = sat[,-LGN]


sat_long = sat  %>% gather(xxx, value, -SID,-cat)
sat_sep = separate(sat_long, xxx, c("satel","ylelend","kp","band"), sep = "_")
#satelliit, lennu trajektoor, kuup�ev, kanal/lainepikkus

sat_sep$date = as.Date(sat_sep$kp, format="%Y%m%d")

table(format(sat_sep$date,"%Y"))
sat18 = sat_sep[format(sat_sep$date,"%Y") == "2018",]   
sat17 = sat_sep[format(sat_sep$date,"%Y") == "2017",]   
dates = names(table(sat18$date))
#2018-06-09 v�lja - ei sobitu teistega


dates17 = names(table(sat17$date))
#[1] "2017-05-02" "2017-05-05" "2017-06-17" "2017-08-30" "2017-09-24"

sat18 = sat18[sat18$date != "2018-06-09",]
dates = names(table(sat18$date))
dates #2 esimest varajane kevad, 4 j�rgmist hiline kevad

# aa = c("kevad1","kevad1","kevad2","kevad2","kevad2", "kevad2", "sygis1","sygis2")
# temp1 = matrix(sat18$date == dates, ncol = 8, byrow = T)
# sat18$aa = apply(temp1, 1, function(x) aa[x])
# aa = rep(c("kevad1","kevad1","kevad2","kevad2","kevad2", "kevad2", "sygis1","sygis2"),length(sat18$date)/8)
# sat18$aa = aa[sat18$date == dates]

funx = function(z){
  x = z[8]
  if(x %in% dates[1:2]){
    return("kevad1")
  }
  if(x %in% dates[3:6]){
    return("kevad2")
  }
  if(x %in% dates[7]){
    return("sygis1")
  }
  if(x %in% dates[8]){
    return("sygis2")
  }
}
#no peaks ikka lihtsamalt saama
sat18$aa = apply(sat18,1,funx)

bands = c(unique(sat18$band))
#test 100 esimest SID. 972 kokku
#100; 8.5 sec; 5.8 glmer
#200: 57 sec; 32.7
#300: 3.14 min; 1.62
#400: .... ; 3.3 min

sort(unique(sat18$SID))[401]
data = sat18

require(nlme)

# start = Sys.time()
# for(band in bands){
#   data_band = data[data$band == band,]
#   mm1 = lme(value ~ factor(SID) + aa, random =~ 1 | ylelend, data = data_band, na.action = na.exclude)
#   data$pred_band[data$band == band] = predict(mm1, data_band, level = 0)
# }
# end = Sys.time()
# 
# end - start


require(lme4)

start = Sys.time()
for(band in bands){
  data_band = data[data$band == band,]
  mm1 = glmer(value ~ factor(SID) + aa +(1|ylelend), data = data_band, na.action = na.exclude)
  data$pred_glmer[data$band == band] = predict(mm1, data_band, level = 0)
}
end = Sys.time()

end - start
#save(data, file = "sat_glmer.RData")

setwd("A:/MAKA")
load(file = "sat_glmer.RData")


#kumb t�psem?
test1 = na.omit(data)
lme = test1$pred_band - test1$value
glmer = test1$pred_glmer - test1$value

mean(lme); sqrt(var(lme))#midagi peab siin mudelis m�da olema
mean(glmer); sqrt(var(glmer))
# > mean(lme); sqrt(var(lme))
# [1] 10.34121
# [1] 609.6943
# > mean(glmer); sqrt(var(glmer))
# [1] 1.352835e-13
# [1] 593.635

#kui glmer mudelis nAGQ = 0


require(reshape2)
require(dplyr)
#sat 2018 aasta, wide format
sat18w = data %>% group_by(SID, band, aa) %>% sample_n(1) %>%
  dcast(SID ~ band + aa , value.var="pred_glmer")

#vahed
nw = names(sat18w)
nk1 = grep("kevad1", nw, value = T) #kevad 1
nk2 = grep("kevad2", nw, value = T) #kevad 2
ns1 = grep("sygis1", nw, value = T) #s�gis 1
ns2 = grep("sygis2", nw, value = T) #s�gis 2

#kevade vahe:
require(stringr)
nkv = paste(gsub("_","",str_sub(nk1,1,3)), "vahe_kevad", sep = "_")
#s�gise vahe:
nsv = paste(gsub("_","",str_sub(ns1,1,3)), "vahe_sygis", sep = "_")

vahedk = data.frame(sat18w[,nk2] - sat18w[,nk1])
names(vahedk) = nkv


# vaheds = data.frame(sat18w[,ns2] - sat18w[,ns1])
# #tuleb v�lja, et s�gise kohta pole j�rjest m��tmisi
# names(vaheds) = nsv

sat18w = cbind(sat18w, vahedk)
prcomp = prcomp(sat18w[,-1]) #1. veerg SID
cumsum(prcomp$sdev**2 / sum(prcomp$sdev**2)) #6 esimest komponenti kirjeldavad 99%
#########

#vaatame "koos" andmestikku, ehk sealt lidar
#vaatame vaid viimaseid m��tmisi, ehk 2017

koos17 = koos[koos$aasta == 2017,]
koos16 = koos[koos$aasta == 2016,]
koos16_17 = rbind(koos17,koos16[!(koos16$aproovitykk_id %in% koos17$aproovitykk_id),])
#tulev vlja, et kattuvaid IDsid pole? antud juhul ongi ainult viimane lidari info?

#table(koos$aasta)
#length(unique(koos$aproovitykk_id))
#jah, ainult viimane lidar

nkoos = names(koos); nkoos[2] = "SID"; names(koos) = nkoos;
#2. on "aproovitykk_id", 10. on "aproovitykkosa_id"
nlidar = names(koos)[c(2,233:331, 334:416)]
lidar = koos[,nlidar]

#966 juhul 972-st on SID olemas lidari andmetes
sat18_lidar = merge(sat18w, lidar, by = "SID", all = T)
sat18_lidar = na.omit(sat18_lidar) #n��d 966 ja j�rgnevat jama pole vaja
tst = head(sat18_lidar)

prc1 = prcomp(sat18_lidar[,-1])

isnumeric = sapply(sat18_lidar,is.numeric)
#mingid puuduvad v��rtused ikkagi

indxNA_INF <- apply(sat18_lidar, 2, function(x) any(is.na(x) | is.infinite(x)))
indxNA <- apply(sat18_lidar, 2, function(x) any(is.na(x)))
indxINF <- apply(sat18_lidar, 2, function(x) any(is.infinite(x)))

names(sat18_lidar)[indxNA_INF]
names(sat18_lidar)[indxNA] #ehk siis puuduvad v��rtused
names(sat18_lidar)[indxINF]

#kas puuduvad v��rtused kindlatel vaatlustel?

SID_NA <- apply(sat18_lidar, 1, function(x) any(is.na(x)))
sat18_lidar$SID[SID_NA]
NAs = sat18_lidar[sat18_lidar$SID %in% c(35563,70014,73427, 109484, 112381, 114729),]

sat18_lidar = sat18_lidar[!(sat18_lidar$SID %in% c(35563,70014,73427, 109484, 112381, 114729)),]

#PCA n��d t��tab?
prc1 = prcomp(sat18_lidar[,-1])
cumsum(prc1$sdev**2 / sum(prc1$sdev**2)) 
#1. komponent kirjeldab isegi rohkem �ra kui ilma lidari andmeteta...
#8 esimest komponennti kirjeldavad 99% hajuvusest


#kui muld ka juurde?


muld = data.frame(muld = muld0)
muld$SID = koos$SID
muld$cat = koos$cat

head(muld)
#mnjah, NA j�lle sees, 28 unikaalsel juhul:
SID_NA <- apply(muld, 1, function(x) any(is.na(x)))
length(unique(muld$SID[SID_NA]))

#aga kui kohe NA v�lja visata
muldNA = na.omit(muld)
length(unique(muld$SID)) #972
length(unique(muldNA$SID)) #955
#972-955 kaotame 17 (vs 28)

SID_NA <- apply(muldNA, 1, function(x) any(is.na(x)))
length(unique(muld$SID[SID_NA]))

asd1 = muldNA
asd = asd1 %>% group_by(SID) %>% summarize (muld1 = names(which.max(table(muld))))


muldx = merge(muld, asd, by = "SID", all.x = T)
muld1 = muldx %>% group_by(SID) %>% sample_n(1)
muld1 = subset(muld1, select = c("SID", "muld1"))


#muld 1-0 t��pi andmestikuks
require(reshape2)
muld2 = dcast(muld1,SID~muld1,fun.aggregate = function(x){as.integer(length(x) > 0)})
muld2 = data.frame(muld2)

sat18_lid_muld = merge(sat18_lidar, muld2, by = "SID", all.x = T)
tst = head(sat18_lid_muld)
pcmuld = prcomp(sat18_lid_muld[,-1])
cumsum(pcmuld$sdev**2 / sum(pcmuld$sdev**2)) 
#8 esimest kirjeldavad 99%

#proovime v�ikse prognoosi ka teha
#esmalt valime parimad :)

hist(koos$arv_maht_es, breaks = 50)
length(unique(koos$aproovitykkosa_id))# 2202
sum((koos$arv_maht_es ==0 & koos$maakatgrp == "ME")*1) #1123
table(koos$maakatgrp) #mets 1055 juhul

test1 = head(koos,10)

###
mets = data.frame(pcmuld$x[,1:8])
mets$SID = sat18_lid_muld$SID
#test

table(koos[koos$SID %in% mets$SID,]$maakatgr)
#end test
mets1 = mets[mets$SID %in% koos[koos$maakatgrp == "ME",]$SID,] #aint 390 j�i nii...
#et knn t��taks
mets1$cl = "cl"

#mahud vaja saada
names_taks = names(koos)[c(2,19,25:31)]
taks_uus = koos[koos$SID %in% mets1$SID,names_taks]
taks_uus = na.omit(taks_uus)
taks_uus$ARV_VMA = taks_uus$arv_maht_es*taks_uus$MA / 100
taks_uus$ARV_VKU = taks_uus$arv_maht_es*taks_uus$KU / 100
taks_uus$ARV_VKS = taks_uus$arv_maht_es*taks_uus$KS / 100
taks_uus$ARV_VHB = taks_uus$arv_maht_es*taks_uus$HB / 100
taks_uus$ARV_VLM = taks_uus$arv_maht_es*taks_uus$LM / 100
taks_uus$ARV_VLV = taks_uus$arv_maht_es*taks_uus$LV / 100
taks_uus$ARV_VKX = taks_uus$arv_maht_es*taks_uus$KX / 100

mets2 = mets1[mets1$SID %in% taks_uus$SID,]


#lineaarne mudel t�vemahule?
datmets = merge(mets2, taks_uus, by = "SID")
forumla = as.formula(arv_maht_es ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8)
mod = step(lm(forumla, data = datmets))
p1 = predict(mod, newdata = datmets)
head(p1) #l�heb negatiivseks, aga see vist k�ige v�iksem mure
head(datmets$arv_maht_es)


