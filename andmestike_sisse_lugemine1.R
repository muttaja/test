#andmete sisselugemine
setwd( "C:/Users/Mats/Documents/Kool/MAKATÖÖ/loplik_andmestik/naidised/SMI")
#setwd("A:/MAKA/d2_13.11.2018_esimene_andmekaust_nimi_korrastamata/naidised/SMI")
katvus = read.csv("Kagu-Eesti_15m_katvus.csv")
korgus = read.csv("Kagu-Eesti_15m_korgus.csv")
sat = read.csv("smi_prt_13_17_pixphv-heledused.csv")
#kõik ühe andmestikuna:
koos = read.csv("SMI_13_17_taks_ALS_SAT_koos.csv")


#sateliidi andmetes tähistavad nii 0 kui ka NA puuduvaid andmeid:
sat[sat == 0] = NA
koos[,35:230][koos[,35:230] == 0] = NA


require(tidyr)
require(stringr)

#veerunimedest vajaliku info eraldamine:
#probleem: vanema landsati nimetused teises formaadis
satnames = names(sat)
LGN = grep("LGN00", satnames) #vanemas formaadis
LGN1 = c(1:2,LGN) #"cat" ja "aproovitykk_id" ka;
sat1 = sat[,-LGN]; sat1 = subset(sat1, select = -c(label,prtk,muld))
sat_lgn = sat[, LGN1]

sat_long1 = sat1  %>% gather(xxx, value, -aproovitykk_id, -cat) #andmed pikas formaadis
sat_sep1 = separate(sat_long1, xxx, c("satel","ylelend","kp","band"), sep = "_") #pikas formaadis andmetest veergude eraldamine
#kuupäeva formaat:
sat_sep1$kp = as.Date(sat_sep1$kp, format="%Y%m%d")



#varem eraldatud LGN:
names_lgn = names(sat_lgn)[-c(1:2)]
#LGN formaat: LC8 - stalliit; 186019 - trajektoor; aasta; mitmes päev aastas;

n1_lgn = paste(str_sub(names_lgn,1,2),str_sub(names_lgn,4,9),str_sub(names_lgn,10,16),str_sub(names_lgn,23,24), sep = "_")
names(sat_lgn)[-c(1:2)] = n1_lgn

lgn_long = sat_lgn  %>% gather(xxx, value, -aproovitykk_id, -cat)
lgn_sep = separate(lgn_long, xxx, c("satel","ylelend","kp","band"), sep = "_")
lgn_sep$kp = as.Date(lgn_sep$kp, format="%Y%j") #kuupäeva formaat: aasta ja mitmes päev aastas

#liidame ülejäänud andmestiku juurde tagasi
sat_sep = data.frame(rbind(sat_sep1, lgn_sep))
sat_sep$satel[sat_sep$satel  == "LC"] = "LC08" #sama satelliit
sat_sep$satel[sat_sep$satel  == "S2AL1C" | sat_sep$satel  == "S2BL1C"] = "S2"
#Sentinel2 A ja B väga väikeste tehnoloogiliste erinevustega
#NB! Landsati kanalid B2, B3 jne, Sentinelil B02, B03 jne




#2018 aasta põhjal saaks vaadata ka kevadist fenoloogiat (2017 ja vanemad pole piisavalt pilte erinevatel kuupäevadel)
sat18 = sat_sep[format(sat_sep$kp,"%Y") == "2018",]  
sat17 = sat_sep[format(sat_sep$kp,"%Y") == "2017",]  
sat16 = sat_sep[format(sat_sep$kp,"%Y") == "2016",]  
sat15 = sat_sep[format(sat_sep$kp,"%Y") == "2015",]  
table(sat18[,c("satel", "kp")])

#lähedased kuupäevad võtan kokku: kevade algus, kevade lõpp (ja sügise/suve lõpu kohta ka kaks erinevat aega)
kpd = as.Date(names(table(sat18$kp))) 
sat18$aa = ifelse(sat18$kp %in% kpd[1:2], "kevad1",
                  ifelse(sat18$kp %in% kpd[3:7], "kevad2", "sygis"))

#mudel silumiseks ja puuduvate väärtuste kõrvaldamiseks:

<<<<<<< HEAD
require(lme4)
#data1 = sat18
bands = unique(data$band) #hetkel B2 ja B02 jne erinevate kanalitena. mõõdavad sama asja, aga skaalad erinevad

start = Sys.time()
start
for(band in bands){
  print(Sys.time())
  data_band = data1[data1$band == bands[1],]
  mm1 = glmer(value ~ factor(aproovitykk_id) + aa +(1|ylelend), data = data_band, na.action = na.exclude)
  #ülelennul juhuslik mõju: erinevatel trajektooridel lennates jäävad pildid erinevad
  data1$pred_glmer[data1$band == band] = predict(mm1, asd, re.form = NA)
}
end = Sys.time()
end
end - start


#Time difference of 1.803989 hours
#save(data1, file = "data_glmer1.RData")
=======
# require(lme4)
# data1 = sat18
# bands = unique(data$band) #hetkel B2 ja B02 jne erinevate kanalitena. mõõdavad sama asja, aga skaalad erinevad
# 
# start = Sys.time()
# start
# for(band in bands){
#   print(Sys.time())
#   data_band = data1[data1$band == band,]
#   mm1 = glmer(value ~ factor(aproovitykk_id)+ factor(aproovitykk_id)*aa + aa + (1|ylelend), data = data_band, na.action = na.exclude)
#   #ülelennul juhuslik mõju: erinevatel trajektooridel lennates jäävad pildid erinevad
#   data1$pred_glmer[data1$band == band] = predict(mm1, data_band, re.form = NA)
# }
# end = Sys.time()
# end
# end - start


#Time difference of 14.01975 hours
#save(data1, file = "data_glmer2.RData")
>>>>>>> 1e56408bc3c96cc1e2c08f4b2094ca9d18589505

#võtab mõned tunnid aega, seetõttu panin tulemused kirjale kaasa:
#setwd("A:/MAKA/d2_13.11.2018_esimene_andmekaust_nimi_korrastamata/naidised/SMI")
load(file = "data_glmer2.RData")

#tagasi laia formaati:
require(reshape2)
require(dplyr)
#data = data1
sat18_wide = data %>% group_by(aproovitykk_id, band, aa) %>% sample_n(1) %>%
  dcast(aproovitykk_id ~ band + aa , value.var="pred_glmer")

#kevadine fenoloogia. (sügisene ka, aga see pole vist nii tähtis ja sügise kohta pole piisavalt pilte ka)
#vahed
names1 = names(sat18_wide)
nk1 = grep("kevad1", names1, value = T) #kevad 1
nk2 = grep("kevad2", names1, value = T) #kevad 2
ns = grep("sygis", names1, value = T) #sügis
#ns2 = grep("sygis2", names1, value = T) #sügis 2

#kevadine erinevus:
nkv = paste(gsub("_","",str_sub(nk1,1,3)), "vahe_kevad", sep = "_")
vahed_kevad = data.frame(sat18_wide[,nk2] - sat18_wide[,nk1])
names(vahed_kevad) = nkv
#sügisene erinevus pole vist väga sisukas? ja mõõtmisi pole ka piisavalt

sat18_wide = cbind(sat18_wide, vahed_kevad)

#siit peaks eraldama metsa:
intr = intersect(koos$aproovitykk_id, sat18_wide$aproovitykk_id) #"koos" ja "sat" ei kattu täielikult
mets_id = koos[koos$aproovitykk_id %in% intr & koos$maakatsgrp == "M", "aproovitykk_id"]
#MM on metsata metsamaa. või võtta ka metsata metsamaa sisse(?), siis koos$maakatgrp == "ME
sat18_mets = sat18_wide[sat18_wide$aproovitykk_id %in% mets_id,]


#lingid, proovitükkide vaatamiseks, tuvastamaks, et kas on raiutud, kuigi raiet pole märgitud (hiljem saab selle automaatseks teha)
link = "https://xgis.maaamet.ee/maps/XGis?app_id=MA29&user_id=at&LANG=1&WIDTH=1220&HEIGHT=1263&zlevel=12,688213.00000001,6446066.9999998&setlegend=HMAMULD_YLD=0,HMAHYBR_ALUS01_29=1,HMAHYBR_ALUS02_29=0"
raied = koos[(koos$raie_aeg1 == -1) & (koos$aproovitykk_id %in% sat18_mets$aproovitykk_id) , c("aproovitykk_id","vanus","arv_maht_es","koord_e","koord_n")]
raied$link = str_replace(link, str_sub(link, 98, 103),as.character(raied$koord_e))
raied$link = str_replace(raied$link, str_sub(link, 114,120),as.character(raied$koord_n -1))
#peaks kontrollima ka neid, kus mägitud, et (mingi) raie on tehtud?

#nende puhul, kus tüvemaht väike, on selge, et takseerandmete põhjal on raitud ja probleemi pole
raied50 = raied[raied$arv_maht_es > 50,]

#kui tahta maa-ametist vaadata, avab brauseris:
count = 0
count=count +1;print(raied50$aproovitykk_id[count]);browseURL(raied50$link[count], browser = getOption("browser"),encodeIfNeeded = FALSE)


nlidar = names(koos)[c(2,233:331, 334:416)]
lidar = koos[,nlidar]
sat18_lidar = merge(sat18_wide, lidar, by = "aproovitykk_id", all = T)
sat18_lidar = na.omit(sat18_lidar) #966 vaatlust




