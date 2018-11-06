#adnmed sisse;
setwd("A:/MAKA/kagu.andmenaidis")

sat = read.csv("Kagu-Eesti_SMI_prt_pix.csv.sort")
#sat1 = subset(sat, select = -c(label,prtk))
sat[sat == 0] = NA



takseer = read.csv("SMI_100PRT12_16_TAKSEER.csv", header = T, sep = ";")
names = names(takseer)
names[1] = "SID"
names(takseer) = names
names
attach(takseer)
takseer$ARV_KOKKU = as.numeric(ARV_VMA) +  as.numeric(ARV_VKU) + 
  as.numeric(ARV_VKS) + as.numeric(ARV_VHB) + as.numeric(ARV_VLM) +
  as.numeric(ARV_VLV) + as.numeric(ARV_VXX)

mx = apply(takseer[,15:21],1,max)
enamus = colnames(takseer[,15:21])[apply(takseer[,15:21],1,which.max)]
takseer$OSAKAAL = mx / takseer$ARV_KOKKU
takseer$ENAMUS = substr(enamus, 6,7)


korgus15 = read.csv("ALS/Kagu-Eesti_15m_korgus.csv")
katvus15 = read.csv("ALS/Kagu-Eesti_15m_katvus.csv")

korgus15_17 = korgus15[grep("2017",korgus15$FileTitle),]
katvus15_17 = katvus15[grep("2017",katvus15$FileTitle),]
katvus15_17$SID = gsub("_", "", substr(katvus15_17$FileTitle,1,4))
korgus15_17$SID = gsub("_", "", substr(korgus15_17$FileTitle,1,4))

katvus15_17 = katvus15_17[,-c(1,2)]; korgus15_17 = korgus15_17[,-c(1,2)]
names(katvus15_17)
#intensiivsused välja: (seal nagunii palju "-1.#IND00")
katvus_int = katvus15_17[,-(50:82)]
korgus_int = korgus15_17[,-(50:82)]



require(nlme)
#esmalt andmestik pikka formaati
require(tidyr)
#sat1 = head(sat,50); 
sat = sat[,-c(3,4,131)]
require(stringr)
satnames = names(sat);
band = str_sub(satnames[-(1:3)],-3,-1)
satel = str_sub(satnames[-(1:3)],1,2)
dates = as.Date(gsub("_","",str_sub(satnames[-(1:3)],-12,-4)), format="%Y%m%d")
gsub("_","",str_sub(satnames[-(1:3)],-12,-4))

sat_long = sat  %>% gather(xxx, value, -SID,-cat) #töötab
#nüüd loome mudeli ja siis  arvutame prognoosi igale punktile
#aga enne separate

sat_sep = separate(sat_long, xxx, c("satel","ylelend","kp","band"), sep = "_")
sat_sep$aaeg[substr(sat_sep$kp,6,6) < 7] = "kevad";sat_sep$aaeg[substr(sat_sep$kp,6,6) > 7] = "sygis"


##********************************
setwd("A:/MAKA/smi_prt")

koosseis = read.csv("smi_koosseis.csv", stringsAsFactors = F)
prt = read.csv("smi_prt.csv")
kooss1 = koosseis[,-4] #ilma vanuseta
kooss1$osakaal = kooss1$osakaal / 100
kooss1$puuliik[!(kooss1$puuliik %in% c("MA", "KU", "KS", "HB", "LM", "LV"))] = "XX"
require(reshape2)
require(plyr)
tmp <- ddply(kooss1, .(aproovitykkosa_id, puuliik), transform, newid = paste(aproovitykkosa_id, seq_along(puuliik)))
koos_wide <- dcast(tmp, aproovitykkosa_id + newid ~ puuliik, value.var = "osakaal")
koos_wide[is.na(koos_wide)] = 0
#koos_wide %>% group_by(aproovitykkosa_id) %>% summarise_all(sum)
koos_wide = ddply(koos_wide, "aproovitykkosa_id", numcolwise(sum))

taks_uus = merge(prt, koos_wide, by = "aproovitykkosa_id", all.y = T)
taks_uus$ARV_VMA = taks_uus$arv_maht_es*taks_uus$MA
taks_uus$ARV_VKU = taks_uus$arv_maht_es*taks_uus$KU
taks_uus$ARV_VKS = taks_uus$arv_maht_es*taks_uus$KS
taks_uus$ARV_VHB = taks_uus$arv_maht_es*taks_uus$HB
taks_uus$ARV_VLM = taks_uus$arv_maht_es*taks_uus$LM
taks_uus$ARV_VLV = taks_uus$arv_maht_es*taks_uus$LV
taks_uus$ARV_VXX = taks_uus$arv_maht_es*taks_uus$XX

takseer[takseer$TR_NUMBER == 4632,]
taks_uus[taks_uus$ptraktnr == 4632,][2,]
koosseis[koosseis$aproovitykkosa_id == 76125,]


takseer[takseer$TR_NUMBER == 5116,]
taks_uus[taks_uus$ptraktnr == 5116,]



