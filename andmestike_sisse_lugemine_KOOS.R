setwd("A:/MAKA/d2_13.11.2018_esimene_andmekaust_nimi_korrastamata/naidised/SMI")
katvus = read.csv("Kagu-Eesti_15m_katvus.csv")
korgus = read.csv("Kagu-Eesti_15m_korgus.csv")
sat = read.csv("smi_prt_13_17_pixphv-heledused.csv")
#kõik ühe andmestikuna:
koos = read.csv("SMI_13_17_taks_ALS_SAT_koos.csv")


#sateliidi andmetes tähistavad nii 0 kui ka NA puuduvaid andmeid:
sat[sat == 0] = NA
koos[,35:230][koos[,35:230] == 0] = NA

#kasutame #koos, sest #sat pole kõiki andmeid?
satnames = names(sat); satnames = satnames[satnames != "label"]
sat = koos[,satnames]


require(tidyr)
require(stringr)

#veerunimedest vajaliku info eraldamine:
#probleem: vanema landsati nimetused teises formaadis
satnames = names(sat)
LGN = grep("LGN00", satnames) #vanemas formaadis
LGN1 = c(1:2,LGN) #"cat" ja "aproovitykk_id" ka;
sat1 = sat[,-LGN]; sat1 = subset(sat1, select = -c(prtk,muld))
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
sat_sep_koos = data.frame(rbind(sat_sep1, lgn_sep))

sat_sep_koos$satel[sat_sep_koos$satel  == "LC"] = "LC08" #sama satelliit
sat_sep_koos$satel[sat_sep_koos$satel  == "S2AL1C" | sat_sep_koos$satel  == "S2BL1C"] = "S2"
