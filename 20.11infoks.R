#20.11.18

setwd("C:/Users/Mats/Documents/Kool/MAKAT÷÷/loplik_andmestik/naidised/mreg")
katvus = read.csv("Kagu-Eesti_15m_katvus.csv")
korgus = read.csv("Kagu-Eesti_15m_korgus.csv")
koos = read.csv("SMI_13_17_taks_ALS_SAT_koos.csv")
sat = read.csv("smi_prt_13_17_pixphv-heledused.csv")

mreg = read.csv("eraldised2018.taks_SAT.koos.csv")

katvus1 = head(katvus)
korgus1 = head(korgus)
koos1 = head(koos)
sat1 = head(sat)

#LGN?

satnames = names(sat)
LGN = grep("LGN00", satnames)
satnames[LGN] #aasta ja p‰eva number aastas


#kuup‰evad 2017:
require(tidyr)
require(stringr)
satnames = names(sat); satnames[2] = "SID"; names(sat) = satnames;
band = str_sub(satnames[-(1:3)],-3,-1)
satel = str_sub(satnames[-(1:3)],1,2)
dates = as.Date(gsub("_","",str_sub(satnames[-(1:3)],-12,-4)), format="%Y%m%d")

sat_long = sat  %>% gather(xxx, value, -SID,-cat)
sat_sep = separate(sat_long, xxx, c("satel","ylelend","kp","band"), sep = "_")
#satelliit, lennu trajektoor, kuup‰ev, kanal/lainepikkus

sat_sep$date = as.Date(sat_sep$kp, format="%Y%m%d")

table(format(sat_sep$date,"%Y"))
sat18 = sat_sep[format(sat_sep$date,"%Y") == "2018",]   
sat17 = sat_sep[format(sat_sep$date,"%Y") == "2017",]   
dates = names(table(sat17$date))
dates

#2018-06-09 v‰lja - ei sobitu teistega


######### mets ja muu maa

hist(koos$arv_maht_es, breaks = 25)
length(unique(koos$aproovitykkosa_id))# 2202
sum((koos$arv_maht_es == 0 & koos$maakatgrp == "ME")*1)# 75 juhul on "mets" ilma metsata
table(koos$maakatgrp) #mets 1055 juhul

#lidarist 6 ID kohta info puudu
#17 juhul mullainfo puudu




#TIFs
require(raster)
setwd("C:/Users/Mats/Documents/Kool/MAKAT÷÷/loplik_andmestik")
options(stringsAsFactors = FALSE)

list.files()
all_tifs <- list.files(pattern = ".tif$",full.names = TRUE)
all_tifs

#kıikidel peegeldustel pıhinev katvus 2017:
kk2017 <- raster(all_tifs[1])
plot(kk2017, col = gray(0:100 / 100))
#metsaregistrist m‰nd:
mand <- raster(all_tifs[2])
plot(mand, col = gray(0:100 / 100))

#palju seal Koiva jıe ‰‰res m‰ndi on?
xy = cbind(seq(640000,645000, by = 200),  rep(6385500,26))
result <- extract(mand, xy)
cbind(result,xy)


