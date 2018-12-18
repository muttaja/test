#Tõde ja metsaregistri andmed
setwd("A:/MAKA/d2_13.11.2018_esimene_andmekaust_nimi_korrastamata/naidised/mreg")
mreg = read.csv("eraldised2018.taks_SAT.koos.csv")

mregx = mreg[mreg$NV_CAT %in% SID_temp,]
SID_temp %in% mreg$NV_CAT
