setwd("A:/MAKA/d2_13.11.2018_esimene_andmekaust_nimi_korrastamata/naidised/SMI")

sat = read.csv("smi_prt_13_17_pixphv-heledused.csv")
sat[sat == 0] = NA #0 ja NA on sama
muld0 = sat$muld
sat = subset(sat, select = -c(label,prtk,muld))
satnames = names(sat); satnames[2] = "SID"; names(sat) = satnames;

LGN = grep("LGN00", satnames); LGN1 = c(1:2, LGN)
sat_lgn = sat[,LGN1]
names_lgn = names(sat_lgn)[-c(1:2)]
#LGN formaat: LC8 - stalliit; 186019 - trajektoor; aasta; mitmes päev aastas;

# date_lgn = as.Date(str_sub(names_lgn,10,16), format="%Y%j")
# band_lgn = str_sub(names_lgn,-2,-1)
# satel_lgn = str_sub(names_lgn,1,2)

n1_lgn = paste(str_sub(names_lgn,1,2),str_sub(names_lgn,4,9),str_sub(names_lgn,10,16),str_sub(names_lgn,23,24), sep = "_")
names(sat_lgn)[-c(1:2)] = n1_lgn

lgn_long = sat_lgn  %>% gather(xxx, value, -SID, -cat)
lgn_sep = separate(lgn_long, xxx, c("satel","ylelend","kp","band"), sep = "_")
lgn_sep$date = as.Date(lgn_sep$kp, format="%Y%j")

#liidame ülejäänud andmestiku juurde tagasi
sat1 = sat[,-LGN]
sat_long = sat1  %>% gather(xxx, value, -SID,-cat)
sat_sep = separate(sat_long, xxx, c("satel","ylelend","kp","band"), sep = "_")
sat_sep$date = as.Date(sat_sep$kp, format="%Y%m%d")

sat_sep = data.frame(rbind(sat_sep, lgn_sep))
sat_sep$satel[sat_sep$satel  == "LC"] = "LC08"

#mudel
dates = names(table(sat_sep$date))
dates
kevad1 = dates[c(6,7,9,10,14,15)]
kevad2 = dates[c(1,8,11,16,17,18,19,20)]
sygis1 = dates[c(2,3,4,12,21)]
sygis2 = dates[c(5,13,22)]

funx = function(z){
  x = z[8]
  if(x %in% kevad1){
    return("kevad1")
  }
  if(x %in% kevad2){
    return("kevad2")
  }
  if(x %in% sygis1){
    return("sygis1")
  }
  if(x %in% sygis2){
    return("sygis2")
  }
}

sat_sep$aa = apply(sat_sep,1,funx)
bands = c(unique(sat_sep$band))
#test
bands = bands[1]

require(lme4)

pred_lmer = function(data){
  for(band in bands){
    data_band = data[data$band == band,]
    mm1 = glmer(value ~ factor(SID) + aa +(1|ylelend), data = data_band, na.action = na.exclude)
    data$pred_glmer[data$band == band] = predict(mm1, data_band, level = 0)
  }
}

start = Sys.time()
pred_lmer(sat_sep)
end = Sys.time()
end - start
save(sat_sep, file = "sat_all_glmer.RData")

