#aint mudel bandide jaoks:

setwd("A:/MAKA/kagu.andmenaidis")
sat = read.csv("Kagu-Eesti_SMI_prt_pix.csv.sort")
sat = sat[,-c(3,4,131)]
sat[sat == 0] = NA

require(tidyr)
require(stringr)

satnames = names(sat);
band = str_sub(satnames[-(1:3)],-3,-1)
satel = str_sub(satnames[-(1:3)],1,2)
dates = as.Date(gsub("_","",str_sub(satnames[-(1:3)],-12,-4)), format="%Y%m%d")
gsub("_","",str_sub(satnames[-(1:3)],-12,-4))

sat_long = sat  %>% gather(xxx, value, -SID,-cat)
sat_sep = separate(sat_long, xxx, c("satel","ylelend","kp","band"), sep = "_")
sat_sep$aaeg[substr(sat_sep$kp,6,6) < 7] = "kevad";sat_sep$aaeg[substr(sat_sep$kp,6,6) > 7] = "sygis"
#test, ega 0 pole enam sees:
#test127 = sat_sep[sat_sep$SID == 127 & sat_sep$band == "B11",]

#3157 SID, siis esimesed 300
#MUIDU EI JOOKSUTA ÄRA!
# require(nlme)
sat_sep = sat_sep[sat_sep$SID <= 3157,]
# s1 = Sys.time()
# mm = lme(value ~ factor(SID) + band + aaeg , random = ~ 1 | ylelend, data = sat_sep, na.action = na.exclude)
# sat_sep$predict = predict(mm,sat_sep)
# e1 = Sys.time()
# e1-s1 #Time difference of 7.624749 mins
# ranef(mm)


#aga kui nyyd iga band eraldi võtta
# bands = c(unique(sat_sep$band))
# data = sat_sep
# start = Sys.time()
# 
# for(band in bands){
#   data_band = data[data$band == band,]
#   mm1 = lme(value ~ factor(SID) + aaeg, random =~ 1 | ylelend, data = data_band, na.action = na.exclude)
#   data$pred_band[data$band == band] = predict(mm1, data_band, level = 0)
# }
# 
# end = Sys.time()
# 
# end - start #Time difference of 32.6507 secs

# #kogu andmestiku põhjal
# start = Sys.time()
# dataz = sat_sep
#   data_band = dataz[data$band == "B2",]
#   mm1 = lme(value ~ factor(SID) + aaeg, random =~ 1 | ylelend, data = data_band, na.action = na.exclude)
#   dataz$pred_band[data$band == band] = predict(mm1, data_band, level = 0)
#   end = Sys.time()
#   end - start #11.5 minutit

##################### 25.10.2018 kell 03.34 jätk, kust enne pooleli jäi

#muld
muld = read.csv("Kagu-Eesti_SMI_prt_pix.csv.sort")
muld = muld[,131]
muld1 = data.frame(muld = muld)
muld1$SID = sat$SID
#siin parandus 29.10!!! lõpuks peaks "õige" ehk kõige levinum muld olema
#muld1 = muld1 %>% group_by(SID) %>% sample_n(1) #vana lahendus

require(dplyr)
muld1 = muld1 %>% group_by(SID) %>% summarize (muld1 = names(which.max(table(muld))))

#muld = merge(muld, asd, by = "SID", all.x = T)


require(reshape2)
dataw1 = data %>% group_by(SID, band, aaeg) %>% sample_n(1) %>%
  dcast(SID ~ band + aaeg , value.var="pred_band") %>%
  merge(takseer, by = "SID", all.y = T) %>% merge(muld1, by = "SID", all.x = T) %>%
  merge(lpca5, by = "SID")

hist(dataw1$ARV_KOKKU, breaks = 20)
dataw = dataw1[dataw1$ARV_KOKKU > 100,] #kas 50 või 100 võtta praegu piiriks?
dim(dataw)[1]
dataw = dataw[dataw$OSAKAAL > 0.6,]
dim(dataw)[1]
dataw = dataw[dataw$ENAMUS != "XX",] #see on just väikse tüvemahuga vaatluste probleem
                                    # pajuvõsa vms?
dim(dataw)[1]

table(dataw$ENAMUS)
# HB KS KU LV MA 
# 5 12 16  2 20 
dataw = dataw[dataw$ENAMUS %in% c("KS", "KU", "MA"),]
dim(dataw)[1]

#see on hea andmestik


#LOOCV

require(class)
for(k in 3:9){
  knncv = knn.cv(train = dataw[,n3], cl = dataw$ENAMUS, k = k, prob = F)
  classes = data.frame(true = dataw$ENAMUS, knnclass = knncv)
  assign(paste( "confm",k,sep =""), classes)
  assign(paste( "ER",k,sep =""), 1 - sum(diag(table(classes)))/sum(table(classes)))
}
ER3;ER4;ER5;ER6;ER7;ER8;ER9

# [1] 0.625
# [1] 0.5416667
# [1] 0.5208333
# [1] 0.5
# [1] 0.5208333
# [1] 0.5
# [1] 0.5833333

#50m3:
# [1] 0.5090909
# [1] 0.6
# [1] 0.5818182
# [1] 0.5636364
# [1] 0.5818182
# [1] 0.6
# [1] 0.5636364


# knn11cv = knn.cv(train = data4[,n11], cl = data4$ENAMUS, k = 6)
# class11 = data.frame(true = data4$ENAMUS, knnclass = knn5cv)
# table(class11) #pole mingit vahet, kas kasutada 5 või 11 PC-d


#LOOCV multinomial regressioni korral

nw = names(dataw)[c(2:31,56:60)]
formula = as.formula(paste("ENAMUS", paste(nw, collapse=" + "), sep=" ~ "))


#multinom_predict = c()
#data4$multi_pred = data4$ENAMUS
observations = dataw$SID
require(nnet)
for(obs in observations){
  model = multinom(data = dataw[dataw$SID != obs,], formula = formula)
  pred = predict(model, dataw[dataw$SID == obs,])
  dataw$multi_pred[dataw$SID == obs] = as.character(pred)
  #print(pred)
}

confm = table(data.frame(true = dataw$ENAMUS, pred = dataw$multi_pred))
confm

1 - sum(diag(confm))/sum(confm) #0.52


#50m3:
# pred
# true KS KU MA
# KS  8  2  4
# KU  3  9  6
# MA  4  5 14

#ER 0.4363636



#PCA ka sateliidi andmete peal?

sat.pca = prcomp(dataw[,2:31])
summary(sat.pca)
cumvar = cumsum(sat.pca$sdev**2)/sum(sat.pca$sdev**2)
cumvar

pcax3 = sat.pca$x[,1:3]
datawPCA = cbind(dataw, pcax3)
names(datawPCA)[63:65] = c("PCA_SAT1", "PCA_SAT2", "PCA_SAT3")

#nx = c(n3[11:15], c("PCA_SAT1", "PCA_SAT2", "PCA_SAT3"))
nx = c("PC1", "PC2", "PC3", "PC4", "PCA_SAT1", "PCA_SAT2", "PCA_SAT3")

for(k in 3:9){
  knncv = knn.cv(train = datawPCA[,nx], cl = datawPCA$ENAMUS, k = k, prob = F)
  classes = data.frame(true = datawPCA$ENAMUS, knnclass = knncv)
  assign(paste( "confm",k,sep =""), classes)
  assign(paste( "ER",k,sep =""), 1 - sum(diag(table(classes)))/sum(table(classes)))
}
ER3;ER4;ER5;ER6;ER7;ER8;ER9

# [1] 0.3958333
# [1] 0.4375
# [1] 0.4166667
# [1] 0.4583333
# [1] 0.4583333
# [1] 0.4375
# [1] 0.4375

#50m3:
# [1] 0.4181818
# [1] 0.4
# [1] 0.4
# [1] 0.4
# [1] 0.4545455
# [1] 0.3818182
# [1] 0.4181818

################################

datawPCA$muld1 = as.factor(datawPCA$muld1)
nx_muld = c(nx, "muld1") #kas saab ka mulla mängu võtta?
for(k in 3:9){
  knncv = knn.cv(train = datawPCA[,nx_muld], cl = datawPCA$ENAMUS, k = k, prob = F)
  classes = data.frame(true = datawPCA$ENAMUS, knnclass = knncv)
  assign(paste( "confm",k,sep =""), classes)
  assign(paste( "ER",k,sep =""), 1 - sum(diag(table(classes)))/sum(table(classes)))
}
ER3;ER4;ER5;ER6;ER7;ER8;ER9

#################################

#test, kui võtta uuesti PCA
sat.all = prcomp(datawPCA[,nx])
summary(sat.all)
cumvar = cumsum(sat.all$sdev**2)/sum(sat.all$sdev**2)
cumvar

pca4all = sat.all$x[,1:4]
data4all = cbind(dataw[,c("SID", "ENAMUS")], pca4all)
data4all = cbind(data4all, datawPCA$muld1)
nms = names(data4all); nms[7] = "muld1"
names(data4all) = nms

for(k in 3:10){
  knncv = knn.cv(train = data4all[,3:5], cl = data4all$ENAMUS, k = k, prob = F)
  classes = data.frame(true = data4all$ENAMUS, knnclass = knncv)
  assign(paste( "confm",k,sep =""), classes)
  assign(paste( "ER",k,sep =""), 1 - sum(diag(table(classes)))/sum(table(classes)))
}
ER3;ER4;ER5;ER6;ER7;ER8;ER9;ER10



################################



#multinomial
formulaPCA = as.formula(paste("ENAMUS", paste(nx, collapse=" + "), sep=" ~ "))

for(obs in observations){
  model = multinom(data = datawPCA[datawPCA$SID != obs,], formula = formulaPCA)
  pred = predict(model, datawPCA[datawPCA$SID == obs,])
  dataw$multi_pred_PCA[dataw$SID == obs] = as.character(pred)
  #print(pred)
}

confmPCA = table(data.frame(true = dataw$ENAMUS, pred = dataw$multi_pred_PCA))
confmPCA

1 - sum(diag(confmPCA))/sum(confmPCA) #0.5625

#50m3
# pred
# true KS KU MA
# KS  5  8  1
# KU  6  6  6
# MA  1  5 17

#ER: 0.4909091



#NÜÜD VEEL MULLAINFO JUURDE
table(dataw$muld)
kk = data.frame(table(dataw$muld))
pp = ggplot(kk, aes(Var1, Freq))
pp + geom_bar(stat = "identity")
# 21 37 42 43 44 45 48 51 53 57 61 63 64 73 
# 1  1  5  3  3  6  7  4  1  2  9  1  2  3 
#kui võtta 48, 61 ja "teised"?
dataw$muldx = dataw$muld; dataw$muldx[dataw$muld != 48 & dataw$muld != 61] = 999

nm = names(dataw)[c(2:31,56:60,63)]
formulaMULD = as.formula(paste("ENAMUS", paste(nm, collapse=" + "), sep=" ~ "))
formulaMULD = as.formula(ENAMUS ~ B02_kevad + B02_sygis + B03_kevad + B03_sygis + B04_kevad + 
                           B04_sygis + B05_kevad + B05_sygis + B06_kevad + B06_sygis + 
                           B07_kevad + B07_sygis + B08_kevad + B08_sygis + B11_kevad + 
                           B11_sygis + B12_kevad + B12_sygis + B2_kevad + B2_sygis + 
                           B3_kevad + B3_sygis + B4_kevad + B4_sygis + B5_kevad + B5_sygis + 
                           B6_kevad + B6_sygis + B7_kevad + B7_sygis + PC1 + PC2 + PC3 + 
                           PC4 + PC5 + factor(muldx))

for(obs in observations){
  model = multinom(data = dataw[dataw$SID != obs,], formula = formulaMULD)
  pred = predict(model, dataw[dataw$SID == obs,])
  dataw$multi_pred_muld[dataw$SID == obs] = as.character(pred)
  #print(pred)
}

confmMULD = table(data.frame(true = dataw$ENAMUS, pred = dataw$multi_pred_muld))
confmMULD
1 - sum(diag(confmMULD))/sum(confmMULD) #0.458 valesti
#muld andis kuusele kõvasti juurde
diag(confmMULD)/rowSums(confmMULD)

#100m3
# pred
# true KS KU MA
# KS  6  3  3
# KU  2  8  6
# MA  2  7 11

# KS   KU   MA 
# 0.50 0.50 0.55 



#kui võtta vaatlused, kus vähemalt 50m3 tüvemahtu hektari kohta:

# true KS KU MA
# KS 10  2  2
# KU  2 10  6
# MA  3  7 13

#error rate 0.4

# KS        KU        MA 
# 0.7142857 0.5555556 0.5652174 


#mis edasi?

### proovida veel erinevaid meetodeid
### fenoloogia
### lidari info parem kasutamine (tippude paiknemine üksteise suhtes jne)

#### 29.10 ####

#vt muld eraldi programmina

#muld ja kNN?

#### knn.dist ####

for(k in 3:10){
  #3:6 kui ilma mullata
  dists = knn.dist(train = data4all[,3:7], k = 10)
  assign(paste( "dist",k,sep =""), dist)
}
dist1;

require(FNN)
#siit saab nii kaugused kui ka neile vastavad punktid
dists = knn.cv(train = data4all[,3:6], cl = data4all$ENAMUS, k = 10, algorithm = "cover_tree")



