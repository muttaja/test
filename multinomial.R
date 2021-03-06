#multinomial


#NB! vaata ka random forest
#The ranger package in R (pdf), which is relatively new, will do this. 
#The ranger implementation of random forests has a case.weights argument that takes a vector with individual case / observation weights.

#raied_ok = raied[raied$raie == 0,]$aproovitykk_id #427
#load("ID_OK.RData", verbose = T)# 174

setwd("A:/MAKA/TEST/test")
setwd("C:/Users/Mats/Documents/Kool/MAKAT��/TEST/test")
load("sid601.RData")
load("mets_id.RData")


#dk andmestik failist landsat_to_sentinel
load("lnds_to_sent.RData")
dk[,-1] = scale(dk[,-1])

muld1 = data.frame(muld = koos[koos$aproovitykk_id %in% mets_id,]$muld)
muld1$aproovitykk_id = koos$aproovitykk_id[koos$aproovitykk_id %in% mets_id]
muld1[is.na(muld1$muld),]$muld = 999

#midagi kokku v�tta?
table(muld1[muld1$aproovitykk_id %in% sidxx,]$muld) #alla 10 kindlasti kokku; 2 puuduvat v��rtust
#v�tan alla 30 v�lja

muld1[muld1$muld %in% c(10,11,16,73,200,999,31,37,53,57,63), "muld"] = 999

#muld 1-0 t��pi andmestikuks
muld2 = dcast(muld1,aproovitykk_id~muld,fun.aggregate = function(x){as.integer(length(x) > 0)})

dkm = merge(dk, muld2, by = "aproovitykk_id")

#klassid, kui v�tta piiriks 80%

koos$cl = "cl";
koos$cl70 = "cl70";
koos$cl50 = "cl50";
koos$KX1 = koos$HB + koos$LV + koos$LM + koos$KX
require(dplyr)
koos = koos %>% mutate(weight = pmax(MA,KU, KS, KX1))

koos[!(is.na(koos$MA)) & koos$MA >= 80,]$cl = "MA"
koos[!(is.na(koos$KU)) & koos$KU >= 80,]$cl = "KU"
koos[!(is.na(koos$KS)) & koos$KS >= 80,]$cl = "KS"
koos[!(is.na(koos$KX1)) & koos$KX1 >= 80,]$cl = "KX1"

koos[!(is.na(koos$MA)) & koos$MA >= 70,]$cl70 = "MA"
koos[!(is.na(koos$KU)) & koos$KU >= 70,]$cl70 = "KU"
koos[!(is.na(koos$KS)) & koos$KS >= 70,]$cl70 = "KS"
koos[!(is.na(koos$KX1)) & koos$KX1 >= 70,]$cl70 = "KX1"

koos[!(is.na(koos$MA)) & koos$MA > 50,]$cl50 = "MA"
koos[!(is.na(koos$KU)) & koos$KU > 50,]$cl50 = "KU"
koos[!(is.na(koos$KS)) & koos$KS > 50,]$cl50 = "KS"
koos[!(is.na(koos$KX1)) & koos$KX1 > 50,]$cl50 = "KX1"


#dkm0 = dkm;
dkm0 = merge(dkm,koos[,c("aproovitykk_id", "cl", "weight", "muld")], all.x = T)
dkm70 = merge(dkm,koos[,c("aproovitykk_id", "cl70", "weight", "muld")], all.x = T)
dkm50 = merge(dkm,koos[,c("aproovitykk_id", "cl50", "weight", "muld")], all.x = T)


dkm0 = dkm0[dkm0$aproovitykk_id %in% sidxx,]
dkm70 = dkm70[dkm70$aproovitykk_id %in% sidxx,]
dkm50 = dkm50[dkm50$aproovitykk_id %in% sidxx,]
table(dkm0$cl)     #52  37  36 138    kui 80%
table(dkm70$cl70) #85   51   43  163 kui 70%
table(dkm50$cl50) #136   89   67  224

dp00 = taks_uus[, puud]; dp00 = dp00 / rowSums(dp00)
ddd = cbind(dkm0, dp00)
#write.csv(ddd, file = "d601.csv")
ddd = read.csv("d601.csv")



dkm1 = dkm0 %>% filter(cl != "cl")
dkm50_ = dkm50 %>% filter(cl50 != "cl50")
dkm70_ = dkm70 %>% filter(cl70 != "cl70")


table(dkm1$cl)
dkm1[dkm1$muld %in% names(table(dkm1$muld) < 10)[table(dkm1$muld) < 10],"muld"] = 999
dkm1[is.na(dkm1$muld),"muld"] = 999

dkm0[!(dkm0$muld %in% names(table(dkm1$muld) < 10)[table(dkm1$muld) >= 10]),"muld"] = 999
dkm0[is.na(dkm0$muld),"muld"] = 999
table(dkm0$muld)

dkm50_[dkm50_$muld %in% names(table(dkm50_$muld) < 10)[table(dkm50_$muld) < 10],"muld"] = 999
dkm50_[is.na(dkm50_$muld),"muld"] = 999
dkm70_[dkm70_$muld %in% names(table(dkm70_$muld) < 10)[table(dkm70_$muld) < 10],"muld"] = 999
dkm70_[is.na(dkm70_$muld),"muld"] = 999

nms = names(dkm50_); nms[49] = "cl"; names(dkm50_) = nms
nms = names(dkm70_); nms[49] = "cl"; names(dkm70_) = nms

#kui n��d anda kaalud vastavalt
#a.) osakaalule
#b.) t�vemahu hinnagule


require(nnet)

namesz = names(dkm1)[c(2:37,51)]
namesz[37] = "factor(muld)"
namesz = namesz[-37] #testiks!

formula1 = as.formula(paste("cl", paste(namesz, collapse=" + "), sep=" ~ "))
m1 = step(multinom(formula1, dkm50_, weights = weight,maxit = 1000))
m1
m1
summary(m1)
require(car)
require(lmtest)
require(MASS)
Anova(m1)
m2 = stepAIC(multinom(formula1, dkm50_)) #, weights = weight
Anova(m2)

p1 = predict(m2, dkm50_, type = "probs")
p1 = predict(m2, newdata = dkm0[!(dkm0$aproovitykk_id %in% dkm50_$aproovitykk_id),], type = "probs")
par(mfrow = c(2,2))
hist(p1[,1]); hist(p1[,2]); hist(p1[,3]);hist(p1[,4])

#aga vanakooli meetodil?
frm = as.formula(cl ~ + B02_kevad2 + B02_sygis + B03_kevad1 + 
  B03_sygis + B04_kevad2 + B04_sygis+ 
  B05_sygis + B06_sygis + 
  B07_sygis + B08_kevad2 + 
  B11_kevad2 + B11_sygis + 
  B12_sygis + B11_vahe_kevad)

#kui panna kaalud ka, siis tuleb muld siiski oluline
m0 = multinom(frm, dkm50_) #, weights = weight
Anova(m0)


frmw = as.formula(cl ~ B02_kevad1 + B02_kevad2 + B02_sygis + B03_kevad1 + B03_kevad2 + 
                   B03_sygis + B04_kevad1 + B04_kevad2 + B04_sygis + B05_kevad1 + 
                   B05_sygis + B06_kevad1 + B06_kevad2 + B06_sygis + 
                   B07_kevad1 + B07_kevad2 + B07_sygis + B08_kevad2 + 
                   B08_sygis + B11_kevad1 + B11_kevad2 + B11_sygis + B12_kevad1 + 
                   B12_kevad2 + B12_sygis + B02_vahe_kevad +
                   B05_vahe_kevad + B06_vahe_kevad + B07_vahe_kevad + 
                   B08_vahe_kevad + factor(muld))

#kui panna kaalud ka, siis tuleb muld siiski oluline
m0w = multinom(frmw, dkm50_, weights = weight) #, weights = weight
#m0wstep = step(multinom(frm, dkm50_, weights = weight))
Anova(m0w)

p1 = predict(m0w, newdata = dkm0[!(dkm0$aproovitykk_id %in% dkm50_$aproovitykk_id),], type = "probs")
par(mfrow = c(2,2))
hist(p1[,1]); hist(p1[,2]); hist(p1[,3]);hist(p1[,4])


#################################
#tulemused selle pealt:
obsvs = sidxx #601
probsw = vector("list", length = length(obsvs))
#modelpcastep = step(multinom(formula = formulapca, data = dkmpca1)) #, weights = weight
for(i in 1:length(obsvs)){ 
  obs = obsvs[i]
  if(!(obs %in% dkm50_$aproovitykk_id)){
    probsw[[i]] = predict(m0w, newdata = dkm0[dkm0$aproovitykk_id == obs,], type = "probs")
  }
  else{
    model = multinom(frmw, dkm50_[dkm50_$aproovitykk_id != obs,]) #, weights = weight
    probsw[[i]] = predict(model, newdata = dkm50_[dkm50_$aproovitykk_id == obs,], type = "probs")
  }
}

dfprobw <- data.frame(matrix(unlist(probsw), nrow=length(probsw), byrow=T))

#load("dprp.RData")
#dpp = data_puud_raie_props

plot(dpp[,1], dfprob0[,4])
plot(dpp[,2], dfprob0[,2])
plot(dpp[,3], dfprob0[,1])
plot(dpp[,4], dfprob0[,3])

plot(dpp[,1], dfprobw[,4])
plot(dpp[,2], dfprobw[,2])
plot(dpp[,3], dfprobw[,1])
plot(dpp[,4], dfprobw[,3])

sqrt(mean((dpp[,1]- dfprob0[,4])**2))
sqrt(mean((dpp[,2]- dfprob0[,2])**2))
sqrt(mean((dpp[,3]- dfprob0[,1])**2))
sqrt(mean((dpp[,4]- dfprob0[,3])**2))

sqrt(mean((dpp[,1]- dfprobw[,4])**2))
sqrt(mean((dpp[,2]- dfprobw[,2])**2))
sqrt(mean((dpp[,3]- dfprobw[,1])**2))
sqrt(mean((dpp[,4]- dfprobw[,3])**2))


#################################





frm = as.formula(cl ~ B02_kevad2 + B02_sygis + B03_kevad1  + B04_kevad1 + B04_sygis + 
                   B11_kevad1 + B03_vahe_kevad + B04_vahe_kevad  +
                   B12_kevad2 +  
                    B11_vahe_kevad + B12_vahe_kevad + factor(muld))



m00 = multinom(frm, dkm70_) #, weights = weight
Anova(m00)
p00 = predict(m0, dkm70_, type = "probs")
hist(p00[,1]); hist(p00[,2]); hist(p00[,3]);hist(p00[,4])

#nb 50 ja 70 peal v�ga erinevad tulemused!


obsvs = dkm1$aproovitykk_id #263
probs = vector("list", length = length(obsvs))
for(i in 1:length(obsvs)){ 
  obs = obsvs[i]
  model = step(multinom(formula1, dkm1[dkm1$aproovitykk_id != obs,], weights = weight))
  probs[[i]] = predict(model, dkm1[dkm1$aproovitykk_id == obs,], type = "probs")
}

obsvs = sidxx #601
probs_all = vector("list", length = length(obsvs))
model0 = step(multinom(formula1, dkm1, weights = weight))
for(i in 1:length(obsvs)){ 
  obs = obsvs[i]
  if(!(obs %in% dkm1$aproovitykk_id)){
    probs_all[[i]] = predict(model0, newdata = dkm0[dkm0$aproovitykk_id == obs,], type = "probs")
  }
  else{
    model = step(multinom(formula1, dkm1[dkm1$aproovitykk_id != obs,], weights = weight))
    probs_all[[i]] = predict(model, dkm1[dkm1$aproovitykk_id == obs,], type = "probs")
  }
}

#v�is tekkida see probleem, et l�hedal asuvad takseeralad ei ole k�ll dkm1-s, aga k�rvalala on

dfprob <- data.frame(matrix(unlist(probs_all), nrow=length(probs_all), byrow=T))
plot(data_puud_raie_props[,2],dfprob[,2])
hist(dfprob[,4])
sum(dfprob)
#k�ik pmst 1 v�i 0...

sid0 = sidxx[!(sidxx %in% dkm1$aproovitykk_id)] #338
dkmsid0 = dkm0[dkm0$aproovitykk_id %in% sid0,]

pred00 = predict(model0, newdata = dkmsid0, type = "probs") #miks k�ik 1-l�hedased?!


confm = table(data.frame(true = dkm1$cl, pred = dkm1$multi_pred_step_muld))
confm

1 - sum(diag(confm))/sum(confm)

#jama :/


#PCA?

dpca = prcomp(dkm0[,2:37])
summary(dpca)
names(dpca)

dkmpca = dkm0
dkmpca[2:37] = dpca$x;
names(dkmpca)[2:37] = paste0(rep("PC",36), c(1:36))
dkmpca1 = dkmpca[dkmpca$aproovitykk_id %in% dkm1$aproovitykk_id,]
formulapca = as.formula(cl ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + factor(muld))
modelpcastep = step(multinom(formula = formulapca, data = dkmpca1, weights = weight)) #step ei andnud midagi juurde
Anova(modelpcastep)

pcasid0 = dkmpca[dkmpca$aproovitykk_id %in% sid0,]
pred0 = predict(modelpca, newdata = pcasid0, type = "probs")
hist(pred0[,1])
hist(pred0[,2])
hist(pred0[,3])
hist(pred0[,4])

puud = c("ARV_VMA", "ARV_VKU", "ARV_VKS", "ARV_VXX")
dp0 = taks_uus[taks_uus$aproovitykk_id %in% sid0, puud]
dp0props = dp0 / rowSums(dp0)

plot(dp0props[,1], pred0[,4])
plot(dp0props[,2], pred0[,2])
plot(dp0props[,3], pred0[,1])
plot(dp0props[,4], pred0[,3])

#k�igi andmete peal:
obsvs = sidxx #601
probs_pca = vector("list", length = length(obsvs))
#modelpcastep = step(multinom(formula = formulapca, data = dkmpca1)) #, weights = weight
for(i in 1:length(obsvs)){ 
  obs = obsvs[i]
  if(!(obs %in% dkm1$aproovitykk_id)){
    probs_pca[[i]] = predict(modelpcastep, newdata = dkmpca[dkmpca$aproovitykk_id == obs,], type = "probs")
  }
  else{
    model = step(multinom(formulapca, dkmpca1[dkmpca1$aproovitykk_id != obs,])) #, weights = weight
    probs_pca[[i]] = predict(model, dkmpca1[dkmpca1$aproovitykk_id == obs,], type = "probs")
  }
}

dfprob <- data.frame(matrix(unlist(probs_pca), nrow=length(probs_pca), byrow=T))
#KS, KU KX1, MA

mean((data_puud_raie_props[,1]- dfprob[,4])**2) # oli: 0.2988303; n��d kui kuni PCA12: 0.3732013
mean((data_puud_raie_props[,2]- dfprob[,2])**2) # oli: 0.1491831;                      0.2007019
mean((data_puud_raie_props[,3]- dfprob[,1])**2) # oli: 0.1723321;                     0.2314027
mean((data_puud_raie_props[,4]- dfprob[,3])**2) # oli: 0.1359855;                     0.1803003

hist(dfprob[,1])
hist(dfprob[,2])

#kaaludega koos veel hullem pilti, aga ega v�ga vahet pole:
# > mean((data_puud_raie_props[,1]- dfprob[,4])**2) #0.2988303
# [1] 0.3271569
# > mean((data_puud_raie_props[,2]- dfprob[,2])**2) #0.1491831
# [1] 0.1754541
# > mean((data_puud_raie_props[,3]- dfprob[,1])**2) #0.1723321
# [1] 0.1912671
# > mean((data_puud_raie_props[,4]- dfprob[,3])**2) #0.1359855
# [1] 0.1500178

#kui hinnata k�igile samade argumentidega mudel?
obsvs = sidxx #601
probs_same = vector("list", length = length(obsvs))
model0 = step(multinom(formula1, dkm1, weights = weight))
# multinom(formula = cl ~ B02_kevad1 + B02_kevad2 + B03_kevad1 + 
#            B03_kevad2 + B04_kevad1 + B04_kevad2 + B05_kevad1 + B05_kevad2 + 
#            B06_kevad1 + B06_kevad2 + B07_kevad1 + B07_kevad2 + B08_kevad1 + 
#            B08_kevad2 + B11_kevad1 + B11_kevad2 + B12_kevad1 + B12_kevad2 + 
#            B02_vahe_kevad + B03_vahe_kevad + B04_vahe_kevad + B05_vahe_kevad + 
#            B06_vahe_kevad + B07_vahe_kevad + B08_vahe_kevad + B11_vahe_kevad + 
#            B12_vahe_kevad, data = dkm1, weights = weight)
#selle p�hjal k�igile sama mudel hinnata!


for(i in 1:length(obsvs)){ 
  obs = obsvs[i]
  if(!(obs %in% dkm1$aproovitykk_id)){
    probs_same[[i]] = predict(model0, newdata = dkm0[dkm0$aproovitykk_id == obs,], type = "probs")
  }
  else{
    model = multinom(formula = cl ~ B02_kevad1 + B02_kevad2 + B03_kevad1 + 
                        B03_kevad2 + B04_kevad1 + B04_kevad2 + B05_kevad1 + B05_kevad2 + 
                        B06_kevad1 + B06_kevad2 + B07_kevad1 + B07_kevad2 + B08_kevad1 + 
                        B08_kevad2 + B11_kevad1 + B11_kevad2 + B12_kevad1 + B12_kevad2 + 
                        B02_vahe_kevad + B03_vahe_kevad + B04_vahe_kevad + B05_vahe_kevad + 
                        B06_vahe_kevad + B07_vahe_kevad + B08_vahe_kevad + B11_vahe_kevad + 
                        B12_vahe_kevad, data = dkm1[dkm1$aproovitykk_id != obs,], weights = weight)
    probs_same[[i]] = predict(model, dkm1[dkm1$aproovitykk_id == obs,], type = "probs")
  }
}
dfprob1 <- data.frame(matrix(unlist(probs_same), nrow=length(probs_same), byrow=T))
hist(dfprob1[,3])
hist(dfprob1[,4])

#
z <- summary(model0)$coefficients/summary(model0)$standard.errors
# For simplicity, use z-test to approximate t test.
pv <- (1 - pnorm(abs(z)))*2 
pv

require(car);require(lmtest)
Anova(model0) #mingi error sellega! p-v��rtused ju 1 :D

model = multinom(formula = cl ~ B02_kevad1 + B02_kevad2 + B03_kevad1 + 
                   B03_kevad2 + B04_kevad1 + B04_kevad2 + B05_kevad1 + B05_kevad2 + 
                   B06_kevad1 + B06_kevad2 + B07_kevad1 + B07_kevad2 + B08_kevad1 + 
                   B08_kevad2 + B11_kevad1 + B11_kevad2 + B12_kevad1 + B12_kevad2 + 
                   B02_vahe_kevad + B03_vahe_kevad + B04_vahe_kevad + B05_vahe_kevad + 
                   B06_vahe_kevad + B07_vahe_kevad + B08_vahe_kevad + B11_vahe_kevad + 
                   B12_vahe_kevad, data = dkm1, weights = weight)
Anova(model)

test = multinom(formula = cl ~ B02_kevad1 + B02_vahe_kevad, data = dkm1, weights = weight)
Anova(test1)

test1 = multinom(formula = cl ~  B03_kevad1 + 
                   B08_kevad2 + B02_vahe_kevad + B03_vahe_kevad + B04_vahe_kevad + B08_vahe_kevad + factor(muld), data = dkm1, weights = weight)

test1 #ikka ei saa olla �ige asi, liiga sarnased koefitsendid, nii ei ole ju v�imalik eristada

pt1 = predict(test1, newdata = dkmsid0, type = "probs")
plot(dp0props[,1], pt1[,4])
plot(dp0props[,2], pt1[,2])
plot(dp0props[,3], pt1[,1])
plot(dp0props[,4], pt1[,3])

require(MASS)
model0 = stepAIC(multinom(formula1, dkm1, weights = weight), direction = "forward")
model0
Anova(model0)

###### multinomial 506  / 455 ###########################

setwd("A:/MAKA/TEST/test")
load("taks_info.RData")
load("sid506.RData")
d506.100 = read.csv("d506_100.csv")
data = d506.100
vars = names(data)[3:49]

#seda allj�rgnevat jama pole vaja!

# #klassid, kui v�tta piiriks 80%
# 
# koos$cl80 = "cl80";
# koos$cl70 = "cl70";
# koos$cl50 = "cl50";
# koos$KX1 = koos$HB + koos$LV + koos$LM + koos$KX
# require(dplyr)
# koos = koos %>% mutate(weight = pmax(MA,KU, KS, KX1))
# koos = koos[!(is.na(koos$MA)),]
# 
# koos$cl80 = "cl80"
# koos[koos$MA >= 80,]$cl80 = "MA"
# koos[koos$KU >= 80,]$cl80 = "KU"
# koos[koos$KS >= 80,]$cl80 = "KS"
# koos[koos$KX1 >= 80,]$cl80 = "KX1"
# 
# koos$cl70 = "cl70"
# koos[koos$MA >= 70,]$cl70 = "MA"
# koos[koos$KU >= 70,]$cl70 = "KU"
# koos[koos$KS >= 70,]$cl70 = "KS"
# koos[koos$KX1 >= 70,]$cl70 = "KX1"
# 
# koos$cl50 = "cl50"
# koos[koos$MA > 50,]$cl50 = "MA"
# koos[koos$KU > 50,]$cl50 = "KU"
# koos[koos$KS > 50,]$cl50 = "KS"
# koos[koos$KX1 > 50,]$cl50 = "KX1"
# 
# 
# #dkm0 = dkm;
# dkm80 = merge(d506.100,koos[,c("aproovitykk_id", "cl80", "muld")], by = "aproovitykk_id", all.x = T)
# dkm70 = merge(d506.100,koos[,c("aproovitykk_id", "cl70", "muld")], by = "aproovitykk_id", all.x = T)
# dkm50 = merge(d506.100,koos[,c("aproovitykk_id", "cl50", "muld")], by = "aproovitykk_id", all.x = T)
# 
# dkm = merge(d506.100,koos[,c("aproovitykk_id", "cl50", "cl70","cl80", "weight", "muld")], by = "aproovitykk_id", all.x = T)
# dkm1 = na.omit(dkm)

data$cl80 = "cl80";data$cl70 = "cl70";data$cl50 = "cl50";


data[data$ARV_VMA >= .8,]$cl80 = "MA"
data[data$ARV_VKU >= .8,]$cl80 = "KU"
data[data$ARV_VKS >= .8,]$cl80 = "KS"
data[data$ARV_VXX >= .8,]$cl80 = "KX"

data[data$ARV_VMA >= .7,]$cl70 = "MA"
data[data$ARV_VKU >= .7,]$cl70 = "KU"
data[data$ARV_VKS >= .7,]$cl70 = "KS"
data[data$ARV_VXX >= .7,]$cl70 = "KX"

data[data$ARV_VMA >= .5,]$cl50 = "MA"
data[data$ARV_VKU >= .5,]$cl50 = "KU"
data[data$ARV_VKS >= .5,]$cl50 = "KS"
data[data$ARV_VXX >= .5,]$cl50 = "KX"




#################### GAM-iga ##################???
d70 = data[data$cl70 != "cl70",]
d80 = data[data$cl80 != "cl80",]

#dlogit = mlogit.data(d70, choice = "cl70", alt.levels = c("MA", "KU", "KS", "KX1"))
#require(mlogit)#�kki selle paketiga t��tab logit ja step koos?

require(nnet)
require(MASS)

namesz = names(d70)[c(5:40)] #3. on muld
namesz[1] = "factor(muld)"
formula1 = as.formula(paste("cl70", paste(namesz, collapse=" + "), sep=" ~ "))
ft = as.formula(paste("cl70", paste(namesz[1:10], collapse=" + "), sep=" ~ "))

m1 = step(multinom(formula1, d70, weights = weight, maxit = 1000))
m2 = stepAIC(multinom(ft, d70, maxit = 1000), direction = "backward")

d_70 = data[data$cl70 == "cl70",]
d_80 = data[data$cl80 == "cl80",]


tst = data.frame(predict(m2, newdata = d_70, type = "probs"))
tst$aproovitykk_id = d_70$aproovitykk_id
dp = merge(tst, taks.info, by = "aproovitykk_id", all.x = T)
dev.off()
par(mfrow = c(2,2))
plot(dp[,11],dp[,5], xlab = "M�nd", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dp[,12],dp[,3], xlab = "Kuusk", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dp[,13],dp[,2], xlab = "Kask", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dp[,14],dp[,4], xlab = "Muu", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))


#mingit liiki bagging; valime juhuslikud tunnused!
#HETKEL ILMA MULLATA!
var.mult = names(d80)[c(5:38,47:60)] #3. on muld
#var.mult[1] = "factor(muld)"

pred = matrix(0,nrow = dim(d_80)[1], ncol = 4)
N = 1000; ssize = 5
for(i in 1:N){
  var.mult.sample = sample(var.mult,ssize)
  formula = as.formula(paste("cl80", paste(var.mult.sample, collapse=" + "), sep=" ~ "))
  m = stepAIC(multinom(formula, d80, maxit = 10000), trace = F)
  #m = multinom(formula, d80, maxit = 100) #v�tsin maxit 1000 pealt 100 peale
  pred0 = predict(m, newdata = d_80, type = "probs")
  pred = pred0 + pred
}

pred = pred/N 
tst = round(pred,3)

predx = data.frame(pred);predx = predx[,c("MA","KU","KS","KX")]
predx$aproovitykk_id = d_80$aproovitykk_id

dp = merge(predx, taks.info, by = "aproovitykk_id", all.x = T)
dev.off()
par(mfrow = c(2,2))
plot(dp[,11],dp[,2], xlab = "M�nd", ylab = "Hinnang", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dp[,12],dp[,3], xlab = "Kuusk", ylab = "Hinnang", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dp[,13],dp[,4], xlab = "Kask", ylab = "Hinnang", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dp[,14],dp[,5], xlab = "Muu", ylab = "Hinnang", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))

sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4) 
#ssize = 3; N = 500: #0.2063837; N = 2500: 0.2065031
#sama ilma kaaludeta: 0.2075632; 0.2056211; n500 mullaga 0.2054041/0.2054204 N1000/0.2061199 N1000 kaaludega
#ssize 4 n500 0.2107895, n1000 mullaga 0.2088891, viimane d70: 0.2124759
#ssize 10 N100 muld 0.230615, kui t�sta maxit 10000 peale, siis 0.2273758

#otsustame, et parim on 3! aga kui muld mitte faktor vaid 0-1? ega vahet pole
#ssize = 4 korral n��d 0.2033682

#n��d k�ik elemendid:

#aga kui v�tta RF-olulised?
var.mult = vars # FI2$Feature[1:20]
N = 50; ssize = 5
#obsinout = vector("list", length = 2); obsinout[[1]] = d80$aproovitykk_id;obsinout[[2]] = d_80$aproovitykk_id
pred = matrix(0,nrow = dim(d_80)[1], ncol = 4)
pred1 = data.frame("aproovitykk_id" = sidxx, "MA" = 0, "KU" = 0, "KS" = 0, "KX" = 0)
for(i in 1:N){
  print(i);print(Sys.time())
  var.mult.sample = sample(var.mult,ssize)
  formula = as.formula(paste("cl80", paste(var.mult.sample, collapse=" + "), sep=" ~ "))
  m = stepAIC(multinom(formula, d80, maxit = 10000, trace = F), trace = F)
  pred0 = predict(m, newdata = d_80, type = "probs")
  pred0 = data.frame(pred0)
  pred0 = pred0[,c("MA","KU","KS","KX")]
  #pred[pred$aproovitykk_id %in% unlist(obsinout[[2]]),c("MA","KU","KS","KX")] = pred[pred$aproovitykk_id %in% unlist(obsinout[[2]]),c("MA","KU","KS","KX")] + pred0
  pred = pred + pred0
  for (obs in d80$aproovitykk_id) {
    dobs = d80[!(d80$aproovitykk_id %in% obs),]
    m = stepAIC(multinom(formula, dobs, maxit = 10000, trace = F), trace = F)
    pred0 = predict(m, newdata = d80[d80$aproovitykk_id == obs,], type = "probs")
    pred0 = pred0[c("MA","KU","KS","KX")]
    pred1[pred1$aproovitykk_id == obs,c("MA","KU","KS","KX")] = pred1[pred1$aproovitykk_id == obs,c("MA","KU","KS","KX")] + pred0
  }
}
pred$aproovitykk_id = d_80$aproovitykk_id
pred1[pred1$aproovitykk_id %in% d_80$aproovitykk_id,2:5] = pred[,1:4]
pred1[,2:5] = pred1[,2:5] / N

dp = merge(pred1, taks.info, by = "aproovitykk_id", all.x = T)
dev.off()
par(mfrow = c(2,2))
plot(dp[,11],dp[,2], xlab = "M�nd", ylab = "Hinnang", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dp[,12],dp[,3], xlab = "Kuusk", ylab = "Hinnang", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dp[,13],dp[,4], xlab = "Kask", ylab = "Hinnang", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dp[,14],dp[,5], xlab = "Muu", ylab = "Hinnang", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))

sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4) 
#                         RF-oluliste tunnuste pealt    
                           #N10 ssize 1: 0.2565358
                          #N50 ssize 2: 0.2027469
                          #N50 ssize 3: 0.1986232
#N100 ssize 4: 0.1987356                0.2081682
#N100 ssize 5: 0.198063                 0.2109939
#N100 ssize 6: 0.1985812                
#N100 ssize 7: 0.1989825
#n100 ssize 8: 0.1989967
#n100 ssize 9: 0.1989967

N = 100; ssize = 9
#obsinout = vector("list", length = 2); obsinout[[1]] = d80$aproovitykk_id;obsinout[[2]] = d_80$aproovitykk_id
pred = matrix(0,nrow = dim(d_80)[1], ncol = 4)
pred1 = data.frame("aproovitykk_id" = sidxx, "MA" = 0, "KU" = 0, "KS" = 0, "KX" = 0)
for(i in 1:N){
  print(i);print(Sys.time())
  var.mult.sample = sample(var.mult,ssize)
  formula = as.formula(paste("cl80", paste(var.mult.sample, collapse=" + "), sep=" ~ "))
  m = stepAIC(multinom(formula, d80, maxit = 10000, trace = F), trace = F)
  pred0 = predict(m, newdata = d_80, type = "probs")
  pred0 = data.frame(pred0)
  pred0 = pred0[,c("MA","KU","KS","KX")]
  #pred[pred$aproovitykk_id %in% unlist(obsinout[[2]]),c("MA","KU","KS","KX")] = pred[pred$aproovitykk_id %in% unlist(obsinout[[2]]),c("MA","KU","KS","KX")] + pred0
  pred = pred + pred0
  for (obs in d80$aproovitykk_id) {
    dobs = d80[!(d80$aproovitykk_id %in% obs),]
    m = stepAIC(multinom(formula, dobs, maxit = 10000, trace = F), trace = F)
    pred0 = predict(m, newdata = d80[d80$aproovitykk_id == obs,], type = "probs")
    pred0 = pred0[c("MA","KU","KS","KX")]
    pred1[pred1$aproovitykk_id == obs,c("MA","KU","KS","KX")] = pred1[pred1$aproovitykk_id == obs,c("MA","KU","KS","KX")] + pred0
  }
}
pred$aproovitykk_id = d_80$aproovitykk_id
pred1[pred1$aproovitykk_id %in% d_80$aproovitykk_id,2:5] = pred[,1:4]
pred1[,2:5] = pred1[,2:5] / N

dp = merge(pred1, taks.info, by = "aproovitykk_id", all.x = T)
dev.off()
par(mfrow = c(2,2))
plot(dp[,11],dp[,2], xlab = "M�nd", ylab = "Hinnang", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dp[,12],dp[,3], xlab = "Kuusk", ylab = "Hinnang", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dp[,13],dp[,4], xlab = "Kask", ylab = "Hinnang", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dp[,14],dp[,5], xlab = "Muu", ylab = "Hinnang", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))

sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4) 



#multinomial w feature importance

FI2 = read.csv("feature_IMP_500k.csv", header = T)
imp10 = FI2$Feature[1:10]; imp10 = as.character(imp10)
imp3 = FI2$Feature[1:3]; imp3 = as.character(imp3)
formula.imp = as.formula(paste("cl70", paste(imp10, collapse=" + "), sep=" ~ "))
formula.imp3 = as.formula(paste("cl70", paste(imp3, collapse=" + "), sep=" ~ "))

m1imp = step(multinom(formula.imp3, d70, maxit = 1000)) #weights?
#B11_kevad2 l�ks v�lja
m2imp = stepAIC(multinom(formula.imp, d70, maxit = 1000)) #weights?
#B11_kevad2 v�lja, AIC ka sama
Anova(m1imp)

tst = data.frame(predict(m1imp, newdata = d_70, type = "probs"))
tst$aproovitykk_id = d_70$aproovitykk_id
dp = merge(tst, taks.info, by = "aproovitykk_id", all.x = T)
dev.off()
par(mfrow = c(2,2))
plot(dp[,11],dp[,5], xlab = "M�nd", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dp[,12],dp[,3], xlab = "Kuusk", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dp[,13],dp[,2], xlab = "Kask", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dp[,14],dp[,4], xlab = "Muu", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))

sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4)
# 0.4795727 ilma weights; aga weights argument ei muuda midagi!?

#npmr?
require(npmr)

RMSE.imps.min = c()
for(j in 15:15){
imps = FI2$Feature[1:j]; imps = as.character(imps)
require(stringi)
imps[stri_length(imps) < 4] = paste("X",imps[stri_length(imps) < 4], sep = "")
#imps = c(imps,"muld")
dd = d80
dd0 = d_80
Y = dd$cl80 #siia 70 v�i 80
X = as.matrix(dd[imps]); 
lammas = 1:200
m1 = npmr(X = X, Y = Y, lambda = lammas)
testX = as.matrix(dd0[imps])
tst = predict.npmr(m1, testX)
tst.train = predict.npmr(m1,X)

#imps = FI2$Feature[1:5]; imps = as.character(imps)
RMSE.lammas = c()
for(i in 1:200){
  tt = as.data.frame(tst[,,i])
  tt$aproovitykk_id = dd0$aproovitykk_id
  tt.train = as.data.frame(tst.train[,,i])
  tt.train$aproovitykk_id = dd$aproovitykk_id
  tt = rbind(tt,tt.train)
  dp = merge(tt, taks.info, by = "aproovitykk_id", all.x = T)
  nms = names(dp); nms[2] = "V4";nms[3] = "V2"; nms[4] = "V1";nms[5] = "V3"; dp = dp[nms]
  RMSE.lammas[i] = sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4)
}
print(RMSE.lammas)
plot(RMSE.lammas); which.min(RMSE.lammas)
RMSE.imps.min[j] = min(RMSE.lammas)
}
plot(RMSE.imps.min, type = "o")
min(RMSE.imps.min); which.min(RMSE.imps.min)
#d 70: 0.1911186, imps = 28
#d 80: 0.1905613, imps = 29, 0.1902863 imps 40 (kui v�tta 40, siis on parim tulemus 40 juures)
#d80 n�ks paremad tulemused
#bagging!?

#parim lambda 5 tunnuse korral?


imps = FI2$Feature[1:20]; imps = as.character(imps);imps[stri_length(imps) < 4] = paste("X",imps[stri_length(imps) < 4], sep = "")
N = 100
m = 5
lambda = 10
Y = dd$cl80
ids = data$aproovitykk_id
pred = matrix(0,nrow = 455, ncol = 4)
for(i in 1:N){
  imp.sample = sample(imps,m)
  X = as.matrix(dd[imp.sample]) 
  m1 = npmr(X = X, Y = Y, lambda = lambda)
  dataX = as.matrix(data[imp.sample]) 
  pred0 = predict.npmr(m1, dataX)[,,1]
  #pred$aproovitykk_id = ids ei ole vaja, kui lihtsalt keskmine v�tta
  pred = pred + pred0
}

rowSums(pred / N)
predN = data.frame(pred/N)
predN$aproovitykk_id = ids
dp = merge(predN, taks.info, by = "aproovitykk_id", all.x = T)
nms = names(dp); nms[2] = "X4";nms[3] = "X2"; nms[4] = "X1";nms[5] = "X3"; dp = dp[nms]
sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4)
#0.2072665 :(

dev.off()
par(mfrow = c(2,2))
plot(dp[,11],dp[,2], xlab = "M�nd", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dp[,12],dp[,3], xlab = "Kuusk", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dp[,13],dp[,4], xlab = "Kask", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dp[,14],dp[,5], xlab = "Muu", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))

N = 100
Y = dd$cl80
ids = data$aproovitykk_id
heat = matrix(NA,20,20)
for(l in 1:20){
  for(m in 1:20) {
    lambda = l
    pred = matrix(0,nrow = 455, ncol = 4)
    for(i in 1:N){
      imp.sample = sample(imps,m)
      X = as.matrix(dd[imp.sample]) 
      m1 = npmr(X = X, Y = Y, lambda = lambda)
      dataX = as.matrix(data[imp.sample]) 
      pred0 = predict.npmr(m1, dataX)[,,1]
      #pred$aproovitykk_id = ids ei ole vaja, kui lihtsalt keskmine v�tta
      pred = pred + pred0
    }
    predN = data.frame(pred/N)
    predN$aproovitykk_id = ids
    dp = merge(predN, taks.info, by = "aproovitykk_id", all.x = T)
    nms = names(dp); nms[2] = "X4";nms[3] = "X2"; nms[4] = "X1";nms[5] = "X3"; dp = dp[nms]
    heat[l,m] = sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4)
  }
}

#save(heat, file = "penalizedMR_N100_features_40_parima_seast.RData")
#ave(heat, file = "penalizedMR_N100_features_20_parima_seast.RData")

load(file = "penalizedMR_N100_features_40_parima_seast.RData")


min(heat) #18.6: tunnuste arv 1!!!, lambda = 6; teisel katsel 0.1876159, lambda 8, tunnuste arv 6
which(heat == min(heat), arr.ind = TRUE)

#n��d sain tunnuste arv 3, lambda = 7 ja  0.1871927
#kui valida 20 parima tunnuse seast? siis 0.189083

#
moos = as.data.frame(heat)
colnames(moos) = paste("Lambda_", 1:20, sep = "")
moos$tunnuseid = paste("Tunnuste arv_", 1:20, sep = "")
moos$sort = 1:20

moos.m <- melt(moos,id.vars = c("tunnuseid","sort"))
moos.m$tunnuseid = as.factor(moos.m$tunnuseid)

moos.m$tunnuseid <- factor(moos.m$tunnuseid, levels= unique((moos.m$tunnuseid)[order(moos.m$sort)]))


plot = ggplot(moos.m,aes(variable,tunnuseid)) + geom_tile(aes(fill=value),color = "white") +
  guides(fill=guide_colorbar("RMSE")) +
  scale_fill_gradientn(colors=c("skyblue","yellow","tomato"),guide="colorbar") +
  theme(axis.text.x = element_text(angle = 270, hjust = 0,vjust=-0.05))
plot + labs(x = "", y = "")

#proovime selle lambda 6, tunnuseid 8: N = 1000 korral tuli 0.188552
#kui beta-ga agregeerida, siis:

N = 100;
m = 8
lambda = 6
Y = dd$cl80
ids = data$aproovitykk_id
pred = matrix(0,nrow = 455, ncol = 4)

df = data.frame(X1 = c(), X2 = c(), X3 = c(), X4 = c(), aproovitykk_id = c())


for(i in 1:N){
  imp.sample = sample(imps,m)
  X = as.matrix(dd[imp.sample]) 
  m1 = npmr(X = X, Y = Y, lambda = lambda)
  dataX = as.matrix(data[imp.sample]) 
  pred0 = data.frame(predict.npmr(m1, dataX)[,,1])
  pred0$aproovitykk_id = ids #ei ole vaja, kui lihtsalt keskmine v�tta
  df = rbind(df,pred0)
  #pred = pred + pred0
}

#need ainult mean jaoks
#predN = data.frame(pred/N)
#predN$aproovitykk_id = ids

df1 = df
#df1[,1:4] = (df1[,1:4]*5488 + 0.5)/5489
df1[,1:4] = df1[,1:4] / rowSums(df1[,1:4])
mult = df1 %>% group_by(aproovitykk_id) %>% summarise_all(funs(epa.kernel)) #bets_fun(.,method = "mme")
dp = merge(mult, taks.info, by = "aproovitykk_id", all.x = T)
nms = names(dp); nms[2] = "X4";nms[3] = "X2"; nms[4] = "X1";nms[5] = "X3"; dp = dp[nms]
sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4)
#bets: 0.1985807
#mean 0.1876734
#epa.kernel 0.1952159

dev.off()
par(mfrow = c(2,2))
plot(dp[,11],dp[,2], xlab = "M�nd", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dp[,12],dp[,3], xlab = "Kuusk", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dp[,13],dp[,4], xlab = "Kask", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dp[,14],dp[,5], xlab = "Muu", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))


N = 100;  m = 3; lambda = 7
Y1 = dd$cl80
pred = matrix(0, ncol = 4,nrow = 455)

pred = data.frame("X1" = 0, "X2" = 0, "X3" = 0, "X4" = 0, "aproovitkk_id" = data$aproovitykk_id)

for(i in 1:N){
  imp.sample = sample(imps,m)
  X1 = as.matrix(d80[imp.sample]) 
  m1 = npmr(X = X1, Y = Y1, lambda = lambda)
  dataX = as.matrix(d_80[imp.sample]) 
  pred1 = data.frame(predict.npmr(m1, dataX)[,,1])
  pred1$aproovitykk_id = d_80$aproovitykk_id
  pred[pred$aproovitkk_id %in% pred1$aproovitykk_id,1:4] = pred[pred$aproovitkk_id %in% pred1$aproovitykk_id,1:4] + pred1[,1:4]
  m2 = cv.npmr(X = X2, Y = Y2, lambda = lambda)
  pred2 = data.frame(predict.npmr(m1, X1)[,,1])
  pred2$aproovitykk_id = d80$aproovitykk_id
  pred[pred$aproovitkk_id %in% pred2$aproovitykk_id,1:4] = pred[pred$aproovitkk_id %in% pred2$aproovitykk_id,1:4] + pred2[,1:4]
}


predN = pred[,1:4] / N
predN$aproovitykk_id = pred$aproovitkk_id
dp = merge(predN, taks.info, by = "aproovitykk_id", all.x = T)
nms = names(dp); nms[2] = "X4";nms[3] = "X2"; nms[4] = "X1";nms[5] = "X3"; dp = dp[nms]

dev.off()
par(mfrow = c(2,2))
plot(dp[,11],dp[,2], xlab = "M�nd", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dp[,12],dp[,3], xlab = "Kuusk", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dp[,13],dp[,4], xlab = "Kask", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dp[,14],dp[,5], xlab = "Muu", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4) #0.1999261


#ristvalideerime natuke teisiti:
# 3 7 parim, proovin ka 2 6
N = 500;  m = 2; lambda = 6
Y1 = dd$cl80
pred = matrix(0, ncol = 4,nrow = 455)
#pred = data.frame("X1" = 0, "X2" = 0, "X3" = 0, "X4" = 0, "aproovitkk_id" = data$aproovitykk_id)
for(i in 1:N){
  imp.sample = sample(imps,m)
  X1 = as.matrix(d80[imp.sample]) 
  m1 = npmr(X = X1, Y = Y1, lambda = lambda)
  dataX = as.matrix(d_80[imp.sample]) 
  pred1 = predict.npmr(m1, dataX)[,,1]
  out1 = 1:dim(d80)[1];out2 = 2:dim(d80)[1];out2 = c(out2,1) #saab ainult kahe-kaupa predictida
  pred00 = matrix(,nrow = 0, ncol = 4)
  for(i in 1:dim(d80)[1]){
    Y2 = Y1[-c(out1[i],out2[i])]
    X2 = X1[-c(out1[i],out2[i]),]
    m2 = npmr(X = X2, Y = Y2, lambda = lambda)
    dataX = as.matrix(d80[imp.sample]) 
    dataX = dataX[c(out1[i],out2[i]),]
    pred2 = predict.npmr(m2, dataX)[,,1][1,]
    pred00 = rbind(pred00,pred2)
  }
  pred1 = rbind(pred1,pred00)
  pred = pred + pred1
}

predN = pred[,1:4] / N
predN = as.data.frame(predN)
predN$aproovitykk_id = c(d_80$aproovitykk_id,d80$aproovitykk_id)
dp = merge(predN, taks.info, by = "aproovitykk_id", all.x = T)
nms = names(dp); nms[2] = "V4";nms[3] = "V2"; nms[4] = "V1";nms[5] = "V3"; dp = dp[nms]
sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4) #0.1999261
#N=2 0.1992425

#0.2034 n��d n100
#0.2010435 N1000
#0.1952n100
#kui paljude imp seast see on?


dev.off()
par(mfrow = c(2,2))
plot(dp[,11],dp[,2], xlab = "M�nd", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369), pch = 16)
abline(lm(dp[,2] ~ dp[,11]))
plot(dp[,12],dp[,3], xlab = "Kuusk", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369), pch = 16)
abline(lm(dp[,3] ~ dp[,12]))
plot(dp[,13],dp[,4], xlab = "Kask", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369), pch = 16)
abline(lm(dp[,4] ~ dp[,13]))
plot(dp[,14],dp[,5], xlab = "Muu", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369), pch = 16)
abline(lm(dp[,5] ~ dp[,14]))












