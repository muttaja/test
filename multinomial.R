#multinomial


#NB! vaata ka random forest
#The ranger package in R (pdf), which is relatively new, will do this. 
#The ranger implementation of random forests has a case.weights argument that takes a vector with individual case / observation weights.

raied_ok = raied[raied$raie == 0,]$aproovitykk_id #427
load("ID_OK.RData", verbose = T)# 174

sidxx = c(id_ok, raied_ok) #601


#dk andmestik failist landsat_to_sentinel
dk[,-1] = scale(dk[,-1])

muld1 = data.frame(muld = koos[koos$aproovitykk_id %in% mets_id,]$muld)
muld1$aproovitykk_id = koos$aproovitykk_id[koos$aproovitykk_id %in% mets_id]
muld1[is.na(muld1$muld),]$muld = 999

#midagi kokku võtta?
table(muld1[muld1$aproovitykk_id %in% sidxx,]$muld) #alla 10 kindlasti kokku; 2 puuduvat väärtust
#võtan alla 30 välja

muld1[muld1$muld %in% c(10,11,16,73,200,999,31,37,53,57,63), "muld"] = 999

#muld 1-0 tüüpi andmestikuks
muld2 = dcast(muld1,aproovitykk_id~muld,fun.aggregate = function(x){as.integer(length(x) > 0)})

dkm = merge(dk, muld2, by = "aproovitykk_id")

#klassid, kui võtta piiriks 80%

koos$cl = "cl";
koos$KX1 = koos$HB + koos$LV + koos$LM + koos$KX
require(dplyr)
koos = koos %>% mutate(weight = pmax(MA,KU, KS, KX1))

koos[!(is.na(koos$MA)) & koos$MA >= 80,]$cl = "MA"
koos[!(is.na(koos$KU)) & koos$KU >= 80,]$cl = "KU"
koos[!(is.na(koos$KS)) & koos$KS >= 80,]$cl = "KS"
koos[!(is.na(koos$KX1)) & koos$KX1 >= 80,]$cl = "KX1"

dkm0 = dkm;
dkm0 = merge(dkm0,koos[,c("aproovitykk_id", "cl", "weight", "muld")], all.x = T)
table(dkm0$cl)
dkm0 = dkm0[dkm0$aproovitykk_id %in% sidxx,]


dkm1 = dkm0 %>% filter(cl != "cl")
table(dkm1$cl)
dkm1[dkm1$muld %in% names(table(dkm1$muld) < 10)[table(dkm1$muld) < 10],"muld"] = 999
dkm1[is.na(dkm1$muld),"muld"] = 999

dkm0[!(dkm0$muld %in% names(table(dkm1$muld) < 10)[table(dkm1$muld) >= 10]),"muld"] = 999
dkm0[is.na(dkm0$muld),"muld"] = 999
table(dkm0$muld)


#kui nüüd anda kaalud vastavalt
#a.) osakaalule
#b.) tüvemahu hinnagule


require(nnet)

namesz = names(dkm1)[c(2:37,51)]
namesz[37] = "factor(muld)"

formula1 = as.formula(paste("cl", paste(namesz, collapse=" + "), sep=" ~ "))
m1 = multinom(formula1, dkm1, weights = weight)
m1
summary(m1)
p1 = predict(m1, dkm1, type = "probs")

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

#võis tekkida see probleem, et lähedal asuvad takseeralad ei ole küll dkm1-s, aga kõrvalala on

dfprob <- data.frame(matrix(unlist(probs_all), nrow=length(probs_all), byrow=T))
plot(data_puud_raie_props[,2],dfprob[,2])
hist(dfprob[,4])
sum(dfprob)
#kõik pmst 1 või 0...

sid0 = sidxx[!(sidxx %in% dkm1$aproovitykk_id)] #338
dkmsid0 = dkm0[dkm0$aproovitykk_id %in% sid0,]

pred00 = predict(model0, newdata = dkmsid0, type = "probs") #miks kõik 1-lähedased?!


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
formulapca = as.formula(cl ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + factor(muld))
modelpcastep = step(multinom(formula = formulapca, data = dkmpca1, weights = weight)) #step ei andnud midagi juurde

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

#kõigi andmete peal:
obsvs = sidxx #601
probs_pca = vector("list", length = length(obsvs))
modelpcastep = step(multinom(formula = formulapca, data = dkmpca1)) #, weights = weight
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

mean((data_puud_raie_props[,1]- dfprob[,4])**2) #0.2988303
mean((data_puud_raie_props[,2]- dfprob[,2])**2) #0.1491831
mean((data_puud_raie_props[,3]- dfprob[,1])**2) #0.1723321
mean((data_puud_raie_props[,4]- dfprob[,3])**2) #0.1359855

hist(dfprob[,1])
hist(dfprob[,2])

#kaaludega koos veel hullem pilti, aga ega väga vahet pole:
# > mean((data_puud_raie_props[,1]- dfprob[,4])**2) #0.2988303
# [1] 0.3271569
# > mean((data_puud_raie_props[,2]- dfprob[,2])**2) #0.1491831
# [1] 0.1754541
# > mean((data_puud_raie_props[,3]- dfprob[,1])**2) #0.1723321
# [1] 0.1912671
# > mean((data_puud_raie_props[,4]- dfprob[,3])**2) #0.1359855
# [1] 0.1500178

#kui hinnata kõigile samade argumentidega mudel?
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
#selle põhjal kõigile sama mudel hinnata!


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
Anova(model0) #mingi error sellega! p-väärtused ju 1 :D

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

test1 #ikka ei saa olla õige asi, liiga sarnased koefitsendid, nii ei ole ju võimalik eristada

pt1 = predict(test1, newdata = dkmsid0, type = "probs")
plot(dp0props[,1], pt1[,4])
plot(dp0props[,2], pt1[,2])
plot(dp0props[,3], pt1[,1])
plot(dp0props[,4], pt1[,3])

require(MASS)
model0 = stepAIC(multinom(formula1, dkm1, weights = weight), direction = "forward")
model0
Anova(model0)
