#multinomiaalne regressioon



require(nnet)
require(car)
#require(lmtest)
require(MASS)



load("taks_info.RData")
d506.100 = read.csv("d506_100.csv") #manuaalselt selekteeritud 506 pilti. neist need, millel t¸vemaht 100+
data = d506.100
vars = names(data)[3:49]

#tunnus selle kohta, mis "klassi" kuulub, kui klassi kuulumise piiriks vıtta 80%, 70% vıi 50%
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





d80 = data[data$cl80 != "cl80",]; d_80 = data[data$cl80 == "cl80",]
d70 = data[data$cl70 != "cl70",]; d_70 = data[data$cl70 == "cl70",]
d50 = data[data$cl50 != "cl50",]; d_50 = data[data$cl50 == "cl50",]




vars = names(data)[c(3:38)]; vars.muld = names(data)[c(47:60)]

#hetkel ilma mullata
formula = as.formula(paste("cl80", paste(vars, collapse=" + "), sep=" ~ "))
m1 = step(multinom(formula, d80, weights = weight, maxit = 10000))
m1 #step samm ei muuda midagi
Anova(m1)

m2 = stepAIC(multinom(formula, d80, weights = weight, maxit = 10000))
m2
Anova(m2)


#prognoosida nende vaatluste peal, kus osakaal alla 80
pred2 = data.frame(predict(m2, newdata = d_80, type = "probs"))
pred2$aproovitykk_id = d_80$aproovitykk_id
dp = merge(pred2, taks.info, by = "aproovitykk_id", all.x = T)
dev.off()
par(mfrow = c(2,2))
plot(dp[,11],dp[,5], xlab = "M‰nd", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dp[,12],dp[,3], xlab = "Kuusk", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dp[,13],dp[,2], xlab = "Kask", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dp[,14],dp[,4], xlab = "Muu", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))

#kıik v.a. 2 prognoosi 0 vıi 1

#kui vıtta 5 juhuslikku tunnust:
rvars = sample(vars,5)
formula = as.formula(paste("cl80", paste(rvars, collapse=" + "), sep=" ~ "))
m3 = stepAIC(multinom(formula, d80, weights = weight, maxit = 10000))

pred2 = data.frame(predict(m3, newdata = d_80, type = "probs"))
pred2$aproovitykk_id = d_80$aproovitykk_id
dp = merge(pred2, taks.info, by = "aproovitykk_id", all.x = T)
dev.off()
par(mfrow = c(2,2))
plot(dp[,11],dp[,5], xlab = "M‰nd", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dp[,12],dp[,3], xlab = "Kuusk", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dp[,13],dp[,2], xlab = "Kask", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dp[,14],dp[,4], xlab = "Muu", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
#on tekkinud vahepealseid v‰‰rtusi

#kui teha prognoos viie Random Forestis kıige olulisemaks osutunud tunnuse pealt:
FI2 = read.csv("feature_IMP_500k.csv", header = T)
imp5 = FI2$Feature[1:5]; imp5 = as.character(imp5)
formula = as.formula(paste("cl80", paste(imp5, collapse=" + "), sep=" ~ "))
m3 = stepAIC(multinom(formula, d80, weights = weight, maxit = 10000))
pred2 = data.frame(predict(m3, newdata = d_80, type = "probs"))
pred2$aproovitykk_id = d_80$aproovitykk_id
dp = merge(pred2, taks.info, by = "aproovitykk_id", all.x = T)
dev.off()
par(mfrow = c(2,2))
plot(dp[,11],dp[,5], xlab = "M‰nd", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dp[,12],dp[,3], xlab = "Kuusk", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dp[,13],dp[,2], xlab = "Kask", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dp[,14],dp[,4], xlab = "Muu", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
#ei ole suurt kasu neist parimatest tunnustest
#plotis ainult need vaatlused, kus enimlevinud liigi osakaal alla 0.8





#### penalized multinomial regression ###





require(npmr)
#mitme suurima olulisusega (RandomForest pıhjal) tunnuse kasutamine ja kui suure karistusliikme kasutamine annab parima hinnangu?
Y1 = dd$cl80
ids = c(d_80$aproovitykk_id, d80$aproovitykk_id)
heat1 = matrix(NA,20,20)
for(imp in 2:21){
  imps = FI2$Feature[1:imp]; imps = as.character(imps)
  for(lambda in 1:20) {
    pred = matrix(0, nrow = 455, ncol = 4)
    X1 = as.matrix(d80[imps]) 
    m1 = npmr(X = X1, Y = Y1, lambda = lambda)
    dataX = as.matrix(d_80[imps]) 
    pred1 = predict.npmr(m1, dataX)[,,1]
    out1 = 1:dim(d80)[1];out2 = 2:dim(d80)[1];out2 = c(out2,1) #saab ainult kahe-kaupa predictida
    pred00 = matrix(,nrow = 0, ncol = 4)
    for(i in 1:dim(d80)[1]){
      Y2 = Y1[-c(out1[i],out2[i])]
      X2 = as.matrix(X1[-c(out1[i],out2[i]),])
      m2 = npmr(X = X2, Y = Y2, lambda = lambda)
      dataX = as.matrix(d80[imps]) 
      dataX = dataX[c(out1[i],out2[i]),]
      pred2 = predict.npmr(m2, dataX)[,,1][1,]
      pred00 = rbind(pred00,pred2)
    }
    pred1 = rbind(pred1,pred00)
    pred = as.data.frame(pred + pred1)
    pred$aproovitykk_id = ids
    dp = merge(pred, taks.info, by = "aproovitykk_id", all.x = T)
    nms = names(dp);nms[2] = "V4";nms[3] = "V2"; nms[4] = "V1";nms[5] = "V3"; dp = dp[nms]
    rmse = sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4)
    heat1[lambda,imp-1] = rmse; print(rmse)
  }
}

moos = as.data.frame(t(heat1))
colnames(moos) = paste("Lambda_", 1:20, sep = "")
moos$tunnuseid = paste("Tunnuste arv_", 2:21, sep = "")
moos$sort = 1:20
moos.m <- melt(moos,id.vars = c("tunnuseid","sort"))
moos.m$tunnuseid = as.factor(moos.m$tunnuseid)
moos.m$tunnuseid <- factor(moos.m$tunnuseid, levels= unique((moos.m$tunnuseid)[order(moos.m$sort)]))


plot = ggplot(moos.m,aes(variable,tunnuseid)) + geom_tile(aes(fill=value),color = "white") +
  guides(fill=guide_colorbar("RMSE")) +
  scale_fill_gradientn(colors=c("skyblue","yellow","tomato"),guide="colorbar") +
  theme(axis.text.x = element_text(angle = 270, hjust = 0,vjust=-0.05))
plot + labs(x = "", y = "")

min(heat1) #0.1958656
which(heat1 == min(heat1), arr.ind = TRUE); #lambda 10, esimesed 5 tunnust



#Parim tulemus kui lambda = 10 ja 5 esimest olulist tunnust mudelis:
imps = FI2$Feature[1:5]; imps = as.character(imps)
lambda = 10
Y1 = dd$cl80
pred = matrix(0, ncol = 4,nrow = 455)
X1 = as.matrix(d80[imps]) 
m1 = npmr(X = X1, Y = Y1, lambda = lambda)
dataX = as.matrix(d_80[imps])
pred1 = predict.npmr(m1, dataX)[,,1]
out1 = 1:dim(d80)[1];out2 = 2:dim(d80)[1];out2 = c(out2,1) #saab ainult kahe-kaupa predictida
pred00 = matrix(,nrow = 0, ncol = 4)
for(i in 1:dim(d80)[1]){
  Y2 = Y1[-c(out1[i],out2[i])]
  X2 = X1[-c(out1[i],out2[i]),]
  m2 = npmr(X = X2, Y = Y2, lambda = lambda)
  dataX = as.matrix(d80[imps]) 
  dataX = dataX[c(out1[i],out2[i]),]
  pred2 = predict.npmr(m2, dataX)[,,1][1,]
  pred00 = rbind(pred00,pred2)
}
pred1 = rbind(pred1,pred00)
pred = pred + pred1

pred1 = as.data.frame(pred)
pred1$aproovitykk_id = c(d_80$aproovitykk_id,d80$aproovitykk_id)
dp = merge(pred1, taks.info, by = "aproovitykk_id", all.x = T)
nms = names(dp); nms[2] = "V4";nms[3] = "V2"; nms[4] = "V1";nms[5] = "V3"; dp = dp[nms]
sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4) 
#0.1958656 lambda 10, tunnuseid 5

dev.off()
par(mfrow = c(2,2))
plot(dp[,11],dp[,2], xlab = "M‰nd", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369), pch = 16)
abline(lm(dp[,2] ~ dp[,11]))
plot(dp[,12],dp[,3], xlab = "Kuusk", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369), pch = 16)
abline(lm(dp[,3] ~ dp[,12]))
plot(dp[,13],dp[,4], xlab = "Kask", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369), pch = 16)
abline(lm(dp[,4] ~ dp[,13]))
plot(dp[,14],dp[,5], xlab = "Muu", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369), pch = 16)
abline(lm(dp[,5] ~ dp[,14]))



#mitme (suvalise) tunnuse ja kui suure karistusliikme kasutamine annab parima hinnangu?
imps = FI2$Feature[1:40]; imps = as.character(imps)
N = 100
Y = dd$cl80
ids = data$aproovitykk_id
heat2 = matrix(NA,20,20)
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
      pred = pred + pred0
    }
    predN = data.frame(pred/N)
    predN$aproovitykk_id = ids
    dp = merge(predN, taks.info, by = "aproovitykk_id", all.x = T)
    nms = names(dp); nms[2] = "X4";nms[3] = "X2"; nms[4] = "X1";nms[5] = "X3"; dp = dp[nms]
    heat2[l,m] = sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4)
  }
}

min(heat2);which(heat2 == min(heat2), arr.ind = TRUE) #sain tunnuste arv 3, lambda = 7, min=0.1871927


#see oli ristvalideerimata (muidu oleks ajakulu meeletud): parimate parameetritega ristvalideeritult:
N = 1000;  m = 3; lambda = 7
Y1 = dd$cl80
pred = matrix(0, ncol = 4,nrow = 455)
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
sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4)
#0.2010435 N1000


dev.off()
par(mfrow = c(2,2))
plot(dp[,11],dp[,2], xlab = "M‰nd", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
abline(lm(dp[,2] ~ dp[,11]))
plot(dp[,12],dp[,3], xlab = "Kuusk", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
abline(lm(dp[,3] ~ dp[,12]))
plot(dp[,13],dp[,4], xlab = "Kask", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
abline(lm(dp[,4] ~ dp[,13]))
plot(dp[,14],dp[,5], xlab = "Muu", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
abline(lm(dp[,5] ~ dp[,14]))










