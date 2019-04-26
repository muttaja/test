#Random forest

require(randomForest)

data = sentinel599sc

rf <- randomForest(MA ~ B02+B03+B04+B05+B06+B07+B08+B11+B12+X21+X31+X37+X42+X43+X44+X45+X48+X51+X53+X57+X61+X63+X64+X77+X999, data=data, mtry=3,importance=TRUE, na.action=na.omit, ntree = 10)
predrf = predict(rf, data)
names(predrf)
hist(predrf)

setwd("A:/MAKA/TEST/test")
data = read.csv("d601.csv")

liqs = names(data)[53:56]
namesx = names(data)[3:49]

formula1 = as.formula(paste(liqs[1], paste(namesx, collapse=" + "), sep=" ~ "))
formula2 = as.formula(paste(liqs[2], paste(namesx, collapse=" + "), sep=" ~ "))
formula3 = as.formula(paste(liqs[3], paste(namesx, collapse=" + "), sep=" ~ "))
formula4 = as.formula(paste(liqs[4], paste(namesx, collapse=" + "), sep=" ~ "))

rf_MA <- randomForest(formula,data=data, mtry=3,importance=TRUE, na.action=na.omit, ntree = n)
plot(data$ARV_VMA, rf_MA$predicted)

ns = c(10,50,200,1000)
dev.off()
par(mfrow = c(2, 2))
for(i in 1:4){
  n = ns[i]
  rf_MA <- randomForest(formula1,data=data, mtry=3,importance=TRUE, na.action=na.omit, ntree = n)
  plot(data$ARV_VMA, rf_MA$predicted)
}

dev.off()
par(mfrow = c(2, 2))
for(i in 1:4){
  n = ns[i]
  rf_MA <- randomForest(formula2,data=data, mtry=3,importance=TRUE, na.action=na.omit, ntree = n)
  plot(data$ARV_VKU, rf_MA$predicted)
}

dev.off()
par(mfrow = c(2, 2))
for(i in 1:4){
  n = ns[i]
  rf_MA <- randomForest(formula3,data=data, mtry=3,importance=TRUE, na.action=na.omit, ntree = n)
  plot(data$ARV_VKS, rf_MA$predicted)
}

dev.off()
par(mfrow = c(2, 2))
for(i in 1:4){
  n = ns[i]
  rf_MA <- randomForest(formula4,data=data, mtry=3,importance=TRUE, na.action=na.omit, ntree = n)
  plot(data$ARV_VXX, rf_MA$predicted)
}

#loocv
rf.loocv = function(row,formula=formula1,n=10){
  sid = row[2]
  #sid = sidxx[1]; n = 20
  data0 = data[!(data$aproovitykk_id %in% sid),]
  rf <- randomForest(formula,data=data, mtry=3,importance=TRUE, na.action=na.omit, ntree = n)
  pff = predict(rf, newdata = data[data$aproovitykk_id %in% sid,])
  pff
}

rf.pred = apply(data, 1, rf.loocv)
rf.pred = data.frame(rf.pred)
hist(rf.pred$rf.pred)


#kõik koos prognoosimine pythonis
setwd("A:/MAKA/TEST")
rf = read.csv("rf_max_11_20.csv")
par(mfrow = c(2,2))

plot(dpp[,1],rf[,1], xlab = "MÄND", ylab = "RF prognoos", xlim = c(0,1), ylim = c(0,1))
plot(dpp[,2],rf[,2], xlab = "KUUSK", ylab = "RF prognoos", xlim = c(0,1), ylim = c(0,1))
plot(dpp[,3],rf[,3], xlab = "KASK", ylab = "RF prognoos", xlim = c(0,1), ylim = c(0,1))
plot(dpp[,4],rf[,4], xlab = "MUU", ylab = "RF prognoos", xlim = c(0,1), ylim = c(0,1))

rfs = vector("list", length = 13)
mds = seq(10,35, by = 5)
for(j in 2:14){
  vec = vector("list", length = length(mds))
  for(k in 1:length(mds)){
    md = mds[k]
    name <- paste("rf_max",j,md, sep = "_")
    name <- paste(name,".csv", sep = "")
    vec[[k]] <- read.csv(name)
  }
  rfs[[j-1]] = vec
}

mav = matrix(NA, nrow = 13, ncol = 6)
kuv = matrix(NA, nrow = 13, ncol = 6)
ksv = matrix(NA, nrow = 13, ncol = 6)
muv = matrix(NA, nrow = 13, ncol = 6)
for(j in 1:13){
  for(k in 1:length(mds)){
    mav[j,k] = sqrt(mean((unlist(rfs[[j]][[k]][,1]) - dpp[,1])**2))
    kuv[j,k] = sqrt(mean((unlist(rfs[[j]][[k]][,2]) - dpp[,2])**2))
    ksv[j,k] = sqrt(mean((unlist(rfs[[j]][[k]][,3]) - dpp[,3])**2))
    muv[j,k] = sqrt(mean((unlist(rfs[[j]][[k]][,4]) - dpp[,4])**2))
  }
}

require(pheatmap)
pheatmap(mav, cluster_rows = F, cluster_cols = F)
pheatmap(kuv, cluster_rows = F, cluster_cols = F)
pheatmap(ksv, cluster_rows = F, cluster_cols = F)
pheatmap(muv, cluster_rows = F, cluster_cols = F)

library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)

vv = vector("list", length = 4)
vv[[1]] = mav;vv[[2]] = kuv;vv[[3]] = ksv;vv[[4]] = muv;
plot_list=list()
for (a in 1:4){
  x=pheatmap(vv[[a]],cluster_rows = F, cluster_cols = F)
  plot_list[[a]] = x[[4]]     ##to save each plot into a list. note the [[4]]
}
g<-do.call(grid.arrange,plot_list)
ggsave("RF_heatmap.pdf",g)

#kõik pildid koos + määramatus
rf = read.csv("rf_koos.csv") #msl = 69, md = 30
#nahhui mul siin ID-sid ei ole :( :( :(
par(mfrow = c(2,2))

plot(dpp[,1],rf[,1], xlab = "MÄND", ylab = "RF prognoos", xlim = c(0,1), ylim = c(0,1))
plot(dpp[,2],rf[,2], xlab = "KUUSK", ylab = "RF prognoos", xlim = c(0,1), ylim = c(0,1))
plot(dpp[,3],rf[,3], xlab = "KASK", ylab = "RF prognoos", xlim = c(0,1), ylim = c(0,1))
plot(dpp[,4],rf[,4], xlab = "MUU", ylab = "RF prognoos", xlim = c(0,1), ylim = c(0,1))

#UUESTI, vaata, kas id-id ikka klappisisd!!!

setwd("A:/MAKA/TEST")
rf = read.csv("rf.csv")


#taks.info, kus on ka id-d olemas
setwd("A:/MAKA/TEST/test");load("taks_info.RData") #taks.info
dats = merge(rf,taks.info, by = "aproovitykk_id", all.x = T)

dev.off()
par(mfrow = c(2,2))
plot(dats[,11],dats[,2], xlab = "MÄND", ylab = "RF: msl=.., max.depth=.", xlim = c(0,1), ylim = c(0,1))
plot(dats[,12],dats[,3], xlab = "KUUSK", ylab = "RF: msl=.., max.depth=.", xlim = c(0,1), ylim = c(0,1))
plot(dats[,13],dats[,4], xlab = "KASK", ylab = "RF: msl=.., max.depth=.", xlim = c(0,1), ylim = c(0,1))
plot(dats[,14],dats[,5], xlab = "MUU", ylab = "RF: msl=.., max.depth=.", xlim = c(0,1), ylim = c(0,1))


sqrt(mean((dats[,11] - dats[,2])**2))
sqrt(mean((dats[,12] - dats[,3])**2))
sqrt(mean((dats[,13] - dats[,4])**2))
sqrt(mean((dats[,14] - dats[,5])**2))
sqrt(sum((dats[,11:14] - dats[,2:5])**2) /(dim(dpp)[1]*4)) #0.2121045 msl 12 md 18; 0.22573 kui msl 5, md 32

#uus catse, kus id-d kindlasti õiged:
#koondandmestik

rfs = vector("list", length = 12)
mds = seq(10,30, by = 2)
for(j in 2:13){
  vec = vector("list", length = length(mds))
  for(k in 1:length(mds)){
    md = mds[k]
    name <- paste("rf_uuscatse",j,md, sep = "")
    name <- paste(name,".csv", sep = "")
    vec[[k]] <- read.csv(name)
  }
  rfs[[j-1]] = vec
}

j2rts = rfs[[1]][[1]][,5]
dpp1 = taks.info[match(j2rts, taks.info$aproovitykk_id),]


mav = matrix(NA, nrow = 12, ncol = 11)
kuv = matrix(NA, nrow = 12, ncol = 11)
ksv = matrix(NA, nrow = 12, ncol = 11)
muv = matrix(NA, nrow = 12, ncol = 11)
kov = matrix(NA, nrow = 12, ncol = 11)

for(j in 1:12){
  for(k in 1:length(mds)){
    mav[j,k] = sqrt(mean((unlist(rfs[[j]][[k]][,1]) - dpp1[,7])**2))
    kuv[j,k] = sqrt(mean((unlist(rfs[[j]][[k]][,2]) - dpp1[,8])**2))
    ksv[j,k] = sqrt(mean((unlist(rfs[[j]][[k]][,3]) - dpp1[,9])**2))
    muv[j,k] = sqrt(mean((unlist(rfs[[j]][[k]][,4]) - dpp1[,10])**2))
    kov[j,k]= sqrt((sum((unlist(rfs[[j]][[k]][,1:4]) - dpp1[,7:10])**2))/(dim(dpp)[1]*4))
  }
}


require(pheatmap)
pheatmap(mav, cluster_rows = F, cluster_cols = F)
pheatmap(kuv, cluster_rows = F, cluster_cols = F)
pheatmap(ksv, cluster_rows = F, cluster_cols = F)
pheatmap(muv, cluster_rows = F, cluster_cols = F)
pheatmap(kov, cluster_rows = F, cluster_cols = F)

which(kov == min(kov), arr.ind = TRUE)#2; 8, seega msl = 3 ja md = 24 

library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)

vv = vector("list", length = 4)
vv[[1]] = mav;vv[[2]] = kuv;vv[[3]] = ksv;vv[[4]] = muv;
plot_list=list()
for (a in 1:4){
  x=pheatmap(vv[[a]],cluster_rows = F, cluster_cols = F)
  plot_list[[a]] = x[[4]]     ##to save each plot into a list. note the [[4]]
}
g<-do.call(grid.arrange,plot_list)
ggsave("RF_heatmap_uus.pdf",g)

which(kov == min(kov), arr.ind = TRUE)#2; 8, seega msl = 3 ja md = 24 
#vaatame seda:

setwd("A:/MAKA/TEST/test");load("taks_info.RData") #taks.info
setwd("A:/MAKA/TEST")
rf = read.csv("rf_uuscatse324.csv")
dats = merge(rf,taks.info, by = "aproovitykk_id", all.x = T)

dev.off()
par(mfrow = c(2,2))
plot(dats[,11],dats[,2], xlab = "MÄND", ylab = "RF: msl=.., max.depth=.", xlim = c(0,1), ylim = c(0,1))
plot(dats[,12],dats[,3], xlab = "KUUSK", ylab = "RF: msl=.., max.depth=.", xlim = c(0,1), ylim = c(0,1))
plot(dats[,13],dats[,4], xlab = "KASK", ylab = "RF: msl=.., max.depth=.", xlim = c(0,1), ylim = c(0,1))
plot(dats[,14],dats[,5], xlab = "MUU", ylab = "RF: msl=.., max.depth=.", xlim = c(0,1), ylim = c(0,1))

setwd("A:/MAKA/TEST/test")
RF_3_24 = dats; save(RF_3_24, file = "RF_3_24.RData")

#suht kena ju!

sqrt(mean((dats[,11] - dats[,2])**2))
sqrt(mean((dats[,12] - dats[,3])**2))
sqrt(mean((dats[,13] - dats[,4])**2))
sqrt(mean((dats[,14] - dats[,5])**2))
sqrt(sum((dats[,11:14] - dats[,2:5])**2) /(dim(dpp)[1]*4))

#RF kuupäevade kaupa
#võibolla siiski pole kuupäevad õiged?
rfkp = rf = read.csv("rf_pbp.csv"); rfkp[,1:4] = rfkp[,1:4] / 100
kps = unique(rfkp$kp)
#männid
dev.off()
par(mfrow = c(4, 4))
for (i in 1:15){
  kp = kps[i]
  kp_data = rfkp[rfkp$kp == kp,]
  kp_data = merge(kp_data, taks.info, all.x = T, by = "aproovitykk_id")
  plot(kp_data[,12],kp_data[,2], main = paste("Mänd_", kp), xlim = c(0,1), ylim = c(0,1))
}

#kuused
dev.off()
par(mfrow = c(4, 4))
for (i in 1:15){
  kp = kps[i]
  kp_data = rfkp[rfkp$kp == kp,]
  kp_data = merge(kp_data, taks.info, all.x = T, by = "aproovitykk_id")
  plot(kp_data[,13],kp_data[,3], main = paste("Kuusk_", kp), xlim = c(0,1), ylim = c(0,1))
}


##uus RB pbp prognoos, kus kuupäevad lõpuks kindlasti klapivad. n = 10, pärast saab suurema n-iga läbi lasta
setwd("A:/MAKA/TEST")
rfs = vector("list", length = 13)
mds = seq(2,30, by = 2)
msls = c(1:13)
for(j in 1:length(msls)){
  vec = vector("list", length = length(mds))
  for(k in 1:length(mds)){
    md = mds[k]
    name <- paste("rf_n10_pbp",j,md, sep = "_")
    name <- paste(name,".csv", sep = "")
    vec[[k]] <- read.csv(name)
  }
  rfs[[j]] = vec
}


mav = matrix(NA, nrow = length(msls), ncol = length(mds))
kuv = matrix(NA, nrow = length(msls), ncol = length(mds))
ksv = matrix(NA, nrow = length(msls), ncol = length(mds))
muv = matrix(NA, nrow = length(msls), ncol = length(mds))
kov = matrix(NA, nrow = length(msls), ncol = length(mds))


for(j in 1:length(msls)){
  for(k in 1:length(mds)){
    data.j.k = rfs[[j]][[k]]
    nms = names(data.j.k);nms[5] = "aproovitykk_id";names(data.j.k) = nms
    data.j.k[,1:4] = data.j.k[,1:4] / 100
    vead = c()
    for(i in 1:15){
      kp = kps[i]
      data0 = data.j.k[data.j.k$KP == kp,];
      RF_PBP = data0[,1:5]
      RF_PBP = RF_PBP %>% group_by(aproovitykk_id) %>% summarise_all(funs(mean))
      kp_data = merge(RF_PBP, taks.info, all.x = T, by = "aproovitykk_id")
      print(head(kp_data))
      viga = sqrt(sum((kp_data[,2:5] - kp_data[,11:14])**2) /(dim(kp_data)[1]*4))
      vead[i] = viga
    }
    viga = mean(vead)
    print(j);print(k);print(viga)
    kov[j,k] = viga
  }
}

pheatmap(kov, cluster_rows = F, cluster_cols = F)
which(kov == min(kov), arr.ind = TRUE)

rf = rfs[[13]][[1]]
nms = names(rf);nms[5] = "aproovitykk_id";names(rf) = nms
rf[,1:4] = rf[,1:4] / 100
dev.off()
par(mfrow = c(4, 4))
for (i in 1:15){
  #i = 1
  kp = kps[i]
  kp_data = rf[rf$KP == kp,]
  kp_data = merge(kp_data, taks.info, all.x = T, by = "aproovitykk_id")
  plot(kp_data[,12],kp_data[,2], main = paste("Mänd_", kp), xlim = c(0,1), ylim = c(0,1))
}

"täyspask"


#RF n = 100, 506 id peal nüüd!
setwd("A:/MAKA/TEST")
rfs = vector("list", length = 12)
mds = seq(1,16, by = 3)
msls = seq(2,20,3)
for(j in 1:length(msls)){
  vec = vector("list", length = length(mds))
  jj = msls[j]
  for(k in 1:length(mds)){
    md = mds[k]
    name <- paste("RF506_",jj,md, sep = "")
    name <- paste(name,".csv", sep = "")
    vec[[k]] <- read.csv(name)
  }
  rfs[[j]] = vec
}

ordung = rfs[[1]][[1]][,5]
dpp1 = taks.info[match(ordung, taks.info$aproovitykk_id),]


mav = matrix(NA, nrow = length(msls), ncol = length(mds))
kuv = matrix(NA, nrow = length(msls), ncol = length(mds))
ksv = matrix(NA, nrow = length(msls), ncol = length(mds))
muv = matrix(NA, nrow = length(msls), ncol = length(mds))
kov = matrix(NA, nrow = length(msls), ncol = length(mds))

for(j in 1:length(msls)){
  for(k in 1:length(mds)){
    mav[j,k] = sqrt(mean((unlist(rfs[[j]][[k]][,1]) - dpp1[,7])**2))
    kuv[j,k] = sqrt(mean((unlist(rfs[[j]][[k]][,2]) - dpp1[,8])**2))
    ksv[j,k] = sqrt(mean((unlist(rfs[[j]][[k]][,3]) - dpp1[,9])**2))
    muv[j,k] = sqrt(mean((unlist(rfs[[j]][[k]][,4]) - dpp1[,10])**2))
    kov[j,k]= sqrt((sum((unlist(rfs[[j]][[k]][,1:4]) - dpp1[,7:10])**2))/(dim(dpp)[1]*4))
  }
}


require(pheatmap)
pheatmap(mav, cluster_rows = F, cluster_cols = F)
pheatmap(kuv, cluster_rows = F, cluster_cols = F)
pheatmap(ksv, cluster_rows = F, cluster_cols = F)
pheatmap(muv, cluster_rows = F, cluster_cols = F)
pheatmap(kov, cluster_rows = F, cluster_cols = F)

which(kov == min(kov), arr.ind = TRUE)#2,5 seega msl 5, ja md 13?
min(kov) #0.1756626

library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)

vv = vector("list", length = 4)
vv[[1]] = mav;vv[[2]] = kuv;vv[[3]] = ksv;vv[[4]] = muv;
plot_list=list()
for (a in 1:4){
  x=pheatmap(vv[[a]],cluster_rows = F, cluster_cols = F)
  plot_list[[a]] = x[[4]]
}
g<-do.call(grid.arrange,plot_list)
ggsave("RF_heatmap506.pdf",g)

setwd("A:/MAKA/TEST")
rf = read.csv("RF506_513.csv")
dats = merge(rf,taks.info, by = "aproovitykk_id", all.x = T)

dev.off()
par(mfrow = c(2,2))
plot(dats[,11],dats[,2], xlab = "MÄND", ylab = "RF: msl=5, max.depth=13", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dats[,12],dats[,3], xlab = "KUUSK", ylab = "RF: msl=5, max.depth=13", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dats[,13],dats[,4], xlab = "KASK", ylab = "RF: msl=5, max.depth=13", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dats[,14],dats[,5], xlab = "MUU", ylab = "RF: msl=5, max.depth=13", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))




#CART bagging /kas on korrektne? n = 100

setwd("A:/MAKA/TEST")
rfs = vector("list", length = 12)
mds = seq(5,18, by = 3)
msls = seq(1,6,1)
for(j in 1:length(msls)){
  vec = vector("list", length = length(mds))
  jj = msls[j]
  for(k in 1:length(mds)){
    md = mds[k]
    name <- paste("CART_BAGGING_506_",jj,md, sep = "")
    name <- paste(name,".csv", sep = "")
    vec[[k]] <- read.csv(name)
  }
  rfs[[j]] = vec
}

ordung = rfs[[1]][[1]][,5]
dpp1 = taks.info[match(ordung, taks.info$aproovitykk_id),]


mav = matrix(NA, nrow = length(msls), ncol = length(mds))
kuv = matrix(NA, nrow = length(msls), ncol = length(mds))
ksv = matrix(NA, nrow = length(msls), ncol = length(mds))
muv = matrix(NA, nrow = length(msls), ncol = length(mds))
kov = matrix(NA, nrow = length(msls), ncol = length(mds))

for(j in 1:length(msls)){
  for(k in 1:length(mds)){
    mav[j,k] = sqrt(mean((unlist(rfs[[j]][[k]][,1]) - dpp1[,7])**2))
    kuv[j,k] = sqrt(mean((unlist(rfs[[j]][[k]][,2]) - dpp1[,8])**2))
    ksv[j,k] = sqrt(mean((unlist(rfs[[j]][[k]][,3]) - dpp1[,9])**2))
    muv[j,k] = sqrt(mean((unlist(rfs[[j]][[k]][,4]) - dpp1[,10])**2))
    kov[j,k]= sqrt((sum((unlist(rfs[[j]][[k]][,1:4]) - dpp1[,7:10])**2))/(dim(dpp)[1]*4))
  }
}


require(pheatmap)
pheatmap(mav, cluster_rows = F, cluster_cols = F)
pheatmap(kuv, cluster_rows = F, cluster_cols = F)
pheatmap(ksv, cluster_rows = F, cluster_cols = F)
pheatmap(muv, cluster_rows = F, cluster_cols = F)
pheatmap(kov, cluster_rows = F, cluster_cols = F)

which(kov == min(kov), arr.ind = TRUE)#1 ja 4, seega msl = 2 ja md = 14

library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)

vv = vector("list", length = 4)
vv[[1]] = mav;vv[[2]] = kuv;vv[[3]] = ksv;vv[[4]] = muv;
plot_list=list()
for (a in 1:4){
  x=pheatmap(vv[[a]],cluster_rows = F, cluster_cols = F)
  plot_list[[a]] = x[[4]]
}
g<-do.call(grid.arrange,plot_list)
ggsave("CART_BAGGINGN100_506.pdf",g)

setwd("A:/MAKA/TEST")
rf = read.csv("CART_BAGGING_506_214.csv")
dats = merge(rf,taks.info, by = "aproovitykk_id", all.x = T)

dev.off()
par(mfrow = c(2,2))
plot(dats[,11],dats[,2], xlab = "MÄND", ylab = "CART BAGGING: msl=2, max.depth=14", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dats[,12],dats[,3], xlab = "KUUSK", ylab = "RF: msl=5, max.depth=13", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dats[,13],dats[,4], xlab = "KASK", ylab = "RF: msl=5, max.depth=13", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dats[,14],dats[,5], xlab = "MUU", ylab = "RF: msl=5, max.depth=13", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
#0.1748119

#### RF BAGGING N = 100 ##############;

setwd("A:/MAKA/TEST")
rfs = vector("list", length = 12)
mds = seq(5,18, by = 3)
msls = seq(1,6,1)
for(j in 1:length(msls)){
  vec = vector("list", length = length(mds))
  jj = msls[j]
  for(k in 1:length(mds)){
    md = mds[k]
    name <- paste("RF_BAGGING_506_",jj,md, sep = "")
    name <- paste(name,".csv", sep = "")
    vec[[k]] <- read.csv(name)
  }
  rfs[[j]] = vec
}

ordung = rfs[[1]][[1]][,5]
dpp1 = taks.info[match(ordung, taks.info$aproovitykk_id),]


mav = matrix(NA, nrow = length(msls), ncol = length(mds))
kuv = matrix(NA, nrow = length(msls), ncol = length(mds))
ksv = matrix(NA, nrow = length(msls), ncol = length(mds))
muv = matrix(NA, nrow = length(msls), ncol = length(mds))
kov = matrix(NA, nrow = length(msls), ncol = length(mds))

for(j in 1:length(msls)){
  for(k in 1:length(mds)){
    mav[j,k] = sqrt(mean((unlist(rfs[[j]][[k]][,1]) - dpp1[,7])**2))
    kuv[j,k] = sqrt(mean((unlist(rfs[[j]][[k]][,2]) - dpp1[,8])**2))
    ksv[j,k] = sqrt(mean((unlist(rfs[[j]][[k]][,3]) - dpp1[,9])**2))
    muv[j,k] = sqrt(mean((unlist(rfs[[j]][[k]][,4]) - dpp1[,10])**2))
    kov[j,k]= sqrt((sum((unlist(rfs[[j]][[k]][,1:4]) - dpp1[,7:10])**2))/(dim(dpp)[1]*4))
  }
}


require(pheatmap)
pheatmap(mav, cluster_rows = F, cluster_cols = F)
pheatmap(kuv, cluster_rows = F, cluster_cols = F)
pheatmap(ksv, cluster_rows = F, cluster_cols = F)
pheatmap(muv, cluster_rows = F, cluster_cols = F)
pheatmap(kov, cluster_rows = F, cluster_cols = F)

which(kov == min(kov), arr.ind = TRUE)#1 ja 4, seega msl = 2 ja md = 14
min(kov)

library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)

vv = vector("list", length = 4)
vv[[1]] = mav;vv[[2]] = kuv;vv[[3]] = ksv;vv[[4]] = muv;
plot_list=list()
for (a in 1:4){
  x=pheatmap(vv[[a]],cluster_rows = F, cluster_cols = F)
  plot_list[[a]] = x[[4]]
}
g<-do.call(grid.arrange,plot_list)
ggsave("CART_BAGGINGN100_506.pdf",g)

setwd("A:/MAKA/TEST")
rf = read.csv("RF_BAGGING_506_517.csv")
dats = merge(rf,taks.info, by = "aproovitykk_id", all.x = T)

dev.off()
par(mfrow = c(2,2))
plot(dats[,11],dats[,2], xlab = "MÄND", ylab = "CART BAGGING: msl=2, max.depth=14", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dats[,12],dats[,3], xlab = "KUUSK", ylab = "RF: msl=5, max.depth=13", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dats[,13],dats[,4], xlab = "KASK", ylab = "RF: msl=5, max.depth=13", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dats[,14],dats[,5], xlab = "MUU", ylab = "RF: msl=5, max.depth=13", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
#0.1748035; vrdl CART BAGGING 0.1748119

#######################



#########################


#tüvemahu üle 100

#########################
#RF N = 100, tüvemaht 100+

setwd("A:/MAKA/TEST")
rfs = vector("list", length = 12)
mds = seq(5,23, by = 3)
msls = seq(1,10,1)
for(j in 1:length(msls)){
  vec = vector("list", length = length(mds))
  jj = msls[j]
  for(k in 1:length(mds)){
    md = mds[k]
    name <- paste("RF_BAGGING_100_506_",jj,md, sep = "") #no tegelt 455...
    name <- paste(name,".csv", sep = "")
    vec[[k]] <- read.csv(name)
  }
  rfs[[j]] = vec
}


ordung = rfs[[1]][[1]][,5]
dpp1 = taks.info[match(ordung, taks.info$aproovitykk_id),]


mav = matrix(NA, nrow = length(msls), ncol = length(mds))
kuv = matrix(NA, nrow = length(msls), ncol = length(mds))
ksv = matrix(NA, nrow = length(msls), ncol = length(mds))
muv = matrix(NA, nrow = length(msls), ncol = length(mds))
kov = matrix(NA, nrow = length(msls), ncol = length(mds))

for(j in 1:length(msls)){
  for(k in 1:length(mds)){
    mav[j,k] = sqrt(mean((unlist(rfs[[j]][[k]][,1]) - dpp1[,7])**2))
    kuv[j,k] = sqrt(mean((unlist(rfs[[j]][[k]][,2]) - dpp1[,8])**2))
    ksv[j,k] = sqrt(mean((unlist(rfs[[j]][[k]][,3]) - dpp1[,9])**2))
    muv[j,k] = sqrt(mean((unlist(rfs[[j]][[k]][,4]) - dpp1[,10])**2))
    kov[j,k]= sqrt((sum((unlist(rfs[[j]][[k]][,1:4]) - dpp1[,7:10])**2))/(dim(dpp1)[1]*4))
  }
}

which(kov == min(kov), arr.ind = TRUE)#2 ja 3, seega msl = 2 ja md = 11
min(kov) #0.1651199 .... 20.04 saan  0.1741281


moos = as.data.frame(kov)
colnames(moos) = paste("Max_depth_", mds, sep = "")
moos$msl = paste("Min_sample_leaf_", msls, sep = "")
moos$sort = msls

moos.m <- melt(moos,id.vars = c("msl","sort"))
moos.m$msl = as.factor(moos.m$msl)
moos.m$msl <- factor(moos.m$msl, levels= unique((moos.m$msl)[order(moos.m$sort)]))


plot = ggplot(moos.m,aes(variable,msl)) + geom_tile(aes(fill=value),color = "white") +
  guides(fill=guide_colorbar("RMSE")) +
  scale_fill_gradientn(colors=c("skyblue","yellow","tomato"),guide="colorbar") +
  theme(axis.text.x = element_text(angle = 270, hjust = 0,vjust=-0.05))
plot + labs(x = "", y = "")


which.rf = which(kov == min(kov), arr.ind = TRUE)[1,]
which.rf
rf = rfs[[which.rf[1]]][[which.rf[2]]]





vv = vector("list", length = 4)
vv[[1]] = mav;vv[[2]] = kuv;vv[[3]] = ksv;vv[[4]] = muv;
plot_list=list()
for (a in 1:4){
  x=pheatmap(vv[[a]],cluster_rows = F, cluster_cols = F)
  plot_list[[a]] = x[[4]]
}
g<-do.call(grid.arrange,plot_list)
ggsave("CART_BAGGINGN100_455.pdf",g)

setwd("A:/MAKA/TEST")
rf = read.csv("RF_BAGGING_100_506_211.csv")
dats = merge(rf,taks.info, by = "aproovitykk_id", all.x = T)

dev.off()
par(mfrow = c(2,2))
plot(dats[,11],dats[,2], xlab = "MÄND", ylab = "RF: msl=2, max.depth=13", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dats[,12],dats[,3], xlab = "KUUSK", ylab = "RF: msl=2, max.depth=13", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dats[,13],dats[,4], xlab = "KASK", ylab = "RF: msl=2, max.depth=13", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dats[,14],dats[,5], xlab = "MUU", ylab = "RF: msl=2, max.depth=13", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))

######### CART tavaline #############

setwd("A:/MAKA/TEST")
rfs = vector("list", length = 12)
mds = seq(4,26, by = 2)
msls = seq(1,20,1)
for(j in 1:length(msls)){
  vec = vector("list", length = length(mds))
  jj = msls[j]
  for(k in 1:length(mds)){
    md = mds[k]
    name <- paste("CART_455_",jj,md, sep = "")
    name <- paste(name,".csv", sep = "")
    vec[[k]] <- read.csv(name)
  }
  rfs[[j]] = vec
}


ordung = rfs[[1]][[1]][,5]
dpp1 = taks.info[match(ordung, taks.info$aproovitykk_id),]

mav = matrix(NA, nrow = length(msls), ncol = length(mds))
kuv = matrix(NA, nrow = length(msls), ncol = length(mds))
ksv = matrix(NA, nrow = length(msls), ncol = length(mds))
muv = matrix(NA, nrow = length(msls), ncol = length(mds))
kov = matrix(NA, nrow = length(msls), ncol = length(mds))

for(j in 1:length(msls)){
  for(k in 1:length(mds)){
    mav[j,k] = sqrt(mean((unlist(rfs[[j]][[k]][,1]) - dpp1[,7])**2))
    kuv[j,k] = sqrt(mean((unlist(rfs[[j]][[k]][,2]) - dpp1[,8])**2))
    ksv[j,k] = sqrt(mean((unlist(rfs[[j]][[k]][,3]) - dpp1[,9])**2))
    muv[j,k] = sqrt(mean((unlist(rfs[[j]][[k]][,4]) - dpp1[,10])**2))
    kov[j,k]= sqrt((sum((unlist(rfs[[j]][[k]][,1:4]) - dpp1[,7:10])**2))/(dim(dpp)[1]*4))
  }
}

pheatmap(kov, cluster_rows = F, cluster_cols = F)
which(kov == min(kov), arr.ind = TRUE)#2 ja 3, seega msl = 2 ja md = 11
min(kov) #0.1930893 jama


#### CART BAGGING N=100;


setwd("A:/MAKA/TEST")
mds = seq(21,24,1)
msls = seq(1,4,1)
rfs = vector("list", length = length(msls))
for(j in 1:length(msls)){
  vec = vector("list", length = length(mds))
  jj = msls[j]
  for(k in 1:length(mds)){
    md = mds[k]
    name <- paste("CART_455_VALITUD_BAGGING_",jj,md, sep = "")
    name <- paste(name,".csv", sep = "")
    vec[[k]] <- read.csv(name)
  }
  rfs[[j]] = vec
}


ordung = rfs[[1]][[1]][,5]
dpp1 = taks.info[match(ordung, taks.info$aproovitykk_id),]

mav = matrix(NA, nrow = length(msls), ncol = length(mds))
kuv = matrix(NA, nrow = length(msls), ncol = length(mds))
ksv = matrix(NA, nrow = length(msls), ncol = length(mds))
muv = matrix(NA, nrow = length(msls), ncol = length(mds))
kov = matrix(NA, nrow = length(msls), ncol = length(mds))

for(j in 1:length(msls)){
  for(k in 1:length(mds)){
    mav[j,k] = sqrt(mean((unlist(rfs[[j]][[k]][,1]) - dpp1[,7])**2))
    kuv[j,k] = sqrt(mean((unlist(rfs[[j]][[k]][,2]) - dpp1[,8])**2))
    ksv[j,k] = sqrt(mean((unlist(rfs[[j]][[k]][,3]) - dpp1[,9])**2))
    muv[j,k] = sqrt(mean((unlist(rfs[[j]][[k]][,4]) - dpp1[,10])**2))
    kov[j,k]= sqrt((sum((unlist(rfs[[j]][[k]][,1:4]) - dpp1[,7:10])**2))/(dim(dpp1)[1]*4))
  }
}

pheatmap(kov, cluster_rows = F, cluster_cols = F)
which(kov == min(kov), arr.ind = TRUE)#2 ja 7, seega msl = 2 ja md = 23
min(kov) #0.1649619 seni parim! N100 pealt; nüüd saan 100 pealt 0.1739615
#saan nüüd samade andmete pealt N250 korral 0.1741755
dim(kov)

moos = as.data.frame(kov)
colnames(moos) = paste("Max_depth_", mds, sep = "")
moos$msl = paste("Min_sample_leaf_", msls, sep = "")
moos$sort = msls

moos.m <- melt(moos,id.vars = c("msl","sort"))
moos.m$msl = as.factor(moos.m$msl)
moos.m$msl <- factor(moos.m$msl, levels= unique((moos.m$msl)[order(moos.m$sort)]))


plot = ggplot(moos.m,aes(variable,msl)) + geom_tile(aes(fill=value),color = "white") +
  guides(fill=guide_colorbar("RMSE")) +
  scale_fill_gradientn(colors=c("skyblue","yellow","tomato"),guide="colorbar") +
  theme(axis.text.x = element_text(angle = 270, hjust = 0,vjust=-0.05))
plot + labs(x = "", y = "")


which.rf = which(kov == min(kov), arr.ind = TRUE)[1,]
which.rf
rf = rfs[[which.rf[1]]][[which.rf[2]]]

dp = merge(rf, taks.info, all.x = T, by = "aproovitykk_id")
dev.off()
par(mfrow = c(2,2))
plot(dp[,11],dp[,2], xlab = "Mänd", ylab = "Hinnang", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369), pch = 16)
abline(lm(dp[,2] ~ dp[,11]))
plot(dp[,12],dp[,3], xlab = "Kuusk", ylab = "Hinnang", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369), pch = 16)
abline(lm(dp[,3] ~ dp[,12]))
plot(dp[,13],dp[,4], xlab = "Kask", ylab = "Hinnang", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369), pch = 16)
abline(lm(dp[,4] ~ dp[,13]))
plot(dp[,14],dp[,5], xlab = "Muu", ylab = "Hinnang", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369), pch = 16)
abline(lm(dp[,5] ~ dp[,14]))







############# kuupäevade kaupa! ##############
#andmestik
# sentinel601sc = read.csv("sentinel601sc.csv")
sent455 = sentinel601sc[sentinel601sc$aproovitykk_id %in% dats$aproovitykk_id,]
# write.csv(sent455, file = "sent455.csv")
st = read.csv(file = "sent455.csv")

length(unique(dats$aproovitykk_id))
length(unique(sent455$aproovitykk_id))
length(unique(st$aproovitykk_id))

setwd("A:/MAKA/TEST")
rfs = vector("list", length = 13)
mds = seq(2,30, by = 4)
msls = seq(1,9,2)
for(j in 1:length(msls)){
  jj = msls[j]
  vec = vector("list", length = length(mds))
  for(k in 1:length(mds)){
    md = mds[k]
    name <- paste("CART_B10_tm100p_PBP",jj,md, sep = "_")
    name <- paste(name,".csv", sep = "")
    vec[[k]] <- read.csv(name)
  }
  rfs[[j]] = vec
}

kov = matrix(NA, nrow = length(msls), ncol = length(mds))


for(j in 1:length(msls)){
  for(k in 1:length(mds)){
    data.j.k = rfs[[j]][[k]]
    data.j.k[,1:4] = data.j.k[,1:4] / 100
    data.j.k[,1:4] = (data.j.k[,1:4]*5488 + 0.5)/5489
    beta.pbp = data.j.k[,1:5] %>% group_by(aproovitykk_id) %>% summarise_all(funs(bets_fun))
    beta.pbp = merge(beta.pbp, taks.info, all.x = T, by = "aproovitykk_id")
    beta.pbp[,2:5] = beta.pbp[2:5] / rowSums(beta.pbp[2:5])
    RMSE = sqrt(sum((beta.pbp[,2:5] - beta.pbp[,11:14])**2) /(dim(beta.pbp)[1]*4))
    print(j);print(k);print(RMSE)
    kov[j,k] = RMSE
  }
}

moos = as.data.frame(kov)
colnames(moos) = paste("Max_depth_", mds, sep = "")
moos$msl = paste("Min_sample_leaf_", msls, sep = "")
moos$sort = msls

moos.m <- melt(moos,id.vars = c("msl","sort"))
moos.m$msl = as.factor(moos.m$msl)
moos.m$msl <- factor(moos.m$msl, levels= unique((moos.m$msl)[order(moos.m$sort)]))


plot = ggplot(moos.m,aes(variable,msl)) + geom_tile(aes(fill=value),color = "white") +
  guides(fill=guide_colorbar("RMSE")) +
  scale_fill_gradientn(colors=c("skyblue","yellow","tomato"),guide="colorbar") +
  theme(axis.text.x = element_text(angle = 270, hjust = 0,vjust=-0.05))
plot + labs(x = "", y = "")


which.rf = which(kov == min(kov), arr.ind = TRUE)[1,]
which.rf
rf = rfs[[which.rf[1]]][[which.rf[2]]]

dp = merge(rf, taks.info, all.x = T, by = "aproovitykk_id")
dev.off()
par(mfrow = c(2,2))
plot(dp[,11],dp[,2], xlab = "Mänd", ylab = "Hinnang", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369), pch = 16)
abline(lm(dp[,2] ~ dp[,11]))
plot(dp[,12],dp[,3], xlab = "Kuusk", ylab = "Hinnang", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369), pch = 16)
abline(lm(dp[,3] ~ dp[,12]))
plot(dp[,13],dp[,4], xlab = "Kask", ylab = "Hinnang", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369), pch = 16)
abline(lm(dp[,4] ~ dp[,13]))
plot(dp[,14],dp[,5], xlab = "Muu", ylab = "Hinnang", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369), pch = 16)
abline(lm(dp[,5] ~ dp[,14]))







rf = rfs[[3]][[7]]
rf[,1:4] = rf[,1:4] / 100






dev.off()
par(mfrow = c(4, 4))
for (i in 1:15){
  #i = 1
  kp = kps[i]
  kp_data = rf[rf$kp == kp,]
  kp_data = merge(kp_data, taks.info, all.x = T, by = "aproovitykk_id")
  plot(kp_data[,12],kp_data[,2], main = paste("Mänd_", kp), xlim = c(0,1), ylim = c(0,1))
}

dev.off()
par(mfrow = c(4, 4))
for (i in 1:15){
  #i = 1
  kp = kps[i]
  kp_data = rf[rf$kp == kp,]
  kp_data = merge(kp_data, taks.info, all.x = T, by = "aproovitykk_id")
  plot(kp_data[,13],kp_data[,3], main = paste("Kuusk ", kp), xlim = c(0,1), ylim = c(0,1))
}


#parima ümbruses 100x;
setwd("A:/MAKA/TEST")
msls = seq(1,3,1)
mds = seq(9,25,2)
rfs = vector("list", length = length(msls))
for(j in 1:length(msls)){
  jj = msls[j]
  vec = vector("list", length = length(mds))
  for(k in 1:length(mds)){
    md = mds[k]
    name <- paste("CART_B250_tm100p_PBP",jj,md, sep = "_")
    name <- paste(name,".csv", sep = "")
    vec[[k]] <- read.csv(name)
  }
  rfs[[j]] = vec
}



bets_fun = function(vec, method = "mle"){
  if(any(is.na(vec))){
    stop("NA")
  }
  if(length(unique(vec)) < 2){
    max.d = mean(vec)
  }
  else{
    eb = ebeta(vec, method = method)
    a = eb$parameters[1]; b = eb$parameters[2]
    #opt = optim(par = 0.5, lower = 0, upper = 1, dbeta, shape1 = a, shape2 = b, method = "Brent", control=list(fnscale=-1))
    #max.d = opt$`par`
    opt = optimize(interval = c(0,1), dbeta, shape1 = a, shape2 = b, maximum = T)
    max.d = opt$`maximum`
    if((a < 1 & b < 1)){
      max.d = mean(vec)
    }
  }
  max.d
}
bets_fun2sd = function(vec, method = "mle"){
  if(any(is.na(vec))){
    stop("NA")
  }
  vec0 = vec
  if(length(vec) > 2){sd = sqrt(var(vec));mu = mean(vec);vec = vec[(vec > mu-2*sd & vec < mu+2*sd)]}
  if(length(unique(vec)) < 2){
    vec = vec0
    max.d = mean(vec)
  }
  else{
    eb = ebeta(vec, method = method)
    a = eb$parameters[1]; b = eb$parameters[2]
    #opt = optim(par = 0.5, lower = 0, upper = 1, dbeta, shape1 = a, shape2 = b, method = "Brent", control=list(fnscale=-1))
    #max.d = opt$`par`
    opt = optimize(interval = c(0,1), dbeta, shape1 = a, shape2 = b, maximum = T)
    max.d = opt$`maximum`
    if((a < 1 & b < 1)){
      max.d = mean(vec)
    }
  }
  max.d
}
kernel.dens = function(vec, kernel = "epanechnikov"){
  if(length(vec) < 2){max = vec}
  else{
    d = density(vec, kernel = "epanechnikov", from = 0, to = 1)
    max = d$x[which.max(d$y)]
  }
  max
}


kov = matrix(NA, nrow = length(msls), ncol = length(mds))
#see nüüd õige beta!
for(j in 1:length(msls)){
  for(k in 1:length(mds)){
    data.j.k = rfs[[j]][[k]]
    data.j.k[,1:4] = data.j.k[,1:4] / 100
    data.j.k[,1:4] = (data.j.k[,1:4]*5488 + 0.5)/5489
    beta.pbp = data.j.k[,1:5] %>% group_by(aproovitykk_id) %>% summarise_all(funs(bets_fun))
    beta.pbp = merge(beta.pbp, taks.info, all.x = T, by = "aproovitykk_id")
    beta.pbp[,2:5] = beta.pbp[2:5] / rowSums(beta.pbp[2:5])
    RMSE = sqrt(sum((beta.pbp[,2:5] - beta.pbp[,11:14])**2) /(dim(beta.pbp)[1]*4))
    print(j);print(k);print(RMSE)
    kov[j,k] = RMSE
  }
}

moos = as.data.frame(kov)
colnames(moos) = paste("Max_depth_", mds, sep = "")
moos$msl = paste("Min_sample_leaf_", msls, sep = "")
moos$sort = msls

moos.m <- melt(moos,id.vars = c("msl","sort"))
moos.m$msl = as.factor(moos.m$msl)
moos.m$msl <- factor(moos.m$msl, levels= unique((moos.m$msl)[order(moos.m$sort)]))


plot = ggplot(moos.m,aes(variable,msl)) + geom_tile(aes(fill=value),color = "white") +
  guides(fill=guide_colorbar("RMSE")) +
  scale_fill_gradientn(colors=c("skyblue","yellow","tomato"),guide="colorbar") +
  theme(axis.text.x = element_text(angle = 270, hjust = 0,vjust=-0.05))
plot + labs(x = "", y = "")







#N25 korral kernel.dens 1 10 0.1780591
#           bets_fun    2 4 0.1770362
#           bets_fun2sd 2 4 0.1763261
#           mean        1 5 0.1840078

#N250 mean        0.1838875
#     bets_fun2sd 0.1762102
#     bets_fun    0.1759777
#     kernel.dens 0.1776897

which.rf = which(kov == min(kov), arr.ind = TRUE)[1,]
which.rf
rf = rfs[[which.rf[1]]][[which.rf[2]]]
rf[,1:4] = rf[,1:4] / 100

dp = merge(rf, taks.info, all.x = T, by = "aproovitykk_id")

dev.off()
par(mfrow = c(2,2))
plot(dp[,11],dp[,2], xlab = "Mänd", ylab = "Hinnang", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369), pch = 16)
abline(lm(dp[,2] ~ dp[,11]))
plot(dp[,12],dp[,3], xlab = "Kuusk", ylab = "Hinnang", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369), pch = 16)
abline(lm(dp[,3] ~ dp[,12]))
plot(dp[,13],dp[,4], xlab = "Kask", ylab = "Hinnang", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369), pch = 16)
abline(lm(dp[,4] ~ dp[,13]))
plot(dp[,14],dp[,5], xlab = "Muu", ylab = "Hinnang", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369), pch = 16)
abline(lm(dp[,5] ~ dp[,14]))


kps1 = sort(as.Date(kps, format = "%Y-%m-%d"))

dev.off()
par(mfrow = c(4, 4))
for (i in 1:15){
  kp = kps1[i]
  kp_data = rf[rf$kp == kp,]
  kp_data = merge(kp_data, taks.info, all.x = T, by = "aproovitykk_id")
  plot(kp_data[,12],kp_data[,2], main = paste("Mänd ", kp), xlim = c(0,1), ylim = c(0,1), xlab = "", ylab = "", col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369), pch = 16)
}

dev.off()
par(mfrow = c(4, 4))
for (i in 1:15){
  kp = kps1[i]
  kp_data = rf[rf$kp == kp,]
  kp_data = merge(kp_data, taks.info, all.x = T, by = "aproovitykk_id")
  plot(kp_data[,13],kp_data[,3], main = paste("Kuusk ", kp), xlim = c(0,1), ylim = c(0,1), xlab = "", ylab = "", col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369), pch = 16)
}

dev.off()
par(mfrow = c(4, 4))
for (i in 1:15){
  kp = kps1[i]
  kp_data = rf[rf$kp == kp,]
  kp_data = merge(kp_data, taks.info, all.x = T, by = "aproovitykk_id")
  plot(kp_data[,14],kp_data[,4], main = paste("Kask ", kp), xlim = c(0,1), ylim = c(0,1), xlab = "", ylab = "", col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369), pch = 16)
}

dev.off()
par(mfrow = c(4, 4))
for (i in 1:15){
  kp = kps1[i]
  kp_data = rf[rf$kp == kp,]
  kp_data = merge(kp_data, taks.info, all.x = T, by = "aproovitykk_id")
  plot(kp_data[,15],kp_data[,5], main = paste("Muu ", kp), xlim = c(0,1), ylim = c(0,1), xlab = "", ylab = "", col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369), pch = 16)
}



data.kp = rf
data.kp[,1:4] = (data.kp[,1:4]*5488 + 0.5)/5489 #0 ja 1 probleem
data.kp = data.kp[,1:5]

CART_PBP_BETA = data.kp %>% group_by(aproovitykk_id) %>% summarise_all(funs(bets_fun))
kb1 = CART_PBP_BETA
rsm = rowSums(kb1[,2:5])
kb1[,2:5] = kb1[2:5] / rsm

dp = merge(kb1, taks.info, by = "aproovitykk_id", all.x = T)
dev.off()
par(mfrow = c(2,2))
plot(dp[,11],dp[,2], xlab = "Mänd", ylab = "Hinnang: CART+Bagging", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dp[,12],dp[,3], xlab = "Kuusk", ylab = "Hinnang: CART+Bagging", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dp[,13],dp[,4], xlab = "Kask", ylab = "Hinnang: CART+Bagging", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dp[,14],dp[,5], xlab = "Muu", ylab = "Hinnang: CART+Bagging", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))

sqrt(mean((dp[,11]-dp[,2])**2))                     
sqrt(mean((dp[,12]-dp[,3])**2))                  
sqrt(mean((dp[,13]-dp[,4])**2))                    
sqrt(mean((dp[,14]-dp[,5])**2))                     
sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4)
#  0.1762025 beta; 0.1839324 sama peal mean; 0.1805264 epa kernel      
#N250: beta: 0.1759777

####vead kp kaupa ########
RMSE = function(data){
  sqrt(sum((data[,2:5]-data[,12:15])**2)/(dim(data)[1]*4))
}
RMSE_liik = function(data){
  l1 = sqrt(sum((data[,2]-data[,12])**2)/(dim(data)[1]))
  l2 = sqrt(sum((data[,3]-data[,13])**2)/(dim(data)[1]))
  l3 = sqrt(sum((data[,4]-data[,14])**2)/(dim(data)[1]))
  l4 = sqrt(sum((data[,5]-data[,15])**2)/(dim(data)[1]))
  c(l1,l2,l3,l4)
}

vead = c();vaatlusi=c();vead.liik = matrix(NA, nrow = 15, ncol = 4)
for(i in 1:15){
  #i = 1
  kp = kps[i]
  kp_data = rf[rf$kp == kp,]
  kp_data = merge(kp_data, taks.info, all.x = T, by = "aproovitykk_id")
  vead[i] = RMSE(kp_data)
  vead.liik[i,] = RMSE_liik(kp_data)
  vaatlusi[i] = dim(kp_data)[1]
}

#kps = unique(data0$kp)
#kps = kps[order(kps)]
kps = as.Date(kps)
kps <- format(as.Date(kps), "%j")
kps = as.numeric(kps)


require(ggplot2)
vead = data.frame(vead = vead, kp = kps, vaatlusi = vaatlusi) #mitmes päev aastas
vead = vead[order(vead$kp),]

dev.off()
par(mfrow = c(1, 1))
ggplot(data = vead, aes(x = kp, y = vead)) + geom_point(aes(size = vaatlusi)) +
  scale_x_continuous(breaks = kps) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(size = "Takseeralasid pildil", x = "Päev aastas", y = "RMSE")  +
  scale_x_continuous(minor_breaks = seq(100,300,10), lim = c(105,285)) + 
  scale_y_continuous(minor_breaks = seq(0.15,0.3,0.05), lim = c(.16,.3))
#punkti suuruseks vaatluste arv

#liigikaupa

vead.liik = data.frame(vead.liik);names(vead.liik) = c("MA","KU","KS","MUU")
vead.liik$kp = kps; vead.liik$vaatlusi = vaatlusi
vead.liik = vead.liik[order(vead.liik$kp),]
vead.liik.long = melt(vead.liik, id.vars = c("kp","vaatlusi"))

par(mfrow = c(1, 1))
ggplot(data = vead.liik.long, aes(x = kp, y = value)) + geom_point(aes(size = vaatlusi, colour = variable), alpha = 0.69) +
  scale_x_continuous(breaks = kps) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(size = "Takseeralasid pildil",colour = "Liik", x = "Päev aastas", y = "RMSE")  +
  scale_x_continuous(minor_breaks = seq(100,300,10), lim = c(105,285)) + 
  scale_y_continuous(minor_breaks = seq(0.15,0.3,0.05), lim = c(.16,.3))


#kuidas cartb toimib, kui kehvemad pildid välja jätta?
kps=unique(rf$kp)
kps0 = kps
kps0 = kps0[-6]

pilt.beta = c()
for(i in 1:length(kps0)){
  kp.out = kps0[i]
  kp_data = rf[rf$kp != kp.out,]
  kp_data[,1:4] = (kp_data[,1:4]*(dim(kp_data)[1]-1) + 0.5)/dim(kp_data)[1]
  KNN_PBP = kp_data[,1:5] %>% group_by(aproovitykk_id) %>% summarise_all(funs(bets_fun))
  KNN_PBP[,2:5] = KNN_PBP[,2:5] / rowSums(KNN_PBP[,2:5])
  dp = merge(KNN_PBP, taks.info, by = "aproovitykk_id", all.x = T)
  #idx.df = merge(idx.df, KNN_PBP, by = "aproovitykk_id", all.x = T)
  viga = sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4)
  print(viga)
  pilt.beta[i] = viga
}
par(mfrow=(c(1,1)))
plot(pilt.beta-0.1750397)

#vaata, kuidas piltide arv tulemust mõjutab!

list.kps = vector("list", length=length(kps))
for(i in 1:length(kps)){
  list.kps[[i]] = combn(kps,i)
}


veadNpilt.beta = c()
for(i in 1:length(kps)){
  print(i);print(Sys.time())
  vead = c()
  for(j in 1:choose(length(kps),i)){
    kp = list.kps[[i]][,j]
    kp_data = rf[rf$kp %in% kp,]
    kp_data[,1:4] = (kp_data[,1:4]*(dim(kp_data)[1]-1) + 0.5)/dim(kp_data)[1] #seda on aint beta jaoks vaja
    KNN_PBP = kp_data[,1:5] %>% group_by(aproovitykk_id) %>% summarise_all(funs(bets_fun(.,method = "mle")))
    KNN_PBP[,2:5] = KNN_PBP[2:5] / rowSums(KNN_PBP[,2:5])
    dp = merge(KNN_PBP, taks.info, by = "aproovitykk_id", all.x = T)
    viga = sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4)
    vead[j] = viga
  }
  veadNpilt.beta[i] = mean(vead)
  print(veadNpilt.beta)
}

veadNpilt.mean = c()
for(i in 1:length(kps)){
  print(i);print(Sys.time())
  vead = c()
  for(j in 1:choose(length(kps),i)){
    kp = list.kps[[i]][,j]
    kp_data = rf[rf$kp %in% kp,]
    #kp_data[,1:4] = (kp_data[,1:4]*(dim(kp_data)[1]-1) + 0.5)/dim(kp_data)[1] #seda on aint beta jaoks vaja
    KNN_PBP = kp_data[,1:5] %>% group_by(aproovitykk_id) %>% summarise_all(funs(mean))
    KNN_PBP[,2:5] = KNN_PBP[2:5] / rowSums(KNN_PBP[,2:5])
    dp = merge(KNN_PBP, taks.info, by = "aproovitykk_id", all.x = T)
    viga = sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4)
    vead[j] = viga
  }
  veadNpilt.mean[i] = mean(vead)
  print(veadNpilt.mean)
}

veadNpilt.epa = c()
for(i in 1:length(kps)){
  print(i);print(Sys.time())
  vead = c()
  for(j in 1:choose(length(kps),i)){
    kp = list.kps[[i]][,j]
    kp_data = rf[rf$kp %in% kp,]
    #kp_data[,1:4] = (kp_data[,1:4]*(dim(kp_data)[1]-1) + 0.5)/dim(kp_data)[1] #seda on aint beta jaoks vaja
    KNN_PBP = kp_data[,1:5] %>% group_by(aproovitykk_id) %>% summarise_all(funs(kernel.dens))
    KNN_PBP[,2:5] = KNN_PBP[2:5] / rowSums(KNN_PBP[,2:5])
    dp = merge(KNN_PBP, taks.info, by = "aproovitykk_id", all.x = T)
    viga = sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4)
    vead[j] = viga
  }
  veadNpilt.epa[i] = mean(vead)
  print(veadNpilt.epa)
}

par(mfrow=c(1,3))
plot(unlist(veadNpilt.mean), type = "o", ylab = "RMSE: mean", xlab = "Hinnangus kasutatud pilte", ylim = c(0.175,0.237))
plot(unlist(veadNpilt.beta), type = "o", ylab = "RMSE: beta", xlab = "Hinnangus kasutatud pilte", ylim = c(0.175,0.237))
plot(unlist(veadNpilt.epa), type = "o", ylab = "RMSE: Epanechnikov kernel", xlab = "Hinnangus kasutatud pilte", ylim = c(0.175,0.237))


















#LANDSAT! ja SCALED SENTINEL/landsat





#SKALEERIMINE ON MÕTTETU !!!!


#LANDSAT! 


setwd("A:/MAKA/TEST")
msls = seq(1,3,1)
mds = seq(13,27,2)
rfs = vector("list", length = length(msls))
for(j in 1:length(msls)){
  jj = msls[j]
  vec = vector("list", length = length(mds))
  for(k in 1:length(mds)){
    md = mds[k]
    name <- paste("CART_B250_tm100p_PBP_LANDSAT",jj,md, sep = "_")
    name <- paste(name,".csv", sep = "")
    vec[[k]] <- read.csv(name)
  }
  rfs[[j]] = vec
}

kov = matrix(NA, nrow = length(msls), ncol = length(mds))
#see nüüd õige beta!
for(j in 1:length(msls)){
  for(k in 1:length(mds)){
    data.j.k = rfs[[j]][[k]]
    data.j.k[,1:4] = data.j.k[,1:4] / 100
    data.j.k[,1:4] = (data.j.k[,1:4]*5488 + 0.5)/5489
    beta.pbp = data.j.k[,1:5] %>% group_by(aproovitykk_id) %>% summarise_all(funs(bets_fun))
    beta.pbp = merge(beta.pbp, taks.info, all.x = T, by = "aproovitykk_id")
    beta.pbp[,2:5] = beta.pbp[2:5] / rowSums(beta.pbp[2:5])
    RMSE = sqrt(sum((beta.pbp[,2:5] - beta.pbp[,11:14])**2) /(dim(beta.pbp)[1]*4))
    print(j);print(k);print(RMSE)
    kov[j,k] = RMSE
  }
}



pheatmap(kov.mean, cluster_rows = F, cluster_cols = F);
pheatmap(kov.beta, cluster_rows = F, cluster_cols = F)
pheatmap(kov.beta2sd, cluster_rows = F, cluster_cols = F)

pheatmap(kov, cluster_rows = F, cluster_cols = F)
which(kov == min(kov), arr.ind = TRUE)
min(kov) 



rf = rfs[[2]][[5]];rf[,1:4] = rf[,1:4] / 100

kps=unique(rf$kp)
kps1 = sort(as.Date(kps, format = "%Y-%m-%d"))

dev.off()
par(mfrow = c(3, 3))
for (i in 1:9){
  kp = kps1[i]
  kp_data = rf[rf$kp == kp,]
  kp_data = merge(kp_data, taks.info, all.x = T, by = "aproovitykk_id")
  plot(kp_data[,12],kp_data[,2], main = paste("Mänd ", kp), xlim = c(0,1), ylim = c(0,1), xlab = "", ylab = "", col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
}

dev.off()
par(mfrow = c(3, 3))
for (i in 1:9){
  kp = kps1[i]
  kp_data = rf[rf$kp == kp,]
  kp_data = merge(kp_data, taks.info, all.x = T, by = "aproovitykk_id")
  plot(kp_data[,13],kp_data[,3], main = paste("Kuusk ", kp), xlim = c(0,1), ylim = c(0,1), xlab = "", ylab = "", col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
}

dev.off()
par(mfrow = c(3, 3))
for (i in 1:9){
  kp = kps1[i]
  kp_data = rf[rf$kp == kp,]
  kp_data = merge(kp_data, taks.info, all.x = T, by = "aproovitykk_id")
  plot(kp_data[,14],kp_data[,4], main = paste("Kask ", kp), xlim = c(0,1), ylim = c(0,1), xlab = "", ylab = "", col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
}

dev.off()
par(mfrow = c(3, 3))
for (i in 1:9){
  kp = kps1[i]
  kp_data = rf[rf$kp == kp,]
  kp_data = merge(kp_data, taks.info, all.x = T, by = "aproovitykk_id")
  plot(kp_data[,15],kp_data[,5], main = paste("Muu ", kp), xlim = c(0,1), ylim = c(0,1), xlab = "", ylab = "", col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
}



data.kp = rf
data.kp[,1:4] = (data.kp[,1:4]*5488 + 0.5)/5489 #0 ja 1 probleem
data.kp = data.kp[,1:5]

CART_PBP_BETA = data.kp %>% group_by(aproovitykk_id) %>% summarise_all(funs(bets_fun2sd))
kb1 = CART_PBP_BETA
rsm = rowSums(kb1[,2:5])
kb1[,2:5] = kb1[2:5] / rsm

dp = merge(kb1, taks.info, by = "aproovitykk_id", all.x = T)
dev.off()
par(mfrow = c(2,2))
plot(dp[,11],dp[,2], xlab = "Mänd", ylab = "Hinnang: CART+Bagging", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dp[,12],dp[,3], xlab = "Kuusk", ylab = "Hinnang: CART+Bagging", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dp[,13],dp[,4], xlab = "Kask", ylab = "Hinnang: CART+Bagging", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dp[,14],dp[,5], xlab = "Muu", ylab = "Hinnang: CART+Bagging", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))

sqrt(mean((dp[,11]-dp[,2])**2))                     
sqrt(mean((dp[,12]-dp[,3])**2))                  
sqrt(mean((dp[,13]-dp[,4])**2))                    
sqrt(mean((dp[,14]-dp[,5])**2))                     
sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4)


#sentinel ja landsat koos!
#setwd( "A:/MAKA/TEST/test");load(file ="RF_SENT_LANDSAT_PBP.RData")
rfl = rf
rfs = rf

table(rfs$kp)
table(rfl$kp)

rfl$satel = "landsat";rfs$satel = "sentinel"
rfall = rbind(rfs, rfl)


data.kp = rfall
data.kp[,1:4] = (data.kp[,1:4]*5488 + 0.5)/5489 #0 ja 1 probleem
data.kp = data.kp[,1:5]

CART_PBP_BETA = data.kp %>% group_by(aproovitykk_id) %>% summarise_all(funs(bets_fun))
kb1 = CART_PBP_BETA
rsm = rowSums(kb1[,2:5])
kb1[,2:5] = kb1[2:5] / rsm

dp = merge(kb1, taks.info, by = "aproovitykk_id", all.x = T)
dev.off()
par(mfrow = c(2,2))
plot(dp[,11],dp[,2], xlab = "Mänd", ylab = "Hinnang: CART+Bagging", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dp[,12],dp[,3], xlab = "Kuusk", ylab = "Hinnang: CART+Bagging", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dp[,13],dp[,4], xlab = "Kask", ylab = "Hinnang: CART+Bagging", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dp[,14],dp[,5], xlab = "Muu", ylab = "Hinnang: CART+Bagging", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))

sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4)

#optimeerime betat
bets.shape.sd = function(vec, method = "mle", sdcut, shapecut){
  #sdcut = par1[1]; shapecut = par1[2]
  if(any(is.na(vec))){
    stop("NA")
  }
  vec0 = vec
  if(length(vec) > 2){sd = sqrt(var(vec));mu = mean(vec);vec = vec[(vec > mu-sdcut*sd & vec < mu+sdcut*sd)]}
  if(length(unique(vec)) < 2){
    vec = vec0
    max.d = mean(vec)
  }
  else{
    eb = ebeta(vec, method = method)
    a = eb$parameters[1]; b = eb$parameters[2]
    opt = optimize(interval = c(0,1), dbeta, shape1 = a, shape2 = b, maximum = T)
    max.d = opt$`maximum`
    # if((a < shapecut | b < shapecut)){
    #   max.d = mean(vec)
    # }
    if((a < shapecut & b < shapecut)){
      max.d = mean(vec)
    }
  }
  max.d
}

opti.beta = function(par1,df){
  par1=c(2,1)
  sdcut = par1[1]; shapecut = par1[2]
  #df = df[,1:5]
  df = data.kp
  beta.pbp = df %>% group_by(aproovitykk_id) %>% summarise_all(funs(bets.shape.sd(.,sdcut = sdcut, shapecut = shapecut)))
  dp = merge(beta.pbp, taks.info, by = "aproovitykk_id", all.x = T)
  dp[,2:5] = dp[,2:5]/rowSums(dp[,2:5])
  sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4)
}

beta.opt = optim(par = c(69,1), fn = opti.beta, df = data.kp, lower=c(0,Inf), upper=c(1,Inf), method = "L-BFGS-B")
#see ei tööta :S



#plot enne välja jätmist

#save(rfall, file = "RF_SENT_LANDSAT_PBP.RData")
load(file = "RF_SENT_LANDSAT_PBP.RData", verbose = T)
#rfall

df = rfall
df[,1:4] = (df[,1:4]*5488 + 0.5)/5489
beta.pbp = df[,1:5] %>% group_by(aproovitykk_id) %>% summarise_all(funs(bets_fun))
dp = merge(beta.pbp, taks.info, by = "aproovitykk_id", all.x = T)
dp[,2:5] = dp[,2:5]/rowSums(dp[,2:5])

dev.off()
par(mfrow = c(2,2))
plot(dp[,11],dp[,2], xlab = "Mänd", ylab = "Hinnang", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369), pch = 16)
abline(lm(dp[,2] ~ dp[,11]))
plot(dp[,12],dp[,3], xlab = "Kuusk", ylab = "Hinnang", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369), pch = 16)
abline(lm(dp[,3] ~ dp[,12]))
plot(dp[,13],dp[,4], xlab = "Kask", ylab = "Hinnang", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369), pch = 16)
abline(lm(dp[,4] ~ dp[,13]))
plot(dp[,14],dp[,5], xlab = "Muu", ylab = "Hinnang", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369), pch = 16)
abline(lm(dp[,5] ~ dp[,14]))



#####################################################



#Vead ENN



######################################################

rfall$kp.satel = paste(rfall$kp, rfall$satel)
kpsatels = unique(rfall$kp.satel)
df.sl = rfall

ss = 500
veadNpilt.epa.sample = c()
for(i in 1:length(kpsatels)){
  print(i);print(Sys.time())
  vead = c()
  if(choose(length(kpsatels),i) <= ss){
    for(j in 1:choose(length(kpsatels),i)){
      kpsatel = list.kpsatel[[i]][,j]
      kp_data = df.sl[df.sl$kp.satel %in% kpsatel,]
      #kp_data[,1:4] = (kp_data[,1:4]*(dim(kp_data)[1]-1) + 0.5)/dim(kp_data)[1] #seda on aint beta jaoks vaja
      KNN_PBP = kp_data[,1:5] %>% group_by(aproovitykk_id) %>% summarise_all(funs(epa.kernel))
      KNN_PBP[,2:5] = KNN_PBP[2:5] / rowSums(KNN_PBP[,2:5])
      dp = merge(KNN_PBP, taks.info, by = "aproovitykk_id", all.x = T)
      viga = sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4)
      vead[j] = viga
    }
    veadNpilt.epa.sample[i] = mean(vead)
  }
  else{
    sample = list.kpsatel[[i]][,sample(ncol(list.kpsatel[[i]]),size=ss,replace=T)]
    for(j in 1:ss){
      kpsatel = sample[,j]
      kp_data = df.sl[df.sl$kp.satel %in% kpsatel,]
      #kp_data[,1:4] = (kp_data[,1:4]*(dim(kp_data)[1]-1) + 0.5)/dim(kp_data)[1] #seda on aint beta jaoks vaja
      KNN_PBP = kp_data[,1:5] %>% group_by(aproovitykk_id) %>% summarise_all(funs(epa.kernel))
      KNN_PBP[,2:5] = KNN_PBP[2:5] / rowSums(KNN_PBP[,2:5])
      dp = merge(KNN_PBP, taks.info, by = "aproovitykk_id", all.x = T)
      viga = sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4)
      vead[j] = viga
    }
    veadNpilt.epa.sample[i] = mean(vead)
  }
  print(veadNpilt.epa.sample)
}

save(veadNpilt.epa.sample, file = "veadNpilt_epa_sample500RF.RData")


veadNpilt.beta.sample = c()
for(i in 1:length(kpsatels)){
  print(i);print(Sys.time())
  vead = c()
  if(choose(length(kpsatels),i) <= ss){
    for(j in 1:choose(length(kpsatels),i)){
      kpsatel = list.kpsatel[[i]][,j]
      kp_data = df.sl[df.sl$kp.satel %in% kpsatel,]
      kp_data[,1:4] = (kp_data[,1:4]*(dim(kp_data)[1]-1) + 0.5)/dim(kp_data)[1] #seda on aint beta jaoks vaja
      KNN_PBP = kp_data[,1:5] %>% group_by(aproovitykk_id) %>% summarise_all(funs(bets_fun))
      KNN_PBP[,2:5] = KNN_PBP[2:5] / rowSums(KNN_PBP[,2:5])
      dp = merge(KNN_PBP, taks.info, by = "aproovitykk_id", all.x = T)
      viga = sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4)
      vead[j] = viga
    }
    veadNpilt.beta.sample[i] = mean(vead)
  }
  else{
    sample = list.kpsatel[[i]][,sample(ncol(list.kpsatel[[i]]),size=ss,replace=T)]
    for(j in 1:ss){
      kpsatel = sample[,j]
      kp_data = df.sl[df.sl$kp.satel %in% kpsatel,]
      kp_data[,1:4] = (kp_data[,1:4]*(dim(kp_data)[1]-1) + 0.5)/dim(kp_data)[1] #seda on aint beta jaoks vaja
      KNN_PBP = kp_data[,1:5] %>% group_by(aproovitykk_id) %>% summarise_all(funs(bets_fun))
      KNN_PBP[,2:5] = KNN_PBP[2:5] / rowSums(KNN_PBP[,2:5])
      dp = merge(KNN_PBP, taks.info, by = "aproovitykk_id", all.x = T)
      viga = sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4)
      vead[j] = viga
    }
    veadNpilt.beta.sample[i] = mean(vead)
  }
  print(veadNpilt.beta.sample)
}

save(veadNpilt.beta.sample, file = "veadNpilt_beta_sample500RF.RData")

veadNpilt.mean.sample = c()
for(i in 1:length(kpsatels)){
  print(i);print(Sys.time())
  vead = c()
  if(choose(length(kpsatels),i) <= ss){
    for(j in 1:choose(length(kpsatels),i)){
      kpsatel = list.kpsatel[[i]][,j]
      kp_data = df.sl[df.sl$kp.satel %in% kpsatel,]
      #kp_data[,1:4] = (kp_data[,1:4]*(dim(kp_data)[1]-1) + 0.5)/dim(kp_data)[1] #seda on aint beta jaoks vaja
      KNN_PBP = kp_data[,1:5] %>% group_by(aproovitykk_id) %>% summarise_all(funs(mean))
      KNN_PBP[,2:5] = KNN_PBP[2:5] / rowSums(KNN_PBP[,2:5])
      dp = merge(KNN_PBP, taks.info, by = "aproovitykk_id", all.x = T)
      viga = sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4)
      vead[j] = viga
    }
    veadNpilt.mean.sample[i] = mean(vead)
  }
  else{
    sample = list.kpsatel[[i]][,sample(ncol(list.kpsatel[[i]]),size=ss,replace=T)]
    for(j in 1:ss){
      kpsatel = sample[,j]
      kp_data = df.sl[df.sl$kp.satel %in% kpsatel,]
      #kp_data[,1:4] = (kp_data[,1:4]*(dim(kp_data)[1]-1) + 0.5)/dim(kp_data)[1] #seda on aint beta jaoks vaja
      KNN_PBP = kp_data[,1:5] %>% group_by(aproovitykk_id) %>% summarise_all(funs(mean))
      KNN_PBP[,2:5] = KNN_PBP[2:5] / rowSums(KNN_PBP[,2:5])
      dp = merge(KNN_PBP, taks.info, by = "aproovitykk_id", all.x = T)
      viga = sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4)
      vead[j] = viga
    }
    veadNpilt.mean.sample[i] = mean(vead)
  }
  print(veadNpilt.mean.sample)
}

save(veadNpilt.mean.sample, file = "veadNpilt_mean_sample500RF.RData")



par(mfrow=c(1,3))
plot(unlist(veadNpilt.mean.sample), type = "o", ylab = "RMSE: mean", xlab = "Hinnangus kasutatud pilte", ylim = c(0.17,0.235))
plot(unlist(veadNpilt.beta.sample), type = "o", ylab = "RMSE: beta", xlab = "Hinnangus kasutatud pilte", ylim = c(0.17,0.235))
plot(unlist(veadNpilt.epa.sample), type = "o", ylab = "RMSE: Epanechnikov kernel", xlab = "Hinnangus kasutatud pilte", ylim = c(0.17,0.235))

#Piltide välja jätmine:
rfall$kp.satel = paste(rfall$kp, rfall$satel)
kpsatels = unique(rfall$kp.satel)
df.sl = rfall


kps0 = kpsatel
min0 = 69; min = 0.185 #; min = 0
out = c()
out.rmse = c()
j = 0
while (min < min0) {
  j = j+1
  min0 = min
  viga = c()
  for(i in 1:length(kps0)){
    kp.out = kps0[i]
    df = df.sl[df.sl$kp.satel %in% kps0,]
    df = df[df$kp.satel != kp.out,]
    df[,1:4] = (df[,1:4]*(dim(df)[1]-1) + 0.5)/dim(df)[1]
    KNN_PBP = df[,1:5] %>% group_by(aproovitykk_id) %>% summarise_all(funs(bets_fun))
    KNN_PBP[,2:5] = KNN_PBP[,2:5] / rowSums(KNN_PBP[,2:5])
    dp = merge(KNN_PBP, taks.info, by = "aproovitykk_id", all.x = T)
    rmse = sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4)
    viga[i] = rmse
  }
  out.rmse[j] = min
  print(viga)
  out[j] = kps0[which.min(viga)]
  kps0 = kps0[-which.min(viga)]
  min = min(viga);
  print(c(min,min0))
}

par(mfrow = c(1,1))
plot(out.rmse, type = "o", xlab = "Mitu kehvemat välja jäetud", ylab = "RMSE")



######### millised vaatlused välja jäid ##################

RMSE = function(data){
  sqrt(sum((data[,2:5]-data[,15:18])**2)/(dim(data)[1]*4))
}

RMSE_liik = function(data){
  l1 = sqrt(sum((data[,2]-data[,15])**2)/(dim(data)[1]))
  l2 = sqrt(sum((data[,3]-data[,16])**2)/(dim(data)[1]))
  l3 = sqrt(sum((data[,4]-data[,17])**2)/(dim(data)[1]))
  l4 = sqrt(sum((data[,5]-data[,18])**2)/(dim(data)[1]))
  c(l1,l2,l3,l4)
}

vead = c();vaatlusi=c();vead.liik = matrix(NA, nrow = 24, ncol = 4)
for(i in 1:24){
  kp = kpsatel[i]
  kp_data = df.sl[df.sl$kp.satel == kp,]
  kp_data = merge(kp_data, taks.info, all.x = T, by = "aproovitykk_id")
  vead[i] = RMSE(kp_data)
  vead.liik[i,] = RMSE_liik(kp_data)
  vaatlusi[i] = dim(kp_data)[1]
}


vead = data.frame(vead = vead, kp = kpsatel, vaatlusi = vaatlusi) #mitmes päev aastas
vead$kpsatel = vead$kp
vead$kp = substr(vead$kp, 1, 10)
vead$kp <- format(as.Date(vead$kp), "%j")
vead = vead[order(vead$kp),];vead$kp = as.numeric(vead$kp)


# vead.liik = data.frame(vead.liik);names(vead.liik) = c("MA","KU","KS","MUU")
# vead.liik$kp = kpsatel; vead.liik$vaatlusi = vaatlusi
# vead.liik$kp = substr(vead$kp, 1, 10)
# vead.liik$kp <- format(as.Date(vead$kp), "%j")
# vead.liik$kp = as.numeric(vead$kp)
# vead.liik.long = melt(vead.liik, id.vars = c("kp","vaatlusi"))


kps.out.b = c()
kps0 = kpsatel
for(i in 1:length(out)){
  out0 = out[i]
  kps.out.b[i] = kps0[kps0 == out0];kps0 = kps0[kps0 != out0]
}
vead$out = "Jah"
vead[vead$kpsatel %in% kps.out.b,]$out = "Ei"

dev.off()
p1 = ggplot(data = vead, aes(x = kp, y = vead)) + geom_point(aes(size = vaatlusi, colour = out, shape = out)) +
  scale_x_continuous(breaks = kps) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(size = "Takseeralasid pildil", x = "Päev aastas", y = "RMSE", colour = "Parandab hinnangut", shape = "Parandab hinnangut") +
  scale_x_continuous(minor_breaks = seq(100,300,10), lim = c(105,285)) + 
  scale_y_continuous(minor_breaks = seq(0.15,0.3,0.05), lim = c(.14,.3)) + ggtitle("") + theme(plot.title = element_text(hjust = 0.5))
p1

#visuaalselt

data.kp = rfall[!(rfall$kp.satel %in% out),]
data.kp[,1:4] = (data.kp[,1:4]*5488 + 0.5)/5489 #0 ja 1 probleem
data.kp = data.kp[,1:5]
CART_PBP_BETA = data.kp %>% group_by(aproovitykk_id) %>% summarise_all(funs(bets_fun))
kb1 = CART_PBP_BETA
rsm = rowSums(kb1[,2:5])
kb1[,2:5] = kb1[2:5] / rsm

dp = merge(kb1, taks.info, by = "aproovitykk_id", all.x = T)

dev.off()
par(mfrow = c(2,2))
plot(dp[,11],dp[,2], xlab = "Mänd", ylab = "Hinnang", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.2), pch = 19)
abline(lm(dp[,2]~dp[,11]))
plot(dp[,12],dp[,3], xlab = "Kuusk", ylab = "Hinnang", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.2), pch = 19)
abline(lm(dp[,3]~dp[,12]))
plot(dp[,13],dp[,4], xlab = "Kask", ylab = "Hinnang", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.2),  pch = 19)
abline(lm(dp[,4]~dp[,13]))
plot(dp[,14],dp[,5], xlab = "Muu", ylab = "Hinnang", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.2), pch = 19)
abline(lm(dp[,5]~dp[,14]))

sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4)



#mis oleks kui kasutaks siin baggingut!?
#ehk võtaks tagasipanekuga valimeid ja lõpuks nende keskmise
N = 250
CART_PBP_BETA = data.frame(aproovitykk_id = c(), MA = c(), KU = c(), KS = c(), MUU = c())
#sample.kp.satel = replicate(N,sample(x = kpsatels, size = 24, replace = T))

out.rmse[8] #0.1711379
#kui võtta 8 kõige kehvemat pilti välja:
sample.kp.satel = replicate(N,sample(x = kpsatels[!(kpsatels %in% out[1:8])], size = 8, replace = T))

for(i in 1:N){
  sample.i = sample.kp.satel[,i]
  df = data.frame(MA = c(), KU = c(), KS = c(), MUU = c(), aproovitykk_id = c())
  for(j in 1:length(sample.i)){
    s1 = sample.i[j]
    df0 = rfall[rfall$kp.satel == s1,]; df0 = df0[,c("MA", "KU", "KS", "MUU", "aproovitykk_id")]
    df = rbind(df,df0)
  }
  df[,1:4] = (df[,1:4]*5488 + 0.5)/5489 
  CART_PBP_BETA0 = df %>% group_by(aproovitykk_id) %>% summarise_all(funs(bets_fun))
  CART_PBP_BETA = rbind(CART_PBP_BETA,CART_PBP_BETA0)
}

kb0 = CART_PBP_BETA; kb0[,2:5] = CART_PBP_BETA[,2:5] / rowSums(CART_PBP_BETA[,2:5])
kb0 = CART_PBP_BETA
kb1 = kb0 %>% group_by(aproovitykk_id) %>% summarise_all(funs(mean))
kb1[,2:5] = kb1[2:5] / rowSums(kb1[,2:5])

dp = merge(kb1, taks.info, by = "aproovitykk_id", all.x = T)

dev.off()
par(mfrow = c(2,2))
plot(dp[,11],dp[,2], xlab = "Mänd", ylab = "Hinnang", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.2), pch = 19)
abline(lm(dp[,2]~dp[,11]))
plot(dp[,12],dp[,3], xlab = "Kuusk", ylab = "Hinnang", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.2), pch = 19)
abline(lm(dp[,3]~dp[,12]))
plot(dp[,13],dp[,4], xlab = "Kask", ylab = "Hinnang", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.2),  pch = 19)
abline(lm(dp[,4]~dp[,13]))
plot(dp[,14],dp[,5], xlab = "Muu", ylab = "Hinnang", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.2), pch = 19)
abline(lm(dp[,5]~dp[,14]))


sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4)

#0.1768545 N = 10 korral; 0.1769705 kui esmalt ühikvektoriks

tst = CART_PBP_BETA[CART_PBP_BETA$aproovitykk_id == 34264, ]



