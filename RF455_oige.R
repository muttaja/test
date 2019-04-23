#RF 455 õige! varem oli max featrues = n featrues :()
setwd("A:/MAKA/TEST/test")
load("taks_info.RData")


setwd("A:/MAKA/TEST")
mds = seq(12,16, by = 1)
msls = seq(1,2,1)
rfs = vector("list", length = length(msls))
for(j in 1:length(msls)){
  vec = vector("list", length = length(mds))
  jj = msls[j]
  for(k in 1:length(mds)){
    md = mds[k]
    name <- paste("RF455_OIGE_N250_VALIMATA_",jj,md, sep = "")
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
#nüüd "õige" rf-iga, et max_featues = 1/3 saan 0.1704135


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



#############################

#FEATURE IMPORTANCE!

FI = read.csv("RF_feat_importances.csv", header = F)
fi.std = read.csv("RF_std.csv", header = F)
FI$std = fi.std
FI = FI[order(FI$V2, decreasing = T),]
plot(FI)
par(mfrow = c(1,1))
barplot(FI$V2[1:10], names.arg=FI$V1[1:10],cex.names=.6, las = 2)

text(text(bbr, 3, labels=FI$V1[1:10]))

FI2 = read.csv("feature_IMP_500k.csv", header = T)
barplot(FI2$Importance[1:10], names.arg=FI2$Feature[1:10],cex.names=.6, las = 2)
barplot(FI2$Importance[41:50], names.arg=FI2$Feature[41:50],cex.names=.6, las = 2)

df = FI2[c(1:10,41:50),];df0 = data.frame("...",0); names(df0) = names(df)
df = rbind(df, df0)

#df = FI2


df$Feature <- factor(df$Feature, levels= unique((df$Feature)[order(-df$Importance)]))


p = ggplot(data=df, aes(y=Importance, x=Feature)) +
  geom_bar(stat="identity")

p + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Tunnused") + ylab("Olulisus")


#
df=FI
df$V1 <- factor(df$V1, levels= unique((df$V1)[order(-df$V2)]))
p = ggplot(data=df, aes(y=V2, x=V1)) +
  geom_bar(stat="identity")

p + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Tunnused") + ylab("Olulisus")



#kui valida aint oluliste tunnuste seast:

feat = read.csv("RF455_OIGE_N500_feature_inportance_212.csv")
dp = merge(feat, taks.info, all.x = T, by = "aproovitykk_id")
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
sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4) #[1] 0.173522










#PILT BY PILT

mds = seq(11,16, by = 1)
msls = seq(1,1,1)
rfs = vector("list", length = length(msls))
for(j in 1:length(msls)){
  vec = vector("list", length = length(mds))
  jj = msls[j]
  for(k in 1:length(mds)){
    md = mds[k]
    name <- paste("RF_250_PBP_SENTINEL",jj,md, sep = "_")
    name <- paste(name,".csv", sep = "")
    vec[[k]] <- read.csv(name)
  }
  rfs[[j]] = vec
}
#rfs$satel = "sentinel"
#rfs$kp.satel = paste(rfs$kp, rfs$satel)
rfs.s = rfs

mds = seq(11,16, by = 1)
msls = seq(1,1,1)
rfs = vector("list", length = length(msls))
for(j in 1:length(msls)){
  vec = vector("list", length = length(mds))
  jj = msls[j]
  for(k in 1:length(mds)){
    md = mds[k]
    name <- paste("RF_250_PBP_LANDSAT",jj,md, sep = "_")
    name <- paste(name,".csv", sep = "")
    vec[[k]] <- read.csv(name)
  }
  rfs[[j]] = vec
}

#rfs$satel = "landsat"
#rfs$kp.satel = paste(rfs$kp, rfs$satel)
rfs.l = rfs
#rf.pbp.all = rbind(rfs.l, rfs.s)
#save(rf.pbp.all, file = "RF_PBP_landsat_ja_sentinel.RData")

kov = matrix(NA, nrow = length(msls), ncol = length(mds))
#see nüüd õige beta!
for(j in 1:length(msls)){
  for(k in 1:length(mds)){
    data.s = rfs.s[[j]][[k]]; data.s$kp.satel = paste(data.s$kp,"sentinel")
    data.l = rfs.l[[j]][[k]]; data.l$kp.satel = paste(data.l$kp,"landsat")
    data.j.k = rbind(data.s, data.l)
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

min(kov)

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


rfss = rfs.s[[1]][[5]]
rfss$satel = "sentinel"
rfsl = rfs.l[[1]][[5]]
rfsl$satel = "landsat"

rfs.sl = rbind(rfss,rfsl)
rfs.sl$kp.satel = paste(rfs.sl$kp,rfs.sl$satel)
rfs.sl[,1:4] = rfs.sl[,1:4] / 100

kpsatels = unique(rfs.sl$kp.satel)
list.kpsatel = vector("list", length=length(kpsatels)) #see pm 1.5gb...
for(i in 1:length(kpsatels)){
  list.kpsatel[[i]] = combn(kpsatels,i)
}


#mean, beta ja epa RMSE:
df = rfs.sl #proportsioonid
df1 = df[,1:5]
#df1[,1:4] = (df1[,1:4]*5488 + 0.5)/5489
df1[,1:4] = df1[,1:4] / rowSums(df1[,1:4])
print(head(df1))
KNN_PBP = df1 %>% group_by(aproovitykk_id) %>% summarise_all(funs(epa.kernel))
dp = merge(KNN_PBP, taks.info, by = "aproovitykk_id", all.x = T)
dp[,2:5] = dp[,2:5] / rowSums(dp[,2:5])
sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4)

#beta 0.1780004
#beta2sd 0.177383
#mean 0.1860343
#epa.kernel 0.1786128

#RF veadNpilt
df.sl = rfs.sl
ss = 500
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

save(veadNpilt.beta.sample, file = "veadNpilt_beta_sample_RF.RData")

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

save(veadNpilt.epa.sample, file = "veadNpilt_epa_sample_RF.RData")



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

save(veadNpilt.mean.sample, file = "veadNpilt_mean_sample_RF.RData")


par(mfrow=c(1,3))
plot(unlist(veadNpilt.mean.sample), type = "o", ylab = "RMSE: mean", xlab = "Hinnangus kasutatud pilte", ylim = c(0.164,0.235))
plot(unlist(veadNpilt.beta.sample), type = "o", ylab = "RMSE: beta", xlab = "Hinnangus kasutatud pilte", ylim = c(0.164,0.235))
plot(unlist(veadNpilt.epa.sample), type = "o", ylab = "RMSE: Epanechnikov kernel", xlab = "Hinnangus kasutatud pilte", ylim = c(0.164,0.235))


#vead liikide ja piltide kaupa
#mis aastajal saab parimad prognoosid!?
RMSE = function(data){
  sqrt(sum((data[,2:5]-data[,14:17])**2)/(dim(data)[1]*4))
}

RMSE_liik = function(data){
  l1 = sqrt(sum((data[,2]-data[,14])**2)/(dim(data)[1]))
  l2 = sqrt(sum((data[,3]-data[,15])**2)/(dim(data)[1]))
  l3 = sqrt(sum((data[,4]-data[,16])**2)/(dim(data)[1]))
  l4 = sqrt(sum((data[,5]-data[,17])**2)/(dim(data)[1]))
  c(l1,l2,l3,l4)
}

vead = c();vaatlusi=c();vead.liik = matrix(NA, nrow = 24, ncol = 4)
for(i in 1:24){
  #i = 1
  kp = kpsatel[i]
  kp_data = df.sl[df.sl$kp.satel == kp,]
  kp_data = merge(kp_data, taks.info, all.x = T, by = "aproovitykk_id")
  vead[i] = RMSE(kp_data)
  vead.liik[i,] = RMSE_liik(kp_data)
  vaatlusi[i] = dim(kp_data)[1]
}


vead = data.frame(vead = vead, kp = kpsatel, vaatlusi = vaatlusi) #mitmes päev aastas
vead$kp.satel = vead$kp
vead$kp = substr(vead$kp.satel, 1, 10)
vead$kp <- format(as.Date(vead$kp), "%j")
vead = vead[order(vead$kp),];vead$kp = as.numeric(vead$kp)

# vead.liik = data.frame(vead.liik);names(vead.liik) = c("MA","KU","KS","MUU")
# vead.liik$vaatlusi = vaatlusi
# vead.liik$kpsatel = kpsatel; vead.liik$kp = substr(vead.liik$kpsatel, 1, 10)
# vead.liik$kp <- format(as.Date(vead.liik$kp), "%j"); 
# vead.liik$kp = as.integer(vead.liik$kp)
# vead.liik.long = melt(vead.liik, id.vars = c("kp","vaatlusi"))

vead.liik = data.frame(vead.liik);names(vead.liik) = c("MA","KU","KS","MUU")
vead.liik$kp = kpsatel; vead.liik$vaatlusi = vaatlusi
vead.liik$kp = substr(vead.liik$kp, 1, 10)
vead.liik$kp <- format(as.Date(vead.liik$kp), "%j")
vead.liik$kp = as.numeric(vead.liik$kp)
vead.liik.long = melt(vead.liik, id.vars = c("kp","vaatlusi"))



par(mfrow = c(1, 1))
ggplot(data = vead.liik.long, aes(x = kp, y = value)) + geom_point(aes(size = vaatlusi, colour = variable), alpha = 0.69) +
  scale_x_continuous(breaks = kps) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(size = "Takseeralasid pildil",colour = "Liik", x = "Päev aastas", y = "RMSE")  +
  scale_x_continuous(minor_breaks = seq(100,300,10), lim = c(100,300)) + 
  scale_y_continuous(minor_breaks = seq(0.15,0.3,0.05), lim = c(.14,.3))


#piltide välja jätmine:

kps0 = as.character(vead$kp.satel)
min0 = 69; min = 0.1786128 #; min = 0
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
    df[,1:4] = df[,1:4] / rowSums(df[,1:4])
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
  print(out)
  kps0 = kps0[-which.min(viga)]
  min = min(viga);
  print(c(min,min0))
}

par(mfrow = c(1,1))
plot(out.rmse, type = "o", xlab = "Mitu kehvemat välja jäetud", ylab = "RMSE")
min(out.rmse); length(out)

#beta.out = out

#väärtused enne:
#beta 0.1780004 0.1713209  16
#beta2sd 0.177383 0.1710908 15
#mean 0.1860343 0.177532 19
#epa.kernel 0.1786128 0.1766965 6

kps.out.b = c()
kps0 = as.character(vead$kp.satel)
for(i in 1:length(out)){
  #i = 16
  out0 = out[i]
  kps.out.b[i] = kps0[kps0 == out0]
  kps0 = kps0[kps0 != out0]
}
vead$out = "Jah"
vead[vead$kp.satel %in% kps.out.b,]$out = "Ei"

dev.off()
p1 = ggplot(data = vead, aes(x = kp, y = vead)) + geom_point(aes(size = vaatlusi, colour = out, shape = out)) +
  scale_x_continuous(breaks = kps) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(size = "Takseeralasid pildil", x = "Päev aastas", y = "RMSE", colour = "Parandab hinnangut", shape = "Parandab hinnangut") +
  scale_x_continuous(minor_breaks = seq(100,300,10), lim = c(105,285)) + 
  scale_y_continuous(minor_breaks = seq(0.15,0.3,0.05), lim = c(.14,.3)) + ggtitle("") + theme(plot.title = element_text(hjust = 0.5))
p1


#ja lõpuks pilt ka

df = rfs.sl #proportsioonid
df1 = df[,1:5]
df1[,1:4] = (df1[,1:4]*5488 + 0.5)/5489
df1[,1:4] = df1[,1:4] / rowSums(df1[,1:4])
KNN_PBP = df1 %>% group_by(aproovitykk_id) %>% summarise_all(funs(bets_fun))
dp = merge(KNN_PBP, taks.info, by = "aproovitykk_id", all.x = T)
dp[,2:5] = dp[,2:5] / rowSums(dp[,2:5])


dp = merge(feat, taks.info, all.x = T, by = "aproovitykk_id")
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
sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4) #[1] 0.173522





