############ kuupäevade kaupa ###########
require(FNN)

tvmaht = 2 #proportsioonid
setwd("A:/MAKA/TEST/test")
data0 = read.csv("landsat455.csv")
names(data0)

data0 = data0[,c(2:10,16:28)]
vars = names(data0)[c(4:22)]
kps = unique(data0$kp)

result.ls = vector("list", length = 15)
bestk = c()

for(i in 1:9){
  print(Sys.time());print("i-s pilt:");print(i)
  #i = 1
  kp = kps[i]
  data = na.omit(data0[data0$kp == kp,])
  sid0 = data$aproovitykk_id
  
  data_puud = taks.info[taks.info$aproovitykk_id %in% sid0,]
  puud_true =  data_puud[,7:10]
  data_puud = data_puud[,(4*tvmaht-1):(4*tvmaht+2)] #vastavalt kas proportsioonid või tüvemahud
  
  
  bestvars.k = fun_bestvars(1,20, sidxx = sid0, kernel = epa)
  vark = bestvars.k[[1]]
  rss = unlist(bestvars.k[2])
  #print(rss)
  bests = c(match(rss[order(rss)][1],rss),match(rss[order(rss)][2],rss)) #,match(rss[order(rss)][3],rss)
  op1 = optim(par = rep(1, length(vark[[bests[1]]])), fn = fun_opti, k = bests[1], vars = vark[[bests[1]]], data = data, sidxx = sid0,method = "BFGS", kernel = epa)
  op2 = optim(par = rep(1, length(vark[[bests[2]]])), fn = fun_opti, k = bests[2], vars = vark[[bests[2]]], data = data, sidxx = sid0,method = "BFGS", kernel = epa)
  w.opt = vector("list", length=2);
  rss.opt = c()
  w.opt[[1]] = op1$par;   w.opt[[2]] = op2$par;   #w.opt[[3]] = op3$par;
  rss.opt[1] = op1$value; rss.opt[2] = op2$value; #rss.opt[3] = op3$value;
  brss = match(rss.opt[order(rss.opt)][1],rss.opt) #kuidagi tuleb parim k salvestada
  best_k = bests[brss]
  best_vars = vark[[best_k]]
  wgt = w.opt[[brss]]
  
  dex = data[,c("aproovitykk_id",best_vars)]
  dex = dex[dex$aproovitykk_id %in% sid0,]
  dex[,-1] = t((t(as.matrix(dex[,-1])))*wgt)
  dex$cl = "cl"
  result.ls[[i]] = fun_agre_kernel(dex, data_puud, k = best_k, sid = sid0, kernel = epa)
  bestk[i] = best_k
}

#save(result.ls, file ="KNN_pbp_landsat.RData")
#load(file ="KNN_pbp_landsat.RData")

kps = unique(as.Date(data0$kp))
kps1 = sort(as.Date(kps, format = "%Y-%m-%d"))
#männid
dev.off()
par(mfrow = c(3, 3))
for (i in 1:9) {
  #i = 15
  #kp0 = kps[i]
  #kp1 = kps1[kps1 == kp0]
  kp = kps[i]
  kp_data = result.ls[[i]]
  kp_data = merge(kp_data, taks.info, all.x = T, by = "aproovitykk_id")
  plot(kp_data[,11],kp_data[,2], main = paste("Mänd ", kp), xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.2), ylab = "", xlab = "", pch = 19)
}

#kuused
dev.off()
par(mfrow = c(3, 3))
for (i in 1:9) {
  kp = kps[i]
  kp_data = result.ls[[i]]
  kp_data = merge(kp_data, taks.info, all.x = T, by = "aproovitykk_id")
  plot(kp_data[,12],kp_data[,3], main = paste("Kuusk ", kp), xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.2), ylab = "", xlab = "", pch = 19)
}

#kased
dev.off()
par(mfrow = c(3, 3))
for (i in 1:9) {
  kp = kps[i]
  kp_data = result.ls[[i]]
  kp_data = merge(kp_data, taks.info, all.x = T, by = "aproovitykk_id")
  plot(kp_data[,13],kp_data[,4], main = paste("Kask ", kp), xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.2), ylab = "", xlab = "", pch = 19)
}

#muud
dev.off()
par(mfrow = c(3, 3))
for (i in 1:9) {
  kp = kps[i]
  kp_data = result.ls[[i]]
  kp_data = merge(kp_data, taks.info, all.x = T, by = "aproovitykk_id")
  plot(kp_data[,14],kp_data[,5], main = paste("Muu ", kp), xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.2), ylab = "", xlab = "", pch = 19)
}


#mis aastajal saab parimad prognoosid!?
RMSE = function(data){
  sqrt(sum((data[,2:5]-data[,11:14])**2)/(dim(data)[1]*4))
}

RMSE_liik = function(data){
  l1 = sqrt(sum((data[,2]-data[,11])**2)/(dim(data)[1]))
  l2 = sqrt(sum((data[,3]-data[,12])**2)/(dim(data)[1]))
  l3 = sqrt(sum((data[,4]-data[,13])**2)/(dim(data)[1]))
  l4 = sqrt(sum((data[,5]-data[,14])**2)/(dim(data)[1]))
  c(l1,l2,l3,l4)
}

kps = unique(data0$kp)
kps = kps[order(kps)]
kps = as.Date(kps)
kps <- format(as.Date(kps), "%j")
kps = as.numeric(kps)

#võtame landsati ja sentineli kokku!




vead = c();vaatlusi=c();vead.liik = matrix(NA, nrow = 9, ncol = 4)
for(i in 1:9){
  kp = kps[i]
  kp_data = result.ls[[i]]
  kp_data = merge(kp_data, taks.info, all.x = T, by = "aproovitykk_id")
  vead[i] = RMSE(kp_data)
  vead.liik[i,] = RMSE_liik(kp_data)
  vaatlusi[i] = dim(kp_data)[1]
}

require(ggplot2)
vead = data.frame(vead = vead, kp = kps, vaatlusi = vaatlusi) #mitmes päev aastas
dev.off()
par(mfrow = c(1, 1))
ggplot(data = vead, aes(x = kp, y = vead)) + geom_point(aes(size = vaatlusi)) +
  scale_x_continuous(breaks = kps) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(size = "Takseeralasid pildil", x = "Päev aastas", y = "RMSE") +
  scale_x_continuous(minor_breaks = seq(100,300,10), lim = c(105,285)) + 
  scale_y_continuous(minor_breaks = seq(0.15,0.3,0.05), lim = c(.14,.3))
#punkti suuruseks vaatluste arv

#liigikaupa

vead.liik = data.frame(vead.liik);names(vead.liik) = c("MA","KU","KS","MUU")
vead.liik$kp = kps; vead.liik$vaatlusi = vaatlusi
vead.liik.long = melt(vead.liik, id.vars = c("kp","vaatlusi"))

par(mfrow = c(1, 1))
ggplot(data = vead.liik.long, aes(x = kp, y = value)) + geom_point(aes(size = vaatlusi, colour = variable)) +
  scale_x_continuous(breaks = kps) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(size = "Takseeralasid pildil",colour = "Liik", x = "Päev aastas", y = "RMSE")


par(mfrow = c(1, 1))
ggplot(data = vead.liik.long, aes(x = kp, y = value)) + geom_point(aes(size = vaatlusi, colour = variable), alpha = 0.69) +
  scale_x_continuous(breaks = kps) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(size = "Takseeralasid pildil",colour = "Liik", x = "Päev aastas", y = "RMSE")  +
  scale_x_continuous(minor_breaks = seq(100,300,10), lim = c(100,300)) + 
  scale_y_continuous(minor_breaks = seq(0.15,0.3,0.05), lim = c(.14,.3))

#CARTB_RMSE_pbp_by_liik



#kui võtta nende piltide tuim keskmine:

kp_data = data.frame()
for(i in 1:9){
  kp = kps[i]
  kp_data0 = result.ls[[i]]; kp_data0$kp = kp
  kp_data0 = merge(kp_data0, taks.info, all.x = T, by = "aproovitykk_id")
  kp_data = rbind(kp_data, kp_data0)
}

KNN_PBP = kp_data[,1:5]

KNN_PBP[,2:5] = (KNN_PBP[,2:5]*5488 + 0.5)/5489
KNN_agr = KNN_PBP %>% group_by(aproovitykk_id) %>% summarise_all(funs(bets_fun)) #bets_fun(.,method = "mme")
KNN_agr[,2:5] = KNN_agr[,2:5] / rowSums(KNN_agr[,2:5])


dp = merge(KNN_agr, taks.info, by = "aproovitykk_id", all.x = T)
dev.off()
par(mfrow = c(2,2))
plot(dp[,11],dp[,2], xlab = "Mänd", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dp[,12],dp[,3], xlab = "Kuusk", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dp[,13],dp[,4], xlab = "Kask", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dp[,14],dp[,5], xlab = "Muu", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))

sqrt(mean((dp[,11]-dp[,2])**2))
sqrt(mean((dp[,12]-dp[,3])**2))
sqrt(mean((dp[,13]-dp[,4])**2))
sqrt(mean((dp[,14]-dp[,5])**2))
sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4)
#landsat mean: 0.1963024; beta: 0.2212548; epa.kernel 0.1967257
#see vist oli sentineli 9 esimest pili :S

#landsat mean: 0.1895437; epa.kernel: 0.1890741; beta: 0.1921513



#üks variant:

# Dave,
# 
# A common approach to this problem is to fit 2 logistic regression models to predict whether a case is 0 or 1. 
# Then, a beta regression is used for those in the range (0,1).
# B_mine stackex.


##a better lemon squeezer: seal 0 ka 1 probleem beta-jaotuses


data.kp1 = kp_data;
data.kp1[,2:5] = (data.kp1[,2:5]*5488 + 0.5)/5489
KNN_PBP_BETA0 = data.kp1[,1:5]

KNN_PBP_BETA = KNN_PBP_BETA0 %>% group_by(aproovitykk_id) %>% summarise_all(funs(bets_fun(.,method = "mle")))
KNN_PBP_BETA1 = kp_data[,1:5] %>% group_by(aproovitykk_id) %>% summarise_all(funs(epa.kernel))



kb1 = KNN_PBP_BETA
rsm = rowSums(kb1[,2:5])
kb1[,2:5] = kb1[2:5] / rsm

dp = merge(kb1, taks.info, by = "aproovitykk_id", all.x = T)
dev.off()
par(mfrow = c(2,2))
plot(dp[,11],dp[,2], xlab = "Mänd", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dp[,12],dp[,3], xlab = "Kuusk", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dp[,13],dp[,4], xlab = "Kask", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dp[,14],dp[,5], xlab = "Muu", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))

sqrt(mean((dp[,11]-dp[,2])**2))                     
sqrt(mean((dp[,12]-dp[,3])**2))                  
sqrt(mean((dp[,13]-dp[,4])**2))                    
sqrt(mean((dp[,14]-dp[,5])**2))                     
sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4)  


#progress piltide lisamisel:

list.kps = vector("list", length=length(kps))
for(i in 1:length(kps)){
  list.kps[[i]] = combn(kps,i)
}


veadNpilt.1 = c()
for(i in 1:length(kps)){
  print(i);print(Sys.time())
  vead = c()
  for(j in 1:choose(length(kps),i)){
    #siin midagi puudu, ei kasuta ju j!!!
    kp_data = data.frame()
    for(h in 1:i){
      kp = list.kps[[i]][h,j]
      kk = match(kp, kps)
      kp_data0 = result.kp.tv100[[kk]]; kp_data0$kp = kp
      kp_data0 = merge(kp_data0, taks.info, all.x = T, by = "aproovitykk_id")
      kp_data = rbind(kp_data, kp_data0)
    }
    KNN_PBP = kp_data[,1:5] %>% group_by(aproovitykk_id) %>% summarise_all(funs(mean))
    
    #äkki siin vale järjekord miskitel asjadel?
    
    dp = merge(KNN_PBP, taks.info, by = "aproovitykk_id", all.x = T)
    viga = sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4)
    vead[j] = viga
  }
  veadNpilt.1[i] = mean(vead)
  print(veadNpilt.1)
}
veadNpilt.1
#0.2231024 0.2135297 0.2069004 0.2028360 0.2003812 0.1988090 0.1977205 0.1969187 0.1963024

veadNpilt.epakernel = c()
for(i in 1:length(kps)){
  print(i);print(Sys.time())
  vead = c()
  for(j in 1:choose(length(kps),i)){
    kp_data = data.frame()
    for(h in 1:i){
      kp = list.kps[[i]][h,j]
      kk = match(kp, kps)
      kp_data0 = result.kp.tv100[[kk]]; kp_data0$kp = kp
      kp_data0 = merge(kp_data0, taks.info, all.x = T, by = "aproovitykk_id")
      kp_data = rbind(kp_data, kp_data0)
    }
    #kp_data[,2:5] = (kp_data[,2:5]*(dim(kp_data)[1]-1) + 0.5)/dim(kp_data)[1] #seda on aint beta jaoks vaja
    KNN_PBP = kp_data[,1:5] %>% group_by(aproovitykk_id) %>% summarise_all(funs(epa.kernel))
    kb1 = KNN_PBP
    rsm = rowSums(kb1[,2:5])
    kb1[,2:5] = kb1[2:5] / rsm
    dp = merge(kb1, taks.info, by = "aproovitykk_id", all.x = T)
    viga = sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4)
    vead[j] = viga
  }
  veadNpilt.epakernel[i] = mean(vead)
  print(veadNpilt.epakernel)
}
veadNpilt.epakernel
#0.2231024 0.2185830 0.2130807 0.2073816 0.2024757 0.1988610 0.1965018 0.1948992 0.1941774

veadNpilt.beta = c()
for(i in 1:length(kps)){
  print(i);print(Sys.time())
  vead = c()
  for(j in 1:choose(length(kps),i)){
    kp_data = data.frame()
    for(h in 1:i){
      kp = list.kps[[i]][h,j]
      kk = match(kp, kps)
      kp_data0 = result.kp.tv100[[kk]]; kp_data0$kp = kp
      kp_data0 = merge(kp_data0, taks.info, all.x = T, by = "aproovitykk_id")
      kp_data = rbind(kp_data, kp_data0)
    }
    kp_data[,2:5] = (kp_data[,2:5]*(dim(kp_data)[1]-1) + 0.5)/dim(kp_data)[1] #seda on aint beta jaoks vaja
    KNN_PBP = kp_data[,1:5] %>% group_by(aproovitykk_id) %>% summarise_all(funs(bets_fun(.,method = "mle")))
    kb1 = KNN_PBP
    rsm = rowSums(kb1[,2:5])
    kb1[,2:5] = kb1[2:5] / rsm
    dp = merge(kb1, taks.info, by = "aproovitykk_id", all.x = T)
    viga = sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4)
    vead[j] = viga
  }
  veadNpilt.beta[i] = mean(vead)
  print(veadNpilt.beta)
}
#[1] 0.2230432 0.2381486 0.2327967 0.2183120 0.2062506 0.1996189 0.1963720 0.1945039 0.1924911

par(mfrow=c(1,3))
plot(unlist(veadNpilt.1), type = "o", ylab = "RMSE: mean", xlab = "Hinnangus kasutatud pilte", ylim = c(0.18,0.236))
plot(unlist(veadNpilt.beta), type = "o", ylab = "RMSE: beta", xlab = "Hinnangus kasutatud pilte", ylim = c(0.18,0.236))
plot(unlist(veadNpilt.epakernel), type = "o", ylab = "RMSE: Epanechnikov kernel", xlab = "Hinnangus kasutatud pilte", ylim = c(0.18,0.236))
#KNN_hinnangus_kasutatud_pilte_LANDSAT

#sentinel ja landsat kokku 15 + 9 = 24 pilti, vaatame, kuidas 21...24 töötavad, ülejäänud vahepealsed võtaks liiga palju aega

load(file ="KNN_pbp_tv100_v2.RData")
#results.kp.tv100

dat0 = read.csv(file = "landsat455.csv")
kps = unique(dat0$kp)
df.lnd = data.frame()
for(i in 1:9){
  kp = kps[i]
  kp_data0 = result.ls[[i]]; kp_data0$kp = kp; kp_data0$satel = "landsat"
  kp_data0 = merge(kp_data0, taks.info, all.x = T, by = "aproovitykk_id")
  df.lnd = rbind(df.lnd, kp_data0)
}

dat0 = read.csv(file = "sentinel455.csv")
kps = unique(dat0$kp)

df.sent = data.frame()
for(i in 1:15){
  kp = kps[i]
  kp_data0 = result.kp.tv100[[i]]; kp_data0$kp = kp; kp_data0$satel = "sentinel"
  kp_data0 = merge(kp_data0, taks.info, all.x = T, by = "aproovitykk_id")
  df.sent = rbind(df.sent, kp_data0)
}

#need kokku ja siis hakka arvutama...
df.sl = rbind(df.sent, df.lnd)
df.sl$kp.satel = paste(df.sl$kp, df.sl$satel)
#save(df.sl, file = "knn455_pbp_landsat_ja_sentinel.RData")
#load(file = "knn455_pbp_landsat_ja_sentinel.RData")

kpsatels = unique(df.sl$kp.satel)

list.kpsatel = vector("list", length=length(kpsatels)) #see pm 1.5gb...
for(i in 1:length(kpsatels)){
  list.kpsatel[[i]] = combn(kpsatels,i)
}


veadNpilt.mean = c()
for(i in 21:length(kpsatels)){
  print(i);print(Sys.time())
  vead = c()
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
  veadNpilt.mean[i] = mean(vead)
  print(veadNpilt.mean)
}

veadNpilt.bets2sd = c()
for(i in 21:length(kpsatels)){
  print(i);print(Sys.time())
  vead = c()
  for(j in 1:choose(length(kpsatels),i)){
    kpsatel = list.kpsatel[[i]][,j]
    kp_data = df.sl[df.sl$kp.satel %in% kpsatel,]
    kp_data[,2:5] = (kp_data[,2:5]*(dim(kp_data)[1]-1) + 0.5)/dim(kp_data)[1] #seda on aint beta jaoks vaja
    KNN_PBP = kp_data[,1:5] %>% group_by(aproovitykk_id) %>% summarise_all(funs(bets_fun2sd))
    KNN_PBP[,2:5] = KNN_PBP[2:5] / rowSums(KNN_PBP[,2:5])
    dp = merge(KNN_PBP, taks.info, by = "aproovitykk_id", all.x = T)
    viga = sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4)
    vead[j] = viga
  }
  veadNpilt.bets2sd[i] = mean(vead)
  print(veadNpilt.bets2sd)
}

veadNpilt.21.epa = c()
for(i in 21:length(kpsatels)){
  print(i);print(Sys.time())
  vead = c()
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
  veadNpilt.21.epa[i] = mean(vead)
  print(veadNpilt.21.epa)
}

# 0.1786042 0.1784449 0.1783361 0.1783341



#vead liigi ja kpsatel kaupa koos

#


RMSE = function(data){
  sqrt(sum((data[,2:5]-data[,23:26])**2)/(dim(data)[1]*4))
}

RMSE_liik = function(data){
  l1 = sqrt(sum((data[,2]-data[,23])**2)/(dim(data)[1]))
  l2 = sqrt(sum((data[,3]-data[,24])**2)/(dim(data)[1]))
  l3 = sqrt(sum((data[,4]-data[,25])**2)/(dim(data)[1]))
  l4 = sqrt(sum((data[,5]-data[,26])**2)/(dim(data)[1]))
  c(l1,l2,l3,l4)
}



vead = c();vaatlusi=c();vead.liik = matrix(NA, nrow = 24, ncol = 4)
for(i in 1:24){
  kp = kpsatel[i]
  kp_data = df.sl[df.sl$kp.satel == kp,]
  kp_data = merge(kp_data, taks.info, all.x = T, by = "aproovitykk_id")
  #print(head(kp_data));print(dim(kp_data))
  vead[i] = RMSE(kp_data)
  vead.liik[i,] = RMSE_liik(kp_data)
  vaatlusi[i] = dim(kp_data)[1]
}


vead = data.frame(vead = vead, kp = kpsatel, vaatlusi = vaatlusi) #mitmes päev aastas
vead$kpsatel = vead$kp
vead$kp = substr(vead$kp, 1, 10)
vead$kp <- format(as.Date(vead$kp), "%j")
vead = vead[order(vead$kp),];vead$kp = as.numeric(vead$kp)


require(ggplot2)
dev.off()
par(mfrow = c(1, 1))
ggplot(data = vead, aes(x = kp, y = vead)) + geom_point(aes(size = vaatlusi)) +
  scale_x_continuous(breaks = kps) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(size = "Takseeralasid pildil", x = "Päev aastas", y = "RMSE") +
  scale_x_continuous(minor_breaks = seq(100,300,10), lim = c(105,285)) + 
  scale_y_continuous(minor_breaks = seq(0.15,0.3,0.05), lim = c(.14,.3))
#punkti suuruseks vaatluste arv

#liigikaupa

vead.liik = data.frame(vead.liik);names(vead.liik) = c("MA","KU","KS","MUU")
vead.liik$kp = kpsatel; vead.liik$vaatlusi = vaatlusi

vead.iik$kp = substr(vead$kp, 1, 10)
vead.liik$kp <- format(as.Date(vead$kp), "%j")
vead.liik$kp = as.numeric(vead$kp)
vead.liik.long = melt(vead.liik, id.vars = c("kp","vaatlusi"))

par(mfrow = c(1, 1))
ggplot(data = vead.liik.long, aes(x = kp, y = value)) + geom_point(aes(size = vaatlusi, colour = variable), alpha = 0.69) +
  scale_x_continuous(breaks = kps) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(size = "Takseeralasid pildil",colour = "Liik", x = "Päev aastas", y = "RMSE")  +
  scale_x_continuous(minor_breaks = seq(100,300,10), lim = c(100,300)) + 
  scale_y_continuous(minor_breaks = seq(0.15,0.3,0.05), lim = c(.14,.3))



######### uus ja selgem arvutus ###############

#df.sl - sentinel ja landsat
#df.sent - sentinel
#df.lnd - landsat

kp1 = df.lnd[,1:5]
kp1[,2:5] = (kp1[,2:5]*5488 + 0.5)/5489 #ainult kui beta-agregatsioon
knn.agre = kp1 %>% group_by(aproovitykk_id) %>% summarise_all(funs(bets_fun))
knn.agre[,2:5] = knn.agre[,2:5] / rowSums(knn.agre[,2:5])
dp = merge(knn.agre, taks.info, by = "aproovitykk_id", all.x = T)
sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4)  

#Sentinel: mean 0.1872116, epakernel: 0.1796546, beta:  0.1811502; beta2sd: 0.1816136
#Landsat: mean 0.1895437, epakernel: 0.1890741, beta: 0.1921513; beta2sd: 0.1953428
#Sentinel+Landsat: mean: 0.1850622; epakernel 0.1783341, beta: 0.1806386, beta2sd: 0.1791704




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


#beta ja mean koos

dev.off()
par(mfrow = c(2,2))
plot(dp[,11],dp[,2], xlab = "Mänd", ylab = "Hinnang", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.2), pch = 16)
points(dp1[,11],dp1[,2], xlab = "Mänd", ylab = "Hinnang", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 1, green = 0, blue = 0, alpha = 0.2), pch = 16)
abline(lm(dp[,2] ~ dp[,11]));abline(lm(dp1[,2] ~ dp1[,11]), col = "red", type = "2")
plot(dp[,12],dp[,3], xlab = "Kuusk", ylab = "Hinnang", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.2), pch = 16)
points(dp1[,12],dp1[,3], xlab = "Mänd", ylab = "Hinnang", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 1, green = 0, blue = 0, alpha = 0.2), pch = 16)
abline(lm(dp[,3] ~ dp[,12]));abline(lm(dp1[,3] ~ dp1[,12]), col = "red")
plot(dp[,13],dp[,4], xlab = "Kask", ylab = "Hinnang", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.2), pch = 16)
points(dp1[,13],dp1[,4], xlab = "Mänd", ylab = "Hinnang", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 1, green = 0, blue = 0, alpha = 0.2), pch = 16)
abline(lm(dp[,4] ~ dp[,13]));abline(lm(dp1[,4] ~ dp1[,13]), col = "red")
plot(dp[,14],dp[,5], xlab = "Muu", ylab = "Hinnang", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.2), pch = 16)
points(dp1[,14],dp1[,5], xlab = "Mänd", ylab = "Hinnang", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 1, green = 0, blue = 0, alpha = 0.2), pch = 16)
abline(lm(dp[,5] ~ dp[,14]));abline(lm(dp1[,5] ~ dp1[,14]), col = "red")

#piltie välja jätmine:

kps0 = kpsatel
kps0 = kps0[-5]

pilt.epa = c()
for(i in 1:length(kps0)){
  kp.out = kps0[i]
  df = df.sl[df.sl$kp.satel %in% kps0,]
  df = df[df$kp.satel != kp.out,]
  #df[,2:5] = (df[,2:5]*(dim(df)[1]-1) + 0.5)/dim(df)[1]
  KNN_PBP = df[,1:5] %>% group_by(aproovitykk_id) %>% summarise_all(funs(epa.kernel))
  KNN_PBP[,2:5] = KNN_PBP[,2:5] / rowSums(KNN_PBP[,2:5])
  dp = merge(KNN_PBP, taks.info, by = "aproovitykk_id", all.x = T)
  viga = sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4)
  print(viga)
  pilt.epa[i] = viga
}
par(mfrow=(c(1,1)))
plot(pilt.epa-0.1753999);abline(h = 0)
which.min(pilt.epa);min(pilt.epa)

#epa: 10, 5, 19, 15, 5 
#beta: 17, 10, 12, 21, 6, 12, 15, 15, 11, 7, 5, 10, 5, 10
#beta 2sd: 24, 10, 16, 6, 18, 12, 11, 7

#millised kp-d siis beta korral välja läksid
beta.out=c(17, 10, 12, 21, 6, 12, 15, 15, 11, 7, 5, 10, 5, 10)
kps.out.b = c()
kps0 = kpsatel
for(i in 1:length(beta.out)){
  out = beta.out[i]
  kps.out.b[i] = kps0[out];kps0 = kps0[-out]
}
vead$out = "Jah"
vead[vead$kpsatel %in% kps.out.b,]$out = "Ei"

epa.out = c(10, 5, 19, 15, 5) 
kps.out.e = c()
for(i in 1:length(epa.out)){
  out = epa.out[i]
  kps.out.e[i] = kps0[out];kps0 = kps0[-out]
}
vead$out1 = "Jah"
vead[vead$kpsatel %in% kps.out.e,]$out1 = "Ei"

###################################

#sama asi funktsioonina

kps0 = kpsatel
min0 = 69; min = 0.1850622 #; min = 0
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
    #df[,2:5] = (df[,2:5]*(dim(df)[1]-1) + 0.5)/dim(df)[1]
    KNN_PBP = df[,1:5] %>% group_by(aproovitykk_id) %>% summarise_all(funs(mean))
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
min(out.rmse); length(out)






KNN_PBP = df.sl[,1:5] %>% group_by(aproovitykk_id) %>% summarise_all(funs(mean))
KNN_PBP[,2:5] = KNN_PBP[,2:5] / rowSums(KNN_PBP[,2:5])
dp = merge(KNN_PBP, taks.info, by = "aproovitykk_id", all.x = T)
rmse = sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4)






#######################















require(ggplot2)
dev.off()
par(mfrow = c(1, 2))
p1 = ggplot(data = vead, aes(x = kp, y = vead)) + geom_point(aes(size = vaatlusi, colour = out, shape = out)) +
  scale_x_continuous(breaks = kps) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(size = "Takseeralasid pildil", x = "Päev aastas", y = "RMSE", colour = "Parandab hinnangut", shape = "Parandab hinnangut") +
  scale_x_continuous(minor_breaks = seq(100,300,10), lim = c(105,285)) + 
  scale_y_continuous(minor_breaks = seq(0.15,0.3,0.05), lim = c(.14,.3)) + ggtitle("Beta") + theme(plot.title = element_text(hjust = 0.5))

p2 = ggplot(data = vead, aes(x = kp, y = vead)) + geom_point(aes(size = vaatlusi, colour = out1, shape = out1)) +
  scale_x_continuous(breaks = kps) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(size = "Takseeralasid pildil", x = "Päev aastas", y = "RMSE", colour = "Parandab hinnangut", shape = "Parandab hinnangut") +
  scale_x_continuous(minor_breaks = seq(100,300,10), lim = c(105,285)) + 
  scale_y_continuous(minor_breaks = seq(0.15,0.3,0.05), lim = c(.14,.3)) + ggtitle("Epanechnikov")+ theme(plot.title = element_text(hjust = 0.5))
par(mfrow = c(1, 2))
grid.arrange(p1,p2)


######### vaatame veadNpilt sample 100 #########

ss = 500

veadNpilt.epa.sample100 = c()
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
    veadNpilt.epa.sample100[i] = mean(vead)
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
  veadNpilt.epa.sample100[i] = mean(vead)
  }
  print(veadNpilt.epa.sample100)
}

save(veadNpilt.epa.sample100, file = "KNN_veadNpilt_epa_sample500.RData")


veadNpilt.beta.sample100 = c()
for(i in 1:length(kpsatels)){
  print(i);print(Sys.time())
  vead = c()
  if(choose(length(kpsatels),i) <= ss){
    for(j in 1:choose(length(kpsatels),i)){
      kpsatel = list.kpsatel[[i]][,j]
      kp_data = df.sl[df.sl$kp.satel %in% kpsatel,]
      kp_data[,2:5] = (kp_data[,2:5]*(dim(kp_data)[1]-1) + 0.5)/dim(kp_data)[1] #seda on aint beta jaoks vaja
      KNN_PBP = kp_data[,1:5] %>% group_by(aproovitykk_id) %>% summarise_all(funs(bets_fun))
      KNN_PBP[,2:5] = KNN_PBP[2:5] / rowSums(KNN_PBP[,2:5])
      dp = merge(KNN_PBP, taks.info, by = "aproovitykk_id", all.x = T)
      viga = sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4)
      vead[j] = viga
    }
    veadNpilt.beta.sample100[i] = mean(vead)
  }
  else{
    sample = list.kpsatel[[i]][,sample(ncol(list.kpsatel[[i]]),size=ss,replace=T)]
    for(j in 1:ss){
      kpsatel = sample[,j]
      kp_data = df.sl[df.sl$kp.satel %in% kpsatel,]
      kp_data[,2:5] = (kp_data[,2:5]*(dim(kp_data)[1]-1) + 0.5)/dim(kp_data)[1] #seda on aint beta jaoks vaja
      KNN_PBP = kp_data[,1:5] %>% group_by(aproovitykk_id) %>% summarise_all(funs(bets_fun))
      KNN_PBP[,2:5] = KNN_PBP[2:5] / rowSums(KNN_PBP[,2:5])
      dp = merge(KNN_PBP, taks.info, by = "aproovitykk_id", all.x = T)
      viga = sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4)
      vead[j] = viga
    }
    veadNpilt.beta.sample100[i] = mean(vead)
  }
  print(veadNpilt.beta.sample100)
}

save(veadNpilt.beta.sample100, file = "KNN_veadNpilt_beta_sample500.RData")

veadNpilt.mean.sample100 = c()
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
    veadNpilt.mean.sample100[i] = mean(vead)
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
    veadNpilt.mean.sample100[i] = mean(vead)
  }
  print(veadNpilt.mean.sample100)
}

save(veadNpilt.mean.sample100, file = "KNN_veadNpilt_mean_sample500.RData")


#KNN PBP, siin veel 500 samplega puudu:

par(mfrow=c(1,3))
plot(unlist(veadNpilt.mean.sample100), type = "o", ylab = "RMSE: mean", xlab = "Hinnangus kasutatud pilte", ylim = c(0.175,0.235))
plot(unlist(veadNpilt.beta.sample100), type = "o", ylab = "RMSE: beta", xlab = "Hinnangus kasutatud pilte", ylim = c(0.175,0.235))
plot(unlist(veadNpilt.epa.sample100), type = "o", ylab = "RMSE: Epanechnikov kernel", xlab = "Hinnangus kasutatud pilte", ylim = c(0.175,0.235))









