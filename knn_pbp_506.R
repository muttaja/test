#knn_pbp 506

fun_opti = function(w,k,vars,data, sidxx, kernel){
  dex = data[,c("aproovitykk_id",vars)]
  dex = dex[dex$aproovitykk_id %in% sidxx,]
  #print(dim(dex)); print(length(w))
  dex[,-1] = t((t(as.matrix(dex[,-1])))*w)
  dex$cl = "cl"
  #data_puud = taks.info[taks.info$aproovitykk_id %in% sidxx,]
  #data_puud = data_puud[,(4*tvmaht-1):(4*tvmaht+2)] #vastavalt kas proportsioonid või tüvemahud
  
  H_puu = fun_agre_kernel(dex, data_puud, k = k, sid = sidxx, kernel)
  #print(dim(H_puu))
  rsdls = H_puu[,1:4] - puud_true
  rss = sqrt((sum(rsdls**2))/(length(sidxx)*4))
  rss
}

fun_agre_kernel = function(data, data_puud, k, sid, kernel = epa)
{
  sid00 = data$aproovitykk_id
  kk = k+1 #"epa" tahab ühe võrra pikemat vektorit?
  dists = knn.cv(train = data[,2:(dim(data)[2]-1)], cl = data$cl, k = kk)
  dist1 = attr(dists,"nn.dist")
  dist1[is.na(dist1)] = 0
  dist1 = dist1 + 1e-5 #et kaugused ei saaks 0 olla. on see vajalik?
  index1 = attr(dists,"nn.index")
  props = apply(dist1, 1, kernel)
  props = t(props)
  indxprops = cbind(index1, props)
  tbr = t(apply(indxprops, 1, agre, data_puud))
  tbr = tbr / rowSums(tbr)
  tbr = data.frame(tbr); tbr$aproovitykk_id = sid00
  tbr
}

epa = function(vec){
  props = 3/4*(1-(vec / vec[length(vec)])**2)
  props1 = props/sum(props)
  #print(sum(props))
  if(sum(props) == 0) {
    props1[1] = 1; props1[2:length(vec)] = 0
  }
  props1
}


agre <- function(arg,data_puud){
  kk = length(arg) / 2
  indx = arg[1:kk]; props = arg[(kk+1):(2*kk)]
  colsums = colSums(data_puud[indx,]*props)
  colsums
}

fun_bestvars = function(k1, k2, sidxx, kernel, varsw = vars, ws = rep(1, length(vars))){ #
  list_return = vector("list", length = 2)
  var_return = vector("list", length = 1+k2-k1)
  rss_return = vector("list", length = 1+k2-k1)
  for(k in k1:k2){
    #data[,varsw] = t((t(as.matrix(data[,varsw])))*ws)
    print(k);print(Sys.time())
    vars0 = vars
    vars1 = c()
    mx = 6969
    vahe = 696
    #print(vahe)
    while(vahe > 0){
      mx0 = mx
      vars00 = c()
      rss = c()
      for(j in 1:length(vars0)){
        vars00 = c(vars1,vars0[j])
        dex = data[,c("aproovitykk_id",vars00)]
        dex = dex[dex$aproovitykk_id %in% sidxx,]
        dex$cl = "cl"
        H_puu = fun_agre_kernel(dex, data_puud, k = k, sid = sidxx, kernel = kernel)
        rsdls = H_puu[,1:4] - puud_true
        rss[j] = sqrt((sum(rsdls**2))/(length(sidxx)*4))
        #print(rss)
      }
      #rss[is.na(rss)] = 1000
      mx0 = min(rss)
      varmin = vars0[which.min(rss)]
      vars0 = vars0[!(vars0 %in% varmin)]
      vars1 = c(vars1,varmin)
      vahe = mx-mx0
      #print(vahe)
      mx = mx0
      #print(vars1)
    }
    var_return[[1+k-k1]] =  vars1
    rss_return[[1+k-k1]] =  mx
    print(mx)
  }
  list_return[[1]] = var_return;list_return[[2]] = rss_return
  list_return
}

# PROGNOOSIMINE #

#praegu kõik prognoosid koos
require(FNN)
setwd("A:/MAKA/TEST/test")
load("taks_info.RData")
load("sid506.RData")
d506 = read.csv("d506.csv")

d506.100 = merge(d506,koos[,c("aproovitykk_id", "arv_maht_es")], by = "aproovitykk_id")
d506.100 = d506.100[d506.100$arv_maht_es > 100,] #455
table(d506.100$muld)
table(koos[koos$aproovitykk_id %in% d506.100$aproovitykk_id,]$muld)

#muld!?
muld1 = data.frame(muld = koos[koos$aproovitykk_id %in% d506.100$aproovitykk_id,]$muld, aproovitykk_id = d506.100$aproovitykk_id)
table(muld1$muld) #alla 15 kokku

muld1[muld1$muld %in% c(10,16,31,63,73,NA), "muld"] = 999
#muld 1-0 tüüpi andmestikuks
muld2 = dcast(muld1,aproovitykk_id~muld,fun.aggregate = function(x){as.integer(length(x) > 0)})

dkm = merge(d506.100, muld2, by = "aproovitykk_id")
nkm = names(dkm);
nkm1 = nkm[-c(2:3,40:50)]

d506.100 = dkm[,nkm1]
#write.csv(d506.100, file = "d506_100.csv")


data = d506.100
vars = names(data)[c(2:37,46:59)]
sidxx = d506.100$aproovitykk_id
tvmaht = 2 #ehk siis proportsioonid!
data_puud = taks.info[taks.info$aproovitykk_id %in% sidxx,]
puud_true =  data_puud[,7:10]
data_puud = data_puud[,(4*tvmaht-1):(4*tvmaht+2)]





bestvars.tv100 = fun_bestvars(1,25,sidxx, kernel = epa)

#save(bestvars.k, file = "bestvars_epa.RData")
#save(bestvars.k, file = "bestvars_epa25.RData")
#save(bestvars.k, file = "bestvars506_epa25.RData")
#save(bestvars.tv1, file = "bestvars506tv_epa25.RData")
#load(file = "bestvars_epa25.RData")

vark = bestvars.tv100[[1]]
rss = unlist(bestvars.tv100[[2]])
plot(rss,type = "o")

#bests = c(match(rss[order(rss)][1],rss),match(rss[order(rss)][2],rss),match(rss[order(rss)][3],rss),match(rss[order(rss)][4],rss))
#op.test = optim(par = rep(1, length(vark[[bests[1]]])), fn = fun_opti, k = bests[1], vars = vark[[bests[1]]], data = data, sidxx = sidxx,method = "BFGS", kernel = epa)
#põhimõtteliselt annavad tüvemahud sama tulemuse: 0.17295 pärast opt, k = 14

# op1 = optim(par = rep(1, length(vark[[bests[1]]])), fn = fun_opti, k = bests[1], vars = vark[[bests[1]]], data = data, sidxx = sidxx,method = "BFGS", kernel = epa)
# op2 = optim(par = rep(1, length(vark[[bests[2]]])), fn = fun_opti, k = bests[2], vars = vark[[bests[2]]], data = data, sidxx = sidxx,method = "BFGS", kernel = epa)
# op3 = optim(par = rep(1, length(vark[[bests[3]]])), fn = fun_opti, k = bests[3], vars = vark[[bests[3]]], data = data, sidxx = sidxx,method = "BFGS", kernel = epa)
# op4 = optim(par = rep(1, length(vark[[bests[4]]])), fn = fun_opti, k = bests[4], vars = vark[[bests[4]]], data = data, sidxx = sidxx,method = "BFGS", kernel = epa)
# w.opt = vector("list", length=4);
# rss.opt = c()
# w.opt[[1]] = op1$par;   w.opt[[2]] = op2$par;   w.opt[[3]] = op3$par;   w.opt[[4]] = op4$par;
# rss.opt[1] = op1$value; rss.opt[2] = op2$value; rss.opt[3] = op3$value; rss.opt[4] = op4$value
# brss = match(rss.opt[order(rss.opt)][1],rss.opt) #kuidagi tuleb parim k salvestada
# best_k = bests[brss]
# best_vars = vark[[best_k]]
# wgt = w.opt[[brss]]
# print(best_k);print(best_vars);print(wgt)



#laseme ilusa ploti jaoks kõik loopis läbi:
w.opt = vector("list", length=25);rss.opt = c()
for(k in 1:25){
  print(Sys.time())
  opt.k = optim(par = rep(1, length(vark[[k]])), fn = fun_opti, k = k, vars = vark[[k]], data = data, sidxx = sidxx,method = "BFGS", kernel = epa)
  w.opt[[k]] = opt.k$par
  rss.opt[k] = opt.k$value
}

plot(rss,type = "o", ylim = c(0.16,0.23))
points(x=c(1:25), y=rss.opt, lty=1, col = "red", pch = 19)
min(rss.opt)

best_vars = vark[[5]]
wgt = w.opt[[5]]


dex = data[,c("aproovitykk_id",best_vars)]
dex = dex[dex$aproovitykk_id %in% sidxx,]
dex[,-1] = t((t(as.matrix(dex[,-1])))*wgt)
dex$cl = "cl"
knn455.prop.epa = fun_agre_kernel(dex, data_puud, k = best_k, sid = sidxx, kernel = epa)

par(mfrow = c(1,1))
plot(rss, type = "o", xlab = "Naabreid", ylab = "RMSE", ylim = c(0.16,0.23)) #RMSE: root-mean-squared-error
points(x=c(1:25), y=rss.opt, lty=1, col = "red", pch = 19)
lines(x=c(1:25), y=rss.opt, lty=3, col = "red")
# > rss[bests]
# [1] 0.1697311 0.1700308 0.1700982 0.1701014
# > rss.opt
# [1] 0.1667020 0.1666487 0.1671875 0.1671335

dats = merge(knn455.prop.epa,taks.info, by = "aproovitykk_id", all.x = T)
dev.off()
par(mfrow = c(2,2))
plot(dats[,11],dats[,2], xlab = "MÄND", ylab = "Hinnang KNN: K=5", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dats[,12],dats[,3], xlab = "KUUSK", ylab = "Hinnang KNN: K=5", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dats[,13],dats[,4], xlab = "KASK", ylab = "Hinnang KNN: K=5", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dats[,14],dats[,5], xlab = "MUU", ylab = "Hinnang KNN: K=5", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))


#uuele ringile juba leitud kaaludega:
bestvars.455.5 = fun_bestvars(5,5,sidxx, kernel = epa, varsw = best_vars, ws = wgt)
op.455.5 = optim(par = rep(1, length(unlist(bestvars.455.5[[1]]))), fn = fun_opti, k = 20, vars = unlist(bestvars.455.5[[1]]), data = data, sidxx = sidxx,method = "BFGS", kernel = epa)
op.455.5 #0.1649958, ei anna midagi juurde, ikka halvemaks!?

#tüvemahud?
tvmaht = 1 #ehk tüvemahud!
data_puud = taks.info[taks.info$aproovitykk_id %in% sidxx,]
puud_true =  data_puud[,7:10]
data_puud = data_puud[,(4*tvmaht-1):(4*tvmaht+2)]
##########################################################
bestvars.tv.tv100 = fun_bestvars(1,25,sidxx, kernel = epa)
##########################################################


vark = bestvars.tv.tv100[[1]]
rss = unlist(bestvars.tv.tv100[[2]])

rss.opt.tm = c()
w.opt.tm = vector("list", length=25)
for(k in 1:25){
  print(Sys.time())
  opt.k = optim(par = rep(1, length(vark[[k]])), fn = fun_opti, k = k, vars = vark[[k]], data = data, sidxx = sidxx,method = "BFGS", kernel = epa)
  w.opt.tm[[k]] = opt.k$par
  rss.opt.tm[k] = opt.k$value
}

par(mfrow = c(1,1))
plot(rss, type = "o", xlab = "Naabreid", ylab = "RMSE", ylim = c(0.16,0.23)) #RMSE: root-mean-squared-error
points(x=c(1:25), y=rss.opt.tm, lty=1, col = "red", pch = 19)
lines(x=c(1:25), y=rss.opt.tm, lty=3, col = "red")


min(rss.opt.tm)





############ kuupäevade kaupa ###########
tvmaht = 2 #proportsioonid
data0 = read.csv("sentinel455.csv")
names(data0)

data0 = data0[,c(2:13,26:38)]
vars = names(data0)[c(4:25)]
kps = unique(data0$kp)

#result.kp.tv100.v2 = vector("list", length = 15)
bestk = c()

for(i in 1:15){
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
  result.kp.tv100.v2[[i]] = fun_agre_kernel(dex, data_puud, k = best_k, sid = sid0, kernel = epa)
  bestk[i] = best_k
}

#save(result.kp.tv100.v2, file ="KNN_pbp_tv100_v2.RData") #see viimane
#varem ei olnud päris sama andmesik, mis CART/RF korral

#save(result.kp.bfgs, file = "KNN_pbp_bfgs.RData")

#19 18 20 20 20 18 17 11 12 13 16  7 19 20 18

load(file ="KNN_pbp_tv100_v2.RData")


#männid
dev.off()
par(mfrow = c(4, 4))
for (i in 1:15) {
  kp = kps[i]
  kp_data = result.kp.tv100[[i]]
  kp_data = merge(kp_data, taks.info, all.x = T, by = "aproovitykk_id")
  plot(kp_data[,11],kp_data[,2], main = paste("Mänd ", kp), xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
}

#kuused
dev.off()
par(mfrow = c(4, 4))
for (i in 1:15) {
  kp = kps[i]
  kp_data = result.kp.tv100[[i]]
  kp_data = merge(kp_data, taks.info, all.x = T, by = "aproovitykk_id")
  plot(kp_data[,12],kp_data[,3], main = paste("Kuusk ", kp), xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
}

#kased
dev.off()
par(mfrow = c(4, 4))
for (i in 1:15) {
  kp = kps[i]
  kp_data = result.kp.tv100[[i]]
  kp_data = merge(kp_data, taks.info, all.x = T, by = "aproovitykk_id")
  plot(kp_data[,13],kp_data[,4], main = paste("Kask ", kp), xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
}

#muud
dev.off()
par(mfrow = c(4, 4))
for (i in 1:15) {
  kp = kps[i]
  kp_data = result.kp.tv100[[i]]
  kp_data = merge(kp_data, taks.info, all.x = T, by = "aproovitykk_id")
  plot(kp_data[,14],kp_data[,5], main = paste("Muu ", kp), xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
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
#kps = as.POSIXct(kps, format = "%Y-%m-%d")
vead = c();vaatlusi=c();vead.liik = matrix(NA, nrow = 15, ncol = 4)
for(i in 1:15){
  kp = kps[i]
  kp_data = result.kp.tv100[[i]]
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
  labs(size = "Takseeralasid pildil", x = "Päev aastas", y = "RMSE")
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


#kui võtta nende piltide tuim keskmine:

kp_data = data.frame()
for(i in 1:15){
  kp = kps[i]
  kp_data0 = result.kp.tv100[[i]]; kp_data0$kp = kp
  kp_data0 = merge(kp_data0, taks.info, all.x = T, by = "aproovitykk_id")
  kp_data = rbind(kp_data, kp_data0)
}

KNN_PBP = kp_data[,1:5]
KNN_PBP = KNN_PBP %>% group_by(aproovitykk_id) %>% summarise_all(funs(mean)) #bets_fun(.,method = "mme")


dp = merge(KNN_PBP, taks.info, by = "aproovitykk_id", all.x = T)
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
sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4) # mean 0.1872116, beta mme 0.1830631



#üks variant:

# Dave,
# 
# A common approach to this problem is to fit 2 logistic regression models to predict whether a case is 0 or 1. 
# Then, a beta regression is used for those in the range (0,1).
# B_mine stackex.


##a better lemon squeezer: seal 0 ka 1 probleem beta-jaotuses


data.kp = kp_data;
data.kp1 = data.kp[,2:5]
data.kp1 = (data.kp1*5488 + 0.5)/5489 ###
max(data.kp1);min(data.kp1)

data.kp[,2:5] = data.kp1
kp_data = data.kp

require(EnvStats)

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

bets_fun1 = function(vec, method = "mle"){
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
    #kui kujuparameetrid väikesed, siis pole head tippu
    if((a < 1 | b < 1)){
      max.d = mean(vec)
    }
  }
  max.d
}


KNN_PBP_BETA0 = kp_data[,1:5]
KNN_PBP_BETA = KNN_PBP_BETA0 %>% group_by(aproovitykk_id) %>% summarise_all(funs(bets_fun(.,method = "mle")))
KNN_PBP_BETA1 = KNN_PBP_BETA0 %>% group_by(aproovitykk_id) %>% summarise_all(funs(bets_fun1))


test = round(KNN_PBP_BETA[,2:5],1);test$id = KNN_PBP_BETA$aproovitykk_id
rsm = rowSums(KNN_PBP_BETA[,2:5]);hist(rsm);
min(rsm);which.min(rsm) #bimodaalsusprobleem!
max(rsm);which.max(rsm) #max on ok!
sits = KNN_PBP_BETA$aproovitykk_id[451]
pdid1 = kp_data[kp_data$aproovitykk_id == sits,]; pdid1 = pdid1[,2:5]
pdid1 = kp_data[kp_data$aproovitykk_id == sits,]; pdid1 = pdid1[,2:5]
mean(pdid1[,1]);mean(pdid1[,2]);mean(pdid1[,3]);mean(pdid1[,4])
bets_fun(pdid1[,1]);bets_fun(pdid1[,2]);bets_fun(pdid1[,3]);bets_fun(pdid1[,4])

#siin on bimodaalsusega probleem :S
eb = ebeta(pdid1[,1])
xx = seq(0,1, 0.001)
dev.off()
par(mfrow = c(2,2))
plot(xx,dbeta(xx, shape1 = eb$parameters[1], shape2 = eb$parameters[2]))
eb = ebeta(pdid1[,2])
plot(xx,dbeta(xx, shape1 = eb$parameters[1], shape2 = eb$parameters[2]))
eb = ebeta(pdid1[,3])
plot(xx,dbeta(xx, shape1 = eb$parameters[1], shape2 = eb$parameters[2]))
eb = ebeta(pdid1[,4])
plot(xx,dbeta(xx, shape1 = eb$parameters[1], shape2 = eb$parameters[2]))

#hetkel jääb bimodaalsusprobleem sisse, aga plotime huvi pärast

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
#0.1811502 bets_fun; 0.1850817 betsfun2; 0.1812432 betsfun1; 0.1872116 mean; 0.1830631 beta mme


#########################################################

#teised kaalud peale epa?
tv = c(15,25,65,68,114)
epa(tv)
tric(tv)
coss(tv)

epa = function(vec){
  vec = (vec / vec[length(vec)])
  props = 3/4*(1-vec**2)
  props1 = props/sum(props)
  if(sum(props) == 0){
    props1[1] = 1; props1[2:length(vec)] = 0
  }
  props1
}

tric = function(vec){
  vec = (vec / vec[length(vec)])
  props = 70/81*((1 -(abs(vec))**3)**3)
  props1 = props/sum(props)
  if(sum(props) == 0){props1[1] = 1; props1[2:length(vec)] = 0}
  props1
}

coss = function(vec){
  vec = (vec / vec[length(vec)])
  props = pi/4*cos(pi/2*vec)
  props1 = props/sum(props)
  if(sum(props) == 0){
    props1[1] = 1; props1[2:length(vec)] = 0
  }
  props1
}

tava = function(vec){
  props = 1/vec
  props1 = props/sum(props)
  if(sum(props) == 0){
    props1[1] = 1; props1[2:length(vec)] = 0
  }
  props1
}

############## tricube

load("taks_info.RData")
load("sid601.RData")
d601 = read.csv("d601.csv")
data = d601
vars = names(data)[3:49]

brss.k = c()
tvmaht = 2
data_puud = taks.info[taks.info$aproovitykk_id %in% sidxx,]
puud_true =  data_puud[,7:10]
data_puud = data_puud[,(4*tvmaht-1):(4*tvmaht+2)]

bestvars.k = fun_bestvars(5,20,sidxx,tric) #alustame viiest
vark = bestvars.k[[1]]
rss = unlist(bestvars.k[[2]])
bests = c(match(rss[order(rss)][1],rss),match(rss[order(rss)][2],rss),match(rss[order(rss)][3],rss))
op1 = optim(par = rep(1, length(vark[[bests[1]]])), fn = fun_opti, k = bests[1], vars = vark[[bests[1]]], data = data, sidxx = sidxx,method = "BFGS", kernel = tric)
op2 = optim(par = rep(1, length(vark[[bests[2]]])), fn = fun_opti, k = bests[2], vars = vark[[bests[2]]], data = data, sidxx = sidxx,method = "BFGS", kernel = tric)
op3 = optim(par = rep(1, length(vark[[bests[3]]])), fn = fun_opti, k = bests[3], vars = vark[[bests[3]]], data = data, sidxx = sidxx,method = "BFGS", kernel = tric)
w.opt = vector("list", length=3);
rss.opt = c()
w.opt[[1]] = op1$par;   w.opt[[2]] = op2$par;   w.opt[[3]] = op3$par;
rss.opt[1] = op1$value; rss.opt[2] = op2$value; rss.opt[3] = op3$value;
brss = match(rss.opt[order(rss.opt)][1],rss.opt) #kuidagi tuleb parim k salvestada
brss.k[i] = rss.opt[order(rss.opt)][1]
best_k = bests[brss]
best_vars = vark[[best_k]]
wgt = w.opt[[brss]]

print(best_k);print(best_vars);print(wgt)

dex = data[,c("aproovitykk_id",best_vars)]
dex = dex[dex$aproovitykk_id %in% sidxx,]
dex[,-1] = t((t(as.matrix(dex[,-1])))*wgt)
dex$cl = "cl"
print(best_k)
print(brss.k)
kp.result.tric = fun_agre_kernel(dex, data_puud, k = best_k, sid = sidxx, kernel = tric)

#k = 20
# > best_vars
# [1] "B08_vahe_kevad" "B04_sygis"      "B12_sygis"      "B05_kevad1"     "B11_kevad2"     "B06_vahe_kevad" "B08_kevad2"     "B03_kevad1"    
# [9] "X21"            "X64"
#wgt: [1] 0.8743273 1.0369697 0.8025519 1.1148504 1.1878113 1.3190514 0.9370502 0.6730311 0.9572639 1.0307777

dp = kp.result.tric #tüvemahtude põhjal
dev.off()
par(mfrow = c(2,2))
plot(puud_true[,1],dp[,1])
plot(puud_true[,2],dp[,2])
plot(puud_true[,3],dp[,3])
plot(puud_true[,4],dp[,4])

sqrt(mean((puud_true[,1]-dp[,1])**2))
sqrt(mean((puud_true[,2]-dp[,2])**2))
sqrt(mean((puud_true[,3]-dp[,3])**2))
sqrt(mean((puud_true[,4]-dp[,4])**2))

#tricube kehvem kui epanechnikov, proovime epa 15...25

brss.k = c()
tvmaht = 2
data_puud = taks.info[taks.info$aproovitykk_id %in% sidxx,]
puud_true =  data_puud[,7:10]
data_puud = data_puud[,(4*tvmaht-1):(4*tvmaht+2)]

bestvars.k = fun_bestvars(15,25,sidxx,epa) #alustame viiest
vark = bestvars.k[[1]]
rss = unlist(bestvars.k[[2]])
bests = c(match(rss[order(rss)][1],rss),match(rss[order(rss)][2],rss),match(rss[order(rss)][3],rss))
op1 = optim(par = rep(1, length(vark[[bests[1]]])), fn = fun_opti, k = bests[1], vars = vark[[bests[1]]], data = data, sidxx = sidxx,method = "BFGS", kernel = epa)
op2 = optim(par = rep(1, length(vark[[bests[2]]])), fn = fun_opti, k = bests[2], vars = vark[[bests[2]]], data = data, sidxx = sidxx,method = "BFGS", kernel = epa)
op3 = optim(par = rep(1, length(vark[[bests[3]]])), fn = fun_opti, k = bests[3], vars = vark[[bests[3]]], data = data, sidxx = sidxx,method = "BFGS", kernel = epa)
w.opt = vector("list", length=3);
rss.opt = c()
w.opt[[1]] = op1$par;   w.opt[[2]] = op2$par;   w.opt[[3]] = op3$par;
rss.opt[1] = op1$value; rss.opt[2] = op2$value; rss.opt[3] = op3$value;
brss = match(rss.opt[order(rss.opt)][1],rss.opt) #kuidagi tuleb parim k salvestada
brss.k[i] = rss.opt[order(rss.opt)][1]
best_k = bests[brss]
best_vars = vark[[best_k]]
wgt = w.opt[[brss]]

print(best_k);print(best_vars);print(wgt)

dex = data[,c("aproovitykk_id",best_vars)]
dex = dex[dex$aproovitykk_id %in% sidxx,]
dex[,-1] = t((t(as.matrix(dex[,-1])))*wgt)
dex$cl = "cl"
print(best_k)
print(brss.k)
kp.result.epa = fun_agre_kernel(dex, data_puud, k = best_k, sid = sidxx, kernel = epa)

dp = kp.result.epa
sqrt(mean((puud_true[,1]-dp[,1])**2))
sqrt(mean((puud_true[,2]-dp[,2])**2))
sqrt(mean((puud_true[,3]-dp[,3])**2))
sqrt(mean((puud_true[,4]-dp[,4])**2))

best_vars


#lasin algse öösel uuesti läbi; kas sai vähem tunnuseid? proovi neid tunnuseid CART-is;
#võta kõige hullemad vigased vaatlused välja. nt 111181 ja 111185 kohta pole ühtki sentineli pilti...
#ja hakka kaarti koostama







####################################################

#kuidas keskmine paraneb sõltuvalt piltide arvust:
# iga id keskmine ühe, kahe, ... 15 pildi korral; omakorda iga juhu keskmine(ruut?viga)
# nii saaks laheda gifi teha, kuidas prognoosid jooksevad diagonaalile kokku :) ... kui jooksevad


#või 1..15-sed komplektid piltidest ja arvutatada vead?
#realiseerime esmalt teise variandi:

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

par(mfrow=c(1,1))
plot(unlist(veadNpilt.1), type = "o", ylab = "RMSE", xlab = "Hinnangus kasutatud pilte") #tundub loogiline

mdl <- smooth.spline(x=c(1:15), y=veadNpilt.1, df =6)
plot(x=c(1:25),y=predict(mdl,newdata = data.frame(x=c(1:25))))



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
    opt = optimize(interval = c(0,1), dbeta, shape1 = a, shape2 = b, maximum = T)
    max.d = opt$`maximum`
    if((a < 1 & b < 1)){
      max.d = mean(vec)
    }
  }
  max.d
}

#kui äärmused välja võtta
bets_fun2sd = function(vec, method = "mle"){
  if(any(is.na(vec))){
    max.d = 999999999969
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
    KNN_PBP = kp_data[,1:5] %>% group_by(aproovitykk_id) %>% summarise_all(funs(dens1))
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
    #kp_data[,2:5] = (kp_data[,2:5]*(dim(kp_data)[1]-1) + 0.5)/dim(kp_data)[1] #seda on aint beta jaoks vaja
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


#11.04 öösel jäin siia
#NB epakerneli puhul tuleks võtta "keskmine" trepi kõrgemaist astmest!











par(mfrow=c(1,3))
plot(unlist(veadNpilt.1), type = "o")
plot(unlist(veadNpilt.beta), type = "o")
plot(unlist(veadNpilt.epakernel), type = "o")

par(mfrow=c(1,3))
plot(unlist(veadNpilt.1), type = "o", ylab = "RMSE: mean", xlab = "Hinnangus kasutatud pilte", ylim = c(0.18,0.235))
plot(unlist(veadNpilt.beta), type = "o", ylab = "RMSE: beta", xlab = "Hinnangus kasutatud pilte", ylim = c(0.18,0.235))
plot(unlist(veadNpilt.epakernel), type = "o", ylab = "RMSE: Epanechnikov kernel", xlab = "Hinnangus kasutatud pilte", ylim = c(0.18,0.235))


#[1] 0.2230905 0.2135501 0.2068331 0.2028079 0.2003610 0.1987671 0.1976517 0.1968266 0.1961908 0.1956853 0.1952738
#[12] 0.1949321 0.1946438 0.1943972 0.1941840
veadNpilt.beta
#[1] 0.2230905 0.2413113 0.2299267 0.2148146 0.2048946 0.1995224 0.1965897 0.1947851 0.1935754 0.1927625 0.1921612
#[12] 0.1915896 0.1910611 0.1907717 0.1907083
veadNpilt.epa
# [1] 0.2230905 0.2204670 0.2136803 0.2066621 0.2013663 0.1982209 0.1964907 0.1953290 0.1943813 0.1936109 0.1930015
# [12] 0.1924686 0.1919404 0.1915716 0.1918551

par(mfrow=c(1,1))
plot(c(unlist(veadNpilt.1)-unlist(veadNpilt.beta))[3:15], type = "o")




#esimene meetod:
idx = unique(sidorov)
results.list.id = vector("list", length=length(idx))
results.matrix = matrix(data = NA, nrow = 599, ncol = 5)

for(id in idx){
  
}


#tegelt pole sel vist pointi, kuna antud juhul sisaldaks igas vea arvutuses juba kõik pildid

#pigem võtame need samad kuupäevad ja vaatame, kuidas prognoos "areneb", arvutame "arengu" põhjal kõvera
#ja vaate, millised kuupäevad jäävad kasvukõvera alla

vead.kasv.beta2 = c()
idx.df = data.frame(aproovitykkid = idx)
for(i in 1:length(kps)){
  kp_data = data.frame()
  for(j in 1:i){
    kp = kps[j]
    kp_data0 = result.kp.bfgs[[j]]; kp_data0$kp = kp
    kp_data0 = merge(kp_data0, taks.info, all.x = T, by = "aproovitykk_id")
    kp_data = rbind(kp_data, kp_data0)
  }
  kp_data[,2:5] = (kp_data[,2:5]*(dim(kp_data)[1]-1) + 0.5)/dim(kp_data)[1]
  KNN_PBP = kp_data[,1:5] %>% group_by(aproovitykk_id) %>% summarise_all(funs(bets_fun(.,method = "mme"))) #bets_fun(.,method = "mme")
  KNN_PBP[,2:5] = KNN_PBP[,2:5] / rowSums(KNN_PBP[,2:5]) #SEE RIDA ON KÕIKJAL PUUDU OLNUD!
  dp = merge(KNN_PBP, taks.info, by = "aproovitykk_id", all.x = T)
  #idx.df = merge(idx.df, KNN_PBP, by = "aproovitykk_id", all.x = T)
  viga = sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4)
  print(viga)
  vead.kasv.beta2[i] = viga
}

par(mfrow=c(2,2))
plot(vead.kasv, type = "o")
plot(vead.kasv.beta, type = "o") #kas see on iga kord natuke erine!?
plot(vead.kasv.beta2sd.mme, type = "o")
plot(vead.kasv.epa, type = "o")

#min(vead.kasv) #0.1926236
#min(vead.kasv.beta1) #0.1893145 #see on mme-ga
#min(vead.kasv.beta2sd) #0.1923166
#min(vead.kasv.beta2sd.mme) #0.1897706
#min(vead.kasv.epa) #0.1896615

#mida ebeta 0-iga teeb?
vec = c(rep(0.15,11), 0.0001,0.69)
eb = ebeta(vec, method = "mle")
eb = beta.mle(vec)
a = eb$parameters[1]; b = eb$parameters[2]
a = eb$param[1]; b = eb$param[2]
#opt = optim(par = 0.5, lower = 0, upper = 1, dbeta, shape1 = a, shape2 = b, method = "Brent", control=list(fnscale=-1))
#max.d = opt$`par`
opt = optimize(interval = c(0,1), dbeta, shape1 = a, shape2 = b, maximum = T)
opt$`maximum`
mean(vec)

xx = seq(0,1, 0.001)
dev.off()
par(mfrow = c(1,1))
plot(xx,dbeta(xx, shape1 = a, shape2 = b))


#kui 0-i asemel 0.0001, siis prognoos tuleb pmst 0.... kui isegi 10 ülejäänud väärtust kõik 0.15
#seega on vaja mitmetasemelist mudelit!? või "erindid" välja võtta?

#millised pildid kõige enam juurde annavad?
#

pilt.beta = c()
for(i in 1:length(kps)){
  seqs = c(1:length(kps))[-i]
  kp_data = data.frame()
  for(j in seqs){
    kp = kps[j]
    kp_data0 = result.kp.bfgs[[j]]; kp_data0$kp = kp
    kp_data0 = merge(kp_data0, taks.info, all.x = T, by = "aproovitykk_id")
    kp_data = rbind(kp_data, kp_data0)
  }
  kp_data[,2:5] = (kp_data[,2:5]*(dim(kp_data)[1]-1) + 0.5)/dim(kp_data)[1]
  KNN_PBP = kp_data[,1:5] %>% group_by(aproovitykk_id) %>% summarise_all(funs(bets_fun(.,method = "mme")))
  KNN_PBP[,2:5] = KNN_PBP[,2:5] / rowSums(KNN_PBP[,2:5])
  dp = merge(KNN_PBP, taks.info, by = "aproovitykk_id", all.x = T)
  #idx.df = merge(idx.df, KNN_PBP, by = "aproovitykk_id", all.x = T)
  viga = sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4)
  print(viga)
  pilt.beta[i] = viga
}




pilt.beta - 0.1935926 

plot(pilt.beta - 0.1935926 )
min(pilt.beta)

kps0 = kps
kp_data = data.frame()
for(i in 1:length(kps0)){
  kp = kps0[i]
  kp_data0 = result.kp.bfgs[[i]]; kp_data0$kp = kp
  kp_data0 = merge(kp_data0, taks.info, all.x = T, by = "aproovitykk_id")
  kp_data = rbind(kp_data, kp_data0)
}
data.kp = kp_data;
data.kp1 = data.kp[,2:5]
data.kp1 = (data.kp1*7191 + 0.5)/7192 ###7192 mitte 599!
data.kp[,2:5] = data.kp1
kp_data = data.kp
KNN_PBP_BETA0 = kp_data[,1:5]
KNN_PBP_BETA = KNN_PBP_BETA0 %>% group_by(aproovitykk_id) %>% summarise_all(funs(mean))
kb1 = KNN_PBP_BETA
rsm = rowSums(kb1[,2:5])
kb1[,2:5] = kb1[2:5] / rsm
dp = merge(kb1, taks.info, by = "aproovitykk_id", all.x = T)
sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4)

pilt.beta1 = c()
for(i in 1:length(kps0)){
  seqs = c(1:length(kps0))[-i]
  kp_data = data.frame()
  for(j in seqs){
    kp = kps0[j]
    kp_data0 = result.kp.bfgs[[j]]; kp_data0$kp = kp
    kp_data0 = merge(kp_data0, taks.info, all.x = T, by = "aproovitykk_id")
    kp_data = rbind(kp_data, kp_data0)
  }
  kp_data[,2:5] = (kp_data[,2:5]*(dim(kp_data)[1]-1) + 0.5)/dim(kp_data)[1]
  KNN_PBP = kp_data[,1:5] %>% group_by(aproovitykk_id) %>% summarise_all(funs(bets_fun(.,method = "mme")))
  KNN_PBP[,2:5] = KNN_PBP[,2:5] / rowSums(KNN_PBP[,2:5])
  dp = merge(KNN_PBP, taks.info, by = "aproovitykk_id", all.x = T)
  #idx.df = merge(idx.df, KNN_PBP, by = "aproovitykk_id", all.x = T)
  viga = sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4)
  print(viga)
  pilt.beta1[i] = viga
}

plot(pilt.beta1 - 0.1898324)
#ülimarginaalselt saab paremaks, ehk 0.1893145


#keskmisega:
kps0 = kps
kps0 = kps0[-1]
pilt.mean = c()
for(i in 1:length(kps0)){
  seqs = c(1:length(kps0))[-i]
  kp_data = data.frame()
  for(j in seqs){
    kp = kps0[j]
    kp_data0 = result.kp.bfgs[[j]]; kp_data0$kp = kp
    kp_data0 = merge(kp_data0, taks.info, all.x = T, by = "aproovitykk_id")
    kp_data = rbind(kp_data, kp_data0)
  }
  kp_data[,2:5] = (kp_data[,2:5]*(dim(kp_data)[1]-1) + 0.5)/dim(kp_data)[1]
  KNN_PBP = kp_data[,1:5] %>% group_by(aproovitykk_id) %>% summarise_all(funs(mean))
  KNN_PBP[,2:5] = KNN_PBP[,2:5] / rowSums(KNN_PBP[,2:5])
  dp = merge(KNN_PBP, taks.info, by = "aproovitykk_id", all.x = T)
  #idx.df = merge(idx.df, KNN_PBP, by = "aproovitykk_id", all.x = T)
  viga = sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4)
  print(viga)
  pilt.mean[i] = viga
}

plot(pilt.mean -  0.1914351)
min(pilt.mean) # 0.1934248 pärast 1.sammu; 2.samm: 0.1928839; 0.1921768; 0.1914351
#0.1914351

###########################################
#kernel density, nt epa

t1 = kp_data[kp_data$aproovitykk_id ==  34094,2:5]
t1m = t1[,1];t1k = t1[,2];t1ka = t1[,3];t1mu = t1[,4]
d1m = density(t1m, kernel = "epanechnikov", from = 0, to = 1);d1k = density(t1k, kernel = "epanechnikov", from = 0, to = 1);d1ka = density(t1ka, kernel = "epanechnikov", from = 0, to = 1);d1mu = density(t1mu, kernel = "epanechnikov", from = 0, to = 1)
par(mfrow = c(2,2))
plot(d1m);plot(d1k);plot(d1ka);plot(d1mu)

d1mu$x[which.max(d1mu$y)]

vec = t1m#c(rep(0.15,11), 0.0001,0.69)
eb = ebeta(vec, method = "mle")
eb = beta.mle(vec)
a = eb$parameters[1]; b = eb$parameters[2]
a = eb$param[1]; b = eb$param[2]
#opt = optim(par = 0.5, lower = 0, upper = 1, dbeta, shape1 = a, shape2 = b, method = "Brent", control=list(fnscale=-1))
#max.d = opt$`par`
opt = optimize(interval = c(0,1), dbeta, shape1 = a, shape2 = b, maximum = T)
opt$`maximum`
mean(vec)

xx = seq(0,1, 0.001)
dev.off()
par(mfrow = c(1,1))
plot(xx,dbeta(xx, shape1 = a, shape2 = b))

dens1 = function(vec, kernel = "epanechnikov"){
  if(length(vec) < 2){max = vec}
  else{
    d = density(vec, kernel = "epanechnikov", from = 0, to = 1)
    max = d$x[which.max(d$y)]
  }
  max
}


#####################################################

#beta "erindid". võtame välja need, mis kaugemal kui 2sd, kuna beta on ülitugevalt mõjutatud erinditest!
mean(t1m)
sqrt(var(t1m))



vead.kasv.epa = c()
idx.df = data.frame(aproovitykkid = idx)
for(i in 1:length(kps)){
  kp_data = data.frame()
  for(j in 1:i){
    kp = kps[j]
    kp_data0 = result.kp.tv100[[j]]; kp_data0$kp = kp
    kp_data0 = merge(kp_data0, taks.info, all.x = T, by = "aproovitykk_id")
    kp_data = rbind(kp_data, kp_data0)
  }
  kp_data[,2:5] = (kp_data[,2:5]*(dim(kp_data)[1]-1) + 0.5)/dim(kp_data)[1]
  KNN_PBP = kp_data[,1:5] %>% group_by(aproovitykk_id) %>% summarise_all(funs(dens1))
  KNN_PBP[,2:5] = KNN_PBP[,2:5] / rowSums(KNN_PBP[,2:5])
  dp = merge(KNN_PBP, taks.info, by = "aproovitykk_id", all.x = T)
  #idx.df = merge(idx.df, KNN_PBP, by = "aproovitykk_id", all.x = T)
  viga = sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4)
  print(viga)
  vead.kasv.epa[i] = viga
}

par(mfrow=c(1,1))
plot(vead.kasv, type = "o")
plot(vead.kasv.epa, type = "o") #

