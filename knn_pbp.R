#Knn pildikaupa


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

fun_bestvars = function(k1, k2, sidxx, kernel){ #, varsw = vars, ws = rep(1, length(vars))
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
      #print(vars1)
      vahe = mx-mx0
      #print(vahe)
      mx = mx0
    }
    var_return[[1+k-k1]] =  vars1
    rss_return[[1+k-k1]] =  mx
  }
  list_return[[1]] = var_return;list_return[[2]] = rss_return
  list_return
}

 # PROGNOOSIMINE #

#praegu kõik prognoosid koos
require(FNN)
setwd("A:/MAKA/TEST/test")
load("taks_info.RData")
load("sid601.RData")
d601 = read.csv("d601.csv")
data = d601
vars = names(data)[3:49]

tvmaht = 2
data_puud = taks.info[taks.info$aproovitykk_id %in% sidxx,]
puud_true =  data_puud[,7:10]
data_puud = data_puud[,(4*tvmaht-1):(4*tvmaht+2)]
  
bestvars.k = fun_bestvars(1,25,sidxx, kernel = epa)
#save(bestvars.k, file = "bestvars_epa.RData")
#save(bestvars.k, file = "bestvars_epa25.RData")
load(file = "bestvars_epa25.RData")
vark = bestvars.k[[1]]
rss = unlist(bestvars.k[[2]])
bests = c(match(rss[order(rss)][1],rss),match(rss[order(rss)][2],rss),match(rss[order(rss)][3],rss),match(rss[order(rss)][4],rss))
op1 = optim(par = rep(1, length(vark[[bests[1]]])), fn = fun_opti, k = bests[1], vars = vark[[bests[1]]], data = data, sidxx = sidxx,method = "BFGS", kernel = epa)
op2 = optim(par = rep(1, length(vark[[bests[2]]])), fn = fun_opti, k = bests[2], vars = vark[[bests[2]]], data = data, sidxx = sidxx,method = "BFGS", kernel = epa)
op3 = optim(par = rep(1, length(vark[[bests[3]]])), fn = fun_opti, k = bests[3], vars = vark[[bests[3]]], data = data, sidxx = sidxx,method = "BFGS", kernel = epa)
op4 = optim(par = rep(1, length(vark[[bests[4]]])), fn = fun_opti, k = bests[4], vars = vark[[bests[4]]], data = data, sidxx = sidxx,method = "BFGS", kernel = epa)
w.opt = vector("list", length=4);
rss.opt = c()
w.opt[[1]] = op1$par;   w.opt[[2]] = op2$par;   w.opt[[3]] = op3$par;   w.opt[[4]] = op4$par;
rss.opt[1] = op1$value; rss.opt[2] = op2$value; rss.opt[3] = op3$value; rss.opt[4] = op4$value
brss = match(rss.opt[order(rss.opt)][1],rss.opt) #kuidagi tuleb parim k salvestada
best_k = bests[brss]
best_vars = vark[[best_k]]
wgt = w.opt[[brss]]
  
print(best_k);print(best_vars);print(wgt)
  
dex = data[,c("aproovitykk_id",best_vars)]
dex = dex[dex$aproovitykk_id %in% sidxx,]
dex[,-1] = t((t(as.matrix(dex[,-1])))*wgt)
dex$cl = "cl"
kp.result.prop = fun_agre_kernel(dex, data_puud, k = best_k, sid = sidxx, kernel = epa)
plot(rss, type = "o")




#uuele ringile juba leitud kaaludega:
bestvars.kw = fun_bestvars(20,20,sidxx, kernel = epa, varsw = best_vars, ws = wgt)
op1kw = optim(par = rep(1, length(unlist(bestvars.kw[[1]]))), fn = fun_opti, k = 20, vars = unlist(bestvars.kw[[1]]), data = data, sidxx = sidxx,method = "BFGS", kernel = epa)




#prognoosid nii tüvemahtude kui proportsioonide pealt!
#nii RMSE kui MSE põhjal?
#taks.info siit võtan tüvemahud ja proportsioonid

dp = kp.result.prop #proportsioonide põhjal
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
sqrt((sum((puud_true[,1:4]-dp[,1:4])**2))/dim(dp)[1]/4) #0.194184
#0.1774445
#0.169774
#0.2088927
#0.1665879
#0.1808612

#kui plottida nii, et 5% alumise ja ülemisi viidud kas 0 või 1:
dp[,1:4][dp[,1:4] < 0.05] = 0; dp[,1:4][dp[,1:4] > 0.95] = 1; dp[,1:4] = dp[,1:4]/rowSums(dp[,1:4])
dev.off()
par(mfrow = c(2,2))
plot(puud_true[,1],dp[,1])
plot(puud_true[,2],dp[,2])
plot(puud_true[,3],dp[,3])
plot(puud_true[,4],dp[,4])
#vt ka cut huilo cot_low.R-is


dp = kp.result.tv[[1]] #tüvemahtude põhjal
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

#0.1804466
#0.1733454
#0.2192768
#0.1752043

#tüvenahud on kehvemad!


############ kuupäevade kaupa ###########

#result.kp = vector("list", length = 15)

#ööseks jooksma ka parema opt. meetodiga
result.kp.bfgs = vector("list", length = 15)
bestk = c()
tvmaht = 2 #proportsioonid

sentinel601sc = read.csv("sentinel601sc.csv")
data0 = sentinel601sc[,c(2:13,26:41)]
vars = names(data0)[4:28]
kps = unique(data0$kp)



for(i in 1:15){
  print(Sys.time());print("i-s pilt:");print(i)
  kp = kps[i]
  data = na.omit(data0[data0$kp == kp,])
  sid0 = data$aproovitykk_id
  
  data_puud = taks.info[taks.info$aproovitykk_id %in% sid0,]
  puud_true =  data_puud[,7:10]
  data_puud = data_puud[,(4*tvmaht-1):(4*tvmaht+2)] #vastavalt kas proportsioonid või tüvemahud

  
  bestvars.k = fun_bestvars(5,20, sidxx = sid0)#alla 5 naabri ei vaata
  vark = bestvars.k[[1]]
  rss = unlist(bestvars.k[2])
  #print(rss)
  bests = c(match(rss[order(rss)][1],rss),match(rss[order(rss)][2],rss)) #,match(rss[order(rss)][3],rss)
  op1 = optim(par = rep(1, length(vark[[bests[1]]])), fn = fun_opti, k = bests[1], vars = vark[[bests[1]]], data = data, sidxx = sid0,method = "BFGS")
  op2 = optim(par = rep(1, length(vark[[bests[2]]])), fn = fun_opti, k = bests[2], vars = vark[[bests[2]]], data = data, sidxx = sid0,method = "BFGS")
  #op3 = optim(par = rep(1, length(vark[[bests[3]]])), fn = fun_opti, k = bests[3], vars = vark[[bests[3]]], data = data, sidxx = sid0)
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
  result.kp.bfgs[[i]] = fun_agre_kernel(dex, data_puud, k = best_k, sid = sid0, kernel = epa)
  bestk[i] = best_k
}

#save(result.kp,file ="KNN_pbp.RData")

#save(result.kp.bfgs, file = "KNN_pbp_bfgs.RData")
#19 18 20 20 20 18 17 11 12 13 16  7 19 20 18


#04:01:49 hakkas teibaarvutus pihta;

load(file ="KNN_pbp_bfgs.RData")


#männid
dev.off()
par(mfrow = c(4, 4))
for (i in 1:15) {
  kp = kps[i]
  kp_data = result.kp.bfgs[[i]]
  kp_data = merge(kp_data, taks.info, all.x = T, by = "aproovitykk_id")
  plot(kp_data[,11],kp_data[,2], main = paste("Mänd_", kp), xlim = c(0,1), ylim = c(0,1))
}

#kuused
dev.off()
par(mfrow = c(4, 4))
for (i in 1:15) {
  kp = kps[i]
  kp_data = result.kp.bfgs[[i]]
  kp_data = merge(kp_data, taks.info, all.x = T, by = "aproovitykk_id")
  plot(kp_data[,12],kp_data[,3], main = paste("Kuusk_", kp), xlim = c(0,1), ylim = c(0,1))
}

#kased
dev.off()
par(mfrow = c(4, 4))
for (i in 1:15) {
  kp = kps[i]
  kp_data = result.kp.bfgs[[i]]
  kp_data = merge(kp_data, taks.info, all.x = T, by = "aproovitykk_id")
  plot(kp_data[,13],kp_data[,4], main = paste("Kask_", kp), xlim = c(0,1), ylim = c(0,1))
}

#muud
dev.off()
par(mfrow = c(4, 4))
for (i in 1:15) {
  kp = kps[i]
  kp_data = result.kp.bfgs[[i]]
  kp_data = merge(kp_data, taks.info, all.x = T, by = "aproovitykk_id")
  plot(kp_data[,14],kp_data[,5], main = paste("Muu_", kp), xlim = c(0,1), ylim = c(0,1))
}


#mis aastajal saab parimad prognoosid!?
MRSS = function(data){
  sqrt(sum((data[,2:5]-data[,11:14])**2)/(dim(data)[1]*4))
}
kps = unique(data0$kp)
kps = kps[order(kps)]
kps = as.Date(kps)
kps <- format(as.Date(kps), "%j")
kps = as.numeric(kps)
#kps = as.POSIXct(kps, format = "%Y-%m-%d")
vead = c()
for(i in 1:15){
  kp = kps[i]
  kp_data = result.kp.bfgs[[i]]
  kp_data = merge(kp_data, taks.info, all.x = T, by = "aproovitykk_id")
  vead[i] = MRSS(kp_data)
}

vead = data.frame(vead = vead, kp = kps)
dev.off()
par(mfrow = c(1, 1))
ggplot(data = vead, aes(x = kp, y = vead)) + geom_point() + scale_x_continuous(breaks = kps) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
#bfgs-iga isegi keskmiselt 0.001 võrra suuremad vead
# vead  kp
# 1  0.2345942 216
# 2  0.1988095 236
# 3  0.2054219 273
# 4  0.2165888 118
# 5  0.2221778 131
# 6  0.2034202 122
# 7  0.2287040 125
# 8  0.2405094 242
# 9  0.2029269 267
# 10 0.2357534 130
# 11 0.2058874 147
# 12 0.2289817 150
# 13 0.2431652 160
# 14 0.2168422 235
# 15 0.2381817 262

#kui võtta nende piltide tuim keskmine:

kp_data = data.frame()
for(i in 1:15){
  kp = kps[i]
  kp_data0 = result.kp.bfgs[[i]]; kp_data0$kp = kp
  kp_data0 = merge(kp_data0, taks.info, all.x = T, by = "aproovitykk_id")
  kp_data = rbind(kp_data, kp_data0)
}

length(unique(kp_data$aproovitykk_id))# juba siin 599...

KNN_PBP = kp_data[,1:5]
KNN_PBP = KNN_PBP %>% group_by(aproovitykk_id) %>% summarise_all(funs(mean))
#kuidas siin 599 on?

sidorov = c()
for(i in 1:15){
  kp = kps[i]
  data = na.omit(data0[data0$kp == kp,])
  sid0 = data$aproovitykk_id
  #print(length(sid0))
  sidorov = c(sidorov,sid0)
}

length(unique(sidorov)) #ja ongi 599, seega 2 id-d pole ühelgi pildil!
puudu = sidxx[!(sidxx %in% unique(sidorov))]

#dp[dp$aproovitykk_id %in% puudu,]
#taks.info[taks.info$aproovitykk_id %in% puudu,]

dp = merge(KNN_PBP, taks.info, by = "aproovitykk_id", all.x = T) #tüvemahtude põhjal
dev.off()
par(mfrow = c(2,2))
plot(dp[,11],dp[,2])
plot(dp[,12],dp[,3])
plot(dp[,13],dp[,4])
plot(dp[,14],dp[,5])

sqrt(mean((dp[,11]-dp[,2])**2)) #0.2001935
sqrt(mean((dp[,12]-dp[,3])**2)) #0.1780051
sqrt(mean((dp[,13]-dp[,4])**2)) #0.2157563
sqrt(mean((dp[,14]-dp[,5])**2)) #0.1803209
sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4) #0.194184



#üks variant:

# Dave,
# 
# A common approach to this problem is to fit 2 logistic regression models to predict whether a case is 0 or 1. 
# Then, a beta regression is used for those in the range (0,1).
# B_mine stackex.


##a better lemon squeezer: seal 0 ka 1 probleem beta-jaotuses
data.kp = kp_data;
data.kp1 = data.kp[,2:5]
data.kp1 = (data.kp1*7191 + 0.5)/7192 ###7192 mitte 599!
max(data.kp1);min(data.kp1)

data.kp[,2:5] = data.kp1
kp_data = data.kp


pdid1 = kp_data[kp_data$aproovitykk_id == sidxx[2],]; pdid1 = pdid1[,2:5]
plot(sort(pdid1[,4]), type = "o")
#siia peaks beeta jaotuse tiheduse projitseerima peale ja siis võtma maximumi!
#beta jaotus!?




install.packages("Rfast")#see keerab FNN-i pekki
require(Rfast)

x = pdid1[,3]
bets = beta.mle(x, tol = 1e-09)

a = bets$param[1];b = bets$param[2]

mode.b = (a-1) / (a + b -2)
mode.b


install.packages("EnvStats")
require(EnvStats)
bets = ebeta(x)


bets_fun = function(vec){
  if(length(vec) < 2){
    max.d = vec
  }
  else{
    eb = ebeta(vec)
    a = eb$parameters[1]; b = eb$parameters[2]
    #mode.b = (a-1) / (a + b -2) #see ei töötanud mõnel juhul
    xx = seq(0,1, 0.001)
    #max.d = which.max(dbeta(xx, shape1 = eb$parameters[1], shape2 = eb$parameters[2])) / 1001
    #siia optim!
    opt = optimize(interval = c(0,1), dbeta, shape1 = a, shape2 = b, maximum = T)
    max.d = opt$`maximum`
    #if(max.d < 0.000999001){max.d = 0}
    #bimodaalsete juhtude tarvis
    #võiks võtta ka tuima keskmise?
    if((a < 1 & b < 1)){
      # if(a < b){
      #   max.d = 0
      # }
      # if(a >=  b){
      #   max.d = 1
      # }
    
      max.d = mean(vec)
    }
    }
  max.d
}


sits = sidxx[3]
pdid1 = kp_data[kp_data$aproovitykk_id == sits,]; pdid1 = pdid1[,2:5]
mean(pdid1[,1]);mean(pdid1[,2]);mean(pdid1[,3]);mean(pdid1[,4])
bets_fun(pdid1[,1]);bets_fun(pdid1[,2]);bets_fun(pdid1[,3]);bets_fun(pdid1[,4])

taks.info[taks.info$aproovitykk_id == sits,]

#kõik, mis alla 0, saab olema 0, ja siis veel summa 1
#TRATRHHHH: BETA-jaotuse parameetrite põhjal "pildi kokkupanek";

KNN_PBP_BETA0 = kp_data[,1:5]
KNN_PBP_BETA = KNN_PBP_BETA0 %>% group_by(aproovitykk_id) %>% summarise_all(funs(bets_fun))

test = round(KNN_PBP_BETA[,2:5],1);test$id = KNN_PBP_BETA$aproovitykk_id

rsm = rowSums(KNN_PBP_BETA[,2:5]);hist(rsm);
min(rsm);which.min(rsm) #bimodaalsusprobleem!

max(rsm);which.max(rsm) #max on ok!

sits = KNN_PBP_BETA$aproovitykk_id[575]
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
plot(dp[,11],dp[,2])
plot(dp[,12],dp[,3])
plot(dp[,13],dp[,4])
plot(dp[,14],dp[,5])
                                                                #bimod "kõrgema tipu järgi" ja bimod keskmiseks
sqrt(mean((dp[,11]-dp[,2])**2))                     #0.1916956  0.1897614 0.188073
sqrt(mean((dp[,12]-dp[,3])**2))                     #0.1742844  0.1746465 0.1745209
sqrt(mean((dp[,13]-dp[,4])**2))                     #0.2226484  0.2208515 0.2200915
sqrt(mean((dp[,14]-dp[,5])**2))                     #0.1834227  0.1834227 0.1834227
sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4)  #0.1938675  0.1929573 0.1922974

#palju on bimodaalseid juhte?
bets_fun.a = function(vec){
  eb = ebeta(vec)
  a = eb$parameters[1]; b = eb$parameters[2]
  a
}

bets_fun.b = function(vec){
  eb = ebeta(vec)
  a = eb$parameters[1]; b = eb$parameters[2]
  b
}

tst = optimize(interval = c(0,1), dbeta, shape1 = bab.bimode[1,3], shape2 = bab.bimode[1,4], maximum = T)
tst = optimize(interval = c(0,1), dbeta, shape1 = 2, shape2 = 7, maximum = T)
xx = seq(0,1, 0.001)
tst1 = which.max(dbeta(xx, shape1 = 2, shape2 = 7)) / 1001 #väike vahe ikka on!

beta.param = KNN_PBP_BETA0 %>% group_by(aproovitykk_id) %>% summarise_all(funs(ebeta)) %>% select(.[[3]][1])


KNN_PBP_BETA0 %>% group_by(aproovitykk_id) %>% mutate(var1 = ebeta(.)[[3]][1])

KNN_PBP_BETA.a = KNN_PBP_BETA0 %>% group_by(aproovitykk_id) %>% summarise_all(funs(bets_fun.a))
KNN_PBP_BETA.b = KNN_PBP_BETA0 %>% group_by(aproovitykk_id) %>% summarise_all(funs(bets_fun.b))
require(reshape2)
b.a = melt(KNN_PBP_BETA.a, id.vars = "aproovitykk_id")
b.b = melt(KNN_PBP_BETA.b, id.vars = "aproovitykk_id")
bab = merge(b.a, b.b, by = c("aproovitykk_id","variable"))

bab.bimode = bab[(bab$value.x < 1 & bab$value.y < 1),]
#tuleb välja, et bimodaalseid keisse on ainult!

bab1 = head(bab, 64)

xx = seq(0,1, 0.001)
dev.off()
par(mfrow = c(1,2))
plot(xx,dbeta(xx, shape1 = bab.bimode[1,3], shape2 = bab.bimode[1,4]))
plot(xx,dbeta(xx, shape1 = bab.bimode[2,3], shape2 = bab.bimode[2,4]))

xx = seq(0,1, 0.001)
dev.off()
par(mfrow = c(8,8))
for(i in 1:64){
  plot(xx,dbeta(xx, shape1 = bab1[i,3], shape2 = bab1[i,4]))
}

k = 4
yy = seq(0.2, 0.8, 0.2) + 0.18
xx = seq(0,1, 0.001)
dev.off()
par(mfrow = c(k,k))
for(i in 1:k){
  for(j in 1:k){
    plot(xx,dbeta(xx, shape1 = yy[i], shape2 = yy[j]), title(main = paste(yy[i],yy[j], sep = "_")))
  }
}

#ots kaldub väiksema parameetri poole üles, st kui a väiksem, siis 0-i tõenäosus suurem



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

#ikka maru palju tunnuseid :S alguses sain epaga ju palju vähem tunnuseid!? jooksuta algne uuesti läbi
best_vars
# [1] "B08_vahe_kevad" "B04_sygis"      "B02_sygis"      "B12_kevad2"     "B08_kevad2"     "B04_vahe_kevad" "X21"           
# [8] "B11_kevad2"     "B06_kevad2"     "B11_sygis"      "X64"            "B06_kevad1"     "B05_sygis"      "B03_kevad2"    
# [15] "X43"            "B02_kevad1"     "X48"            "B06_vahe_kevad" "B08_sygis"


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
      kp_data0 = result.kp.bfgs[[kk]]; kp_data0$kp = kp
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

plot(unlist(veadNpilt), type = "o")
plot(unlist(veadNpilt.1), type = "o") #tundub loogiline

#beta?

bets_fun = function(vec, method = "mle"){
  if(any(is.na(vec))){
    max.d = 999999999969
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


veadNpilt.epa = c()
for(i in 1:length(kps)){
  print(i);print(Sys.time())
  vead = c()
  for(j in 1:choose(length(kps),i)){
    kp_data = data.frame()
    for(h in 1:i){
      kp = list.kps[[i]][h,j]
      kk = match(kp, kps)
      kp_data0 = result.kp.bfgs[[kk]]; kp_data0$kp = kp
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
  veadNpilt.epa[i] = mean(vead)
  print(veadNpilt.epa)
}

par(mfrow=c(1,2))
plot(unlist(veadNpilt.1), type = "o")
plot(unlist(veadNpilt.beta), type = "o")
plot(unlist(veadNpilt.epa), type = "o")
veadNpilt.1
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
    kp_data0 = result.kp.bfgs[[j]]; kp_data0$kp = kp
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

par(mfrow=c(1,2))
plot(vead.kasv, type = "o")
plot(vead.kasv.epa, type = "o") #

