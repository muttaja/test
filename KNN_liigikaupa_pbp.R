#lubada optimeerimisel erinev naabrite arv:
# fun_bestvars_liik = function(liigid,kkkk, sidxx, kernel, varsw = vars, ws = rep(1, length(vars))){
#   lst_return0 = vector("list", length = length(liigid))
#   for(i in 1:length(liigid)){
#     liik = liigid[i]
#     k1 = kkkk[2*i-1];k2 = kkkk[2*i]
#     list_return = vector("list", length = 2)
#     var_return = vector("list", length = 1+k2-k1)
#     rss_return = vector("list", length = 1+k2-k1)
#     for(k in k1:k2){
#       print(k);print(Sys.time())
#       vars0 = vars
#       vars1 = c()
#       mx = 6969
#       vahe = 696
#       while(vahe > 0){
#         mx0 = mx
#         vars00 = c()
#         rss = c()
#         for(j in 1:length(vars0)){
#           vars00 = c(vars1,vars0[j])
#           dex = data[,c("aproovitykk_id",vars00)]
#           dex = dex[dex$aproovitykk_id %in% sidxx,]
#           dex$cl = "cl"
#           H_puu = fun_agre_liik(dex, data_puud, k = k, sid = sidxx, kernel = kernel)
#           rsdls = H_puu[,liik] - puud_true[,liik]
#           rss[j] = sqrt((sum(rsdls**2))/(length(sidxx)))
#         }
#         mx0 = min(rss)
#         varmin = vars0[which.min(rss)]
#         vars0 = vars0[!(vars0 %in% varmin)]
#         vars1 = c(vars1,varmin)
#         vahe = mx-mx0
#         mx = mx0
#       }
#       var_return[[1+k-k1]] =  vars1
#       rss_return[[1+k-k1]] =  mx
#       print(mx)
#     }
#     list_return[[1]] = var_return;list_return[[2]] = rss_return
#     lst_return0[[liik]] = list_return
#   }
#   lst_return0
# }

#tavaline optix ilma kkkk vist siiski parem

# optix = function(varlist, j1, j2, kkkk, method = "BFGS", kernel = epa, data){
#   wlist   = vector("list", length = j2 - j1 +1)
#   rsslist = vector("list", length = j2 - j1 +1)
#   for(j in j1:j2){
#     k1 = kkkk[2*j-1];k2 = kkkk[2*j]
#     lst_return1 = vector("list", length = k2 - k1 +1)
#     lst_return2 = vector("list", length = k2 - k1 +1)
#     for(k in k1:k2){
#       print(c(j,k)); print(Sys.time())
#       varsx =  varlist[[j]][[k]]
#       w = rep(1, length(varsx))
#       opti_liik1 = optim(par = w, fn = fun_opti_liik, k = k, vars = varsx, liik = j, method = method, kernel = kernel, data = data, sidxx = sidxx)
#       #print(j);print(k);print(opti_liik1$value)
#       lst_return1[[k-k1+1]] =  assign(paste("rss", j, k, sep="_"),opti_liik1$value)
#       lst_return2[[k-k1+1]] =  assign(paste("weights", j, k, sep="_"),opti_liik1$par)
#       print(opti_liik1$value)
#     }
#     rsslist[[j]] = lst_return1
#     wlist[[j]] = lst_return2
#   }
#   return(cbind(rsslist, wlist))
# }


optix = function(varlist, j1, j2, k1, k2, method = "BFGS", kernel = epa, data, sidxx = sidxx){
  wlist   = vector("list", length = j2 - j1 +1)
  rsslist = vector("list", length = j2 - j1 +1)
  for(j in j1:j2){
    lst_return1 = vector("list", length = k2 - k1 +1)
    lst_return2 = vector("list", length = k2 - k1 +1)
    for(k in k1:k2){
      print(c(j,k)); print(Sys.time())
      varsx =  varlist[[j]][[k]]
      w = rep(1, length(varsx))
      opti_liik1 = optim(par = w, fn = fun_opti_liik, k = k, vars = varsx, liik = j, method = method, kernel = kernel, data = data, sidxx = sidxx)
      #print(j);print(k);print(opti_liik1$value)
      print(opti_liik1)
      lst_return1[[k-k1+1]] =  assign(paste("rss", j, k, sep="_"),opti_liik1$value)
      lst_return2[[k-k1+1]] =  assign(paste("weights", j, k, sep="_"),opti_liik1$par)
      print(opti_liik1$value)
    }
    rsslist[[j]] = lst_return1
    wlist[[j]] = lst_return2
  }
  return(cbind(rsslist, wlist))
}

fun_opti_liik = function(w, k, vars, data, sidxx, liik, kernel){
  dex = data[,c("aproovitykk_id",vars)]
  dex = dex[dex$aproovitykk_id %in% sidxx,]
  dex[,-1] = t((t(as.matrix(dex[,-1])))*w)
  dex$cl = "cl"
  #print(dim(dex)); print(dim(data_puud));print(k);print(length(sidxx)); print(liik)
  #print(head(dex));print(head(data_puud))
  H_puu = fun_agre_liik(dex, data_puud, k = k, sid = sidxx, liik = liik, kernel = kernel)
  rsdls = H_puu[,1] - puud_true[,liik]
  #print(names(H_puu));print(names(puud_true))
  #print(head(H_puu))
  rmse = sqrt((sum(rsdls**2))/(length(sidxx)))
  rmse
}


opt1 = optim(par = w, fn = fun_opti_liik, k = k, vars = varsx, liik = h, method = "BFGS", kernel = epa, data = data, sidxx = sid0)




bests = function(rmse,k){
  bestk = c()
  for(i in 1:k){
    bestk[i] = match(rmse[order(rmse)][i],rmse)
  }
  bestk
}



############ kuupäevade kaupa ###########
tvmaht = 2 #proportsioonid
data0 = read.csv("sentinel455.csv")
names(data0)

data0 = data0[,c(2:13,26:38)]
vars = names(data0)[c(4:25)]
kps = unique(data0$kp)




RESULTS.KP.LIIK = vector("list",15)
bestk.kp.liik = vector("list",15)
w.opt.kp.liigid = vector("list", length=15)

for(i in 1:15){
  print(Sys.time());print("i-s pilt:");print(i)
  #i = 1
  kp = kps[i]
  data = na.omit(data0[data0$kp == kp,])
  sid0 = data$aproovitykk_id
  
  data_puud = taks.info[taks.info$aproovitykk_id %in% sid0,]
  puud_true =  data_puud[,7:10]
  data_puud = data_puud[,(4*tvmaht-1):(4*tvmaht+2)] #vastavalt kas proportsioonid või tüvemahud
  
  knn.liigiti = fun_bestvars_liik(c(1:4),k1 = 1,k2 = 20, sidxx = sid0, kernel = epa)
  varlist[[1]] = knn.liigiti[[1]][[1]];varlist[[2]] = knn.liigiti[[2]][[1]];varlist[[3]] = knn.liigiti[[3]][[1]];varlist[[4]] = knn.liigiti[[4]][[1]]
  ma.RMSE = unlist(knn.liigiti[[1]][[2]])
  ku.RMSE = unlist(knn.liigiti[[2]][[2]])
  ks.RMSE = unlist(knn.liigiti[[3]][[2]])
  mu.RMSE = unlist(knn.liigiti[[4]][[2]])
  
  bests.ma = bests(ma.RMSE,2) 
  bests.ku = bests(ku.RMSE,2) 
  bests.ks = bests(ks.RMSE,2) 
  bests.mu = bests(mu.RMSE,2) 
  bstl = vector("list",4)
  bstl[[1]] = bests.ma;bstl[[2]] = bests.ku
  bstl[[3]] = bests.ks;bstl[[4]] = bests.mu
  
  liigid = c("ma","ku","ks","mu")
  w.opt.liigid = vector("list", length=4)
  rmse.opt.liigid = vector("list", length=4)
  bestk.liik = c()
  results.liik = vector("list", 4)
  for(h in 1:4){
    bests.liik = bstl[[h]]
    k = bests.liik[1]
    varsx =  varlist[[h]][[k]]
    w = rep(1, length(varsx))
    opt1 = optim(par = w, fn = fun_opti_liik, k = k, vars = varsx, liik = h, method = "BFGS", kernel = epa, data = data, sidxx = sid0)
    
    k = bests.liik[2]
    varsx =  varlist[[h]][[k]]
    w = rep(1, length(varsx))
    opt2 = optim(par = w, fn = fun_opti_liik, k = k, vars = varsx, liik = h, method = "BFGS", kernel = epa, data = data, sidxx = sid0)
    
    w.opt = vector("list", length=2)
    rmse.opt = c()
    
    w.opt[[1]] = opt1[[1]];   w.opt[[2]] = opt2[[1]];
    w.opt.liigid[[h]] = w.opt
    
    rmse.opt[1] = opt1[[2]]; rmse.opt[2] = opt2[[2]];
    rmse.opt = unlist(rmse.opt)

    brss = bests(rmse = rmse.opt, k=1)
    best_k = bests.liik[brss]
    
    
    best_vars = varlist[[h]][[best_k]]
    wgt = unlist(w.opt[[brss]])
    
    dex = data[,c("aproovitykk_id",best_vars)]
    dex = dex[dex$aproovitykk_id %in% sid0,]
    dex[,-1] = t((t(as.matrix(dex[,-1])))*wgt)
    dex$cl = "cl"
    
    pred.kp.liik = fun_agre_liik(dex, data_puud, k = best_k, sid = sid0, liik = h, kernel = epa)
    results.liik[[h]] = pred.kp.liik
    bestk.liik[h] = best_k
  }
  RESULTS.KP.LIIK[[i]] = results.liik
  bestk.kp.liik[i] = best_k
  w.opt.kp.liigid[[i]] = w.opt.liigid
}

#14.29 algas 1. pilt
#11.45 lõpp

#save(RESULTS.KP.LIIK, file = "PRED_PBP_LBL.RData")

pred.kp.liik1 = pred.kp.liik

par(mfrow = c(2,2))
pred.kp.liik4 = RESULTS.KP.LIIK[[2]][[1]]
dp = merge(pred.kp.liik4, taks.info, by = "aproovitykk_id", all.x = T)
plot(dp[,8],dp[,2])
pred.kp.liik4 = RESULTS.KP.LIIK[[2]][[2]]
dp = merge(pred.kp.liik4, taks.info, by = "aproovitykk_id", all.x = T)
plot(dp[,9],dp[,2])
pred.kp.liik4 = RESULTS.KP.LIIK[[2]][[3]]
dp = merge(pred.kp.liik4, taks.info, by = "aproovitykk_id", all.x = T)
plot(dp[,10],dp[,2])
pred.kp.liik4 = RESULTS.KP.LIIK[[2]][[4]]
dp = merge(pred.kp.liik4, taks.info, by = "aproovitykk_id", all.x = T)
plot(dp[,11],dp[,2])


#teeks selle kõik üheks df-iks:
knn.pbp.lbl = data.frame(row.names = c("aproovitykk_id", "MA", "KU", "KS", "MUU", "kp"))
kps = unique(data0$kp)
for(i in 1:15){
  kp = kps[i]
  #siin tekib järjekorraga probleem!?
  #kp.df = Reduce(function(x, y) merge(x, y, all=TRUE, by = "aproovitykk_id"), list(RESULTS.KP.LIIK[[i]][[1]], RESULTS.KP.LIIK[[i]][[2]], RESULTS.KP.LIIK[[i]][[3]]),RESULTS.KP.LIIK[[i]][[4]])
  kp.df = data.frame(aproovitykk_id = RESULTS.KP.LIIK[[i]][[1]][2])
  for(j in 1:4){
    kp.df[,j+1] = RESULTS.KP.LIIK[[i]][[j]][1]
  }
  names(kp.df) = c("aproovitykk_id","MA", "KU", "KS", "MUU"); 
  kp.df$kp = kp #as.Date(kp)
  knn.pbp.lbl = rbind(knn.pbp.lbl, kp.df)
}

par(mfrow=c(1,1))
hist(rowSums(knn.pbp.lbl[,2:5]), main = "", xlab = "Proportsioonide summa", ylab = "")

knn1 = knn.pbp.lbl
knn1[,2:5] = knn1[,2:5] / rowSums(knn1[,2:5])

#kps1 = sort(as.Date(kps, format = "%Y-%m-%d"))
#männid
dev.off()
par(mfrow = c(4, 4))
for (i in 1:15){
  kp = kps[i]
  kp_data = knn1[knn1$kp == kp,]
  kp_data = merge(kp_data, taks.info, all.x = T, by = "aproovitykk_id")
  plot(kp_data[,12],kp_data[,2], main = paste("Mänd ", kp), xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369), ylab = "", xlab = "")
}

#kuused
dev.off()
par(mfrow = c(4, 4))
for (i in 1:15) {
  kp = kps[i]
  kp_data = knn1[knn1$kp == kp,]
  kp_data = merge(kp_data, taks.info, all.x = T, by = "aproovitykk_id")
  plot(kp_data[,13],kp_data[,3], main = paste("Kuusk ", kp), xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369), ylab = "", xlab = "")
}

#kased
dev.off()
par(mfrow = c(4, 4))
for (i in 1:15) {
  kp = kps[i]
  kp_data = knn1[knn1$kp == kp,]
  kp_data = merge(kp_data, taks.info, all.x = T, by = "aproovitykk_id")
  plot(kp_data[,14],kp_data[,4], main = paste("Kask ", kp), xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369), ylab = "", xlab = "")
}

#muud
dev.off()
par(mfrow = c(4, 4))
for (i in 1:15) {
  kp = kps[i]
  kp_data = knn1[knn1$kp == kp,]
  kp_data = merge(kp_data, taks.info, all.x = T, by = "aproovitykk_id")
  plot(kp_data[,15],kp_data[,5], main = paste("Muu ", kp), xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369), ylab = "", xlab = "")
}





#mis aastajal saab parimad prognoosid!?
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

kps = unique(data0$kp)
#kps = kps[order(kps)]
#kps = as.Date(kps)

#kps = as.POSIXct(kps, format = "%Y-%m-%d")
vead = c();vaatlusi=c();vead.liik = matrix(NA, nrow = 15, ncol = 4)
for(i in 1:15){
  kp = kps[i]
  kp_data = knn1[knn1$kp == kp,]
  kp_data = merge(kp_data, taks.info, all.x = T, by = "aproovitykk_id")
  print(head(kp_data))
  vead[i] = RMSE(kp_data)
  vead.liik[i,] = RMSE_liik(kp_data)
  vaatlusi[i] = dim(kp_data)[1]
}


kps <- format(as.Date(kps), "%j")
kps = as.numeric(kps)

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



#agregeerime kokku:

#siin võimalik kasutada erinevaid beta fun-e, mis lõikavad kas erindeid või
#jätavad kõrvale mittesobiva kuju

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
    opt = optimize(interval = c(0,1), dbeta, shape1 = a, shape2 = b, maximum = T)
    max.d = opt$`maximum`
    #kui kujuparameetrid väikesed, siis pole head tippu
    if((a < 1 | b < 1)){
      max.d = mean(vec)
    }
  }
  max.d
}

bets.shape.sd = function(sdcut, shapecut, vec, method = "mle"){
  #sdcut = par1[1]; shapecut = par1[2]
  if(any(is.na(vec))){
    stop("NA")
  }
  vec0 = vec
  if(length(vec) > 2){sd = sqrt(var(vec));mu = mean(vec);vec = vec[((vec > mu-sdcut*sd) & (vec < mu+sdcut*sd))]}
  if(length(unique(vec)) < 2){
    vec = vec0
    max.d = mean(vec)
  }
  else{
    eb = ebeta(vec, method = method)
    a = eb$parameters[1]; b = eb$parameters[2]
    opt = optimize(interval = c(0,1), dbeta, shape1 = a, shape2 = b, maximum = T)
    max.d = opt$`maximum`
    if((a < shapecut | b < shapecut)){
      max.d = mean(vec)
    }
  }
  max.d
}


#optimeerime huvi pärast shape ja sd;

opti.beta = function(par1,df){
  sdcut = par1[1]; shapecut = par1[2]
  KNN_PBP = df[,1:5]
  KNN_PBP = KNN_PBP %>% group_by(aproovitykk_id) %>% summarise_all(funs(bets.shape.sd(.,sdcut,shapecut))) #bets_fun(.,method = "mme")
  dp = merge(KNN_PBP, taks.info, by = "aproovitykk_id", all.x = T)
  dp[,2:5] = dp[,2:5] / rowSums(dp[,2:5])
  sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4)
}

KNN_PBP = knn1[,1:5]; KNN_PBP[,2:5] = (KNN_PBP[,2:5]*5488 + 0.5)/5489
beta.opt = optim(par = c(1,1), fn = opti.beta, df = KNN_PBP)



KNN_PBP = knn1[,1:5]
KNN_PBP = KNN_PBP %>% group_by(aproovitykk_id) %>% summarise_all(funs(mean)) #bets_fun(.,method = "mme")
KNN_PBP[,2:5] = KNN_PBP[,2:5] / rowSums(KNN_PBP[,2:5])
dp = merge(KNN_PBP, taks.info, by = "aproovitykk_id", all.x = T)


KNN_PBP = knn1[,1:5]; KNN_PBP[,2:5] = (KNN_PBP[,2:5]*5488 + 0.5)/5489
KNN_PBP = KNN_PBP %>% group_by(aproovitykk_id) %>% summarise_all(funs(bets_fun)) #bets_fun(.,method = "mme")
KNN_PBP[,2:5] = KNN_PBP[,2:5] / rowSums(KNN_PBP[,2:5])
dp1 = merge(KNN_PBP, taks.info, by = "aproovitykk_id", all.x = T)




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

sqrt(mean((dp[,11]-dp[,2])**2))
sqrt(mean((dp[,12]-dp[,3])**2))
sqrt(mean((dp[,13]-dp[,4])**2))
sqrt(mean((dp[,14]-dp[,5])**2))
sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4)

#samal plotil mean ja beta

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










############ kuupäevade kaupa ###########

#LANDSAT #



tvmaht = 2 #proportsioonid
data0 = read.csv("landsat455sc.csv")
names(data0)

data0 = data0[,c(2:10,16:28)]
vars = names(data0)[c(4:22)]
kps = unique(data0$kp)


RESULTS.KP.LIIK.LANDSAT = vector("list",9)
bestk.kp.liik.landsat = vector("list",9)
w.opt.kp.liigid.landsat = vector("list", length=9)

for(i in 1:9){
  print(Sys.time());print("i-s pilt:");print(i)
  kp = kps[i]
  data = na.omit(data0[data0$kp == kp,])
  sid0 = data$aproovitykk_id
  
  data_puud = taks.info[taks.info$aproovitykk_id %in% sid0,]
  puud_true =  data_puud[,7:10]
  data_puud = data_puud[,(4*tvmaht-1):(4*tvmaht+2)] #vastavalt kas proportsioonid või tüvemahud
  
  knn.liigiti = fun_bestvars_liik(c(1:4),k1 = 1,k2 = 20, sidxx = sid0, kernel = epa)
  varlist[[1]] = knn.liigiti[[1]][[1]];varlist[[2]] = knn.liigiti[[2]][[1]];varlist[[3]] = knn.liigiti[[3]][[1]];varlist[[4]] = knn.liigiti[[4]][[1]]
  ma.RMSE = unlist(knn.liigiti[[1]][[2]])
  ku.RMSE = unlist(knn.liigiti[[2]][[2]])
  ks.RMSE = unlist(knn.liigiti[[3]][[2]])
  mu.RMSE = unlist(knn.liigiti[[4]][[2]])
  
  bests.ma = bests(ma.RMSE,2) 
  bests.ku = bests(ku.RMSE,2) 
  bests.ks = bests(ks.RMSE,2) 
  bests.mu = bests(mu.RMSE,2) 
  bstl = vector("list",4)
  bstl[[1]] = bests.ma;bstl[[2]] = bests.ku
  bstl[[3]] = bests.ks;bstl[[4]] = bests.mu
  
  liigid = c("ma","ku","ks","mu")
  w.opt.liigid = vector("list", length=4)
  rmse.opt.liigid = vector("list", length=4)
  bestk.liik = c()
  results.liik = vector("list", 4)
  for(h in 1:4){
    bests.liik = bstl[[h]]
    k = bests.liik[1]
    varsx =  varlist[[h]][[k]]
    w = rep(1, length(varsx))
    opt1 = optim(par = w, fn = fun_opti_liik, k = k, vars = varsx, liik = h, method = "BFGS", kernel = epa, data = data, sidxx = sid0)
    
    k = bests.liik[2]
    varsx =  varlist[[h]][[k]]
    w = rep(1, length(varsx))
    opt2 = optim(par = w, fn = fun_opti_liik, k = k, vars = varsx, liik = h, method = "BFGS", kernel = epa, data = data, sidxx = sid0)
    
    w.opt = vector("list", length=2)
    rmse.opt = c()
    
    w.opt[[1]] = opt1[[1]];   w.opt[[2]] = opt2[[1]];
    w.opt.liigid[[h]] = w.opt
    
    rmse.opt[1] = opt1[[2]]; rmse.opt[2] = opt2[[2]];
    rmse.opt = unlist(rmse.opt)
    
    brss = bests(rmse = rmse.opt, k=1)
    best_k = bests.liik[brss]
    
    
    best_vars = varlist[[h]][[best_k]]
    wgt = unlist(w.opt[[brss]])
    
    dex = data[,c("aproovitykk_id",best_vars)]
    dex = dex[dex$aproovitykk_id %in% sid0,]
    dex[,-1] = t((t(as.matrix(dex[,-1])))*wgt)
    dex$cl = "cl"
    
    pred.kp.liik = fun_agre_liik(dex, data_puud, k = best_k, sid = sid0, liik = h, kernel = epa)
    results.liik[[h]] = pred.kp.liik
    bestk.liik[h] = best_k
  }
  RESULTS.KP.LIIK.LANDSAT[[i]] = results.liik
  bestk.kp.liik.landsat[i] = best_k
  w.opt.kp.liigid.landsat[[i]] = w.opt.liigid
}

#save(RESULTS.KP.LIIK.LANDSAT, file = "PRED_PBP_LBL_LANDSAT.RData")


#landsat tüvemahud:

tvmaht = 1 #tüvemahud
data0 = read.csv("landsat455sc.csv")
names(data0)

data0 = data0[,c(2:10,16:28)]
vars = names(data0)[c(4:22)]
kps = unique(data0$kp)


RESULTS.KP.LIIK.LANDSAT.TM = vector("list",9)
bestk.kp.liik.landsat.tm = vector("list",9)
w.opt.kp.liigid.landsat.tm = vector("list", length=9)

for(i in 1:9){
  print(Sys.time());print("i-s pilt:");print(i)
  kp = kps[i]
  data = na.omit(data0[data0$kp == kp,])
  sid0 = data$aproovitykk_id
  
  data_puud = taks.info[taks.info$aproovitykk_id %in% sid0,]
  puud_true =  data_puud[,7:10]
  data_puud = data_puud[,(4*tvmaht-1):(4*tvmaht+2)] #vastavalt kas proportsioonid või tüvemahud
  
  knn.liigiti = fun_bestvars_liik(c(1:4),k1 = 1,k2 = 20, sidxx = sid0, kernel = epa)
  varlist[[1]] = knn.liigiti[[1]][[1]];varlist[[2]] = knn.liigiti[[2]][[1]];varlist[[3]] = knn.liigiti[[3]][[1]];varlist[[4]] = knn.liigiti[[4]][[1]]
  ma.RMSE = unlist(knn.liigiti[[1]][[2]])
  ku.RMSE = unlist(knn.liigiti[[2]][[2]])
  ks.RMSE = unlist(knn.liigiti[[3]][[2]])
  mu.RMSE = unlist(knn.liigiti[[4]][[2]])
  
  bests.ma = bests(ma.RMSE,2) 
  bests.ku = bests(ku.RMSE,2) 
  bests.ks = bests(ks.RMSE,2) 
  bests.mu = bests(mu.RMSE,2) 
  bstl = vector("list",4)
  bstl[[1]] = bests.ma;bstl[[2]] = bests.ku
  bstl[[3]] = bests.ks;bstl[[4]] = bests.mu
  
  liigid = c("ma","ku","ks","mu")
  w.opt.liigid = vector("list", length=4)
  rmse.opt.liigid = vector("list", length=4)
  bestk.liik = c()
  results.liik = vector("list", 4)
  for(h in 1:4){
    bests.liik = bstl[[h]]
    k = bests.liik[1]
    varsx =  varlist[[h]][[k]]
    w = rep(1, length(varsx))
    opt1 = optim(par = w, fn = fun_opti_liik, k = k, vars = varsx, liik = h, method = "BFGS", kernel = epa, data = data, sidxx = sid0)
    
    k = bests.liik[2]
    varsx =  varlist[[h]][[k]]
    w = rep(1, length(varsx))
    opt2 = optim(par = w, fn = fun_opti_liik, k = k, vars = varsx, liik = h, method = "BFGS", kernel = epa, data = data, sidxx = sid0)
    
    w.opt = vector("list", length=2)
    rmse.opt = c()
    
    w.opt[[1]] = opt1[[1]];   w.opt[[2]] = opt2[[1]];
    w.opt.liigid[[h]] = w.opt
    
    rmse.opt[1] = opt1[[2]]; rmse.opt[2] = opt2[[2]];
    rmse.opt = unlist(rmse.opt)
    
    brss = bests(rmse = rmse.opt, k=1)
    best_k = bests.liik[brss]
    
    
    best_vars = varlist[[h]][[best_k]]
    wgt = unlist(w.opt[[brss]])
    
    dex = data[,c("aproovitykk_id",best_vars)]
    dex = dex[dex$aproovitykk_id %in% sid0,]
    dex[,-1] = t((t(as.matrix(dex[,-1])))*wgt)
    dex$cl = "cl"
    
    pred.kp.liik = fun_agre_liik(dex, data_puud, k = best_k, sid = sid0, liik = h, kernel = epa)
    results.liik[[h]] = pred.kp.liik
    bestk.liik[h] = best_k
  }
  RESULTS.KP.LIIK.LANDSAT.TM[[i]] = results.liik
  bestk.kp.liik.landsat.tm[i] = best_k
  w.opt.kp.liigid.landsat.tm[[i]] = w.opt.liigid
}

landsat_pbp_lbl = vector("list",3); 
landsat_pbp_lbl[[1]] = RESULTS.KP.LIIK.LANDSAT.TM
landsat_pbp_lbl[[2]] = bestk.kp.liik.landsat.tm
landsat_pbp_lbl[[3]] = w.opt.kp.liigid.landsat.tm

#save(landsat_pbp_lbl,file = "landsat_pbp_lbl_tm.RData")


knn.pbp.lbl.land.tm = data.frame(row.names = c("aproovitykk_id", "MA", "KU", "KS", "MUU", "kp"))
data0 = read.csv("landsat455sc.csv")
kps = unique(data0$kp)
for(i in 1:9){
  kp = kps[i]
  kp.df = data.frame(aproovitykk_id = landsat_pbp_lbl[[1]][[i]][[1]][2])
  for(j in 1:4){
    kp.df[,j+1] = landsat_pbp_lbl[[1]][[i]][[j]][1]
  }
  names(kp.df) = c("aproovitykk_id","MA", "KU", "KS", "MUU"); 
  kp.df$kp = kp
  knn.pbp.lbl.land.tm = rbind(knn.pbp.lbl.land.tm, kp.df)
}
knn.pbp.lbl.land.tm$satel = "sentinel"

df = knn.pbp.lbl.land.tm
df[,2:5] = df[,2:5] / rowSums(df[,2:5])
df1 = df[,1:5]
df1[,2:5] = (df1[,2:5]*5488 + 0.5)/5489
KNN_PBP = df1 %>% group_by(aproovitykk_id) %>% summarise_all(funs(bets_fun2sd)) #bets_fun(.,method = "mme")
dp = merge(KNN_PBP, taks.info, by = "aproovitykk_id", all.x = T)
dp[,2:5] = dp[,2:5] / rowSums(dp[,2:5])
sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4)



###########SENTINEL tüvemahud

tvmaht = 1 #tüvemahud
data0 = read.csv("sentinel455.csv")
names(data0)

data0 = data0[,c(2:13,26:38)]
vars = names(data0)[c(4:25)]
kps = unique(data0$kp)


RESULTS.KP.LIIK.TM = vector("list",15)
bestk.kp.liik.tm = vector("list",15)
w.opt.kp.liigid.tm = vector("list", length=15)

for(i in 1:15){
  print(Sys.time());print("i-s pilt:");print(i)
  #i = 1
  kp = kps[i]
  data = na.omit(data0[data0$kp == kp,])
  sid0 = data$aproovitykk_id
  
  data_puud = taks.info[taks.info$aproovitykk_id %in% sid0,]
  puud_true =  data_puud[,7:10]
  data_puud = data_puud[,(4*tvmaht-1):(4*tvmaht+2)] #vastavalt kas proportsioonid või tüvemahud
  
  knn.liigiti = fun_bestvars_liik(c(1:4),k1 = 1,k2 = 20, sidxx = sid0, kernel = epa)
  varlist[[1]] = knn.liigiti[[1]][[1]];varlist[[2]] = knn.liigiti[[2]][[1]];varlist[[3]] = knn.liigiti[[3]][[1]];varlist[[4]] = knn.liigiti[[4]][[1]]
  ma.RMSE = unlist(knn.liigiti[[1]][[2]])
  ku.RMSE = unlist(knn.liigiti[[2]][[2]])
  ks.RMSE = unlist(knn.liigiti[[3]][[2]])
  mu.RMSE = unlist(knn.liigiti[[4]][[2]])
  
  bests.ma = bests(ma.RMSE,2) 
  bests.ku = bests(ku.RMSE,2) 
  bests.ks = bests(ks.RMSE,2) 
  bests.mu = bests(mu.RMSE,2) 
  bstl = vector("list",4)
  bstl[[1]] = bests.ma;bstl[[2]] = bests.ku
  bstl[[3]] = bests.ks;bstl[[4]] = bests.mu
  
  liigid = c("ma","ku","ks","mu")
  w.opt.liigid = vector("list", length=4)
  rmse.opt.liigid = vector("list", length=4)
  bestk.liik = c()
  results.liik = vector("list", 4)
  for(h in 1:4){
    bests.liik = bstl[[h]]
    k = bests.liik[1]
    varsx =  varlist[[h]][[k]]
    w = rep(1, length(varsx))
    opt1 = optim(par = w, fn = fun_opti_liik, k = k, vars = varsx, liik = h, method = "BFGS", kernel = epa, data = data, sidxx = sid0)
    
    k = bests.liik[2]
    varsx =  varlist[[h]][[k]]
    w = rep(1, length(varsx))
    opt2 = optim(par = w, fn = fun_opti_liik, k = k, vars = varsx, liik = h, method = "BFGS", kernel = epa, data = data, sidxx = sid0)
    
    w.opt = vector("list", length=2)
    rmse.opt = c()
    
    w.opt[[1]] = opt1[[1]];   w.opt[[2]] = opt2[[1]];
    w.opt.liigid[[h]] = w.opt
    
    rmse.opt[1] = opt1[[2]]; rmse.opt[2] = opt2[[2]];
    rmse.opt = unlist(rmse.opt)
    
    brss = bests(rmse = rmse.opt, k=1)
    best_k = bests.liik[brss]
    
    
    best_vars = varlist[[h]][[best_k]]
    wgt = unlist(w.opt[[brss]])
    
    dex = data[,c("aproovitykk_id",best_vars)]
    dex = dex[dex$aproovitykk_id %in% sid0,]
    dex[,-1] = t((t(as.matrix(dex[,-1])))*wgt)
    dex$cl = "cl"
    
    pred.kp.liik = fun_agre_liik(dex, data_puud, k = best_k, sid = sid0, liik = h, kernel = epa)
    results.liik[[h]] = pred.kp.liik
    bestk.liik[h] = best_k
  }
  RESULTS.KP.LIIK.TM[[i]] = results.liik
  bestk.kp.liik.tm[i] = best_k
  w.opt.kp.liigid.tm[[i]] = w.opt.liigid
}

sentinel_pbp_lbl_tm = vector("list",3); 
sentinel_pbp_lbl_tm[[1]] = RESULTS.KP.LIIK.TM
sentinel_pbp_lbl_tm[[2]] = bestk.kp.liik.tm
sentinel_pbp_lbl_tm[[3]] = w.opt.kp.liigid.tm

#save(sentinel_pbp_lbl_tm,file = "sentinel_pbp_lbl_tm.RData")

knn.pbp.lbl.tm = data.frame(row.names = c("aproovitykk_id", "MA", "KU", "KS", "MUU", "kp"))
data0 = read.csv(file = "sentinel455.csv")
kps = unique(data0$kp)
for(i in 1:15){
  kp = kps[i]
  kp.df = data.frame(aproovitykk_id = sentinel_pbp_lbl_tm[[1]][[i]][[1]][2])
  for(j in 1:4){
    kp.df[,j+1] = sentinel_pbp_lbl_tm[[1]][[i]][[j]][1]
  }
  names(kp.df) = c("aproovitykk_id","MA", "KU", "KS", "MUU"); 
  kp.df$kp = kp
  knn.pbp.lbl.tm = rbind(knn.pbp.lbl.tm, kp.df)
}
knn.pbp.lbl.tm$satel = "sentinel"

df = knn.pbp.lbl.tm
df[,2:5] = df[,2:5] / rowSums(df[,2:5])
df1 = df[,1:5]
df1[,2:5] = (df1[,2:5]*5488 + 0.5)/5489
KNN_PBP = df1 %>% group_by(aproovitykk_id) %>% summarise_all(funs(bets_fun2sd)) #bets_fun(.,method = "mme")
dp = merge(KNN_PBP, taks.info, by = "aproovitykk_id", all.x = T)
dp[,2:5] = dp[,2:5] / rowSums(dp[,2:5])
sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4)


############# Tüvemahud landsat ja sentinel kokku ###############

tm.koos = rbind(knn.pbp.lbl.tm, knn.pbp.lbl.land.tm)

df = tm.koos
#df[,2:5] = df[,2:5] / rowSums(df[,2:5])
df1 = df[,1:5]
df1[,2:5] = (df1[,2:5]*5488 + 0.5)/5489
KNN_PBP = df1 %>% group_by(aproovitykk_id) %>% summarise_all(funs(bets_fun2sd)) #bets_fun(.,method = "mme")
dp = merge(KNN_PBP, taks.info, by = "aproovitykk_id", all.x = T)
dp[,2:5] = dp[,2:5] / rowSums(dp[,2:5])

sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4)

#KNN_PBP = knn1[,1:5]; KNN_PBP[,2:5] = (KNN_PBP[,2:5]*5488 + 0.5)/5489
beta.opt.tm = optim(par = c(0.1,0.2), fn = opti.beta, df = df1, lower = c(0,0), upper = c(Inf,Inf),  method = "L-BFGS-B")
beta.opt.tm #see ikka ei tööta korralikult

#teine variant, ehk alguses ei normeeri vektori pikkust üheks:

tm.koos = rbind(knn.pbp.lbl.tm, knn.pbp.lbl.land.tm)

df = tm.koos
#df[,2:5] = df[,2:5] / rowSums(df[,2:5])
df1 = df[,1:5]
df1[,2:5] = (df1[,2:5]*5488 + 0.5)/5489
KNN_PBP = df1 %>% group_by(aproovitykk_id) %>% summarise_all(funs(bets_fun2sd)) #bets_fun(.,method = "mme")
dp = merge(KNN_PBP, taks.info, by = "aproovitykk_id", all.x = T)
dp[,2:5] = dp[,2:5] / rowSums(dp[,2:5])

sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4)
#mean: 0.1776753; epakernel = 0.1678308; beta = 0.1633911; beta2sd: 

 































#landsat ja sentinel kokku
knn.pbp.lbl = data.frame(row.names = c("aproovitykk_id", "MA", "KU", "KS", "MUU", "kp"))
data0 = read.csv(file = "sentinel455.csv")
kps = unique(data0$kp)
for(i in 1:15){
  kp = kps[i]
  #siin tekib järjekorraga probleem!?
  #kp.df = Reduce(function(x, y) merge(x, y, all=TRUE, by = "aproovitykk_id"), list(RESULTS.KP.LIIK[[i]][[1]], RESULTS.KP.LIIK[[i]][[2]], RESULTS.KP.LIIK[[i]][[3]]),RESULTS.KP.LIIK[[i]][[4]])
  kp.df = data.frame(aproovitykk_id = RESULTS.KP.LIIK[[i]][[1]][2])
  for(j in 1:4){
    kp.df[,j+1] = RESULTS.KP.LIIK[[i]][[j]][1]
  }
  names(kp.df) = c("aproovitykk_id","MA", "KU", "KS", "MUU"); 
  kp.df$kp = kp #as.Date(kp)
  knn.pbp.lbl = rbind(knn.pbp.lbl, kp.df)
}
knn.pbp.lbl$satel = "sentinel"

knn.pbp.lbl.land = data.frame(row.names = c("aproovitykk_id", "MA", "KU", "KS", "MUU", "kp"))
data0 = read.csv(file = "landsat455.csv")
kps = unique(data0$kp)
for(i in 1:9){
  kp = kps[i]
  #siin tekib järjekorraga probleem!?
  #kp.df = Reduce(function(x, y) merge(x, y, all=TRUE, by = "aproovitykk_id"), list(RESULTS.KP.LIIK[[i]][[1]], RESULTS.KP.LIIK[[i]][[2]], RESULTS.KP.LIIK[[i]][[3]]),RESULTS.KP.LIIK[[i]][[4]])
  kp.df = data.frame(aproovitykk_id = RESULTS.KP.LIIK.LANDSAT[[i]][[1]][2])
  for(j in 1:4){
    kp.df[,j+1] = RESULTS.KP.LIIK.LANDSAT[[i]][[j]][1]
  }
  names(kp.df) = c("aproovitykk_id","MA", "KU", "KS", "MUU"); 
  kp.df$kp = kp #as.Date(kp)
  knn.pbp.lbl.land = rbind(knn.pbp.lbl.land, kp.df)
}
knn.pbp.lbl.land$satel = "landsat"

knn.pbp.lbl.ls = rbind(knn.pbp.lbl, knn.pbp.lbl.land)
knn.pbp.lbl.ls$kpsatel = paste(knn.pbp.lbl.ls$kp, knn.pbp.lbl.ls$satel)
#save(knn.pbp.lbl.ls, file = "knn_pbp_lbl_landsat_sentinel.RData")


hist(rowSums(knn.pbp.lbl.ls[,2:5]))

#kaks võimalikku lähenemist:
#esmalt rea summad üheks ja siis agregeerida või vastupidi

#1. ja igavam:

df = knn.pbp.lbl.ls
df[,2:5] = df[,2:5] / rowSums(df[,2:5])


df1 = df[,1:5]
#df1[,2:5] = (df1[,2:5]*5488 + 0.5)/5489
KNN_PBP = df1 %>% group_by(aproovitykk_id) %>% summarise_all(funs(mean)) #bets_fun(.,method = "mme")
dp = merge(KNN_PBP, taks.info, by = "aproovitykk_id", all.x = T)
dp[,2:5] = dp[,2:5] / rowSums(dp[,2:5])

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

sqrt(mean((dp[,11]-dp[,2])**2))
sqrt(mean((dp[,12]-dp[,3])**2))
sqrt(mean((dp[,13]-dp[,4])**2))
sqrt(mean((dp[,14]-dp[,5])**2))
sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4)

#1. lähenemine mean: 0.1743427
#1. lähenemine beta: 0.1615959
#1. lähenemine beta2sd: 0.1627827
#1. lähenemine epa kernel: 0.1685174

#mean ja beta koos:
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


#2. lähenemine:
#df = knn.pbp.lbl.ls #tüvemahud
df = knn.pbp.lbl.ls #proportsioonid

df1 = df[,1:5]
df1[,2:5] = (df1[,2:5]*5488 + 0.5)/5489
df1[,2:5] = df1[,2:5] / rowSums(df1[,2:5])
KNN_PBP = df1 %>% group_by(aproovitykk_id) %>% summarise_all(funs(bets_fun2sd)) #bets_fun(.,method = "mme")
dp = merge(KNN_PBP, taks.info, by = "aproovitykk_id", all.x = T)
dp[,2:5] = dp[,2:5] / rowSums(dp[,2:5])

sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4)
#mean 0.1745652 (kas teoreetiliselt peaks jääma samaks?)
#epa kernel: 0.165677
#beta 0.1629532
#beta2sd 0.1632841

###############################

df.sl = knn.pbp.lbl.ls
df.sl$kp.satel = df.sl$kpsatel
kpsatels = unique(df.sl$kp.satel)


list.kpsatel = vector("list", length=length(kpsatels)) #see pm 1.5gb...
for(i in 1:length(kpsatels)){
  list.kpsatel[[i]] = combn(kpsatels,i)
}





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

save(veadNpilt.epa.sample, file = "veadNpilt_epa_sampleknnpbplbl.RData")


veadNpilt.beta.sample = c()
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
    veadNpilt.beta.sample[i] = mean(vead)
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
    veadNpilt.beta.sample[i] = mean(vead)
  }
  print(veadNpilt.beta.sample)
}

save(veadNpilt.beta.sample, file = "veadNpilt_beta_sampleknnpbplbl.RData")

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

save(veadNpilt.mean.sample, file = "veadNpilt_mean_sampleknnpbplbl.RData")


par(mfrow=c(1,3))
plot(unlist(veadNpilt.mean.sample), type = "o", ylab = "RMSE: mean", xlab = "Hinnangus kasutatud pilte", ylim = c(0.164,0.235))
plot(unlist(veadNpilt.beta.sample), type = "o", ylab = "RMSE: beta", xlab = "Hinnangus kasutatud pilte", ylim = c(0.164,0.235))
plot(unlist(veadNpilt.epa.sample), type = "o", ylab = "RMSE: Epanechnikov kernel", xlab = "Hinnangus kasutatud pilte", ylim = c(0.164,0.235))



#piltide väljajätmine:


kps0 = kpsatel
min0 = 69; min = 0.1743 #; min = 0
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
    df[,2:5] = df[,2:5] / rowSums(df[,2:5])
    df[,2:5] = (df[,2:5]*(dim(df)[1]-1) + 0.5)/dim(df)[1]
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
min(out.rmse); length(out)
#mean 0.1632502; 16 out
#epa: 0.1604268 9 out
#beta 0.1577976 9 out
#beta2sd  0.1581205 6 out
#need olid nn 2. variandiga, kus esmalt pold vektori pikkus normeeritud

#nüüd
#mean: 0.1633, 16 
#beta 0.1556, 11
#beta2sd 0.1559, 11
#epa:

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




