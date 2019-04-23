#KNN_liigikaupa_455

optix = function(varlist, j1, j2, k1, k2, method = "BFGS", kernel = epa, data){
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
  H_puu = fun_agre_liik(dex, data_puud, k = k, sid = sidxx, liik = liik, kernel = kernel)
  rsdls = H_puu[,1] - puud_true[,liik]
  rmse = sqrt((sum(rsdls**2))/(length(sidxx)))
  rmse
}


fun_agre_liik = function(data, data_puud, k, sid, kernel = epa, liik)
{
  sid00 = data$aproovitykk_id
  kk = k+1
  dists = knn.cv(train = data[,2:(dim(data)[2]-1)], cl = data$cl, k = kk)
  dist1 = attr(dists,"nn.dist")
  dist1[is.na(dist1)] = 0
  dist1 = dist1 + 1e-5 #et kaugused ei saaks 0 olla. on see vajalik?
  index1 = attr(dists,"nn.index")
  props = apply(dist1, 1, kernel)
  props = t(props)
  indxprops = cbind(index1, props)
  tbr = t(apply(indxprops, 1, agre, data_puud))
  tbr = tbr / rowSums(tbr);tbr = tbr[,liik]
  tbr = data.frame(tbr); tbr$aproovitykk_id = sid00
  tbr
}



fun_bestvars_liik = function(liigid, k1, k2, sidxx, kernel, varsw = vars, ws = rep(1, length(vars))){
  lst_return0 = vector("list", length = length(liigid))
  for(liik in liigid){
    list_return = vector("list", length = 2)
    var_return = vector("list", length = 1+k2-k1)
    rss_return = vector("list", length = 1+k2-k1)
    for(k in k1:k2){
      print(k);print(Sys.time())
      vars0 = vars
      vars1 = c()
      mx = 6969
      vahe = 696
      while(vahe > 0){
        mx0 = mx
        vars00 = c()
        rss = c()
        for(j in 1:length(vars0)){
          vars00 = c(vars1,vars0[j])
          dex = data[,c("aproovitykk_id",vars00)]
          dex = dex[dex$aproovitykk_id %in% sidxx,]
          dex$cl = "cl"
          H_puu = fun_agre_liik(dex, data_puud, k = k, sid = sidxx, kernel = kernel)
          rsdls = H_puu[,liik] - puud_true[,liik]
          rss[j] = sqrt((sum(rsdls**2))/(length(sidxx)))
        }
        mx0 = min(rss)
        varmin = vars0[which.min(rss)]
        vars0 = vars0[!(vars0 %in% varmin)]
        vars1 = c(vars1,varmin)
        vahe = mx-mx0
        mx = mx0
      }
      var_return[[1+k-k1]] =  vars1
      rss_return[[1+k-k1]] =  mx
      print(mx)
    }
    list_return[[1]] = var_return;list_return[[2]] = rss_return
    lst_return0[[liik]] = list_return
  }
  lst_return0
}

############################################################

d506.100 = read.csv(file = "d506_100.csv")
data = d506.100
vars = names(data)[c(2:37,46:59)]
sidxx = d506.100$aproovitykk_id
tvmaht = 2 #ehk siis proportsioonid!
data_puud = taks.info[taks.info$aproovitykk_id %in% sidxx,]
puud_true =  data_puud[,7:10]
data_puud = data_puud[,(4*tvmaht-1):(4*tvmaht+2)]


knn.liigiti = fun_bestvars_liik(liigid = c(1:4), k1 = 1, k2 = 25, sidxx, kernel = epa, varsw = vars)
#save(knn.liigiti, file = "KNN_lbl_enne_optimeerimist.RData")
load(file = "KNN_lbl_enne_optimeerimist.RData", verbose = T)

ma = unlist(knn.liigiti[[1]][[2]])
ku = unlist(knn.liigiti[[2]][[2]])
ks = unlist(knn.liigiti[[3]][[2]])
mu = unlist(knn.liigiti[[4]][[2]])
par(mfrow=c(2,2))
plot(ma, type = "o", ylim = c(.15,.21))
plot(ku, type = "o", ylim = c(.15,.21))
plot(ks, type = "o", ylim = c(.15,.21))
plot(mu, type = "o", ylim = c(.15,.21))

varlist = vector("list",4)
varlist[[1]] = knn.liigiti[[1]][[1]];varlist[[2]] = knn.liigiti[[2]][[1]];varlist[[3]] = knn.liigiti[[3]][[1]];varlist[[4]] = knn.liigiti[[4]][[1]];

Sys.time()
opti.lbl.koik = optix(varlist,1,4,1,25, method = "BFGS", data = data)
Sys.time()
#save(opti.lbl.koik, file = "KNN_lbl_opt_tulemused")
load(file = "KNN_lbl_opt_tulemused", verbose = T)
# 
# opti.lbl.ku = optix(knn.liigiti[[2]][[1]],2,2,1,25, method = "BFGS", data = data)
# Sys.time()
# opti.lbl.ks = optix(knn.liigiti[[3]][[1]],3,3,1,25, method = "BFGS", data = data)
# Sys.time()
# opti.lbl.mu = optix(knn.liigiti[[4]][[1]],4,4,1,25, method = "BFGS", data = data)
# Sys.time()




par(mfrow = c(2,2))
plot(ma, type = "o", xlab = "Naabreid: mänd", ylab = "RMSE", ylim = c(0.145,0.23))
points(x=c(1:25), y=unlist(opti.lbl.koik[[1]]), lty=1, col = "red", pch = 19)
lines(x=c(1:25), y=unlist(opti.lbl.koik[[1]]), lty=3, col = "red")
plot(ku, type = "o", xlab = "Naabreid: kuusk", ylab = "RMSE", ylim = c(0.145,0.23))
points(x=c(1:25), y=unlist(opti.lbl.koik[[2]]), lty=1, col = "red", pch = 19)
lines(x=c(1:25), y=unlist(opti.lbl.koik[[2]]), lty=3, col = "red")
plot(ks, type = "o", xlab = "Naabreid: kask", ylab = "RMSE", ylim = c(0.145,0.23))
points(x=c(1:25), y=unlist(opti.lbl.koik[[3]]), lty=1, col = "red", pch = 19)
lines(x=c(1:25), y=unlist(opti.lbl.koik[[3]]), lty=3, col = "red")
plot(mu, type = "o", xlab = "Naabreid: muu", ylab = "RMSE", ylim = c(0.145,0.23))
points(x=c(1:25), y=unlist(opti.lbl.koik[[4]]), lty=1, col = "red", pch = 19)
lines(x=c(1:25), y=unlist(opti.lbl.koik[[4]]), lty=3, col = "red")

#parim arv naabreid:

parim.arv.naabreid = function(lst,varlist,krange){
  MSRE = c()
  for(k in krange[1]:krange[2]){
    dp0 = matrix(NA,nrow = 455, ncol = 5)
    dp0[,1] = sidxx
    for(j in 1:4){
      varsx = varlist[[j]][[k]]
      k = k; wgt = lst[[j+4]][[k]]
      dex = data[,c("aproovitykk_id",varsx)]
      dex[,-1] = t((t(as.matrix(dex[,-1])))*wgt)
      dex$cl = "cl"
      dp.liik = fun_agre_liik(data = dex, data_puud, k, sid, kernel = epa, liik =j)
      dp0[,j+1] = dp.liik[,1]
      
    }
    dp0[rowSums(dp0[,2:5]) == 0,2:5] = c(0.25,0.25,0.25,0.25)
    dp0[,2:5] = dp0[,2:5] / rowSums(dp0[,2:5])
    dp0 = data.frame(dp0);names(dp0) = c("aproovitykk_id", "MA", "KU", "KS", "MUU")
    dp = merge(dp0, taks.info, by = "aproovitykk_id", all.x = T)
    msre = sqrt((sum((dp[,11:14]-dp[,2:5])**2))/(4*455))
    MSRE[k] = mrse
  }
  MSRE
}


parimK = parim.arv.naabreid(opti.lbl.koik, varlist, c(1,25))

parimK
par(mfrow = c(1,1))
plot(parimK, type = "o",xlab = "Naabreid", ylab = "RMSE")
#11 on parim arv

dp0 = matrix(NA,nrow = 455, ncol = 5)
dp0[,1] = sidxx
k = 11
for(j in 1:4){
  varsx = varlist[[j]][[k]]
  k = k; wgt = lst[[j+4]][[k]]
  dex = data[,c("aproovitykk_id",varsx)]
  dex[,-1] = t((t(as.matrix(dex[,-1])))*wgt)
  dex$cl = "cl"
  dp.liik = fun_agre_liik(data = dex, data_puud, k, sid, kernel = epa, liik =j)
  dp0[,j+1] = dp.liik[,1]
  
}
dp0[rowSums(dp0[,2:5]) == 0,2:5] = c(0.25,0.25,0.25,0.25)
dp0[,2:5] = dp0[,2:5] / rowSums(dp0[,2:5])
dp0 = data.frame(dp0);names(dp0) = c("aproovitykk_id", "MA", "KU", "KS", "MUU")
dp = merge(dp0, taks.info, by = "aproovitykk_id", all.x = T)

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

sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4)



#optimeerime ära, et oleks ilus :)
#see on mul ju tehtud, lollakas!
rmse.opt455 = c()
w.opt455 = vector("list", length=25)
for(k in 1:25){
  print(Sys.time())
  opt.k = optim(par = rep(1, length(vark[[k]])), fn = fun_opti_liik1, k = k, vars = varlist, data = data, sidxx = sidxx,method = "BFGS", kernel = epa)
  rmse.opt455[[k]] = opt.k$par
  w.opt455[k] = opt.k$value
}

#ehk sama arv naabreid optimeerimine
fun_opti_liik1 = function(wlst, k, varlist, data, sidxx, kernel){
  dp0 = matrix(NA,nrow = 455, ncol = 5)
  dp0[,1] = sidxx
  for(j in 1:4){
    varsx = varlist[[j]][[k]]
    k = k; wgt = wlst[[j]]
    dex = data[,c("aproovitykk_id",varsx)]
    dex[,-1] = t((t(as.matrix(dex[,-1])))*wgt)
    dex$cl = "cl"
    dp.liik = fun_agre_liik(data = dex, data_puud, k, sid, kernel = epa, liik =j)
    dp0[,j+1] = dp.liik[,1]
  }
  dp0[rowSums(dp0[,2:5]) == 0,2:5] = c(0.25,0.25,0.25,0.25)
  dp0[,2:5] = dp0[,2:5] / rowSums(dp0[,2:5])
  dp0 = data.frame(dp0);names(dp0) = c("aproovitykk_id", "MA", "KU", "KS", "MUU")
  dp = merge(dp0, taks.info, by = "aproovitykk_id", all.x = T)
  msre = sqrt((sum((dp[,11:14]-dp[,2:5])**2))/(4*455))
  msre
}

#kui igal liigil lubada erinev arv naabreid:
#10 000
load(file = "KNN_lbl_enne_optimeerimist.RData", verbose = T)

#tulem = data.frame();names(tulem) = c("ma","ku", "ks", "muu")
tulem = matrix(NA, nrow = 0, ncol = 5)

for(i in 5:14){
  for(j in 5:14){
    for(h in 5:14){
      for(e in 5:14){
        dp0 = matrix(NA,nrow = 455, ncol = 5)
        dp0[,1] = sidxx
        ks = c(i,j,h,e)
        for(l in 1:4){
          k = ks[l]
          varsx = knn.liigiti[[l]][[1]][[k]]
          wgt = opti.lbl.koik[[l+4]][[k]]
          dex = data[,c("aproovitykk_id",varsx)]
          dex[,-1] = t((t(as.matrix(dex[,-1])))*wgt)
          dex$cl = "cl"
          dp.liik = fun_agre_liik(data = dex, data_puud, k = k, sid, kernel = epa, liik =l)
          dp0[,l+1] = dp.liik[,1]
        }
        dp0[rowSums(dp0[,2:5]) == 0,2:5] = c(0.25,0.25,0.25,0.25)
        dp0[,2:5] = dp0[,2:5] / rowSums(dp0[,2:5])
        dp0 = data.frame(dp0);names(dp0) = c("aproovitykk_id", "MA", "KU", "KS", "MUU")
        dp = merge(dp0, taks.info, by = "aproovitykk_id", all.x = T)
        rmse = sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4)
        t0 = c(i,j,h,e,rmse)
        tulem = rbind(tulem,t0)
      }
    }
  }
}


which(tulem == min(tulem), arr.ind = TRUE)
tulem[4267,]

tulem = tulem[order(tulem[,1]),]





















