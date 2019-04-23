#knn_koos uuesti

data = read.csv(file = "d506_100.csv")
vars = names(data)[c(3:38,47:60)]
sidxx = data$aproovitykk_id
tvmaht = 2 #ehk siis proportsioonid!
data_puud = taks.info[taks.info$aproovitykk_id %in% sidxx,]
puud_true =  data_puud[,7:10]
data_puud = data_puud[,(4*tvmaht-1):(4*tvmaht+2)]




##########################################################
bestvars.koos.455 = fun_bestvars(1,25,sidxx, kernel = epa)
##########################################################
#save(bestvars.koos.455,file = "bestvars_koos_455.RData")

vark = bestvars.koos.455[[1]]
rss = unlist(bestvars.koos.455[[2]])

rss.opt = c()
w.opt = vector("list", length=25)
for(k in 1:25){
  print(Sys.time())
  opt.k = optim(par = rep(1, length(vark[[k]])), fn = fun_opti, k = k, vars = vark[[k]], data = data, sidxx = sidxx,method = "BFGS", kernel = epa)
  w.opt[[k]] = opt.k$par
  rss.opt[k] = opt.k$value
}

#save(w.opt, file = "bestvars_koos_455_optweights.RData")
load(file = "bestvars_koos_455_optweights.RData", verbose = T)

par(mfrow = c(1,1))
plot(rss, type = "o", xlab = "Naabreid", ylab = "RMSE", ylim = c(0.16,0.23)) #RMSE: root-mean-squared-error
points(x=c(1:25), y=rss.opt, lty=1, col = "red", pch = 19)
lines(x=c(1:25), y=rss.opt, lty=3, col = "red")


best_vars = vark[[5]]
wgt = w.opt[[5]]
dex = data[,c("aproovitykk_id",best_vars)]
dex = dex[dex$aproovitykk_id %in% sidxx,]
dex[,-1] = t((t(as.matrix(dex[,-1])))*wgt)
dex$cl = "cl"
knn455.prop.epa.k5 = fun_agre_kernel(dex, data_puud, k = 5, sid = sidxx, kernel = epa)
#save(knn455.prop.epa.k5, file = "knn455_k5_props.RData")

dp = merge(knn455.prop.epa.k5, taks.info, all.x = T, by = "aproovitykk_id")

dev.off()
par(mfrow = c(2,2))
plot(dp[,11],dp[,2], xlab = "Mänd", ylab = "Hinnang", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.2), pch = 16)
abline(lm(dp[,2] ~ dp[,11]))
plot(dp[,12],dp[,3], xlab = "Kuusk", ylab = "Hinnang", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.2), pch = 16)
abline(lm(dp[,3] ~ dp[,12]));
plot(dp[,13],dp[,4], xlab = "Kask", ylab = "Hinnang", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.2), pch = 16)
abline(lm(dp[,4] ~ dp[,13]));
plot(dp[,14],dp[,5], xlab = "Muu", ylab = "Hinnang", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.2), pch = 16)
abline(lm(dp[,5] ~ dp[,14]));

#

sqrt(mean((dp[,11]-dp[,2])**2))
sqrt(mean((dp[,12]-dp[,3])**2))
sqrt(mean((dp[,13]-dp[,4])**2))
sqrt(mean((dp[,14]-dp[,5])**2))
sqrt((sum((dp[,11:14]-dp[,2:5])**2))/dim(dp)[1]/4)

#kuidas tunnuste arv teiste k- korral
#plottida tunnustearv
nrvar = c()
for(i in 1:25){
  nrvar[i] = length(vark[[i]])
}

par(mfrow = c(1,1))
par(mar = c(5, 4, 4, 4) + 0.3)
plot(rss, type = "o", xlab = "Naabreid", ylab = "RMSE", ylim = c(0.16,0.23)) #RMSE: root-mean-squared-error
points(x=c(1:25), y=rss.opt, lty=1, col = "red", pch = 19)
lines(x=c(1:25), y=rss.opt, lty=3, col = "red")
par(new = TRUE)
plot(x = 1:25, y = nrvar,type = "o", axes = FALSE, bty = "n", xlab = "", ylab = "", col = "blue", pch = 19, ylim = c(0,15))
axis(side=4, at = 0:15)
mtext("Tunnuste arv", side=4, line=3)






######### kõik uuesti natuke parandatud landsat-to-sentinel mudeliga #######
#kui vahe on marginaalne, siis jääb nii
require(FNN)
data0 = read.csv(file = "d506_100.csv")
load(file = "lnds_to_sent_linear_model_16_04_2019.RData")
load(file = "sid455.RData")
load(file = "taks_info.RData")
sidxx = sid455

dk[,2:37] = scale(dk[,2:37])
data = dk[dk$aproovitykk_id %in% sidxx,]
data[,38:59] = data0[,39:60]
#save(data, file = "koond455_uus_landsat_to_sentinel_mudel.RData")
#kohati päris suured vahed, max 6 pärast standardiseerimist :O

vars = names(data)[c(2:37,46:59)]
tvmaht = 2 #ehk siis proportsioonid!
data_puud = taks.info[taks.info$aproovitykk_id %in% sidxx,]
puud_true =  data_puud[,7:10]
data_puud = data_puud[,(4*tvmaht-1):(4*tvmaht+2)]

bestvars.koos.455.uus.landsat.to.sentinel = fun_bestvars(1,25,sidxx, kernel = epa)
##########################################################
save(bestvars.koos.455.uus.landsat.to.sentinel,file = "bestvars_koos_455_uus_landsat_to_sentinel.RData")

vark = bestvars.koos.455.uus.landsat.to.sentinel[[1]]
rss = unlist(bestvars.koos.455.uus.landsat.to.sentinel[[2]])

rss.opt = c()
w.opt = vector("list", length=25)
for(k in 1:25){
  print(Sys.time())
  opt.k = optim(par = rep(1, length(vark[[k]])), fn = fun_opti, k = k, vars = vark[[k]], data = data, sidxx = sidxx,method = "BFGS", kernel = epa)
  w.opt[[k]] = opt.k$par
  rss.opt[k] = opt.k$value
}

plot(rss.opt)
save(w.opt, file = "bestvars_koos_455_uus_landsat_to_sentinel_optweights.RData")



########### aga mis oleks, kui juba knn agregeerimisfaasis ei kasutaks mitte kaalutud keskmist,
#vaid tuumafnktsiooni või betajaotust

fun_agre_kernel = function(data, data_puud, k, sid, kernel = epa, agre = agre.epa){
  sid00 = data$aproovitykk_id
  kk = k+1 #"epa" tahab ühe võrra pikemat vektorit?
  dists = knn.cv(train = data[,2:(dim(data)[2]-1)], cl = data$cl, k = kk)
  dist1 = attr(dists,"nn.dist")
  dist1[is.na(dist1)] = 0
  #dist1 = dist1 + 1e-5 #et kaugused ei saaks 0 olla. on see vajalik?
  index1 = attr(dists,"nn.index")
  props = apply(dist1, 1, kernel)
  props = t(props)
  indxprops = cbind(index1, props)
  tbr = t(apply(indxprops, 1, agre, data_puud))
  tbr = tbr / rowSums(tbr)
  tbr = data.frame(tbr); tbr$aproovitykk_id = sid00
  tbr
}

agre.epa <- function(arg,data_puud){
  kk = length(arg) / 2
  indx = arg[1:kk]; props = arg[(kk+1):(2*kk)]
  estimations = data_puud[indx,] #*props
  est.epa = t(apply(estimations, 2, epa.kernel.w, weights = props))
  #print(dim(est.epa));print(est.epa);print(sum(est.epa[1,]))
  if(sum(est.epa[1,]) == 0){est.epa = c(0.25,0.25,0.25,0.25)}
  est.epa
}

agre.beta <- function(arg,data_puud){
  kk = length(arg) / 2
  indx = arg[1:kk]; props = arg[(kk+1):(2*kk)]
  estimations = data_puud[indx,] #*props
  est.beta = t(apply(estimations, 2, beta.w, weights = props))
  #print(est.beta);print(props);print(estimations)
  #print(dim(est.epa));print(est.epa);print(sum(est.epa[1,]))
  if(sum(est.beta[1,]) == 0){est.beta = c(0.25,0.25,0.25,0.25)}
  est.beta
}


epa.kernel.w = function(vec, kernel = "epanechnikov", weights){
  #print(vec)
  vec = vec[-length(vec)]
  weights = weights[-length(weights)]
  weights[is.na(weights)] = 1
  
  #siin valiku koht, millised peavad kaalud välja nägema. EPA-ga see ei tööta!
  weights = weights / max(weights)
  
  
  if(length(vec) < 2){max = vec}
  else{
    if(length(vec[weights >0]) < 2){max = mean(vec)}
    else{
      d = density(vec, kernel = "epanechnikov", from = 0, to = 1, weights = weights)
      max = d$x[which.max(d$y)]
        }
    }
  max
}

#require(RPMM)
beta.w = function(vec, weights){
  #print(vec)
  vec = vec[-length(vec)]
  weights[is.na(weights)] = 0
  weights = weights[-length(weights)]
  weights = weights/max(weights)
  #weights = weights / sum(weights)
  if(length(vec) < 2){max = vec}
  else{
    if(length(vec[weights >0]) < 2){max = sum(vec*weights)}
    else{
      #weights = c(0.31496338, 0.22639652, 0.20455069, 0.17876645, 0.07532296)
      #vec = c(0,.65,0,0,0)
      #vec = c(.1,0,0,.3,0,.8)
      #vec = c(.5,.4,.2,.5,.4)
      be = betaEst(y = vec, weights = weights, w = rep(1/length(vec), length(vec))) #mis on posterior weigths?
      print(be);print(vec)
      if(be[1] == 1 & be[2] == 1){max = sum(vec*weights)}
      else{
        opt = optimize(interval = c(0,1), dbeta, shape1 = be[1], shape2 = be[2], maximum = T)
        max = opt$`maximum`
      }
      if((be[1] < 1 & be[2] < 1)){
        max = sum(vec*weights)
      }
    }
  }
  max
}

cc = c(0,1,0.2,0.3);
cc = matrix(rexp(n = 4*6), nrow = 4,ncol = 6); cc = cc/colSums(cc)
ww = c(0.5,0.000045,0.1,0.1)
ww = rep(0.25,4)
betaEstMultiple(cc,ww)
ebeta(cc[,6])
eba = betaEst(ww, w = rep(1,4), weights = ww)
#võtta posteriorid lihstalt 1?

#######################################################

data = read.csv(file = "d506_100.csv")
vars = names(data)[c(3:38,47:60)]
sidxx = data$aproovitykk_id
tvmaht = 2 #ehk siis proportsioonid!
data_puud = taks.info[taks.info$aproovitykk_id %in% sidxx,]
puud_true =  data_puud[,7:10]
data_puud = data_puud[,(4*tvmaht-1):(4*tvmaht+2)]


##########################################################
#bestvars.koos.455.agre.epa = fun_bestvars(1,25,sidxx, kernel = epa, agre = agre.epa)
##########################################################

#save(bestvars.koos.455.agre.beta,file = "bestvars_koos_455_agre_beta.RData")
bestvars.koos.455.agre.tava.beta = fun_bestvars(10,25,sidxx, kernel = tava, agre = agre.beta)
#save(bestvars.koos.455.agre.tava.beta,file = "bestvars_koos_455_agre_tava_beta.RData")

rss = unlist(bestvars.koos.455.agre.beta[[2]]); plot(rss, type = "o")
rss = unlist(bestvars.koos.455.agre.tava.beta[[2]]); plot(rss, type = "o")

#võtta w suhtena max kaalu!???


#save(bestvars.koos.455.agre.tava.epa,file = "bestvars_koos_455_agre_tava_epa.RData")

vark = bestvars.koos.455.agre.beta[[1]][[10]] #7. ehk tegelt k = 16 parim
opt.epa_kaugus.beta_agre = optim(par = rep(1, length(vark)), fn = fun_opti, k = 19, vars = vark, data = data, sidxx = sidxx,method = "BFGS", kernel = epa, agre = agre.beta)
#0.1718480 --->>> 0.1664395
#opt.epa.beta16 [1] 0.1664395
#lollakas, mul algab ju k = 10-st...

#kuidas parim välja näeb?
wgt = opt.epa.beta16$par
best_vars = bestvars.koos.455.agre.beta[[1]][[7]]
dex = data[,c("aproovitykk_id",best_vars)]
dex = dex[dex$aproovitykk_id %in% sidxx,]
dex[,-1] = t((t(as.matrix(dex[,-1])))*wgt)
dex$cl = "cl"

test  = fun_agre_kernel(dex, data_puud, k = 16, sid = sidxx, kernel = epa, agre = agre.beta)
dp = merge(test, taks.info, all.x = T, by = "aproovitykk_id")
par(mfrow=C(2,2))
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
#0.1780016 --->>> opt 0.1763254
#0.1785899 --->>> opt 0.1760782 tava16

#k = 9
vark = bestvars.koos.455.agre.tava.epa[[1]][[16]]
vark20 = bestvars.koos.455.agre.tava.epa[[1]][[20]]
#opt9 = optim(par = rep(1, length(vark)), fn = fun_opti, k = k, vars = vark, data = data, sidxx = sidxx,method = "BFGS", kernel = epa)
opt.tava.16 = optim(par = rep(1, length(vark)), fn = fun_opti, k = 16, vars = vark, data = data, sidxx = sidxx,method = "BFGS", kernel = tava)
opt.tava.20 = optim(par = rep(1, length(vark20)), fn = fun_opti, k = 20, vars = vark20, data = data, sidxx = sidxx,method = "BFGS", kernel = tava)

#muutsin epa-s kaalud ära kaalud = kaalud / max(kaalud)

test16 = fun_bestvars(16,16,sidxx, kernel = tava, agre = agre.epa)










fun_bestvars = function(k1, k2, sidxx, kernel, varsw = vars, ws = rep(1, length(vars)), agre = agre){ #
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
        H_puu = fun_agre_kernel(dex, data_puud, k = k, sid = sidxx, kernel = kernel, agre = agre)
        rsdls = H_puu[,1:4] - puud_true
        rss[j] = sqrt((sum(rsdls**2))/(length(sidxx)*4))
        #print(vars00)
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

tava = function(vec){
  vec = vec[-length(vec)] #annab ühe võrra pikema argumendi ette, kuna Epa-l on nii vaja
  props = 1/vec
  props1 = props/sum(props)
  if(sum(props) == 0){
    props1[1] = 1; props1[2:length(vec)] = 0
  }
  props1 = c(props1,0)
}

epa.kernel = function(vec, kernel = "epanechnikov"){
  vec = vec[-length(vec)]
  if(length(vec) < 3){max = mean(vec)}
  else{
    d = density(vec, kernel = "epanechnikov", from = 0, to = 1)
    max = d$x[which.max(d$y)]
    }
  max
}


epa = function(vec){
  props = 3/4*(1-(vec / vec[length(vec)])**2)
  props[is.na(props)] = 69
  props1 = props/sum(props)
  if(length(props) == 1){props1[1] = 1}
  if(sum(props) == 0){props1[1] = 1; props1[2:length(vec)] = 0}
  props1
}

fun_opti = function(w,k,vars,data, sidxx, kernel, agre = agre){
  dex = data[,c("aproovitykk_id",vars)]
  dex = dex[dex$aproovitykk_id %in% sidxx,]
  #print(dim(dex)); print(length(w))
  dex[,-1] = t((t(as.matrix(dex[,-1])))*w)
  dex$cl = "cl"
  #data_puud = taks.info[taks.info$aproovitykk_id %in% sidxx,]
  #data_puud = data_puud[,(4*tvmaht-1):(4*tvmaht+2)] #vastavalt kas proportsioonid või tüvemahud
  
  H_puu = fun_agre_kernel(dex, data_puud, k = k, sid = sidxx, kernel, agre = agre)
  #print(dim(H_puu))
  rsdls = H_puu[,1:4] - puud_true
  rss = sqrt((sum(rsdls**2))/(length(sidxx)*4))
  rss
}



