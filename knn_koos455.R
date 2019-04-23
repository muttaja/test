#knn_koos uuesti
setwd("A:/MAKA/TEST/test")
data = read.csv(file = "d506_100.csv")
vars = names(data)[c(3:38,47:60)]
sidxx = data$aproovitykk_id
tvmaht = 2 #ehk siis proportsioonid!
data_puud = taks.info[taks.info$aproovitykk_id %in% sidxx,]
puud_true =  data_puud[,7:10]
data_puud = data_puud[,(4*tvmaht-1):(4*tvmaht+2)]




##########################################################
#bestvars.koos.455 = fun_bestvars(1,25,sidxx, kernel = epa)
##########################################################
#save(bestvars.koos.455,file = "bestvars_koos_455.RData")
load(file = "bestvars_koos_455.RData", verbose = T)

vark = bestvars.koos.455[[1]]
rss = unlist(bestvars.koos.455[[2]])

# rss.opt = c()
# w.opt = vector("list", length=25)
# for(k in 1:25){
#   print(Sys.time())
#   opt.k = optim(par = rep(1, length(vark[[k]])), fn = fun_opti, k = k, vars = vark[[k]], data = data, sidxx = sidxx,method = "BFGS", kernel = epa)
#   w.opt[[k]] = opt.k$par
#   rss.opt[k] = opt.k$value
# }

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
#save(w.opt, file = "bestvars_koos_455_uus_landsat_to_sentinel_optweights.RData")
#kirjutasin kahjuks üle



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
  if(any(is.na(est.epa))|sum(est.epa[1,]) == 0){est.epa = c(0.25,0.25,0.25,0.25)}
  est.epa
}

agre.beta <- function(arg,data_puud){
  kk = length(arg) / 2
  indx = arg[1:kk]; props = arg[(kk+1):(2*kk)]
  estimations = data_puud[indx,] #*props
  est.beta = t(apply(estimations, 2, beta.w, weights = props))
  if(any(is.na(est.beta))|sum(est.beta[1,]) == 0){est.beta = c(0.25,0.25,0.25,0.25)}
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
require(EnvStats)
require(RPMM)
beta.w = function(vec, weights){
  #print(vec)
  vec = vec[-length(vec)]
  #weights[is.na(weights)] = 1
  weights = weights[-length(weights)]
  weights = weights/sum(weights)
  #weights = weights / sum(weights)
  if(length(vec) < 2){max = vec}
  else{
    if(length(vec[weights >0]) < 2){max = sum(vec*weights)}
    else{
      max = tryCatch(try.beta(vec = vec, weights = weights), error=function(err) sum(vec*weights))
    }
  }
  max
}


try.beta = function(vec, weights){
  be = est.beta(vec = vec, ws = weights)
  if(be$par[1] == 1 & be$par[2] == 1){max = sum(vec*weights)}
  else{
    opt = optimize(interval = c(0,1), dbeta, shape1 = be$par[1], shape2 = be$par[2], maximum = T)
    max = opt$`maximum`;
  }
  if((be$par[1] < 1 & be$par[2] < 1)){
    max = sum(vec*weights)
  }
  max
}


cc = c(0,0.1,0.2,0.3);
cc = matrix(rexp(n = 4*6), nrow = 4,ncol = 6); cc = cc/max(cc)
ww = c(0.5,0.000045,0.1,0.1)
ww = rep(1,4)
betaEstMultiple(cc,ww)
ebeta(cc[,2])
eba = betaEst(cc, w = rep(1,4), weights = ww)
#võtta posteriorid lihstalt 1?

#tuleb ikka ise kirjutada1?

lf.beta = function(D,x,w){
  #D = c(,2); x = cc[,2] -0.05 ;w = rep(1,length(x))
  #x = cc[,2]
  x = (x*698+0.5)/699
  a = D[1];b=D[2];n = length(x)
  w = w/sum(w)*n
  #n*log(gamma(a+b)) - n*log(gamma(a)) - n*log(gamma(b))+(a-1)*sum(log(x*w)) + (b-1)*sum(log(1-x*w))
  #loglik = n*log(gamma(a+b)) - n*log(gamma(a)) - n*log(gamma(b)) + sum(log((w*((x)**(a-1))))) + sum(log((w*((1-x)**(b-1)))))
  loglik = n*log(gamma(a+b)) - n*log(gamma(a)) - n*log(gamma(b)) + sum(w*(log((((x)**(a-1)))))) + sum(w*(log((((1-x)**(b-1)))))) #kurrrat, äkki see ikka vale!?
  -loglik
}

est.beta = function(vec, ws = rep(1,length(vec)), init = c(6,9)){
  opt = optim(par = init, fn = lf.beta, x = vec, w = ws, method = "L-BFGS-B", lower = c(1.1,1.1), upper = c(50,50)) #muidu gamma läheb out of range!? kui sunniks kujupar olema üle 1?
  opt
}


b2 = ebeta(cc[,2])
xx = seq(0,1,0.01)
par(mfrow = c(2,2))
#cc[,2] #1.0000000 0.7636090 0.5730523 0.134233
b1 = est.beta(cc[,2],init = c(4,8), ws = c(1.05,0.05,0.1,0.81))
plot(xx,dbeta(xx,shape1 = b1$par[1],shape2 = b1$par[2]))
b1 = est.beta(cc[,2],init = c(4,8), ws = c(0.05,0.05,0.1,0.81))
plot(xx,dbeta(xx,shape1 = b1$par[1],shape2 = b1$par[2]))
b1 = est.beta(cc[,2],init = c(4,8), ws = c(0.05,1.5,0.1,0.21))
plot(xx,dbeta(xx,shape1 = b1$par[1],shape2 = b1$par[2]))
b1 = est.beta(cc[,2],init = c(4,8), ws = c(0.05,0.05,1.1,0.11))
plot(xx,dbeta(xx,shape1 = b1$par[1],shape2 = b1$par[2]))



plot(xx,dbeta(xx,shape1 = b2$parameters[1],shape2 = b2$parameters[2]))
#töötab pmst :), aga kaaludega mitte....
#nüüd kaalud ka meigivad nats senssi

w = c(0.05,0.05,0.1,0.81)
sum(log((w*((x)**(a-1)))))



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
bestvars.koos.455.agre.tava.beta = fun_bestvars(1,25,sidxx, kernel = tava, agre = agre.beta)
bestvars.koos.455.agre.epa.beta = fun_bestvars(1,25,sidxx, kernel = epa, agre = agre.beta)

#save(bestvars.koos.455.agre.tava.beta,file = "bestvars_koos_455_agre_tava_beta.RData")

rss = unlist(bestvars.koos.455.agre.beta[[2]]); plot(rss, type = "o")
rss = unlist(bestvars.koos.455.agre.tava.beta[[2]]); plot(rss, type = "o")

#võtta w suhtena max kaalu!???


#save(bestvars.koos.455.agre.tava.epa,file = "bestvars_koos_455_agre_tava_epa.RData")

vark = bestvars.koos.455.agre.tava.beta[[1]][[1]] #7. ehk tegelt k = 16 parim
opt.epa_kaugus.beta_agre = optim(par = rep(1, length(vark)), fn = fun_opti, k = 19, vars = vark, data = data, sidxx = sidxx,method = "BFGS", kernel = epa, agre = agre.beta)
#0.1718480 --->>> 0.1664395
#opt.epa.beta16 [1] 0.1664395
#lollakas, mul algab ju k = 10-st...

#kuidas parim välja näeb?
wgt = opt.epa.beta16$par
best_vars = bestvars.koos.455.agre.tava.beta[[1]][[1]]

#testiks

#best_vars =vars[28:37]
dex = data[,c("aproovitykk_id",best_vars)]
dex = dex[dex$aproovitykk_id %in% sidxx,]
dex[,-1] = t((t(as.matrix(dex[,-1])))*wgt)
dex$cl = "cl"

test  = fun_agre_kernel(dex, data_puud, k = 20, sid = sidxx, kernel = tava, agre = agre.beta)
dp = merge(test, taks.info, all.x = T, by = "aproovitykk_id")
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
#beta 20 naabriga optimeerimata: 0.1984361

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


############################

#ENAMUSPUULIIK!
#Piiriks, kui ühte puuliiki enam kui 75%
data = dp;
data = data[,c(2:5,11:14)]
names(data)#4 esimest hinnangud, 4 viimast tõde

liiks = c("MA", "KU", "KS", "MUU")
max.liik = function(vec){
  c(max(vec),liiks[which.max(vec)])
}

data$max = NA;data$max.liik = NA

data[,9:10] = t(apply(data[,5:8],1,max.liik))
data.max = data[data$max > 0.75,]
data.max$suurim.prop = "ei"

data.max[,11] = t(apply(data.max[,1:4],1,max.liik))[,2]
confusion.matrix = as.matrix(table(data.max$max.liik, data.max$suurim.prop)[liiks,liiks])
confusion.matrix

#       MA  KU  KS MUU
# MA  110   0   1   0
# KU    1  25   2   0
# KS    0   0  33   2
# MUU   0   1   4  17

#veerud "tõde"; nt 1 mänd on kuuseks hinnatud

sum(diag(confusion.matrix)) / sum(confusion.matrix)
#0.9438776 õigesti hinnatud

#kui paljudel kordadel kõige suurem hinnang on kõige levinum liik?
#nt piiriks 50%?

data$max.hinnang = NA;data$max.hinnang.liik = NA
data[,11:12] = t(apply(data[,1:4],1,max.liik))
data.max1 = data[data$max.hinnang > 0.5,]
conf1 = as.matrix(table(data.max1$max.hinnang.liik, data.max1$max.liik)[liiks,liiks])
t(conf1)

#siin saad teha ploti, alates mis % on tegemist ALATI kõige levinuma liigiga :)

      # KS  KU  MA MUU
# KS   57   8   2   9
# KU    1  45   8   1
# MA    0   3 161   0
# MUU   4   1   0  26

#Näiteks, kui suurim hinnang on üle 50% ja on "muu", siis tegelikult on 9 juhul 36-st tegemist kasega ja ühel juhul kuusega

#data.max1[data.max1$max.hinnang.liik == "MUU",]

#plot

xx = c(min(data$max.hinnang),seq(0.32,1,0.01))
pc = c()

for(i in 1:length(xx)){
  data.max1 = data[data$max.hinnang >= xx[i],]
  conf1 = table(data.max1$max.hinnang.liik, data.max1$max.liik)
  pc0 = sum(diag(conf1)) / sum(conf1)
  pc = c(pc,pc0)
}

par(mfrow = c(1,1))
plot(xx,pc, type = "o", col = rgb(red = 0, green = 0, blue = 0, alpha = 0.69), pch = 16, xlab = "Osakaalude hinnangu maksimum", ylab = "Täpsus")

min(xx[pc > 0.95]) #Kui mingi liigi osakaal on üle 58% hinnatud, siis 95% juhtudek on see kõige levinum liik
min(xx[pc >= 1]) #Kui mingi liigi osakaal on üle 73% hinnatud, siis kõikidel juhtudel on see olnud kõige levinum liik
