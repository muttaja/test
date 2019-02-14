#rss ühekaupa lsiamine

#viia läbi erinevate arvu naabritega, nt 5 kuni 12?
fun_agre_epa = function(data, data_puud, k)
{
  kk = k+1
  dists = knn.cv(train = data[,2:(dim(data)[2]-1)], cl = data$cl, k = kk)
  dist1 = attr(dists,"nn.dist")
  #dist1 = dist1 + 1 #kui lähim naaber on kaugusel 0
  index1 = attr(dists,"nn.index")
  props = apply(dist1, 1, epa)
  props = t(props)
  indxprops = cbind(index1, props)
  data_puud = data_puud
  t(apply(indxprops, 1, agre))
}



vars = c(vars_sat, lidar_intless)
for(k in 1:20){
  #print(k)
  vars0 = vars
  vars1 = c()
  mx = 69
  vahe = 96
  #while tingimus kuni lisatav argument andis positiivse kasu
  while (vahe > 0) {
    mx0 = mx
    print(vahe)
    vars00 = c()
    rss = c()
    for(j in 1:length(vars0)){
      #print(j)
      vars00 = c(vars1,vars0[j])
      #print(vars00)
      dex = data10[,c("SID",vars00)]
      dex = dex[dex$SID %in% sidxx,]
      dex$cl = "cl"
      
      H_eks1 = fun_agre_epa(dex, data_puud, k = k); H1 = H_eks1 / rowSums(H_eks1)
      EH1 = data.frame(cbind(sidxx, H1, data_puud_raie_props))
      rsdls = (EH1[,2:8] - EH1[,9:15])
      w = colSums(EH1[,9:15]) / dim(EH1[,9:15])[1]
      cols = colSums(rsdls**2)
      RSS_col = cols*w
      rss[j] = sum(RSS_col)
    }
    rss[is.na(rss)] = 1000
    mx0 = min(rss)
    varmin = vars0[which.min(rss)]
    vars0 = vars0[!(vars0 %in% varmin)]
    vars1 = c(vars1,varmin)
    vahe = mx-mx0
    mx = mx0
  }
  assign(paste("var_naabreid", k, sep="_"),vars1)
  print(vars1)
}


nr_neigh = function(k, vars){
  dex = data10[,c("SID",vars)]
  dex = dex[dex$SID %in% sidxx,]
  dex[,-1] = t((t(as.matrix(dex[,-1]))))#*bw - ta
  dex$cl = "cl"
  
  
  H_eks1 = fun_agre_epa(dex, data_puud, k = k); H1 = H_eks1 / rowSums(H_eks1)
  data_puud_raie_props = data_puud / rowSums(data_puud)
  
  EH1 = data.frame(cbind(sidxx, H1, data_puud_raie_props))
  names(EH1) = c("SID",paste(rep(c("MA", "KU", "KS", "HB", "LV", "LM", "KX"),2),
                             rep(c("EKS_VALIK","TODE"), each = 7), sep = "."))
  
  rsdls = (EH1[,2:8] - EH1[,9:15])
  w = colSums(EH1[,9:15]) / dim(EH1[,9:15])[1]
  cols = colSums(rsdls**2)
  RSS_col = cols*w
  sum(RSS_col)
}
  
list_vars = list(var_naabreid_1,var_naabreid_2,var_naabreid_3,var_naabreid_4,var_naabreid_5,var_naabreid_6,var_naabreid_7,var_naabreid_8,
                 var_naabreid_9,var_naabreid_10,var_naabreid_11,var_naabreid_12,var_naabreid_13,var_naabreid_14,var_naabreid_15,var_naabreid_16,
                 var_naabreid_17,var_naabreid_18,var_naabreid_19,var_naabreid_20)

rss= c()
for(k in 1:20){
  rss[k] = nr_neigh(k, vars = list_vars[[k]])
}

plot(rss, type = "o")


dex = data10[,c("SID",list_vars[[5]])]
dex = dex[dex$SID %in% sidxx,]
dex[,-1] = t((t(as.matrix(dex[,-1]))))#*bw - ta
dex$cl = "cl"
H_eks1 = fun_agre_epa(dex, data_puud, k = 5); H1 = H_eks1 / rowSums(H_eks1)
data_puud_raie_props = data_puud / rowSums(data_puud)
EH1 = data.frame(cbind(sidxx, H1, data_puud_raie_props))
names(EH1) = c("SID",paste(rep(c("MA", "KU", "KS", "HB", "LV", "LM", "KX"),2),rep(c("EKS_VALIK","TODE"), each = 7), sep = "."))
rsdls = (EH1[,2:8] - EH1[,9:15])
w = colSums(EH1[,9:15]) / dim(EH1[,9:15])[1]
cols = colSums(rsdls**2)
RSS_col = cols*w
sum(RSS_col)

plot(EH1$MA.EKS_VALIK, EH1$MA.TODE)
plot(EH1$KU.EKS_VALIK, EH1$KU.TODE)
plot(EH1$KS.EKS_VALIK, EH1$KS.TODE)
plot(EH1$HB.EKS_VALIK, EH1$HB.TODE)
plot(EH1$LM.EKS_VALIK, EH1$LM.TODE)
plot(EH1$LV.EKS_VALIK, EH1$LV.TODE)
plot(EH1$KX.EKS_VALIK, EH1$KX.TODE)


#optimize?

nr_neigh_opt = function(w,k){
  dex[,-1] = t((t(as.matrix(dex[,-1])))*w)#*bw - ta
  dex$cl = "cl"
  
  H_eks1 = fun_agre_epa(dex, data_puud, k = k); H1 = H_eks1 / rowSums(H_eks1)
  data_puud_raie_props = data_puud / rowSums(data_puud)
  
  EH1 = data.frame(cbind(sidxx, H1, data_puud_raie_props))
  names(EH1) = c("SID",paste(rep(c("MA", "KU", "KS", "HB", "LV", "LM", "KX"),2),
                             rep(c("EKS_VALIK","TODE"), each = 7), sep = "."))
  
  rsdls = (EH1[,2:8] - EH1[,9:15])
  w = colSums(EH1[,9:15]) / dim(EH1[,9:15])[1]
  cols = colSums(rsdls**2)
  RSS_col = cols*w
  sum(RSS_col)
}

opt = optim(par = rep(1/18, 18), fn = nr_neigh_opt)

opt5 = nr_neigh_opt(w = opt$par)

###kuidas prognoosid välja näevad?

dex = data10[,c("SID",list_vars[[5]])]
dex = dex[dex$SID %in% sidxx,]
dex[,-1] = t((t(as.matrix(dex[,-1])))*opt$par)#*bw - ta
dex$cl = "cl"
H_eks1 = fun_agre_epa(dex, data_puud, k = 5); H1 = H_eks1 / rowSums(H_eks1)
data_puud_raie_props = data_puud / rowSums(data_puud)
EH1 = data.frame(cbind(sidxx, H1, data_puud_raie_props))
names(EH1) = c("SID",paste(rep(c("MA", "KU", "KS", "HB", "LV", "LM", "KX"),2),rep(c("EKS_VALIK","TODE"), each = 7), sep = "."))
rsdls = (EH1[,2:8] - EH1[,9:15])
w = colSums(EH1[,9:15]) / dim(EH1[,9:15])[1]
cols = colSums(rsdls**2)
RSS_col = cols*w
sum(RSS_col)

plot(EH1$MA.EKS_VALIK, EH1$MA.TODE)
plot(EH1$KU.EKS_VALIK, EH1$KU.TODE)
plot(EH1$KS.EKS_VALIK, EH1$KS.TODE)
plot(EH1$HB.EKS_VALIK, EH1$HB.TODE)
plot(EH1$LM.EKS_VALIK, EH1$LM.TODE)
plot(EH1$LV.EKS_VALIK, EH1$LV.TODE)
plot(EH1$KX.EKS_VALIK, EH1$KX.TODE)

#optimiseerime kõiki
nr_neigh_opt = function(w,k,vars){
  dex = data10[,c("SID",vars)]
  dex = dex[dex$SID %in% sidxx,]
  dex[,-1] = t((t(as.matrix(dex[,-1])))*w)#*bw - ta
  dex$cl = "cl"
  
  H_eks1 = fun_agre_epa(dex, data_puud, k = k); H1 = H_eks1 / rowSums(H_eks1)
  data_puud_raie_props = data_puud / rowSums(data_puud)
  
  EH1 = data.frame(cbind(sidxx, H1, data_puud_raie_props))
  names(EH1) = c("SID",paste(rep(c("MA", "KU", "KS", "HB", "LV", "LM", "KX"),2),
                             rep(c("EKS_VALIK","TODE"), each = 7), sep = "."))
  
  rsdls = (EH1[,2:8] - EH1[,9:15])
  w = colSums(EH1[,9:15]) / dim(EH1[,9:15])[1]
  cols = colSums(rsdls**2)
  RSS_col = cols*w
  sum(RSS_col)
}


rss_opt= c()
for(k in 1:20){
  vars = list_vars[[k]]
  opt_w = rep(1/length(vars), length(vars))
  opti = optim(par = opt_w, fn = nr_neigh_opt, k = k, vars = vars)
  rss_opt[k] = opti$value
  print(opti$value)
}

plot(rss_opt, type = "o")
#praegu on piiratud 0..1 vahemikku? tegelt ei pea ju?

#optimeeri üle väiksemate arvu parameetrite, st võta lõpp maha!

rss_opt2

rss_opt1= c()
for(k in 3:12){
  vars = list_vars[[k]]; vars = vars[1:(length(vars)-1)]
  opt_w = rep(1/length(vars), length(vars))
  opti = optim(par = opt_w, fn = nr_neigh_opt, k = k, vars = vars)
  rss_opt1[k] = opti$value
  print(opti$value)
}

rss_opt3= c()
for(k in 3:12){
  vars = list_vars[[k]]; vars = vars[1:(length(vars)-3)]
  opt_w = rep(1/length(vars), length(vars))
  opti = optim(par = opt_w, fn = nr_neigh_opt, k = k, vars = vars)
  rss_opt1[k] = opti$value
  print(opti$value)
}

#kui nii, et need 

#kui anda optimeerimisel mingid teised kaalud ette?
#anda kaaludeks, kui palju tulemus erineks, kui valitud tunnust ei ole?

difs = c()
for(j in 1:length(list_vars[[5]])){
  varro = list_vars[[5]][-j]
  dex = data10[,c("SID",varro)]
  dex = dex[dex$SID %in% sidxx,]
  dex$cl = "cl"
  #dex[,-1] = t((t(as.matrix(dex[,-1]))))#*opt$par
  
  
  H_eks1 = fun_agre_epa(dex, data_puud, k = 5); H1 = H_eks1 / rowSums(H_eks1)
  data_puud_raie_props = data_puud / rowSums(data_puud)
  EH1 = data.frame(cbind(sidxx, H1, data_puud_raie_props))
  names(EH1) = c("SID",paste(rep(c("MA", "KU", "KS", "HB", "LV", "LM", "KX"),2),rep(c("EKS_VALIK","TODE"), each = 7), sep = "."))
  rsdls = (EH1[,2:8] - EH1[,9:15])
  w = colSums(EH1[,9:15]) / dim(EH1[,9:15])[1]
  cols = colSums(rsdls**2)
  RSS_col = cols*w
  dif = sum(RSS_col) - 4.083043
  difs[j] = dif
}

w5 = difs * length(difs) / sum(difs)

opti5_w0 = optim(par = rep(1, length(list_vars[[5]])), fn = nr_neigh_opt, k = 5, vars = list_vars[[5]])
opti5_w = optim(par = w5, fn = nr_neigh_opt, k = 5, vars = list_vars[[5]])

#need kaalud ei anna midagi juurde.

opti5_w0 = optim(par = rep(1, length(list_vars[[5]])), fn = nr_neigh_opt, k = 5, vars = list_vars[[5]], method = ("BFGS"))
#BFSG: 3.985108

rss_meth = c()
method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN","Brent")
for(i in 1:6){
  opti5_wx = optim(par = rep(1, length(list_vars[[5]])), fn = nr_neigh_opt, k = 5, vars = list_vars[[5]], method = method[i])
  rss_meth[i] = opti5_wx$value
}

#[1] 4.083043 3.985108 4.048458 3.999206 4.370725
#brent aint 1dim

dex = data10[,c("SID",list_vars[[5]])]
dex = dex[dex$SID %in% sidxx,]
dex[,-1] = t((t(as.matrix(dex[,-1])))*opti5_w0$par)#*bw - ta
dex$cl = "cl"
H_eks1 = fun_agre_epa(dex, data_puud, k = 5); H1 = H_eks1 / rowSums(H_eks1)
data_puud_raie_props = data_puud / rowSums(data_puud)
EH1 = data.frame(cbind(sidxx, H1, data_puud_raie_props))
names(EH1) = c("SID",paste(rep(c("MA", "KU", "KS", "HB", "LV", "LM", "KX"),2),rep(c("EKS_VALIK","TODE"), each = 7), sep = "."))
rsdls = (EH1[,2:8] - EH1[,9:15])
w = colSums(EH1[,9:15]) / dim(EH1[,9:15])[1]
cols = colSums(rsdls**2)
RSS_col = cols*w
sum(RSS_col) #3.985108 #see siis seni parim?

plot(EH1$MA.EKS_VALIK, EH1$MA.TODE)
plot(EH1$KU.EKS_VALIK, EH1$KU.TODE)
plot(EH1$KS.EKS_VALIK, EH1$KS.TODE)
plot(EH1$HB.EKS_VALIK, EH1$HB.TODE)
plot(EH1$LM.EKS_VALIK, EH1$LM.TODE)
plot(EH1$LV.EKS_VALIK, EH1$LV.TODE)
#plot(EH1$KX.EKS_VALIK, EH1$KX.TODE)

require(ggplot2)
ggplot(EH1, aes(x = MA.EKS_VALIK, y = MA.TODE)) + geom_text(aes(label = SID), size = 3, angle = 90)
ggplot(EH1, aes(x = KU.EKS_VALIK, y = KU.TODE)) + geom_text(aes(label = SID), size = 3, angle = 90)
ggplot(EH1, aes(x = KS.EKS_VALIK, y = KS.TODE)) + geom_text(aes(label = SID), size = 3, angle = 90)
#MA: 125057, 109143 ... 124807
lnk = raied50[raied50$aproovitykk_id == 109143,]$link
browseURL(lnk, browser = getOption("browser"),encodeIfNeeded = FALSE)
#125057 - pole homog, raiesmik ja soine lage ala kürval ka
#109143 - pole ka väga homog, raiesmik, tee ja maja lähedal ka

#aga kui hinnata ERALDI männi osakaalu, kuuse osakaalu jne, ja siis pärast neist üldine hinnang kombineerida?
#vt see eraldi failina

#KOOS MULLAGA

vars = c(vars_sat, lidar_intless, vars_muld)
for(k in 1:20){
  #print(k)
  vars0 = vars
  vars1 = c()
  mx = 69
  vahe = 96
  #while tingimus kuni lisatav argument andis positiivse kasu
  while (vahe > 0) {
    mx0 = mx
    print(vahe)
    vars00 = c()
    rss = c()
    for(j in 1:length(vars0)){
      #print(j)
      vars00 = c(vars1,vars0[j])
      #print(vars00)
      dex = data10[,c("SID",vars00)]
      dex = dex[dex$SID %in% sidxx,]
      dex$cl = "cl"
      
      H_eks1 = fun_agre_epa(dex, data_puud, k = k); H1 = H_eks1 / rowSums(H_eks1)
      EH1 = data.frame(cbind(sidxx, H1, data_puud_raie_props))
      rsdls = (EH1[,2:8] - EH1[,9:15])
      w = colSums(EH1[,9:15]) / dim(EH1[,9:15])[1]
      cols = colSums(rsdls**2)
      RSS_col = cols*w
      rss[j] = sum(RSS_col)
    }
    rss[is.na(rss)] = 1000
    mx0 = min(rss)
    varmin = vars0[which.min(rss)]
    vars0 = vars0[!(vars0 %in% varmin)]
    vars1 = c(vars1,varmin)
    vahe = mx-mx0
    mx = mx0
  }
  assign(paste("var_naabreid", k, sep="_"),vars1)
  print(vars1)
}


list_vars_muld = list(var_naabreid_1,var_naabreid_2,var_naabreid_3,var_naabreid_4,var_naabreid_5,var_naabreid_6,var_naabreid_7,var_naabreid_8,
                 var_naabreid_9,var_naabreid_10,var_naabreid_11,var_naabreid_12,var_naabreid_13,var_naabreid_14,var_naabreid_15,var_naabreid_16,
                 var_naabreid_17,var_naabreid_18,var_naabreid_19,var_naabreid_20)


wmse= c()
for(k in 1:20){
  wmse[k] = nr_neigh(k, vars = list_vars_muld[[k]]) / 174
}
par(mfrow = c(1,1))
plot(wmse, type = "o", xlam = "naabreid")
#5,6,7,8 võiks optimeerida

#võiks samuti proovida üle väiksema arvu parameetrite. Samas siis peaks optimiseerimine lihtsalt leidma, et parim kaal on 0...

rss_muldB= c()
for(k in 5:8){
  vars = list_vars[[k]]
  opt_w = rep(1/length(vars), length(vars))
  opti = optim(par = opt_w, fn = nr_neigh_opt, k = k, vars = vars, method = "BFGS")
  rss_muldB[k] = opti$value
  print(opti$value)
}
rss_muldB / 174

plot(rss_muldB, type = "o")

vars = list_vars_muld[[5]]
opt_w = rep(1/length(vars), length(vars))
#w_muld_bfgs5 = optim(par = opt_w, fn = nr_neigh_opt, k = 5, vars = vars, method = "BFGS")
#nende kaaludega tuli  3.792161

dex = data10[,c("SID",vars)]
dex = dex[dex$SID %in% sidxx,]
dex[,-1] = t((t(as.matrix(dex[,-1])))*w_muld_bfgs5$par)#*bw - ta
dex$cl = "cl"
H_eks1 = fun_agre_epa(dex, data_puud, k = 5); H1 = H_eks1 / rowSums(H_eks1)
data_puud_raie_props = data_puud / rowSums(data_puud)
EH1 = data.frame(cbind(sidxx, H1, data_puud_raie_props))
names(EH1) = c("SID",paste(rep(c("MA", "KU", "KS", "HB", "LM", "LV", "KX"),2),rep(c("EKS_VALIK","TODE"), each = 7), sep = "."))
rsdls = (EH1[,2:8] - EH1[,9:15])
w = colSums(EH1[,9:15]) / dim(EH1[,9:15])[1]
cols = colSums(rsdls**2)
RSS_col = cols*w
sum(RSS_col) #3.792161, parim koos hinnatutest

plot(EH1$MA.EKS_VALIK, EH1$MA.TODE)
plot(EH1$KU.EKS_VALIK, EH1$KU.TODE)
plot(EH1$KS.EKS_VALIK, EH1$KS.TODE)
plot(EH1$HB.EKS_VALIK, EH1$HB.TODE)
plot(EH1$LM.EKS_VALIK, EH1$LM.TODE)
plot(EH1$LV.EKS_VALIK, EH1$LV.TODE)

par(mfrow = c(3,3))
plot(HPM1$T, EH1$MA.EKS_VALIK, xlim = c(0,1), ylim = c(0,1), ylab = "prognoos", xlab = "mänd")
plot(HPM2$T, EH1$KU.EKS_VALIK, xlim = c(0,1), ylim = c(0,1), ylab = "prognoos", xlab = "kuusk")
plot(HPM3$T, EH1$KS.EKS_VALIK, xlim = c(0,1), ylim = c(0,1), ylab = "prognoos", xlab = "kask")
plot(HPM4$T, EH1$HB.EKS_VALIK, xlim = c(0,1), ylim = c(0,1), ylab = "prognoos", xlab = "haab")
plot(HPM5$T, EH1$LM.EKS_VALIK, xlim = c(0,1), ylim = c(0,1), ylab = "prognoos", xlab = "mustlepp")
plot(HPM6$T, EH1$LV.EKS_VALIK, xlim = c(0,1), ylim = c(0,1), ylab = "prognoos", xlab = "halllepp")
plot(HPM7$T, EH1$KX.EKS_VALIK, xlim = c(0,1), ylim = c(0,1), ylab = "prognoos", xlab = "muu")

fun_rss(EH1[,-1]) #0.07487774


#siin peal cut?
cutrss = c()
for(i in 1:200){
  cutrss[i] = rss_cut(i / 1000, H1)}
par(mfrow = c(1,1))
plot(cutrss, type = "o")
which.min(cutrss) #ei töööa
min(cutrss)

#ilma lidarita

vars = c(vars_sat, vars_muld)
for(k in 1:20){
  #print(k)
  vars0 = vars
  vars1 = c()
  mx = 69
  vahe = 96
  #while tingimus kuni lisatav argument andis positiivse kasu
  while (vahe > 0) {
    mx0 = mx
    print(vahe)
    vars00 = c()
    rss = c()
    for(j in 1:length(vars0)){
      #print(j)
      vars00 = c(vars1,vars0[j])
      #print(vars00)
      dex = data10[,c("SID",vars00)]
      dex = dex[dex$SID %in% sidxx,]
      dex$cl = "cl"
      
      H_eks1 = fun_agre_epa(dex, data_puud, k = k); H1 = H_eks1 / rowSums(H_eks1)
      EH1 = data.frame(cbind(sidxx, H1, data_puud_raie_props))
      rsdls = (EH1[,2:8] - EH1[,9:15])
      w = colSums(EH1[,9:15]) / dim(EH1[,9:15])[1]
      cols = colSums(rsdls**2)
      RSS_col = cols*w
      rss[j] = sum(RSS_col)
    }
    rss[is.na(rss)] = 1000
    mx0 = min(rss)
    varmin = vars0[which.min(rss)]
    vars0 = vars0[!(vars0 %in% varmin)]
    vars1 = c(vars1,varmin)
    vahe = mx-mx0
    mx = mx0
  }
  assign(paste("var_naabreid", k, sep="_"),vars1)
  print(vars1)
}


list_vars_lidarita = list(var_naabreid_1,var_naabreid_2,var_naabreid_3,var_naabreid_4,var_naabreid_5,var_naabreid_6,var_naabreid_7,var_naabreid_8,
                      var_naabreid_9,var_naabreid_10,var_naabreid_11,var_naabreid_12,var_naabreid_13,var_naabreid_14,var_naabreid_15,var_naabreid_16,
                      var_naabreid_17,var_naabreid_18,var_naabreid_19,var_naabreid_20)



wmse= c()
for(k in 1:20){
  wmse[k] = nr_neigh(k, vars = list_vars_lidarita[[k]]) / 174
}
par(mfrow = c(1,1))
plot(wmse, type = "o", xlab = "naabreid")

#6, 11 ja 12 optimeeriks?
rss_muld_lidarita= c()
for(k in c(10)){
  vars = list_vars_lidarita[[k]]
  opt_w = rep(1/length(vars), length(vars))
  opti = optim(par = opt_w, fn = nr_neigh_opt, k = k, vars = vars, method = "BFGS")
  rss_muld_lidarita[k] = opti$value
  print(opti$value)
  assign(paste("par_lidarita", k, sep="_"),opti$par)
}

#10 parim

#ilma lidari andmeteta, ükshaaval tunnuseid lisades leitud parim meetod, kaalud optimeeritud:
vars = list_vars_lidarita[[10]]
dex = data10[,c("SID",vars)]
dex = dex[dex$SID %in% sidxx,]
dex[,-1] = t((t(as.matrix(dex[,-1])))*par_lidarita_10)#*bw - ta
dex$cl = "cl"
H_eks1 = fun_agre_epa(dex, data_puud, k = 10); H1 = H_eks1 / rowSums(H_eks1)
data_puud_raie_props = data_puud / rowSums(data_puud)
EH1 = data.frame(cbind(sidxx, H1, data_puud_raie_props))
names(EH1) = c("SID",paste(rep(c("MA", "KU", "KS", "HB", "LM", "LV", "KX"),2),rep(c("EKS_VALIK","TODE"), each = 7), sep = "."))
rsdls = (EH1[,2:8] - EH1[,9:15])
w = colSums(EH1[,9:15]) / dim(EH1[,9:15])[1]
cols = colSums(rsdls**2)
RSS_col = cols*w
sum(RSS_col) #4.625409

fun_rss(EH1[,-1])


par(mfrow = c(2,3))
plot(EH1$MA.EKS_VALIK, EH1$MA.TODE)
plot(EH1$KU.EKS_VALIK, EH1$KU.TODE)
plot(EH1$KS.EKS_VALIK, EH1$KS.TODE)
plot(EH1$HB.EKS_VALIK, EH1$HB.TODE)
plot(EH1$LM.EKS_VALIK, EH1$LM.TODE)
plot(EH1$LV.EKS_VALIK, EH1$LV.TODE)


par(mfrow = c(3,3))
plot(HPM1$T, EH1$MA.EKS_VALIK, xlim = c(0,1), ylim = c(0,1), ylab = "prognoos", xlab = "mänd")
plot(HPM2$T, EH1$KU.EKS_VALIK, xlim = c(0,1), ylim = c(0,1), ylab = "prognoos", xlab = "kuusk")
plot(HPM3$T, EH1$KS.EKS_VALIK, xlim = c(0,1), ylim = c(0,1), ylab = "prognoos", xlab = "kask")
plot(HPM4$T, EH1$HB.EKS_VALIK, xlim = c(0,1), ylim = c(0,1), ylab = "prognoos", xlab = "haab")
plot(HPM5$T, EH1$LM.EKS_VALIK, xlim = c(0,1), ylim = c(0,1), ylab = "prognoos", xlab = "mustlepp")
plot(HPM6$T, EH1$LV.EKS_VALIK, xlim = c(0,1), ylim = c(0,1), ylab = "prognoos", xlab = "halllepp")
plot(HPM7$T, EH1$KX.EKS_VALIK, xlim = c(0,1), ylim = c(0,1), ylab = "prognoos", xlab = "muu")


#siin peal cut? ei töötanud

EH1_RSS = fun_rss(EH1[,-1])
EH1_RSS #0.08899839 - keskmine viga iga prognoosi kohta

#siit 5 suurimat viga välja:
R2 = rowSums(sqrt(rsdls**2)) #keskmine viga
R2 = data.frame(SID = sidxx, r = R2)
R2 = R2[order(R2$r, decreasing = T),]
head(R2,5)
lnk = raied50[raied50$aproovitykk_id == 124835,]$link
browseURL(lnk, browser = getOption("browser"),encodeIfNeeded = FALSE)


