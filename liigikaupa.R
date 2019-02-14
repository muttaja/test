#prognoosid liigikaupa, hiljem kombineerida

fun_agre_liik = function(data, data_puud, k, liik)
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
  agr = t(apply(indxprops, 1, agre))
  (agr/rowSums(agr))[,liik]
}

#1 - mänd. 2 - kuusk, 3 - kask, 4 - haab, 5 - mustlepp, 6 - halllepp, 7 - muu

vars = c(vars_sat, lidar_intless)

fun_liik = function(liik){
for(k in 1:15){
  #print(k)
  vars0 = vars
  vars1 = c()
  mx = 69
  vahe = 96
  #while tingimus kuni lisatav argument andis positiivse kasu
  while (vahe > 0) {
    mx0 = mx
    #print(vahe)
    vars00 = c()
    rss = c()
    for(j in 1:length(vars0)){
      #print(j)
      vars00 = c(vars1,vars0[j])
      #print(vars00)
      dex = data10[,c("SID",vars00)]
      dex = dex[dex$SID %in% sidxx,]
      dex$cl = "cl"
      
      H_puu = fun_agre_liik(dex, data_puud, k = k, liik = liik)
      rsdls = H_puu - data_puud_raie_props[,liik]
      rss[j] = sum(rsdls**2)
      #print(rss)

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
}
  lst_return = list(var_naabreid_1,var_naabreid_2,var_naabreid_3,var_naabreid_4,var_naabreid_5,var_naabreid_6,var_naabreid_7,var_naabreid_8,
                    var_naabreid_9,var_naabreid_10,var_naabreid_11,var_naabreid_12,var_naabreid_13,var_naabreid_14,var_naabreid_15)
}

#või võtta kõik 7?
for(l in 1:7){
  print(Sys.time()); print(l)
  vars_liik = fun_liik(l)
  assign(paste("puuliik", l, sep="_"),vars_liik)
}

vars_liik = fun_liik(7)
assign(paste("puuliik", 7, sep="_"),vars_liik)


puuliiks = list(puuliik_1,puuliik_2,puuliik_3,puuliik_4,puuliik_5,puuliik_6,puuliik_7)



nr_neigh_liik = function(k, vars, liik){
  dex = data10[,c("SID",vars)]
  dex = dex[dex$SID %in% sidxx,]
  dex[,-1] = t((t(as.matrix(dex[,-1]))))#*bw - ta
  dex$cl = "cl"
  H_puu = fun_agre_liik(dex, data_puud, k = k, liik = liik)
  rsdls = H_puu - data_puud_raie_props[,liik]
  rss = sum(rsdls**2)
  rss
}


for(l in 1:7){
  puuliik = puuliiks[[l]]
  rss= c()
    for(k in 1:15){
    rss[k] = nr_neigh_liik(k, vars = puuliik[[k]], liik = l)
  }
  assign(paste("rss_liik", l, sep="_"),rss)
}

puuliik = puuliiks[[7]]
rss= c()
for(k in 1:15){
  rss[k] = nr_neigh_liik(k, vars = puuliik[[k]], liik = 7)
}
assign(paste("rss_liik", 7, sep="_"),rss)






par(mfrow = c(2,3))
plot(rss_liik_1, type = "o")
plot(rss_liik_2, type = "o")
plot(rss_liik_3, type = "o")
plot(rss_liik_4, type = "o")
plot(rss_liik_5, type = "o")
plot(rss_liik_6, type = "o")
plot(rss_liik_7, type = "o")

liik_mins = c(7,4,10,6,5,6,4)
#optimize
fun_opti_liik = function(w,k,vars,liik){
  dex = data10[,c("SID",vars)]
  dex = dex[dex$SID %in% sidxx,]
  dex[,-1] = t((t(as.matrix(dex[,-1])))*w)
  dex$cl = "cl"
  
  H_puu = fun_agre_liik(dex, data_puud, k = k, liik = liik)
  rsdls = H_puu - data_puud_raie_props[,liik]
  rss = sum(rsdls**2)
  rss
}

liik_mins = c(7,4,10,6,5,6,4)
rss_opti = c()
for(i in 1:6){
  k = liik_mins[i]
  vars =  puuliiks[[i]][[liik_mins[i]]]
  w = rep(1, length(vars))
  opti_liik1 = optim(par = w, fn = fun_opti_liik, k = k, vars = vars, liik = i, method = "BFGS") #
  rss_opti[i] = opti_liik1$value
  print(rss_opti)
}

rss_opti_BFGS = c()
for(i in 7:7){
  k = liik_mins[i]
  vars =  puuliiks[[i]][[liik_mins[i]]]
  w = rep(1, length(vars))
  opti_liik1 = optim(par = w, fn = fun_opti_liik, k = k, vars = vars, liik = i, method = "BFGS") #
  rss_opti_BFGS[i] = opti_liik1$value
  print(rss_opti_BFGS)
  assign(paste("optiweightsBFGS", i, sep="_"),opti_liik1$par)
}

#NB! "Muu" hetkel puudu!
rss_opti = c()
for(i in 7:7){
  k = liik_mins[i]
  vars =  puuliiks[[i]][[liik_mins[i]]]
  w = rep(1, length(vars))
  opti_liik1 = optim(par = w, fn = fun_opti_liik, k = k, vars = vars, liik = i) #
  rss_opti[i] = opti_liik1$value
  print(rss_opti)
  assign(paste("optiweights", i, sep="_"),opti_liik1$par)
}

#vaikeoptimeerimismeetod töötab kehvemini, aga kiiremini

optiweights = list(optiweightsBFGS_1,optiweights_2, optiweightsBFGS_3, optiweights_4, optiweightsBFGS_5, optiweightsBFGS_6, optiweightsBFGS_7)


for(i in 7:7){
liik = i
wws = optiweights[[liik]]
vars =  puuliiks[[liik]][[liik_mins[liik]]]
dex = data10[,c("SID",vars)]
dex = dex[dex$SID %in% sidxx,]
dex[,-1] = t((t(as.matrix(dex[,-1])))*wws)#*bw - ta
dex$cl = "cl"
H_puu = fun_agre_liik(dex, data_puud, k = liik_mins[liik], liik = liik)
HP = data.frame(H = H_puu, T = data_puud_raie_props[,liik])
assign(paste("HP", i, sep=""),HP)
}

par(mfrow = c(3,3))
plot(HP1$H, HP1$T)
plot(HP2$H, HP2$T)
plot(HP3$H, HP3$T)
plot(HP4$H, HP4$T)
plot(HP5$H, HP5$T)
plot(HP6$H, HP6$T)
plot(HP7$H, HP7$T)

fun_rss = function(df){
  pred = df[,1]; true = df[,2]
  rsdls = (pred - true)
  sqrt(sum(rsdls)**2 / dim(df)[1])
}

w_res = c(fun_rss(HP1),fun_rss(HP2),fun_rss(HP4),fun_rss(HP4),fun_rss(HP5),fun_rss(HP6),fun_rss(HP7))
w_res = w_res / sum(w_res)



#1 - mänd. 2 - kuusk, 3 - kask, 4 - haab, 5 - mustlepp, 6 - halllepp, 7 - muu
data_hp = data.frame(MA = HP1$H, KU = HP2$H, KS = HP3$H, HB = HP4$H, LM = HP5$H, LV = HP6$H, KX = HP7$H)
hist(rowSums(data_hp))
#

#kas see töötab ka siis õigesti, kui osakaalude summa on alla 1?

agr_liik = function(df){
  rws = rowSums(df) #kui alla 1, siis pöördkaalud
  w = colSums(data_puud_raie_props) / dim(df)[1]
  w0 = rep(w, dim(df)[1])
  w_neg = 1 / (w) #või 1-w?
  w_neg = w_neg / sum(w_neg) #need kaalud ei tööta ikka õigesti!
  # 
  # weights = matrix(data = w0, nrow = dim(df)[1], ncol = dim(df)[2],  byrow = T)
  # weights = as.data.frame(weights)
  # weights[rws < 1,] = as.list(w_neg)
  # 
  # dfw = df*weights
  # dfw / rowSums(dfw)
  
  wn = matrix(data = w_neg, nrow = dim(df)[1], ncol = dim(df)[2],  byrow = T)
  w_res = matrix(data = w_res, nrow = dim(df)[1], ncol = dim(df)[2],  byrow = T)
  muutus = 1 - rws; muutus = matrix(rep(muutus, 7), nrow = dim(df)[1], ncol = dim(df)[2])
  tt = wn*df*w_res #siia võiks juurde võtta ka keskmise liigikaupa hinnangu täpsuse liigiti? w_res need kaalud keskmiste jääkide põhjal
  ttx = tt / rowSums(tt) #et kaalud annaks 1 kokku
  ttm = ttx*muutus
  tulem = df + ttm
  ##probleem: pärast muutmist võib mõni jääda negatiivne!
  tulem[tulem < 0] = 0; tulem = tulem / rowSums(tulem)
  tulem
  
  #2. rida just väga suured
  # muutus = 1 - rws[2]
  # t3 = w_neg*df[2,]
  # t3x = t3 / sum(t3)
  # t3x*muutus
  # tulem2 = df[2,] + t3x*muutus
  #probleem: pärast muutmist võib mõni jääda negatiivne!
  
}

HPW = agr_liik(data_hp)
#save(HPW, file = "liigikaupa.RData")
HPW1 = agr_liik(data_hp) #kui nüüd agregeerimisel liigikaupa kaalutud


par(mfrow = c(3,3))
plot(HPW1$MA, HP1$T)
plot(HPW1$KU, HP2$T)
plot(HPW1$KS, HP3$T)
plot(HPW1$HB, HP4$T)
plot(HPW1$LM, HP5$T)
plot(HPW1$LV, HP6$T)
plot(HPW1$KX, HP7$T)

WRSS = function(df){
  true = data_puud_raie_props
  #print(dim(df));print(dim(true))
  w = colSums(true) / dim(df)[1] #kaalud vastavalt kui levinud on puuliik
  rsdls = (df - true)
  cols = colSums(rsdls**2)
  RSS_col = cols*w
  sum(RSS_col)
}

wrss1 = WRSS(HPW) #5.87344, tundub palju
wrss1
#võrdluseks:

dex = data10[,c("SID",list_vars[[5]])]
dex = dex[dex$SID %in% sidxx,]
dex[,-1] = t((t(as.matrix(dex[,-1])))*opti5_w0$par)#*bw - ta
dex$cl = "cl"
H_eks1 = fun_agre_epa(dex, data_puud, k = 5); H1 = H_eks1 / rowSums(H_eks1)
wrss2 = WRSS(H1)
wrss2 #3.929893

#aga kui ei kaalu midag? ehk lihtsalt summa peab 1 olema?
HP0 = data_hp / rowSums(data_hp)
wrss3 = WRSS(data_hp0)
wrss3  #nii tuleb 3.427329

#kui kaaluda liigiti
wrss11 = WRSS(HPW1)
wrss11 #3.417236, 3.406541 kui keskmiste vigade kaalud ka mängu võtta


par(mfrow = c(3,3))
plot(HP0$MA, HP1$T)
plot(HP0$KU, HP2$T)
plot(HP0$KS, HP3$T)
plot(HP0$HB, HP4$T)
plot(HP0$LM, HP5$T)
plot(HP0$LV, HP6$T)
plot(HP0$KX, HP7$T)

  

#KOOS MULLAINFOGA?
muld1 = koos[koos$aproovitykk_id %in% sidxx,]$muld
a9 = names(table(muld1)[table(muld1) < 9])
muld1[muld1 %in% a9] = "muu";
muld1[is.na(muld1)] = "muu"
df_muld = data.frame(muld1 = muld1, SID = sidxx)


muld2 = dcast(df_muld,SID~muld1,fun.aggregate = function(x){as.integer(length(x) > 0)})
muldnames = names(muld2)[-1]
muldnames = paste("muld",muldnames,sep = "_")
names(muld2)[-1] = muldnames
vars_muld = names(muld2)[-1]


data10 = cbind(data10, muld2[,-1])
vars = c(vars_sat, lidar_intless, vars_muld)
for(l in 1:7){
  print(Sys.time()); print(l)
  vars_liik = fun_liik(l)
  assign(paste("puuliik_muld", l, sep="_"),vars_liik)
}

puuliiks_muld = list(puuliik_muld_1,puuliik_muld_2,puuliik_muld_3,puuliik_muld_4,puuliik_muld_5,puuliik_muld_6,puuliik_muld_7)

for(l in 1:7){
  print(Sys.time()); print(l)
  puuliik = puuliiks_muld[[l]]
  rss= c()
  for(k in 1:15){
    rss[k] = nr_neigh_liik(k, vars = puuliik[[k]], liik = l)
  }
  assign(paste("rss_muld_liik", l, sep="_"),rss)
}

par(mfrow = c(3,3))
plot(rss_muld_liik_1, type = "o")
plot(rss_muld_liik_2, type = "o")
plot(rss_muld_liik_3, type = "o")
plot(rss_muld_liik_4, type = "o")
plot(rss_muld_liik_5, type = "o")
plot(rss_muld_liik_6, type = "o")
plot(rss_muld_liik_7, type = "o")

k_list = list(c(6,7,8), c(5,6,7), c(6,7,8), c(3,4,5), c(7,8,9),  c(6,7,8),  c(4,5,6))


for(j in 1:7){
  for(k in 1:3){
    kk = k_list[[j]][k]
    vars =  puuliiks_muld[[j]][[kk]]
    w = rep(1, length(vars))
    opti_liik1 = optim(par = w, fn = fun_opti_liik, k = kk, vars = vars, liik = j) #
    print(j);print(k);print(opti_liik1$value)
    assign(paste("optirss_muld", j, k, sep="_"),opti_liik1$value)
    assign(paste("optiweights_muld", j, k, sep="_"),opti_liik1$par)
  }
}

puuliiks_muld1 = list(puuliiks_muld[[1]][[8]],puuliiks_muld[[2]][[5]],puuliiks_muld[[3]][[7]],puuliiks_muld[[4]][[5]]
                      ,puuliiks_muld[[5]][[7]],puuliiks_muld[[6]][[7]],puuliiks_muld[[7]][[6]])
k_vec = c(8,5,7,5,7,7,6)
for(j in 1:7){
    kk = k_vec[j]
    vars =  puuliiks_muld1[[j]]
    w = rep(1, length(vars))
    opti_liik1 = optim(par = w, fn = fun_opti_liik, k = kk, vars = vars, liik = j) #
    print(j);print(kk);print(opti_liik1$value)
    print(opti_liik1$par)
    assign(paste("optirss_muld", j, sep="_"),opti_liik1$value)
    assign(paste("optiweights_muld", j, sep="_"),opti_liik1$par)
}


optiweights_muld = list(optiweights_muld_1_3,optiweights_muld_2_1, optiweights_muld_3_2, optiweights_muld_4_3,
                        optiweights_muld_5_1, optiweights_muld_6_2, optiweights_muld_7_3)

#ööseks jooksma:
#ühised prognoosid koos mullainfoga
#method = "BFGS" kaalude optimeerimine

for(i in 1:7){
  #i = 1
  liik = i
  k = k_vec[i]
  wws = optiweights_muld[[liik]]
  vars =  puuliiks_muld1[[liik]]
  dex = data10[,c("SID",vars)]
  dex = dex[dex$SID %in% sidxx,]
  dex[,-1] = t((t(as.matrix(dex[,-1])))*wws)#*bw - ta
  dex$cl = "cl"
  H_puu = fun_agre_liik(dex, data_puud, k = k, liik = liik)
  HPM = data.frame(H = H_puu, T = data_puud_raie_props[,liik])
  assign(paste("HPM", i, sep=""),HPM)
}

par(mfrow = c(3,3))
plot(HPM1$H, HPM1$T)
plot(HPM2$H, HPM2$T)
plot(HPM3$H, HPM3$T)
plot(HPM4$H, HPM4$T)
plot(HPM5$H, HPM5$T)
plot(HPM6$H, HPM6$T)
plot(HPM7$H, HPM7$T)

data_hpm = data.frame(MA = HPM1$H, KU = HPM2$H, KS = HPM3$H, HB = HPM4$H, LM = HPM5$H, LV = HPM6$H, KX = HPM7$H)

HPWM = agr_liik(data_hpm)
RSSM = WRSS(HPWM)
RSSM #2.718401

#kui ei kaaluks kokku, vaid lihtsalt pärast summa peab 1 olema:
HPM0 = data_hpm / rowSums(data_hpm)
rssm0 = WRSS(HPM0)
rssm0 #2.564753

par(mfrow = c(3,3))
plot(HPM0$MA, HP1$T)
plot(HPM0$KU, HP2$T)
plot(HPM0$KS, HP3$T)
plot(HPM0$HB, HP4$T)
plot(HPM0$LM, HP5$T)
plot(HPM0$LV, HP6$T)
plot(HPM0$KX, HP7$T)

#Näeb kena välja, aga tunnused ses mudelis on ju suht lambised. Lidari põhjal ei saa ju reaalselt prognoosida?

#ööseks jooksma
for(j in 1:7){
  for(k in 1:3){
    kk = k_list[[j]][k]
    vars =  puuliiks_muld[[j]][[kk]]
    w = rep(1, length(vars))
    opti_liik1 = optim(par = w, fn = fun_opti_liik, k = kk, vars = vars, liik = j, method = "BFGS") #
    print(j);print(k);print(opti_liik1$value)
    assign(paste("optirss_muld_BFGS", j, k, sep="_"),opti_liik1$value)
    assign(paste("optiweights_muld_BFGS", j, k, sep="_"),opti_liik1$par)
  }
}

optiweights_muld_BFGS = list(optiweights_muld_BFGS_1_3,optiweights_muld_BFGS_2_1, optiweights_muld_BFGS_3_2, optiweights_muld_BFGS_4_3,
                        optiweights_muld_BFGS_5_1, optiweights_muld_BFGS_6_2, optiweights_muld_BFGS_7_3)

rl <- unlist(rapply(puuliiks_muld1, length, how="list"))

k_vec = c(8,5,7,5,7,7,6)
for(i in 1:7){
  #i = 1
  liik = i
  k = k_vec[i]
  wws = optiweights_muld_BFGS[[liik]]
  vars =  puuliiks_muld1[[liik]]
  dex = data10[,c("SID",vars)]
  dex = dex[dex$SID %in% sidxx,]
  dex[,-1] = t((t(as.matrix(dex[,-1])))*wws)#*bw - ta
  dex$cl = "cl"
  H_puu = fun_agre_liik(dex, data_puud, k = k, liik = liik)
  HPM = data.frame(H = H_puu, T = data_puud_raie_props[,liik])
  assign(paste("HPMB", i, sep=""),HPM)
}

par(mfrow = c(3,3))
plot(HPM1$T, HPMB1$H, xlim = c(0,1), ylim = c(0,1), ylab = "prognoos", xlab = "mänd")
plot(HPM2$T, HPMB2$H, xlim = c(0,1), ylim = c(0,1), ylab = "prognoos", xlab = "kuusk")
plot(HPM3$T, HPMB3$H, xlim = c(0,1), ylim = c(0,1), ylab = "prognoos", xlab = "kask")
plot(HPM4$T, HPMB4$H, xlim = c(0,1), ylim = c(0,1), ylab = "prognoos", xlab = "haab")
plot(HPM5$T, HPMB5$H, xlim = c(0,1), ylim = c(0,1), ylab = "prognoos", xlab = "mustlepp")
plot(HPM6$T, HPMB6$H, xlim = c(0,1), ylim = c(0,1), ylab = "prognoos", xlab = "halllepp")
plot(HPM7$T, HPMB7$H, xlim = c(0,1), ylim = c(0,1), ylab = "prognoos", xlab = "muu")

data_hpmb = data.frame(MA = HPMB1$H, KU = HPMB2$H, KS = HPMB3$H, HB = HPMB4$H, LM = HPMB5$H, LV = HPMB6$H, KX = HPMB7$H)

HPMB0 = data_hpmb / rowSums(data_hpmb)
rssmb0 = WRSS(HPMB0)
rssmb0 #2.499318 ---> 2.46686 kui kõigata 0.035 pealt
fun_rss(cbind(HPMB0,data_puud_raie_props))


#ilma lidarita:
vars = c(vars_sat, vars_muld)
for(l in 1:7){
  print(Sys.time()); print(l)
  vars_liik = fun_liik(l)
  assign(paste("puuliik_lidarita", l, sep="_"),vars_liik)
}


puuliiks_lidarita = list(puuliik_lidarita_1,puuliik_lidarita_2,puuliik_lidarita_3,puuliik_lidarita_4,puuliik_lidarita_5,puuliik_lidarita_6,puuliik_lidarita_7)

for(l in 1:7){
  print(Sys.time()); print(l)
  puuliik = puuliiks_lidarita[[l]]
  rss= c()
  for(k in 1:15){
    rss[k] = nr_neigh_liik(k, vars = puuliik[[k]], liik = l)
  }
  assign(paste("rss_lidarita_liik", l, sep="_"),rss)
}

par(mfrow = c(3,3))
plot(rss_lidarita_liik_1, type = "o")
plot(rss_lidarita_liik_2, type = "o")
plot(rss_lidarita_liik_3, type = "o")
plot(rss_lidarita_liik_4, type = "o")
plot(rss_lidarita_liik_5, type = "o")
plot(rss_lidarita_liik_6, type = "o")
plot(rss_lidarita_liik_7, type = "o")

k_list_muld = list(c(3,4,7), c(5,6,7), c(6,7,8), c(2,3,5), c(4,6,7),  c(6,7,8),  c(2,4,6))

#ööseks jooksma
for(j in 1:7){
  for(k in 1:3){
    kk = k_list_muld[[j]][k]
    vars =  puuliiks_lidarita[[j]][[kk]]
    w = rep(1, length(vars))
    opti_liik1 = optim(par = w, fn = fun_opti_liik, k = kk, vars = vars, liik = j) #, method = "BFGS"
    print(j);print(k);print(opti_liik1$value)
    assign(paste("optirss_muld_BFGS", j, k, sep="_"),opti_liik1$value)
    assign(paste("optiweights_muld_BFGS", j, k, sep="_"),opti_liik1$par)
  }
}

optiweights_muld_lidarita1 = list(optiweights_muld_BFGS_1_1,optiweights_muld_BFGS_2_1, optiweights_muld_BFGS_3_2, optiweights_muld_BFGS_4_1,
                             optiweights_muld_BFGS_5_2, optiweights_muld_BFGS_6_1, optiweights_muld_BFGS_7_1)


k_vec = c(3,5,7,2,6,6,2)

lvars = c()
for(i in 1:7){
  #i = 1
  liik = i
  k = k_vec[i]
  wws = optiweights_muld_lidarita1[[liik]]
  vars =  puuliiks_lidarita[[liik]][[k]]
  lvars[i] = length(vars)
  dex = data10[,c("SID",vars)]
  dex = dex[dex$SID %in% sidxx,]
  dex[,-1] = t((t(as.matrix(dex[,-1])))*wws)#*bw - ta
  dex$cl = "cl"
  H_puu = fun_agre_liik(dex, data_puud, k = k, liik = liik)
  HPM = data.frame(H = H_puu, T = data_puud_raie_props[,liik])
  assign(paste("HPMB_lidless", i, sep=""),HPM)
}

par(mfrow = c(3,3))
plot(HPMB_lidless1$H, HPM1$T, xlim = c(0,1), ylim = c(0,1))
plot(HPMB_lidless2$H, HPM2$T, xlim = c(0,1), ylim = c(0,1))
plot(HPMB_lidless3$H, HPM3$T, xlim = c(0,1), ylim = c(0,1))
plot(HPMB_lidless4$H, HPM4$T, xlim = c(0,1), ylim = c(0,1))
plot(HPMB_lidless5$H, HPM5$T, xlim = c(0,1), ylim = c(0,1))
plot(HPMB_lidless6$H, HPM6$T, xlim = c(0,1), ylim = c(0,1))
plot(HPMB_lidless7$H, HPM7$T, xlim = c(0,1), ylim = c(0,1))

data_hpmb_lidless = data.frame(MA = HPMB_lidless1$H, KU = HPMB_lidless2$H, KS = HPMB_lidless3$H, HB = HPMB_lidless4$H, LM = HPMB_lidless5$H, LV = HPMB_lidless6$H, KX = HPMB_lidless7$H)

HPMB00 = data_hpmb_lidless / rowSums(data_hpmb_lidless)
rssmb00 = WRSS(HPMB00)
rssmb00 #3.096615, minu superagregeerimine andis sitema tulemuse
fun_rss(cbind(HPMB00,data_puud_raie_props))

par(mfrow = c(3,3))
plot(HPM1$T, HPMB00$MA, xlim = c(0,1), ylim = c(0,1), ylab = "prognoos", xlab = "mänd")
plot(HPM2$T, HPMB00$KU, xlim = c(0,1), ylim = c(0,1), ylab = "prognoos", xlab = "kuusk")
plot(HPM3$T, HPMB00$KS, xlim = c(0,1), ylim = c(0,1), ylab = "prognoos", xlab = "kask")
plot(HPM4$T, HPMB00$HB, xlim = c(0,1), ylim = c(0,1), ylab = "prognoos", xlab = "haab")
plot(HPM5$T, HPMB00$LM, xlim = c(0,1), ylim = c(0,1), ylab = "prognoos", xlab = "mustlepp")
plot(HPM6$T, HPMB00$LV, xlim = c(0,1), ylim = c(0,1), ylab = "prognoos", xlab = "halllepp")
plot(HPM7$T, HPMB00$KX, xlim = c(0,1), ylim = c(0,1), ylab = "prognoos", xlab = "muu")




fun_rss = function(df){
  pred = df[,1:7]; true = df[,8:14]
  rsdls = (pred - true)
  sum(sqrt(rsdls**2))/174 / 7
}

df1 = cbind(HPMB00, data_puud_raie_props)

RSS_lidless = fun_rss(df1)
RSS_lidless #0.06581197 - keskmine viga iga prognoosi kohta

#aga mis siis keskmine viga tuleks, kui kõigil oleks sama arv naabreid?
for(j in 1:7){
  for(k in 4:6){
    vars =  puuliiks_lidarita[[j]][[k]]
    w = rep(1, length(vars))
    opti_liik1 = optim(par = w, fn = fun_opti_liik, k = k, vars = vars, liik = j) #, method = "BFGS"
    print(j);print(k);print(opti_liik1$value)
    assign(paste("k_sama_rss", j, k, sep="_"),opti_liik1$value)
    assign(paste("k_sama_w", j, k, sep="_"),opti_liik1$par)
  }
}

k_sama_ws = list(k_sama_w_1_4,k_sama_w_1_5,k_sama_w_1_6, k_sama_w_2_4,k_sama_w_2_5,k_sama_w_2_6,
                 k_sama_w_3_4,k_sama_w_3_5,k_sama_w_3_6, k_sama_w_4_4,k_sama_w_4_5,k_sama_w_4_6,
                 k_sama_w_5_4,k_sama_w_5_5,k_sama_w_5_6, k_sama_w_6_4,k_sama_w_6_5,k_sama_w_6_6,
                 k_sama_w_7_4,k_sama_w_7_5,k_sama_w_7_6)


#ööseks jooksma
k_list_muld = list(c(3,4,7), c(5,6,7), c(6,7,8), c(2,3,5), c(4,6,7),  c(6,7,8),  c(2,4,6))
for(j in 1:7){
  for(k in 1:3){
    kk = k_list_muld[[j]][k]
    vars =  puuliiks_lidarita[[j]][[kk]]
    w = rep(1, length(vars))
    opti_liik1 = optim(par = w, fn = fun_opti_liik, k = kk, vars = vars, liik = j, method = "BFGS") #, method = "BFGS"
    print(j);print(k);print(opti_liik1$value)
    assign(paste("optirss_muld_BFGS", j, k, sep="_"),opti_liik1$value)
    assign(paste("optiweights_muld_BFGS", j, k, sep="_"),opti_liik1$par)
  }
}

#1,1,2,1,2,1,1
optiweights_muld_lidarita1BF = list(optiweights_muld_BFGS_1_1,optiweights_muld_BFGS_2_1, optiweights_muld_BFGS_3_2, optiweights_muld_BFGS_4_1,
                                  optiweights_muld_BFGS_5_2, optiweights_muld_BFGS_6_1, optiweights_muld_BFGS_7_1)
k_vec = c(3,5,7,2,6,6,2)
for(i in 1:7){
  #i = 1
  liik = i
  k = k_vec[i]
  wws = optiweights_muld_lidarita1BF[[liik]]
  vars =  puuliiks_lidarita[[liik]][[k]]
  dex = data10[,c("SID",vars)]
  dex = dex[dex$SID %in% sidxx,]
  dex[,-1] = t((t(as.matrix(dex[,-1])))*wws)#*bw - ta
  dex$cl = "cl"
  H_puu = fun_agre_liik(dex, data_puud, k = k, liik = liik)
  HPM = data.frame(H = H_puu, T = data_puud_raie_props[,liik])
  assign(paste("HPMB_lidlessB", i, sep=""),HPM)
}
lidlessB = data.frame(MA = HPMB_lidlessB1$H, KU = HPMB_lidlessB2$H, KS = HPMB_lidlessB3$H, HB = HPMB_lidlessB4$H,
                               LM = HPMB_lidlessB5$H, LV = HPMB_lidlessB6$H, KX = HPMB_lidlessB7$H)
HPMBF = lidlessB / rowSums(lidlessB)
rssmbf = WRSS(HPMBF)
rssmbf #3.098285
fun_rss(cbind(lidlessB,data_puud_raie_props)) #0.06692343

HPMWBF = agr_liik(lidlessB)
rmpwbf = WRSS(HPMWBF)
rmpwbf #ei ole kasu, cut samuti mitte





#miski siin ei klapi
for(i in 1:7){
  #i = 1
  liik = i
  k = 6
  wws = k_sama_ws[[(3*i )]]
  vars =  puuliiks_lidarita[[liik]][[k]]
  dex = data10[,c("SID",vars)]
  dex = dex[dex$SID %in% sidxx,]
  dex[,-1] = t((t(as.matrix(dex[,-1])))*wws)#*bw - ta
  dex$cl = "cl"
  H_puu = fun_agre_liik(dex, data_puud, k = k, liik = liik)
  HPM = data.frame(H = H_puu, T = data_puud_raie_props[,liik])
  assign(paste("KSM", i,k, sep=""),HPM)
}

KSM4 = data.frame(MA = KSM14$H, KU = KSM24$H, KS = KSM34$H, HB = KSM44$H, LM = KSM54$H, LV = KSM64$H, KX = KSM74$H)
KSM5 = data.frame(MA = KSM15$H, KU = KSM25$H, KS = KSM35$H, HB = KSM45$H, LM = KSM55$H, LV = KSM65$H, KX = KSM75$H)
KSM6 = data.frame(MA = KSM16$H, KU = KSM26$H, KS = KSM36$H, HB = KSM46$H, LM = KSM56$H, LV = KSM66$H, KX = KSM76$H)
k4 = WRSS(KSM4); k4 #4.239147
k5 = WRSS(KSM5); k5 #4.722356
k6 = WRSS(KSM6); k6 #3.962853
fun_rss(cbind(KSM6, data_puud_raie_props)) #0.07487348

par(mfrow = c(3,3))
plot(HPM1$T, KSM6$MA, xlim = c(0,1), ylim = c(0,1), ylab = "prognoos", xlab = "mänd")
plot(HPM2$T, KSM6$KU, xlim = c(0,1), ylim = c(0,1), ylab = "prognoos", xlab = "kuusk")
plot(HPM3$T, KSM6$KS, xlim = c(0,1), ylim = c(0,1), ylab = "prognoos", xlab = "kask")
plot(HPM4$T, KSM6$HB, xlim = c(0,1), ylim = c(0,1), ylab = "prognoos", xlab = "haab")
plot(HPM5$T, KSM6$LM, xlim = c(0,1), ylim = c(0,1), ylab = "prognoos", xlab = "mustlepp")
plot(HPM6$T, KSM6$LV, xlim = c(0,1), ylim = c(0,1), ylab = "prognoos", xlab = "halllepp")
plot(HPM7$T, KSM6$KX, xlim = c(0,1), ylim = c(0,1), ylab = "prognoos", xlab = "muu")


