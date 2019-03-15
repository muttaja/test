#hinnangud koos andmestikul

#dk andmestik failist landsat_to_sentinel
dk[,-1] = scale(dk[,-1])

muld1 = data.frame(muld = koos[koos$aproovitykk_id %in% mets_id,]$muld)
muld1$aproovitykk_id = koos$aproovitykk_id[koos$aproovitykk_id %in% mets_id]
muld1[is.na(muld1$muld),]$muld = 999

#midagi kokku võtta?
table(muld1$muld) #alla 10 kindlasti kokku; 2 puuduvat väärtust
#10, 11, 209, 999

muld1[muld1$muld %in% c(10,11,200,999), "muld"] = 999

#muld 1-0 tüüpi andmestikuks
muld2 = dcast(muld1,aproovitykk_id~muld,fun.aggregate = function(x){as.integer(length(x) > 0)})

dkm = merge(dk, muld2, by = "aproovitykk_id")
#et siin oleks õige "sat"
sat0 = read.csv("smi_prt_13_17_pixphv-heledused.csv")
sat0[sat0 == 0] = NA

stid = unique(sat0$aproovitykk_id); stid = stid[stid %in% mets_id] #mets
kid = unique(koos$aproovitykk_id);
length(stid[!(stid %in% kid)])

load("ID_OK.RData", verbose = T)# saab midagi kohe valja võtta
sidx = id_ok

not_ok = stid[!(stid %in% id_ok)]
mets_id = na.omit(mets_id)
sidxx = mets_id[!(mets_id %in% not_ok)]

#####################################################

#vaata "ekspert_ puhas vajaminevate funktsioonide lugemiseks!

vars = names(dkm[-1])
data10 = dkm[dkm$aproovitykk_id %in% sidxx,]
names(data10)[1] = "SID"
require(FNN)

names_taks = names(koos)[c(2,19,25:31)]
taks_uus = koos[koos$aproovitykk_id %in% sidxx,names_taks]
#taks_uus = na.omit(taks_uus)
taks_uus$ARV_VMA = taks_uus$arv_maht_es*taks_uus$MA / 100
taks_uus$ARV_VKU = taks_uus$arv_maht_es*taks_uus$KU / 100
taks_uus$ARV_VKS = taks_uus$arv_maht_es*taks_uus$KS / 100
taks_uus$ARV_VHB = taks_uus$arv_maht_es*taks_uus$HB / 100
taks_uus$ARV_VLM = taks_uus$arv_maht_es*taks_uus$LM / 100
taks_uus$ARV_VLV = taks_uus$arv_maht_es*taks_uus$LV / 100
taks_uus$ARV_VKX = taks_uus$arv_maht_es*taks_uus$KX / 100

puud = c("ARV_VMA", "ARV_VKU", "ARV_VKS", "ARV_VHB", "ARV_VLM", "ARV_VLV", "ARV_VKX")
data_puud = taks_uus[taks_uus$aproovitykk_id %in% sidxx, puud]
data_puud_raie_props = data_puud / rowSums(data_puud)

sid_puudu = taks_uus$aproovitykk_id[!(complete.cases(data_puud_raie_props))]
sidxx = sidxx[!(sidxx %in% sid_puudu)]
data_puud = taks_uus[taks_uus$aproovitykk_id %in% sidxx, puud]
data_puud_raie_props = data_puud / rowSums(data_puud)


##############################

#funktsioonid

vars = names(dkm[-1])

fun_liik = function(liik){

  for(k in 1:15){
    print(k); print(Sys.time())
    vars0 = vars
    vars1 = c()
    mx = 696
    vahe = 969
    
    while(vahe > 0){
      mx0 = mx
      print(vahe)
      vars00 = c()
      rss = c()
      for(j in 1:length(vars0)){
        vars00 = c(vars1,vars0[j])
        dex = data10[,c("SID",vars00)]
        dex = dex[dex$SID %in% sidxx,]
        dex$cl = "cl"
        H_puu = fun_agre_liik(dex, data_puud, k = k, liik = liik)
        rsdls = H_puu - data_puud_raie_props[,liik]
        rss[j] = sum(rsdls**2)
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


for(l in 1:7){
  print(Sys.time()); print(l)
  vars_liik = fun_liik(l)
  assign(paste("puuliik", l, sep="_"),vars_liik)
}

puuliiks = list(puuliik_1,puuliik_2,puuliik_3,puuliik_4,puuliik_5,puuliik_6,puuliik_7)
#save(puuliiks, file = "puuliiks0_mullaga.RData")


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

par(mfrow = c(3,3))
plot(rss_liik_1, type = "o")
plot(rss_liik_2, type = "o")
plot(rss_liik_3, type = "o")
plot(rss_liik_4, type = "o")
plot(rss_liik_5, type = "o")
plot(rss_liik_6, type = "o")
plot(rss_liik_7, type = "o")

#mänd, kuusk, kask 10+; edasi 11,9,7 ja 4,5


k_list = list(c(10,11,12,13,14,15), c(10,11,12,13,14,15), c(10,11,12,13,14,15), c(11), c(9),  c(7),  c(4,5))


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


for(j in 7:7){
  for(k in 2:2){
    print(c(j,k)); print(Sys.time())
    kk = k_list[[j]][k]
    varsx =  puuliiks[[j]][[kk]]
    w = rep(1, length(varsx))
    opti_liik1 = optim(par = w, fn = fun_opti_liik, k = kk, vars = varsx, liik = j) #
    print(j);print(k);print(opti_liik1$value)
    assign(paste("optirss_muld", j, k, sep="_"),opti_liik1$value)
    assign(paste("optiweights_muld", j, k, sep="_"),opti_liik1$par)
  }
}


#4, 3, 5, 1, 1, 1, 1
liik_mins = c(13,12,14,11,9,7,4)
optiweights = list(optiweights_muld_1_4,optiweights_muld_2_3, optiweights_muld_3_5, optiweights_muld_4_1, optiweights_muld_5_1, optiweights_muld_6_1, optiweights_muld_7_1)
save(liik_mins, file = "liik_mins0_mullaga.RData")
save(optiweights, file = "optiweights0_muld.RData")


for(i in 1:7){
  liik = i
  wws = optiweights[[liik]]
  varsx =  puuliiks[[liik]][[liik_mins[liik]]]
  dex = data10[,c("SID",varsx)]
  dex = dex[dex$SID %in% sidxx,]
  dex[,-1] = t((t(as.matrix(dex[,-1])))*wws)#*bw - ta
  dex$cl = "cl"
  H_puu = fun_agre_liik(dex, data_puud, k = liik_mins[liik], liik = liik)
  HP = data.frame(H = H_puu, T = data_puud_raie_props[,liik])
  assign(paste("HP", i, sep=""),HP)
}

par(mfrow = c(3,3))
plot(HP1$T, HP1$H)
plot(HP2$T, HP2$H)
plot(HP3$T, HP3$H)
plot(HP4$T, HP4$H)
plot(HP5$T, HP5$H)
plot(HP6$T, HP6$H)
plot(HP7$T, HP7$H)

fun_rss = function(df){
  pred = df[,1]; true = df[,2]
  rsdls = (pred - true)
  sqrt(sum(rsdls)**2) / dim(df)[1]
}

w_res = c(fun_rss(HP1),fun_rss(HP2),fun_rss(HP4),fun_rss(HP4),fun_rss(HP5),fun_rss(HP6),fun_rss(HP7))
#w_res = w_res / sum(w_res)
w_res
#[1] 0.0188098568 0.0111026192 0.0022136951 0.0022136951 0.0003588392 0.0013074733 0.0052367201
#see ei saa ju õige olla!?


#1 - mänd. 2 - kuusk, 3 - kask, 4 - haab, 5 - mustlepp, 6 - halllepp, 7 - muu
data_hp = data.frame(MA = HP1$H, KU = HP2$H, KS = HP3$H, HB = HP4$H, LM = HP5$H, LV = HP6$H, KX = HP7$H)

HP0 = data_hp / rowSums(data_hp)

WRSS = function(df){
  true = data_puud_raie_props
  #print(dim(df));print(dim(true))
  w = colSums(true) / dim(df)[1] #kaalud vastavalt kui levinud on puuliik
  rsdls = (df - true)
  cols = colSums(rsdls**2)
  RSS_col = cols*w
  sum(RSS_col)
}

wrss = WRSS(data_hp0)
wrss

par(mfrow = c(3,3))
plot(HP1$T, HP0$MA)
plot(HP2$T, HP0$KU)
plot(HP3$T, HP0$KS)
plot(HP4$T, HP0$HB)
plot(HP5$T, HP0$LM)
plot(HP6$T, HP0$LV)
plot(HP7$T, HP0$KX)



#Kuule, aga kui nüüd ära on kaalutud kõik tunnised, siis võiks ju uuesti läbi jooksutada ja parimaid tunnuseid otsida???

#aga nüüd: võta "lihtsa kõrgusemudeliga" vigased välja ja proovi uuesti naabreid 5-15

koos_mets = koos[(koos$maakatsgrp == "M" & koos$maakatgrp == "ME"),] #miks NA-d sisse jäävad kui panna aint 1. tingimus?
koos_mets$aasta_erinevus = 2018 - koos_mets$aasta

load(file = "korgus_valja.RData", verbose = T)
k1 = koos_mets[!(koos_mets$aproovitykk_id %in% korgus_valja),]
km1 = lm(inv_korgus ~  K_Elev_P60 + H_Elev_P60 + K_Elev_P90 + H_Elev_P90 + aasta_erinevus, data = k1)
summary(km1) #aasta_erinevus negatiivse kordajaga, sest praegu näitaks lidar muidu liiga kõrget metsa. kordaja 3.3, ehk 33cm aastas juurdekasv? palju natuke!
pk1 = predict(km1, data = k1)
res1 = pk1 - k1$inv_korgus
resdf1 = data.frame(SID = k1$aproovitykk_id, res = res1)
plot(k1$inv_korgus, pk1)

########## need kõrgusesest saadud välja

sidxx = mets_id[!(mets_id %in% not_ok)] #769
data_puud = taks_uus[taks_uus$aproovitykk_id %in% sidxx, puud]
data_puud_raie_props = data_puud / rowSums(data_puud)

sid_puudu = taks_uus$aproovitykk_id[!(complete.cases(data_puud_raie_props))]
sidxx = sidxx[!(sidxx %in% sid_puudu)] #765
sum(1*((sidxx %in% korgus_valja))) #16 OK!
sum(1*((korgus_valja %in% sidxx))) #29, kumb nüüd õige on? korgus_valja pole unikaalsed!!!

sidxx = sidxx[!(sidxx %in% korgus_valja)] #749

data_puud = taks_uus[taks_uus$aproovitykk_id %in% sidxx, puud]
data_puud_raie_props = data_puud / rowSums(data_puud)
data10 = dkm[dkm$aproovitykk_id %in% sidxx,]
names(data10)[1] = "SID"

for(i in 1:7){
  liik = i
  wws = optiweights[[liik]]
  varsx =  puuliiks[[liik]][[liik_mins[liik]]]
  dex = data10[,c("SID",varsx)]
  dex = dex[dex$SID %in% sidxx,]
  dex[,-1] = t((t(as.matrix(dex[,-1])))*wws)#*bw - ta
  dex$cl = "cl"
  H_puu = fun_agre_liik(dex, data_puud, k = liik_mins[liik], liik = liik)
  HP = data.frame(H = H_puu, T = data_puud_raie_props[,liik])
  assign(paste("HP", i, sep=""),HP)
}

par(mfrow = c(3,3))
plot(HP1$T, HP1$H)
plot(HP2$T, HP2$H)
plot(HP3$T, HP3$H)
plot(HP4$T, HP4$H)
plot(HP5$T, HP5$H)
plot(HP6$T, HP6$H)
plot(HP7$T, HP7$H)

fun_rss = function(df){
  pred = df[,1]; true = df[,2]
  rsdls = (pred - true)
  sum(abs(rsdls)) / dim(df)[1]
}

w_res = c(fun_rss(HP1),fun_rss(HP2),fun_rss(HP4),fun_rss(HP4),fun_rss(HP5),fun_rss(HP6),fun_rss(HP7))
#w_res = w_res / sum(w_res)
w_res
#749 juhul, kui ei jooksutanud uuesti läbi uute tunnuste ja kaalude leidmiseks
#[1] [1] 0.12734026 0.13474512 0.06289597 0.06289597 0.01798898 0.06396007 0.03488666

####uued hinnangud, kui need 15 väljas
#k 4 ... 15

sidxx = sidx1

fun_liik = function(liik, k1, k2){
  lst_return = vector("list", length = 1+k2-k1)
  for(k in k1:k2){
    print(k); print(Sys.time())
    vars0 = vars
    vars1 = c()
    mx = 696
    vahe = 969
    
    while(vahe > 0){
      mx0 = mx
      print(vahe)
      vars00 = c()
      
      rss = c()
      for(j in 1:length(vars0)){
        vars00 = c(vars1,vars0[j])
        #print(vars00)
        dex = data10[,c("SID",vars00)]
        dex = dex[dex$SID %in% sidxx,]
        dex$cl = "cl"
        H_puu = fun_agre_liik(dex, data_puud, k = k, liik = liik)
        rsdls = H_puu - data_puud_raie_props[,liik]
        rss[j] = sum(rsdls**2)
      }
      rss[is.na(rss)] = 1000
      mx0 = min(rss)
      varmin = vars0[which.min(rss)]
      vars0 = vars0[!(vars0 %in% varmin)]
      vars1 = c(vars1,varmin)
      vahe = mx-mx0
      mx = mx0
    }
    
    lst_return[[1+k-k1]] =  assign(paste("var_naabreid", k, sep="_"),vars1)
  }
  lst_return
}



for(l in 1:7){
  print(Sys.time()); print(l)
  vars_liik = fun_liik(l, 8, 20)
  assign(paste("puuliik_749", l, sep="_"),vars_liik)
}

puuliiks749 = list(puuliik_749_1,puuliik_749_2,puuliik_749_3,puuliik_749_4,puuliik_749_5,puuliik_749_6,puuliik_749_7)
save(puuliiks749, file = "puuliiks749_mullaga.RData")


nr_neigh_liik = function(k, vars, liik){
  dex = data10[,c("SID",vars)] 
  dex = dex[dex$SID %in% sidxx,]      ####### NB milliseid SID kasutad!
  dex[,-1] = t((t(as.matrix(dex[,-1]))))#*bw - ta
  dex$cl = "cl"
  H_puu = fun_agre_liik(dex, data_puud, k = k, liik = liik)
  rsdls = H_puu - data_puud_raie_props[,liik]
  rss = sum(rsdls**2) / dim(dex)[1]
  rss
}


for(l in 1:7){
  puuliik = puuliiks749[[l]]
  rss= c()
  for(k in 1:12){
    rss[k] = nr_neigh_liik(k, vars = puuliik[[k]], liik = l)
  }
  assign(paste("rss_749", l, sep="_"),rss)
}

rss749 = list(rss_749_1, rss_749_2, rss_749_3, rss_749_4, rss_749_5, rss_749_6, rss_749_7)
save(rss749, file = "rss749_0.RData") #ehk see, mis on kehvem kui 765 korral

par(mfrow = c(3,3))
plot(rss_749_1, type = "o")
plot(rss_749_2, type = "o")
plot(rss_749_3, type = "o")
plot(rss_749_4, type = "o")
plot(rss_749_5, type = "o")
plot(rss_749_6, type = "o")
plot(rss_749_7, type = "o")

#võtame 15 kõik praegu lihtsuse huvides

for(j in 1:7){
  for(k in 12:12){
    print(c(j,k)); print(Sys.time())
    kk = k  #k_list[[j]][k]
    varsx =  puuliiks749[[j]][[kk]]
    w = rep(1, length(varsx))
    opti_liik1 = optim(par = w, fn = fun_opti_liik, k = kk, vars = varsx, liik = j) #
    print(j);print(k);print(opti_liik1$value)
    assign(paste("optirss749_muld", j, k, sep="_"),opti_liik1$value)
    assign(paste("optiweights749_muld", j, k, sep="_"),opti_liik1$par)
  }
}

optiweights749 = list(optiweights749_muld_1_12,optiweights749_muld_2_12, optiweights749_muld_3_12, optiweights749_muld_4_12, optiweights749_muld_5_12, optiweights749_muld_6_12, optiweights749_muld_7_12)
save(optiweights749, file = "optiweights749_muld.RData")


for(i in 1:7){
  liik = i
  wws = optiweights749[[liik]]
  varsx =  puuliiks749[[liik]][[12]]
  dex = data10[,c("SID",varsx)]
  dex = dex[dex$SID %in% sidxx,]
  dex[,-1] = t((t(as.matrix(dex[,-1])))*wws)#*bw - ta
  dex$cl = "cl"
  H_puu = fun_agre_liik(dex, data_puud, k = liik_mins[liik], liik = liik)
  HP = data.frame(H = H_puu, T = data_puud_raie_props[,liik])
  assign(paste("HP", i, sep=""),HP)
}

par(mfrow = c(3,3))
plot(HP1$T, HP1$H)
plot(HP2$T, HP2$H)
plot(HP3$T, HP3$H)
plot(HP4$T, HP4$H)
plot(HP5$T, HP5$H)
plot(HP6$T, HP6$H)
plot(HP7$T, HP7$H)

fun_rss = function(df){
  pred = df[,1]; true = df[,2]
  rsdls = (pred - true)
  sum(abs(rsdls)) / dim(df)[1]
}

w_res = c(fun_rss(HP1),fun_rss(HP2),fun_rss(HP4),fun_rss(HP4),fun_rss(HP5),fun_rss(HP6),fun_rss(HP7))
#w_res = w_res / sum(w_res)
w_res
#[1]      0.16378694 0.14633420 0.07979455 0.07979455 0.02553548 0.08091982 0.03627051
#oli: [1] 0.12734026 0.13474512 0.06289597 0.06289597 0.01798898 0.06396007 0.03488666

#wtf, need, mis ma välja võtsin, ei olnud siis välja võtmist väärt? äkki nende sat-pilt vastas siis veel reaalsusele!?

#kas mul ikka andmed/dimensioonid klapivad omavahel???

s1 = data10$SID
data_puud1 = taks_uus[taks_uus$aproovitykk_id %in% sidxx, c("aproovitykk_id",puud)]
s2 = data_puud1$aproovitykk_id

################ uuesti 749
#18 oli kõigil kuuel esimesel seni kõige madalam
for(l in 1:6){
  print(Sys.time()); print(l)
  vars_liik = fun_liik(l, 5, 20)
  assign(paste("puuliik_749_ver2_", l, sep="_"),vars_liik)
}

#NB, "muu" lase eraldi! 7. aint 7 hetkel

puuliiks749_ver1 = list(puuliik_749_ver1__1,puuliik_749_ver1__2,puuliik_749_ver1__3,puuliik_749_ver1__4,puuliik_749_ver1__5,puuliik_749_ver1__6,puuliik_749_ver1__7)
puuliiks749_ver2 = list(puuliik_749_ver2__1,puuliik_749_ver2__2,puuliik_749_ver2__3,puuliik_749_ver2__4,puuliik_749_ver2__5,puuliik_749_ver2__6)

for(l in 1:6){
  puuliik = puuliiks749_ver2[[l]]
  rss= c()
  for(k in 1:16){
    rss[k] = nr_neigh_liik(k+4, vars = puuliik[[k]], liik = l)
  }
  assign(paste("rss_749_ver2", l, sep="_"),rss)
}

rss749_v2 = list(rss_749_ver2_1, rss_749_ver2_2, rss_749_ver2_3, rss_749_ver2_4, rss_749_ver2_5, rss_749_ver2_6)

par(mfrow = c(3,3))
plot(rss_749_ver2_1, type = "o")
plot(rss_749_ver2_2, type = "o")
plot(rss_749_ver2_3, type = "o")
plot(rss_749_ver2_4, type = "o")
plot(rss_749_ver2_5, type = "o")
plot(rss_749_ver2_6, type = "o")

lkm = c(17,18,20,15,9,6)

for(j in 1:6){
    print(j); print(Sys.time())
    kk = lkm[j]
    varsx =  puuliiks749_ver2[[j]][[kk-4]]
    w = rep(1, length(varsx))
    opti_liik1 = optim(par = w, fn = fun_opti_liik, k = kk, vars = varsx, liik = j) #
    print(j);print(k);print(opti_liik1$value)
    assign(paste("optirss749_ver2", j, k, sep="_"),opti_liik1$value)
    assign(paste("optiweights749_ver2", j, kk, sep="_"),opti_liik1$par)
}

for(i in 1:6){
  liik = i
  kk = lkm[liik]
  wws = optiweights749_v2[[liik]]
  varsx =  puuliiks749_ver2[[liik]][[kk-4]]
  dex = data10[,c("SID",varsx)]
  dex = dex[dex$SID %in% sidxx,]
  dex[,-1] = t((t(as.matrix(dex[,-1])))*wws)#*bw - ta
  dex$cl = "cl"
  H_puu = fun_agre_liik(dex, data_puud, k = kk, liik = liik)
  HP = data.frame(H = H_puu, T = data_puud_raie_props[,liik])
  assign(paste("HP", i, sep=""),HP)
}

optiweights749_v2 = list(optiweights749_ver2_1_16,optiweights749_ver2_2_16, optiweights749_ver2_3_16, optiweights749_ver2_4_16, optiweights749_ver2_5_16, optiweights749_ver2_6_16)
save(optiweights749_v2, file = "optiweights749_v2.RData")

for(i in 1:6){
  liik = i
  kk = lkm[liik]
  wws = optiweights749_v2[[liik]]
  varsx =  puuliiks749_ver2[[liik]][[kk-4]]
  dex = data10[,c("SID",varsx)]
  dex = dex[dex$SID %in% sidxx,]
  dex[,-1] = t((t(as.matrix(dex[,-1])))*wws)#*bw - ta
  dex$cl = "cl"
  H_puu = fun_agre_liik(dex, data_puud, k = kk, liik = liik)
  HP = data.frame(H = H_puu, T = data_puud_raie_props[,liik])
  assign(paste("HP", i, sep=""),HP)
}


par(mfrow = c(2,3))
plot(HP1$T, HP1$H)
plot(HP2$T, HP2$H)
plot(HP3$T, HP3$H)
plot(HP4$T, HP4$H)
plot(HP5$T, HP5$H)
plot(HP6$T, HP6$H)

fun_rss = function(df){
  pred = df[,1]; true = df[,2]
  rsdls = (pred - true)
  sum(abs(rsdls)) / dim(df)[1]
}

w_res = c(fun_rss(HP1),fun_rss(HP2),fun_rss(HP3),fun_rss(HP4),fun_rss(HP5),fun_rss(HP6),fun_rss(HP7))
#w_res = w_res / sum(w_res)
w_res

#keskmised vead: 0.12888961 0.13565126 0.16840761 0.06443989 0.01791119 0.06321045 0.03488666

#kaalume läbi ja otsime uusi tunnuseid!

fun_liik = function(liik, k1, k2, wi, vi){
  lst_return = vector("list", length = 1+k2-k1)
  data10[,vi] = t((t(as.matrix(data10[,vi])))*wi) #korrutame eelmisel sammul leitud kaaludega läbi
  for(k in k1:k2){
    print(k); print(Sys.time())
    vars0 = vars
    vars1 = c()
    mx = 696
    vahe = 969
    
    while(vahe > 0){
      mx0 = mx
      print(vahe)
      vars00 = c()
      
      rss = c()
      for(j in 1:length(vars0)){
        vars00 = c(vars1,vars0[j])
        #print(vars00)
        dex = data10[,c("SID",vars00)]
        dex = dex[dex$SID %in% sidxx,]
        dex$cl = "cl"
        H_puu = fun_agre_liik(dex, data_puud, k = k, liik = liik)
        rsdls = H_puu - data_puud_raie_props[,liik]
        rss[j] = sum(rsdls**2)
      }
      rss[is.na(rss)] = 1000
      mx0 = min(rss)
      varmin = vars0[which.min(rss)]
      vars0 = vars0[!(vars0 %in% varmin)]
      vars1 = c(vars1,varmin)
      vahe = mx-mx0
      mx = mx0
    }
    
    lst_return[[1+k-k1]] =  assign(paste("var_naabreid", k, sep="_"),vars1)
  }
  lst_return
}


for(l in 1:3){
  print(Sys.time()); print(l)
  wi = optiweights749_v2[[l]]
  kk = lkm[l]
  vi =  puuliiks749_ver2[[l]][[kk-4]]
  vars_liik = fun_liik(l, 17, 20, wi = wi, vi = vi)
  assign(paste("puuliik_749_ver3_", l, sep="_"),vars_liik)
}

liiksv3 = list(puuliik_749_ver3__1, puuliik_749_ver3__2, puuliik_749_ver3__3)
#liigid 1-3 naabreid 17-20 jooksutatud juba kaalutud andmete peal:
#võrdle tavalsii 749 vigu:
##keskmised vead: 0.12888961 0.13565126 0.16840761 0.06443989 0.01791119 0.06321045 0.03488666
save(liiksv3, file = "liiksv3.RData")

