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

stid = unique(sat$aproovitykk_id); stid = stid[stid %in% mets_id]
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


liik_mins = c(?)

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

rss_opti_BFGS = c()
for(i in 1:7){
  k = liik_mins[i]
  vars =  puuliiks[[i]][[liik_mins[i]]]
  w = rep(1, length(vars))
  opti_liik1 = optim(par = w, fn = fun_opti_liik, k = k, vars = vars, liik = i, method = "BFGS") #
  rss_opti_BFGS[i] = opti_liik1$value
  print(rss_opti_BFGS)
  assign(paste("optiweightsBFGS", i, sep="_"),opti_liik1$par)
}

optiweights = list(optiweightsBFGS_1,optiweights_2, optiweightsBFGS_3, optiweights_4, optiweightsBFGS_5, optiweightsBFGS_6, optiweightsBFGS_7)



for(i in 1:7){
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
plot(HP0$MA, HP1$T)
plot(HP0$KU, HP2$T)
plot(HP0$KS, HP3$T)
plot(HP0$HB, HP4$T)
plot(HP0$LM, HP5$T)
plot(HP0$LV, HP6$T)
plot(HP0$KX, HP7$T)




