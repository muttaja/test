#knn_koos_raied

raied_ok = raied[raied$raie == 0,]$aproovitykk_id #427
load("ID_OK.RData", verbose = T)# 174

sidxx = c(id_ok, raied_ok) #601


#dk andmestik failist landsat_to_sentinel
dk[,-1] = scale(dk[,-1])

muld1 = data.frame(muld = koos[koos$aproovitykk_id %in% mets_id,]$muld)
muld1$aproovitykk_id = koos$aproovitykk_id[koos$aproovitykk_id %in% mets_id]
muld1[is.na(muld1$muld),]$muld = 999

#midagi kokku võtta?
table(muld1[muld1$aproovitykk_id %in% sidxx,]$muld) #alla 10 kindlasti kokku; 2 puuduvat väärtust
#võtan alla 30 välja

muld1[muld1$muld %in% c(10,11,16,73,200,999,31,37,53,57,63), "muld"] = 999

#muld 1-0 tüüpi andmestikuks
muld2 = dcast(muld1,aproovitykk_id~muld,fun.aggregate = function(x){as.integer(length(x) > 0)})

dkm = merge(dk, muld2, by = "aproovitykk_id")


vars = names(dkm[-1])
data10 = dkm[dkm$aproovitykk_id %in% sidxx,]
names(data10)[1] = "SID"

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


############# funid
require(FNN)

fun_liik = function(liik, k1, k2, w, v){
  lst_return = vector("list", length = 1+k2-k1)
  for(k in 1 : (k2-k1+1)){
    data = data10
    vi = v[[k]]
    wi = w[[k]]
    k = k + k1 -1
    data[,vi] = t((t(as.matrix(data[,vi])))*wi) #korrutame eelmisel sammul leitud kaaludega läbi
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
        dex = data[,c("SID",vars00)]
        dex$cl = "cl"
        H_puu = fun_agre_liik(dex, data_puud, k = k, liik = liik)
        rsdls = H_puu - data_puud_raie_props[,liik]
        rss[j] = mean(rsdls**2) #keskmine ruutviga
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


test1 = fun_liik(liik = 1, k1 = 5, k2 = 15, wi = rep(1, length(vars)), vi = vars)


nr_neigh_liik = function(k, vars, liik){
  dex = data10[,c("SID",vars)] 
  dex = dex[dex$SID %in% sidxx,]      ####### NB milliseid SID kasutad!
  dex[,-1] = t((t(as.matrix(dex[,-1]))))#*bw - ta
  dex$cl = "cl"
  H_puu = fun_agre_liik(dex, data_puud, k = k, liik = liik)
  rsdls = H_puu - data_puud_raie_props[,liik]
  rss = mean(rsdls**2)
  rss
}

for(l in 1:1){
  rss= c()
  for(k in 1:11){
    rss[k] = nr_neigh_liik(k+4, vars = test1[[k]], liik = l)
  }
  assign(paste("rss_601", l, sep="_"),rss)
}

plot(rss_601_1, type = "o")
# [1] 0.03755160 0.03199982 0.03326460 0.03106513 0.03089149 0.03302242 0.03291305 0.03293359 0.03045993 0.03127185
#[11] 0.03466584

#arvuta kaalud, kasutame 13 ja 14 naabrit


fun_opti_liik = function(w,k,vars,liik){
  dex = data10[,c("SID",vars)]
  dex = dex[dex$SID %in% sidxx,]
  dex[,-1] = t((t(as.matrix(dex[,-1])))*w)
  dex$cl = "cl"
  
  H_puu = fun_agre_liik(dex, data_puud, k = k, liik = liik)
  rsdls = H_puu - data_puud_raie_props[,liik]
  rss = mean(rsdls**2)
  rss
}

for(j in 1:1){
  for(k in 9:10){
    print(c(j,k)); print(Sys.time())
    kk = k + 4  #k_list[[j]][k]
    varsx =  test1[[k]]
    w = rep(1, length(varsx))
    opti_liik1 = optim(par = w, fn = fun_opti_liik, k = kk, vars = varsx, liik = j) #
    print(j);print(k);print(opti_liik1$value)
    assign(paste("orss601", j, k, sep="_"),opti_liik1$value)
    assign(paste("ow601", j, k, sep="_"),opti_liik1$par)
  }
}

orss601_1_9 / 601; orss601_1_10 / 601 
#[1] 0.02836859
#[1] 0.02909103


test2 = fun_liik(liik = 1, k1 = 13, k2 = 14, w = list(ow601_1_9,ow601_1_10), v = list(test1[[9]],test1[[10]]))

for(l in 1:1){
  rss= c()
  for(k in 1:2){
    rss[k] = nr_neigh_liik(k+12, vars = test2[[k]], liik = l)
  }
  assign(paste("rss_601v2", l, sep="_"),rss)
}

plot(rss_601v2_1, type = "o")


for(j in 1:1){
  for(k in 1:2){
    print(c(j,k)); print(Sys.time())
    kk = k + 12  #k_list[[j]][k]
    varsx =  test2[[k]]
    w = rep(1, length(varsx))
    opti_liik1 = optim(par = w, fn = fun_opti_liik, k = kk, vars = varsx, liik = j) #
    print(j);print(k);print(opti_liik1$value)
    assign(paste("orss601v2", j, k, sep="_"),opti_liik1$value)
    assign(paste("ow601v2", j, k, sep="_"),opti_liik1$par)
  }
}

#ülimarginaalne võit :D

orss601v2_1_1;orss601v2_1_2
#[1] 0.0288201
#[1] 0.02846068

ow601v2_1_1
ow601v2_1_2

test3 = fun_liik(liik = 1, k1 = 13, k2 = 14, w = list(ow601v2_1_1,ow601v2_1_2), v = list(test2[[1]],test2[[2]]))
for(j in 1:1){
  for(k in 1:2){
    print(c(j,k)); print(Sys.time())
    kk = k + 12  #k_list[[j]][k]
    varsx =  test3[[k]]
    w = rep(1, length(varsx))
    opti_liik1 = optim(par = w, fn = fun_opti_liik, k = kk, vars = varsx, liik = j) #
    print(j);print(k);print(opti_liik1$value)
    assign(paste("orss601v3", j, k, sep="_"),opti_liik1$value)
    assign(paste("ow601v3", j, k, sep="_"),opti_liik1$par)
  }
}

test4 = fun_liik(liik = 1, k1 = 13, k2 = 14, w = list(ow601v3_1_1,ow601v3_1_2), v = list(test3[[1]],test3[[2]]))
for(j in 1:1){
  for(k in 1:2){
    print(c(j,k)); print(Sys.time())
    kk = k + 12  #k_list[[j]][k]
    varsx =  test4[[k]]
    w = rep(1, length(varsx))
    opti_liik1 = optim(par = w, fn = fun_opti_liik, k = kk, vars = varsx, liik = j) #
    print(j);print(k);print(opti_liik1$value)
    assign(paste("orss601v4", j, k, sep="_"),opti_liik1$value)
    assign(paste("ow601v4", j, k, sep="_"),opti_liik1$par)
  }
}

#võidud nii marginaalsed, et ei tasu ära; ja ei koondu tunnsued/kaalud



  wws = ow601v3_1_1
  varsx =  test3[[1]]
  dex = data10[,c("SID",varsx)]
  dex = dex[dex$SID %in% sidxx,]
  dex[,-1] = t((t(as.matrix(dex[,-1])))*wws)#*bw - ta
  dex$cl = "cl"
  H_puu = fun_agre_liik(dex, data_puud, 13, liik = 1)
  HP = data.frame(H = H_puu, T = data_puud_raie_props[,1], SID = sidxx, tyvi = koos[koos$aproovitykk_id %in% sidxx,]$arv_maht_es < 100)



plot(HP$T, HP$H)

require(ggplot2)
require(plotly)
p = ggplot(HP, aes(x=T, y=H)) + geom_point(aes(text = SID,color = tyvi))# + geom_text(aes(label = SID), size = 2.5, vjust = 1.2)
ggplotly(p, tooltip = c("color","text"))

raied[raied$aproovitykk_id == 115243,]
#114753 puudu??? järelt teises andmestikus, mida ma viimati ei kontrollinud
koos[koos$aproovitykk_id == 114753,1:15]

#########
asd = koos[koos$aproovitykk_id %in% sidxx,]
asdm = asd[asd$MA >= 90,]; dim(asdm)[1] #102
asdku = asd[asd$KU >= 90,]; dim(asdku)[1] #19
asdks = asd[asd$KS >= 90,]; dim(asdks)[1] #33
#ehk annaks isegi teha lineaarne mudel :D?

####### võtan kõik peale männi, kuuse ja kase kokku;

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

taks_uus$ARV_VXX = taks_uus$ARV_VHB + taks_uus$ARV_VLM + taks_uus$ARV_VLV + taks_uus$ARV_VKX


puud = c("ARV_VMA", "ARV_VKU", "ARV_VKS", "ARV_VXX")
data_puud = taks_uus[taks_uus$aproovitykk_id %in% sidxx, puud]
data_puud_raie_props = data_puud / rowSums(data_puud)


fun_liik0 = function(liigid, k1, k2){
  lst_return0 = vector("list", length = length(liigid))
  for(liik in liigid){
    lst_return = vector("list", length = 1+k2-k1)
    for(k in 1 : (k2-k1+1)){
      data = data10
      k = k + k1 -1
      print(c(liik,k)); print(Sys.time())
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
          dex = data[,c("SID",vars00)]
          dex$cl = "cl"
          H_puu = fun_agre_liik(dex, data_puud, k = k, liik = liik)
          rsdls = H_puu - data_puud_raie_props[,liik]
          rss[j] = mean(rsdls**2) #keskmine ruutviga
        }
        rss[is.na(rss)] = 1000
        mx0 = min(rss)
        varmin = vars0[which.min(rss)]
        vars0 = vars0[!(vars0 %in% varmin)]
        vars1 = c(vars1,varmin)
        vahe = mx-mx0
        mx = mx0
      }
    
      lst_return[[1+k-k1]] =  assign(paste("vn",liik, k, sep="_"),vars1)
    }
    lst_return0[[liik]] = lst_return
  }
  lst_return0
}

#MAKUKS1 = fun_liik0(liigid = c(1:4), k1 = 1, k2 = 20)
#save(MAKUKS1, file = "MAKUKS.RData")

j1 = 1; j2 = 4;
k1 = 1; k2 = 20;
wlist   = vector("list", length = j2 - j1 +1)
rsslist = vector("list", length = j2 - j1 +1)
for(j in j1:j2){
  lst_return1 = vector("list", length = k2 - k1 +1)
  lst_return2 = vector("list", length = k2 - k1 +1)
  for(k in k1:k2){
    print(c(j,k)); print(Sys.time())
    varsx =  MAKUKS1[[j]][[k]]
    w = rep(1, length(varsx))
    opti_liik1 = optim(par = w, fn = fun_opti_liik, k = k, vars = varsx, liik = j)
    print(j);print(k);print(opti_liik1$value)

    lst_return1[[k]] =  assign(paste("rss", j, k, sep="_"),opti_liik1$value)
    lst_return2[[k]] =  assign(paste("weights", j, k, sep="_"),opti_liik1$par)
  }
  wlist[[j]] = lst_return1
  rsslist[[j]] = lst_return2
}


