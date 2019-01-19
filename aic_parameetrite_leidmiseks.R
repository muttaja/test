#AIC oluliste parameetrite leidmiseks
#mis mudel üldse?

#kas AIC saab igasuguste prognooside pealt arvutada?

#kas eeldame, et vead on normaaljaotusest?
#sellisel juhul:
#AIC = 2k + n*ln(RSS)


puud = c("ARV_VMA", "ARV_VKU", "ARV_VKS", "ARV_VHB", "ARV_VLM", "ARV_VLV", "ARV_VKX") #NB! uues andmestikus ARV_VKX!
#siit välja need, kus puid pole taks põhjal!
taks_uus1 = taks_uus[(taks_uus$SID %in% kontrollitud_SID) & taks_uus$arv_maht_es > 50,] #üle 50
sidxx = intersect(kontrollitud_SID, taks_uus1$SID) #207
data_puud = taks_uus[taks_uus$SID %in% sidxx,puud]
data = sat18_lidar[sat18_lidar$SID %in% sidxx,]
data$cl = "cl"
data_puud_raie = taks_uus[taks_uus$SID %in% sidxx,puud]



vars = names(sat18_lidar)[-1]
panus = function(data, var, n){
  #var: millise tunnuse mõju uurime?
  #n: mitu juhuslikku tunnuste kombinatsiooni võtame, mille korral muutust jälgime?
  difs = c()
  for(k in 1:n){
    rn = ceiling(runif(1)*8) + 2 # kolmest kuni kümneni
    rvar = sample(vars[!(vars %in% var)], rn) #juhuslikult valitud tunnused
    var1 = c(var,rvar) # ... koos meile huvipakkuva tunnusega
    dif = aic_knn(rvar) - aic_knn(var1)
    difs = c(difs,dif)
  }
  return(mean(difs, na.rm = TRUE)) #kuskil sügavamal probleem puuduvate väärtustega
}

data = sat18_lidar[sat18_lidar$SID %in% sidxx,]
data$cl = "cl"
aic_knn = function(vars){
  data1 = data[,c(vars,"cl")]
  pred  = fun_agre_ex(data1, data_puud)
  true = data_puud_raie / rowSums(data_puud_raie)
  #hetkel arvutab üle kõigi puuliikide koos
  #üks variant oleks võtta iga vaatluse kohta vaid kõige suurema osakaaluga puuliik?
  true_max = apply(true,1,max)
  indx_max_true = apply(true, 1, which.max)
  pred_max = pred[cbind(1:nrow(pred), unlist(indx_max_true))] / rowSums(pred)
  rsdls = pred_max - true_max #paraku keskmine pole kindlasti 0.
  RSS = sum(rsdls**2)
  AIC = 2*length(vars) + dim(data)[1]*log(RSS)
  AIC
}

fun_agre_ex= function(data, data_puud)
{
  dists = knn.cv(train = data[,2:(dim(data)[2]-1)], cl = data$cl, k = 10)
  dist1 = attr(dists,"nn.dist")
  index1 = attr(dists,"nn.index")
  props = apply(dist1, 1, propcumsum1) #erinevad proportsioonide vektorid võimalikud
  props = t(props)
  indxprops = cbind(index1, props)
  data_puud = data_puud
  data.frame(round(t(apply(indxprops, 1, agre)),0))
}

#test
puud = c("ARV_VMA", "ARV_VKU", "ARV_VKS", "ARV_VHB", "ARV_VLM", "ARV_VLV", "ARV_VKX") #NB! uues andmestikus ARV_VKX!
#siit välja need, kus puid pole taks põhjal!
taks_uus1 = taks_uus[(taks_uus$SID %in% kontrollitud_SID) & taks_uus$arv_maht_es > 50,] #üle 50
sidxx = intersect(kontrollitud_SID, taks_uus1$SID) #207
data_puud = taks_uus[taks_uus$SID %in% sidxx,puud]
data = sat18_lidar[sat18_lidar$SID %in% sidxx,]
data$cl = "cl"
data_puud_raie = taks_uus[taks_uus$SID %in% sidxx,puud]
aic_knn(vars)


#tsükkel? 10 esimest
tulemused = c()
for(i in 1:10){
  tulemused[i] = panus(data = data, var = vars[i], n = 10)
}
tulemused

#kuskilt tulevad NaN sisse
indxNA_INF <- apply(sat18_lidar, 2, function(x) any(is.na(x) | is.infinite(x)))
indxNA <- apply(sat18_lidar, 2, function(x) any(is.na(x)))
indxINF <- apply(sat18_lidar, 2, function(x) any(is.infinite(x)))

names(sat18_lidar)[indxNA_INF]
names(sat18_lidar)[indxNA] #ehk siis puuduvad väärtused
names(sat18_lidar)[indxINF]

#test
panusx = function(data, var, n){
  #var: millise tunnuse mõju uurime?
  #n: mitu juhuslikku tunnuste kombinatsiooni võtame, mille korral muutust jälgime?
  difs = c()
  for(k in 1:50){
    rn = ceiling(runif(1)*8) + 2 # kolmest kuni kümneni
    rvar = sample(vars[!(vars %in% var)], rn) #juhuslikult valitud tunnused
    var1 = c(var,rvar) # ... koos meile huvipakkuva tunnusega
    #print(rvar)
    aic0 = aic_knn(rvar); aic1 = aic_knn(var1)
    aics = cbind(aic0, aic1)
    print(cbind(var, aics))
    if(is.na(aic0)||is.na(aic1)){
      print(rvar)
    }
  }
}

for(i in 1:2){
panusx(data = data, var = vars[i], n = 5)
}

#kui võtta aint satelliidi andmed:
vars = vars[1:69]
#tsükkel? 10 esimest
tulemused = c()
for(i in 1:5){
  tulemused[i] = panus(data = data, var = vars[i], n = 10)
}
tulemused

#n = 20, siis esimesed 5:
#[1]  2.921521  1.626160  4.176110 16.510832  6.492550

#n = 100
#> tulemused
#[1] 3.847184 3.627152 1.279377 6.802651 3.350670 pole just stabiilne :/

#n = 500
#[1] 4.020678 3.709703 5.777116 4.030695 4.641394 # ... ehk pole mingit tolku

tulemused = c()
for(i in 1:69){
  tulemused[i] = panus(data = data, var = vars[i], n = 10)
}
tulemused

#n = 20, kõik
# [1]  0.29895402 10.58653479  7.15221236  5.41530918  0.44497324 11.18153932  3.05745244  7.54944099  1.65871701 12.86274210
# [11] -0.86037120  0.78942566  0.09786433 14.11497573  9.37786538  6.04235474  8.49826391 10.35770005  2.21590978 -3.34954262
# [21]  7.54742140  1.11204931  7.65765275 -3.77089545 -0.36044972  2.98789594  3.53356515 -0.18501469  4.62497537  2.26225986
# [31]  5.81708504 -0.08184010 -1.35717461 10.25547476  0.83247532  4.49167428  2.48304518  2.31015462  5.42561819  4.80435153
# [41] -1.42025501  8.61164452  6.74119722 -0.14407354  1.42584140  8.69556111 -0.22311827  1.33611272  0.60401646 -0.61689150
# [51]  6.60406683  2.65269547  4.88495689  3.34010225  4.15866549  2.99282123  0.80078247  2.67659717  1.41948415 -0.12610416
# [61]  8.46599538  0.39122658  1.49789906  6.26052432  4.88361547  1.67332482  5.00323727  0.51137601  5.67231856

#jääkide teisendus? Praegu on 0...1, aga peaks olema -inf ... inf
aic_knn1 = function(vars){
  data1 = data[,c(vars,"cl")]
  pred  = fun_agre_ex(data1, data_puud)
  true = data_puud_raie / rowSums(data_puud_raie)
  #hetkel arvutab üle kõigi puuliikide koos
  #üks variant oleks võtta iga vaatluse kohta vaid kõige suurema osakaaluga puuliik?
  true_max = apply(true,1,max)
  true_max1 = log(1/true_max -1)
  indx_max_true = apply(true, 1, which.max)
  pred_max = pred[cbind(1:nrow(pred), unlist(indx_max_true))] / rowSums(pred)
  pred_max1 = log(1/pred_max -1)
  rsdls = pred_max1 - true_max1 #nüü osad inf... teisendus?
  rsdls2 = rsdls**2; rsdls2[rsdls2 > 30] = 30
  RSS = sum(rsdls2)
  AIC = 2*length(vars) + dim(data)[1]*log(RSS)
  AIC
}

panus1 = function(data, var, n){
  #var: millise tunnuse mõju uurime?
  #n: mitu juhuslikku tunnuste kombinatsiooni võtame, mille korral muutust jälgime?
  difs = c()
  for(k in 1:n){
    rn = ceiling(runif(1)*8) + 2 # kolmest kuni kümneni
    rvar = sample(vars[!(vars %in% var)], rn) #juhuslikult valitud tunnused
    var1 = c(var,rvar) # ... koos meile huvipakkuva tunnusega
    dif = aic_knn1(rvar) - aic_knn1(var1)
    difs = c(difs,dif)
  }
  return(mean(difs, na.rm = TRUE)) #kuskil sügavamal probleem puuduvate väärtustega
}

tulemused = c()
for(i in 1:10){
  tulemused[i] = panus(data = data, var = vars[i], n = 10)
}
tulemused

#n = 20
#[1] 5.7368438 6.5756510 7.6194671 3.6821864 0.1160092 4.3952050 2.5988484 3.6997115 3.2086329 4.8293926

#n = 40
#[1] 4.73664680 5.17923740 5.81718732 4.28673647 0.37341047 5.72186169 8.14835362 0.75956905 0.02572355 5.45065590

#n = 100
#[1] 3.880284 5.934314 5.756548 3.816995 6.197885 4.234736 4.111711 1.953101 3.768325 2.897584
#need valed, kuna olid avrutatud "panus" mitte "panus1"



#kui tsentreerida jäägid? prognoosidon ju ilmselgelt nihkega
aic_knn1 = function(vars){
data1 = data[,c(vars,"cl")]
pred  = fun_agre_ex(data1, data_puud)
true = data_puud_raie / rowSums(data_puud_raie)
#hetkel arvutab üle kõigi puuliikide koos
#üks variant oleks võtta iga vaatluse kohta vaid kõige suurema osakaaluga puuliik?
true_max = apply(true,1,max)
#true_max1 = log(1/true_max -1)
indx_max_true = apply(true, 1, which.max)
pred_max = pred[cbind(1:nrow(pred), unlist(indx_max_true))] / rowSums(pred)
pred_max0 = pred_max #mingi teisenuds, et keskmine oleks sama, mis õigeteöe proprtsioonidel, samas aga la 0-1 vahel?
#pred_max1 = log(1/pred_max -1)
rsdls = pred_max0 - true_max
#teisendus normaaljaotusele?
RSS = sum(rsdls2)
AIC = 2*length(vars) + dim(data)[1]*log(RSS)
AIC
}

tulemused = c()
for(i in 1:5){
  tulemused[i] = panus1(data = data, var = vars[i], n = 10)
}
tulemused

fun_scale = function(x, mean){
  e = 1e-05
  c = 1
  while(abs(mean(x, na.rm = T) - mean) > e){
    r1 = min(x); r2 = max(x)
    mx = mean(x, na.rm = T)
    x = x + (mean - mx)
    if(c > 0){
      x = x / (max(x)/r2)
    }
    if(c < 0){
      x = x + (r1 - min(x))
    }
    c = c*(-1)
    print(x)
    print(c)
  }
}

xxx = c(1,3,7,12)
mean(xxx)
fun_scale(xxx,3)


fun_scale1 = function(x, mean){
  e = 1e-05
  r1 = min(x); r2 = max(x)
  while(abs(mean(x, na.rm = T) - mean) > e){
    mx = mean(x, na.rm = T)
    x = x + (mean - mx)
    x = x + (r1 - min(x))
    x = x / (max(x)/r2)
    print(x)
  }
}

fun_scale1(xxx,3)

#otsad paika ja ülejäänut muudame?

#teisendus
#props_uus = props**2 / sum(props**2)
#lõikame alla 0.1 ära
#

fun_pred = function(pred){
  pred[pred < 0.1] = 0
  pred = pred / sum(pred)
  px = pred**1.8 / sum(pred**1.8)
  px[px < 0.1] = 0
  px = px / sum(px)
  px
}

pred1 = pred/ rowSums(pred)

px = apply(pred1, 1, fun_pred)
px = t(px)

px_max = apply(px, 1, max)
mean(px_max); hist(px_max, breaks = 10)
mean(true_max); hist(true_max, breaks = 10)


aic_knn2 = function(vars){
  data1 = data[,c(vars,"cl")]
  pred  = fun_agre_ex(data1, data_puud)
  pred1 = pred / rowSums(pred)
  px = apply(pred1, 1, fun_pred)
  px = t(px)
  true = data_puud_raie / rowSums(data_puud_raie)
  #hetkel arvutab üle kõigi puuliikide koos
  #üks variant oleks võtta iga vaatluse kohta vaid kõige suurema osakaaluga puuliik?
  true_max = apply(true,1,max)
  true_max1 = log(1/true_max -1)
  indx_max_true = apply(true, 1, which.max)
  pred_max = px[cbind(1:nrow(px), unlist(indx_max_true))]
  
  rsdls = pred_max - true_max
  #hist(rsdls); mean(rsdls)
  
  rsdls2 = rsdls**2
  RSS = sum(rsdls2)
  AIC = 2*length(vars) + dim(data)[1]*log(RSS)
  AIC
}

panus2 = function(data, var, n, aic_fun){
  #var: millise tunnuse mõju uurime?
  #n: mitu juhuslikku tunnuste kombinatsiooni võtame, mille korral muutust jälgime?
  difs = c()
  for(k in 1:n){
    rn = ceiling(runif(1)*8) + 2 # kolmest kuni kümneni
    rvar = sample(vars[!(vars %in% var)], rn) #juhuslikult valitud tunnused
    var1 = c(var,rvar) # ... koos meile huvipakkuva tunnusega
    dif = aic_fun(rvar) - aic_fun(var1)
    difs = c(difs,dif)
  }
  return(mean(difs, na.rm = TRUE)) #kuskil sügavamal probleem puuduvate väärtustega
}

tulemused = c()
for(i in 1:5){
  tulemused[i] = panus2(data = data, var = vars[i], n = 10, aic_fun = aic_knn2)
}
tulemused
#20
# 15.203289  5.348961  4.788732 10.430181  5.942383
#40
#4.807745 6.423171 5.775628 4.007609 5.922723, ikka täiesti juhsulikud...

aic_knn3 = function(vars){
  data1 = data[,c(vars,"cl")]
  pred  = fun_agre_ex(data1, data_puud)
  pred1 = pred / rowSums(pred)
  px = apply(pred1, 1, fun_pred)
  px = t(px)
  true = data_puud_raie / rowSums(data_puud_raie)
  #hetkel arvutab üle kõigi puuliikide koos
  #üks variant oleks võtta iga vaatluse kohta vaid kõige suurema osakaaluga puuliik?
  true_max = apply(true,1,max)
  true_max1 = log(1/true_max -1)
  indx_max_true = apply(true, 1, which.max)
  pred_max = px[cbind(1:nrow(px), unlist(indx_max_true))]
  
  rsdls = pred_max - true_max
  #hist(rsdls); mean(rsdls)
  
  rsdls2 = rsdls**2
  sum(rsdls2)
  #nüüd pole AIC. kas knn korral üldse saab AIC rääkida?
}

panus3 = function(data, var, n, aic_fun, rn, liik){
  #var: millise tunnuse mõju uurime?
  #n: mitu juhuslikku tunnuste kombinatsiooni võtame, mille korral muutust jälgime?
  difs = c()
  for(k in 1:n){
    #rn = 3 #võtame 3 tunnust ja paneme ühe juurde
    rvar = sample(vars[!(vars %in% var)], rn) #juhuslikult valitud tunnused
    var1 = c(var,rvar) # ... koos meile huvipakkuva tunnusega
    #print(var1)
    dif = aic_fun(rvar, liik) - aic_fun(var1, liik)
    difs = c(difs,dif)
  }
  return(mean(difs))
}

tulemused = c()
varx = vars[c(1, 2, 54, 55)]
for(i in 1:4){
  tulemused[i] = panus3(data = data, var = varx[i], n = 10, aic_fun = aic_knn3)
}
tulemused

#100
#0.03688473 0.04154667 0.02822881 0.03840307
#500
#0.03284340 0.03245319 0.03272368 0.03347857 #VAHET POLE

#proovime vaid ÜHE tunnusega! ÜKS ei töötanud, proovime kahega
tulemused = c()
#varx = vars[c(1, 2, 54, 55)]
for(i in 1:69){
  tulemused[i] = panus3(data = data, var = vars[i], n = 10, aic_fun = aic_knn3, rn = 2)
  print(cbind(i, tulemused[i]))
}
tulemused

#20: 0.05253337 0.05687217 0.05526409 0.07471148
#100: 0.06830967 0.06958378 0.06102318 0.06581159

#küik satika omad n = 100
tulemused10
# [1] 11.422093  4.900251 16.840682  6.216063 13.505481  8.827246 12.855551 12.597319 10.341568  7.929879 19.657180  9.211917
# [13]  9.024320 11.775711 18.806822 10.333297  7.955745  9.625690  8.388676 12.573194 15.435465  9.423039 22.118277 11.635618
# [25] 20.368834 13.481153  8.988393 21.242895  5.061972 12.695666 12.174166  9.312280 12.056746 11.312587 12.749628  7.279505
# [37]  8.627512 13.340438 21.364654 11.908371 10.807283 12.967442 17.754263 11.497716  8.592119 15.595603 12.529423  8.542343
# [49]  4.145470  5.797211 15.570167 16.199292 18.348679  7.967571 14.198324  9.249612 10.496748 10.360632 18.930258  5.344609
# [61] 10.574578 10.772224 10.201506 14.349093 10.006860 16.963136  7.255764 15.817790 11.555021
tulemused100 = tulemused
#tulemused 100
# [1] 12.144525 13.617156 11.415349 13.223004 13.070179 13.678572 14.664705 11.244290 10.625233 10.454488 11.384917  9.872457
# [13] 11.119037 13.375626 12.071344 12.914347 11.633616 12.443405 12.407258 12.553001 10.953461 11.259548 10.917572 11.120029
# [25] 14.105566 11.145832 12.779920 12.619083 12.889044 12.921746 14.931019 10.198466 11.438665 10.980287 13.789267  9.528378
# [37] 13.334496 12.337105  9.793480 11.810508 11.842428 13.980660 13.509627 10.148776 12.913999 11.902727 11.902820 11.208588
# [49] 13.002058 12.614960 12.200634 13.050797 13.598867 10.695830 13.789904 12.206482 12.956705 12.720123 12.472822 13.252364
# [61] 11.081444 11.163021 13.011676 12.341057 12.882933 12.808371 13.343637 13.122093 12.205387

hist(tulemused)

#1000! 69-st 2 kaupa kombinatsioone üldse kokku 2346
tulemused = c()
#varx = vars[c(1, 2, 54, 55)]
for(i in 1:69){
  tulemused[i] = panus3(data = data, var = vars[i], n = 10, aic_fun = aic_knn3, rn = 2)
  print(cbind(i, tulemused[i]))
}
tulemused

#ikka aiateibad
#55 20:28
#56 20:36 - täpselt
#57 20:47 - umbes
#
#59 21:08 hiljemalt
#60 21:16 hiljemalt
#61 21:23 umbes-täpselt
#62 21:32

#seega 9-10 minti

hist(tulemused)
#save(tulemused, file = "RSS_n1000")
#n = 1000
# [1] 11.85653 12.67247 11.98638 12.74371 11.99873 12.33208 12.68200 12.89138 12.39305 11.65187 12.65544 11.99453 11.84120
# [14] 12.34235 12.27241 13.06125 11.78645 12.25854 11.84684 12.35269 12.14521 13.11594 12.29621 12.17218 11.98628 12.55094
# [27] 12.49256 12.40453 11.94331 12.53757 12.21747 12.29191 12.35656 12.98247 12.86566 11.97719 12.83962 12.42506 12.19544
# [40] 12.16691 12.30667 12.50947 11.98934 12.22704 12.12389 12.06277 12.55125 12.14833 12.43611 11.44455 12.35060 11.64033
# [53] 12.67833 12.42841 12.37662 12.11656 12.59649 12.37720 12.48698 12.26430 12.61629 12.42912 12.21012 12.27835 12.39204
# [66] 12.35009 12.56100 12.81598 12.36729

#aiateibad

########################################################################

# kõik uuesti #

#prooviks aint üht puuliiki esmalt, nt mänd?


aic_knn4 = function(vars, liik){
  data1 = data[,c(vars,"cl")]
  pred  = fun_agre_test(data1, data_puud)
  pred = pred / rowSums(pred)
  true = data_puud_raie / rowSums(data_puud_raie)
  print(vars)
  print(pred[,1])
  print(true[,1])
  #hetkel arvutab üle kõigi puuliikide koos
  #üks variant oleks võtta iga vaatluse kohta vaid kõige suurema osakaaluga puuliik?
  rsdls = (pred - true)[,1]#paraku keskmine pole kindlasti 0.
  RSS = sum(rsdls**2)
  RSS
}


for(i in 1:69){
  tulemused[i] = panus3(data = data, var = vars[i], n = 20, aic_fun = aic_knn4, rn = 2)
  print(cbind(i, tulemused[i]))
}
tulemused
#13:06 algas n = 20
#13:21 lõppes

hist(tulemused)
vars1 = vars[tulemused > 10]

tulemused1 = c()
for(i in 1:length(vars1)){
  tulemused1[i] = panus3(data = data, var = vars1[i], n = 100, aic_fun = aic_knn4, rn = 2)
  print(cbind(i, tulemused[i]))
}
tulemused1

hist(tulemused1)

tulemused100x = c()
for(i in 1:69){
  tulemused100x[i] = panus3(data = data, var = vars[i], n = 100, aic_fun = aic_knn4, rn = 2)
  print(cbind(i, tulemused[i]))
}
tulemused100x
hist(tulemused100x)

#kas sama n annab sama tulemuse :O?
#ei, "printis" lihtsalt valet asja
tul = c()
for(i in 1:2){
  tul[i] = panus3(data = data, var = vars[i], n = 100, aic_fun = aic_knn4, rn = 2)
  print(cbind(i, tul[i]))
}
tul
#
tul10 #üle 10 välja võetud
tul10x #samad uuesti läbi lastuna...

test10 = c()
for(i in 1:69){
  test10[i] = panus3(data = data, var = vars[i], n = 10, aic_fun = aic_knn4, rn = 2)
  print(cbind(i, tes10t[i]))
}
test10

test100 = c()
for(i in 1:69){
  test100[i] = panus3(data = data, var = vars[i], n = 10, aic_fun = aic_knn4, rn = 2)
  print(cbind(i, test10[i]))
}
test100


aic_knn4 = function(vars, liik = 1){
  data1 = data[,c("SID", vars,"cl")]
  pred  = fun_agre_test(data1, data_puud)
  pred = pred / rowSums(pred)
  true = data_puud_raie / rowSums(data_puud_raie)
  
  
  #print(vars)
  #print(pred[,1])
  #print(true[,1])
  #hetkel arvutab üle kõigi puuliikide koos
  #üks variant oleks võtta iga vaatluse kohta vaid kõige suurema osakaaluga puuliik?
  rsdls = (pred - true)[,1]#paraku keskmine pole kindlasti 0.
  RSS = sum(rsdls**2)
  RSS
  if(is.na(RSS)){
    print(names(data1))
    print(c("pred", pred))
    print(c("rowsums", rowSums(pred)))
  }
}


fun_agre_test = function(data, data_puud)
{
  dists = knn.cv(train = data[,2:(dim(data)[2]-1)], cl = data$cl, k = 10)
  dist1 = attr(dists,"nn.dist")
  #print(dist1)
  index1 = attr(dists,"nn.index")
  props = apply(dist1, 1, propcumsum1) #erinevad proportsioonide vektorid võimalikud
  props = t(props)
  indxprops = cbind(index1, props)
  #print(indxprops)
  data_puud = data_puud
  data.frame(round(t(apply(indxprops, 1, agre)),0))
}

xx = fun_agre_test(data[,c("SID","B03_sygis1","B12_vahe_kevad","cl")], data_puud)

testfun = function(n, vars, data, rn = 2, liik = 1, aic_fun = aic_knn4){
  test = c()
  for(i in 1:length(vars)){
    test[i] = panus3(data = data, var = vars[i], n = n, aic_fun, rn, liik)
    print(cbind(i, test[i]))
  }
  test
}

vars = vars[1:69]
data = data[,c("SID", vars, "cl")]
test10 = testfun(10, vars = vars, data = data)
test10
hist(test10)
data1 = data; data1[,2:70] = scale(data[,2:70])
test10_scaled = testfun(10, vars = vars, data = data1)
test10_scaled
hist(test10_scaled)

test20_scaled = testfun(20, vars = vars, data = data1)
test20_scaled
hist(test20_scaled)

test100_scaled_10plus = testfun(100, vars = vars[test20_scaled > 10], data = data1)
test100_scaled_10plus

hist(test100_scaled_10plus)

test500_scaled_10plus = testfun(500, vars = vars[test20_scaled > 10][test100_scaled_10plus > 10], data = data1)
#"B06_kevad1" "B06_kevad2" "B06_sygis2" "B07_sygis2" "B08_kevad2" "B08_sygis1" "B08_sygis2" "B5_kevad2"
test500_scaled_10plus
hist(test500_scaled_10plus)
plot(sort(test500_scaled_10plus), type = "o") #3 väiksemat välja
#11.368159  9.944472 10.137164 10.851815 11.844306 11.676929 11.662355  9.798448
vars[test20_scaled > 10][test100_scaled_10plus > 10][test500_scaled_10plus > 11]
#"B06_kevad1" "B08_kevad2" "B08_sygis1" "B08_sygis2"
#kaks B02 sügist. tõenäoliselt üks teisele ju enam suurt midagi juurde ei anna...?

test20 = testfun(20, vars, data)
hist(test20)

test100_9plus = testfun(100, vars = vars[test20 > 9], data = data) #13 nüüd
hist(test100_9plus)

vars[test20 > 9][test100_9plus > 10]
test500_10plus = testfun(500, vars = vars[test20 > 9][test100_9plus > 10], data = data)
test500_10plus


#nüüd sama asi proovida esialgse süsteemiga. Ehk kõige levinuma puuliigi ennustuse põhjal
#ja siis nende põhjal kaugustele kaalud!?




var_valik = c("B06_kevad1","B06_kevad2","B06_sygis2","B07_sygis2","B08_kevad2","B08_sygis1","B08_sygis2","B5_kevad2")


data0 = data; data0[,2:252] = scale(data[,2:252]);
vars0 = vars[70:251]; #"Int" võib julgesti välja võtta
vars = vars[1:69]


aic_knn4 = function(vars, liik){
  data1 = data[,c("SID", vars,"cl")]
  pred  = fun_agre_test(data1, data_puud)
  pred = pred / rowSums(pred)
  true = data_puud_raie / rowSums(data_puud_raie)
  rsdls = (pred - true)[,liik]#paraku keskmine pole kindlasti 0.
  RSS = sum(rsdls**2)
  if(is.na(RSS)){
    print(names(data1))
    print(c("pred", pred))
  }
  RSS
}

#miks "SID"            "B03_sygis2"     "B03_vahe_kevad" "cl"   
#nt vaatluse 166 NA tagastab?

fun_agre_test = function(data, data_puud)
{
  dists = knn.cv(train = data[,2:(dim(data)[2]-1)], cl = data$cl, k = 10)
  dist1 = attr(dists,"nn.dist")
  dist1 = dist1 + 1e-10
  #print(dist1[11:13,])
  index1 = attr(dists,"nn.index")
  props = apply(dist1, 1, propcumsum1) #erinevad proportsioonide vektorid võimalikud
  props = t(props)
  indxprops = cbind(index1, props)
  #print(indxprops[11:13,])
  data_puud = data_puud
  retr = data.frame(round(t(apply(indxprops, 1, agre)),0))
  #print(retr[11:13,])
  #print(dim(data))
  #print(dim(data_puud))
  retr
}

tst1 = fun_agre_test(data = data0[, c("SID", "B07_vahe_kevad","B07_sygis2","cl")], data_puud)
#probleem tekib selles, et hetkel võib leiduda lähim naaber kaugusega 0!
#muutsin: dist1 = dist1 + 1e-10

vars_Int = vars0[-grep("Int",vars0)] 

test_lidar10 = testfun(10, vars_Int, data0)
hist(test_lidar10)

test_lidar50_1.5plus = testfun(50, vars = vars_Int[test_lidar10 > 1.5], data = data0) 
hist(test_lidar50_1.5plus)

test_lidar150_2plus = testfun(150, vars = vars_Int[test_lidar10 > 1.5][test_lidar50_1.5plus > 2], data = data0) 
hist(test_lidar150_2plus)

vars_lidar = vars_Int[test_lidar10 > 1.5][test_lidar50_1.5plus > 2][test_lidar150_2plus > 2]
var_valik = c("B06_kevad1","B06_kevad2","B06_sygis2","B07_sygis2","B08_kevad2","B08_sygis1","B08_sygis2","B5_kevad2")

vars_sat_lid = c(var_valik, vars_lidar)

data00 = data0[,c("SID", vars_sat_lid, "cl")]

#lisame kolmesele komplektile
#3-kaupa 13-st oleks 286 kombinatsiooni
test_all_50 = testfun(50, vars_sat_lid, data00, rn = 3)
hist(test_all_50)
plot(sort(test_all_50), type = "o")
#"lidar" ilmselgelt ei anna suurt midagi juurde

test_all_50_rn4 = testfun(50, vars_sat_lid, data00, rn = 4, liik = 1)
hist(test_all_50_rn4)
plot(sort(test_all_50_rn4), type = "o")

#kask
test_all_50_rn4_kask = testfun(50, vars_sat_lid, data00, rn = 4, liik = 3)
hist(test_all_50_rn4_kask)
plot(sort(test_all_50_rn4_kask), type = "o")

#algne "max":
aic_max = function(vars, liik){
  data1 = data[,c(vars,"cl")]
  pred  = fun_agre_test(data1, data_puud)
  true = data_puud_raie / rowSums(data_puud_raie)
  true_max = apply(true,1,max)
  indx_max_true = apply(true, 1, which.max)
  pred_max = pred[cbind(1:nrow(pred), unlist(indx_max_true))] / rowSums(pred)
  rsdls = pred_max - true_max
  RSS = sum(rsdls**2)
  RSS
}

test_all_50_rn4_max = testfun(50, vars_sat_lid, data00, rn = 4, aic_fun = aic_max, liik = 2) #kuigi liik tegelt suva
hist(test_all_50_rn4_max)
plot(sort(test_all_50_rn4_max), type = "o")

#tuleb nullist alustada...
#
vars_uus = c(vars, vars_Int)
data000 = data0[,c("SID", vars_uus, "cl")]
Sys.time(); t200_all = testfun(200, vars_uus, data000, rn = 4, aic_fun = aic_max, liik = 2); Sys.time()
#~6h
hist(t200_all)
plot(t200_all)

#ei andnud head pilti, rn3?
t100_rn3 = testfun(100, vars_uus[t200_all > 2.5], data000[,c("SID", vars_uus[t200_all > 2.5], "cl")], rn = 3, aic_fun = aic_max, liik = 2); Sys.time()
hist(t100_rn3)
plot(t100_rn3)

t100_rn3_5plus = testfun(100, vars_uus[t200_all > 2.5][t100_rn3 > 5], data000[,c("SID", vars_uus[t200_all > 2.5][t100_rn3 > 5], "cl")], rn = 3, aic_fun = aic_max, liik = 2); Sys.time()
hist(t100_rn3_5plus)
plot(t100_rn3_5plus)
#pigem arvutada ikka iga puuliik eraldi ja korrutada kaaludega läbi üldise RSS arvutamisel


aic_weighted = function(vars, liik){
  data1 = data[,c(vars,"cl")]
  pred  = fun_agre_test(data1, data_puud)
  pred = pred  / rowSums(pred)
  true = data_puud_raie / rowSums(data_puud_raie)
  w = colSums(true) / dim(data1)[1]
  rsdls = (pred - true)*w
  rsdls
  #RSS = sum(rsdls**2)
  #RSS
}

var_test = vars_uus[t200_all > 2.5][t100_rn3 > 5]
data_test = data0[,c("SID",var_test,"cl")]

test1 = testfun(10, var_test, data_test, rn = 3, aic_fun = aic_weighted, liik = 2)
hist(test1)
plot(test1)

test12 = testfun(10, var_test, data_test, rn = 4, aic_fun = aic_weighted, liik = 2)
hist(test12)
plot(test12)

par(mfrow = c(1,2))
plot(test1);plot(test12)

#

fun_vars = function(vars, n, rn){
  k = 1
  while(length(vars) > 10){
  data_vars = data0[,c("SID", vars, "cl")]
  vars_RSS = testfun(n, vars, data_vars, rn = rn, aic_fun = aic_weighted, liik = 2)
  nam = paste("vaheseis", k, sep = "_") 
  assign(nam, cbind(vars, vars_RSS))
  vars = vars[vars_RSS > quantile(vars_RSS, 0.5)]
  print(vars)
  k = k+1
  }
  final = cbind(vars, vars_RSS[vars_RSS > quantile(vars_RSS, 0.5)])
  to_return = list("seis1" = vaheseis_1, "seis2" = vaheseis_2,"seis3" = vaheseis_3, "final" = final)
}

testfun = function(n, vars, data, rn = 2, liik = 1, aic_fun = aic_knn4){
  test = c()
  for(i in 1:length(vars)){
    test[i] = panus3(data = data, var = vars[i], n = n, aic_fun, rn, liik)
    print(cbind(i, vars[i], test[i]))
  }
  test
}

rec = fun_vars(vars)
#sama uuesti:
rec1 = fun_vars(vars, n)

rec100 = fun_vars(vars)
#BTW: kknn pakett "kaalub" kaugusi, mh "epanechnikov"

w = as.numeric(rec100[,2])
w = sqrt(w / 207)
w = w / sum(w)

tst1 = aic_weighted(vars[1:5],1)
tst2 = aic_weighted(vars[6:10],1)
tst2 = aic_weighted(vars[11:15],1)

aic_weighted = function(vars, liik){
  data1 = data[,c(vars,"cl")]
  pred  = fun_agre_test(data1, data_puud)
  pred = pred  / rowSums(pred)
  true = data_puud_raie / rowSums(data_puud_raie)
  w = colSums(true) / dim(data1)[1]
  rsdls = (pred - true)
  rsdls
  cols = colSums(rsdls**2)
  RSS_col = cols*w
  sum(RSS_col)
}

tst = fun_vars(vars,10)
tst10 = tst

Sys.time(); tst50 = fun_vars(vars,50); Sys.time()

hist(as.numeric(tst50$seis1[,2]))
tst50

Sys.time(); tst50_rn2 = fun_vars(vars,50); Sys.time()
hist(as.numeric(tst50_rn2$seis1[,2]))

#vaatame, kas lidar annab midagi:
sat_vars = tst50_rn2$final[,1]

fun_vars1 = function(vars, n){
  k = 1
  while(length(vars) > 20){
    data_vars = data0[,c("SID", sat_vars, vars, "cl")]
    vars_RSS = testfun(n, vars, data_vars, rn = 2, aic_fun = aic_weighted, liik = 2)
    nam = paste("vaheseis", k, sep = "_") 
    assign(nam, cbind(vars, vars_RSS))
    vars = vars[vars_RSS > quantile(vars_RSS, 0.7)]
    print(vars)
    k = k+1
  }
  final = cbind(vars, vars_RSS[vars_RSS > quantile(vars_RSS, 0.5)])
  to_return = list("seis1" = vaheseis_1, "seis2" = vaheseis_2,"seis3" = vaheseis_3, "final" = final)
}

#vars_Int on ilma intensiivsusteta lidari tunnused

lid_vars =  c("K_Return_4_count","K_Total_all_returns",
              "H_Return_1_count_above_130","H_Return_2_count_above_130",
              "H_Return_4_count_above_130","H_Return_9_count_above_130",
              "H_Elev_maximum","H_Elev_mode","H_Elev_P10","H_Elev_P40",
              "H_Elev_P80","H_Total_return_count")  #välja nopitud, viimase lisasin manuaalselt

lid_test = testfun(100, lid_vars, 
                   data0[,c("SID", sat_vars, lid_vars, "cl")],
                   rn = 2, aic_fun = aic_weighted, liik = 1)

#nii, et testitavad poleks ise var-ide hulgas, mille seast valitakse
panus4 = function(data, test_var, samp_var, n, aic_fun, rn, liik){
  #var: millise tunnuse mõju uurime?
  #n: mitu juhuslikku tunnuste kombinatsiooni võtame, mille korral muutust jälgime?
  difs = c()
  for(k in 1:n){
    #rn = 3 #võtame 3 tunnust ja paneme ühe juurde
    rvar = sample(samp_var, rn) #juhuslikult valitud tunnused
    varx = c(test_var,rvar) # ... koos meile huvipakkuva tunnusega
    #print(var1)
    dif = aic_fun(rvar, liik) - aic_fun(varx, liik)
    difs = c(difs,dif)
  }
  return(mean(difs))
}

testfun1 = function(n, test_var, samp_var, data, rn = 2, liik = 1, aic_fun = aic_knn4){
  test = c()
  for(i in 1:length(test_var)){
    #print(i); print(test_var[i])
    test[i] = panus4(data = data, test_var = test_var[i], samp_var, n = n, aic_fun, rn, liik)
    print(cbind(i,test_var[i], test[i]))
  }
  test
}

lid_test = testfun1(20, c("B02_kevad1",lid_vars), sat_vars,
                   data0[,c("SID", sat_vars, lid_vars, "cl")],
                   rn = 4, aic_fun = aic_weighted)
#see B02 testiks lhc
#aga kuidas kõik ikka nii samas augus? midagi valesti? või liiga väha arguemente?
#rn2 andis pmst kõik ~7

lid_test = testfun1(50, c("B02_kevad1",lid_vars), sat_vars,
                    data0[,c("SID", sat_vars, lid_vars, "cl")],
                    rn = 4, aic_fun = aic_weighted)
#aiateibad

Sys.time(); sat_test50_rn5 = fun_vars(vars,50, rn = 5); Sys.time()
hist(as.numeric(sat_test50_rn5$seis1[,2]))
sat_test50_rn5$final

Sys.time(); sat_test250_rn5 = fun_vars(vars,250, rn = 5); Sys.time()
sat_test250_rn5$final
hist(as.numeric(sat_test50_rn5$seis1[,2]));hist(as.numeric(sat_test250_rn5$seis1[,2]))


Sys.time(); sat50_rn4_UUS = fun_vars(vars,50, rn = 4); Sys.time()


