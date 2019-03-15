#uus AIC, koos andmetega
nms = names(sat18_wide); nms[1] = "SID"; names(sat18_wide) = nms

sat18_lidar = merge(sat18_wide, lidar, by = "SID", all = T)
sat18_lidar = na.omit(sat18_lidar) #nüüd 966 ja järgnevat jama pole vaja


puud = c("ARV_VMA", "ARV_VKU", "ARV_VKS", "ARV_VHB", "ARV_VLM", "ARV_VLV", "ARV_VKX") #NB! uues andmestikus ARV_VKX!
#siit välja need, kus puid pole taks põhjal!
taks_uus1 = taks_uus[(taks_uus$SID %in% kontrollitud_SID) & taks_uus$arv_maht_es > 50,] #üle 50
sidxx = intersect(kontrollitud_SID, taks_uus1$SID) #207
data_puud = taks_uus[taks_uus$SID %in% sidxx,puud]
data = sat18_lidar[sat18_lidar$SID %in% sidxx,]
data$cl = "cl"
data_puud_raie = taks_uus[taks_uus$SID %in% sidxx,puud]

#funktsioon, mis testib tunnuste olulisust:
testfun = function(n, test_var, samp_var, data, rn, liik = 1, aic_fun){
  #mitu juhuslikku tunnuste gruppi; milliseid tunnuseid testime; milliste tunnuste hulgas; mis andmetel;
  #mitu tunnust valime; liik, kui tahta teada panust phe konkreetse liigi korral; millist "aic" või rss funi kasutame;
  test = c()
  for(i in 1:length(test_var)){
    test[i] = panus(data = data, test_var = test_var[i], samp_var, n = n, aic_fun, rn, liik)
    print(cbind(i,test_var[i], test[i]))
  }
  test
}

panus = function(data, test_var, samp_var, n, aic_fun, rn, liik){
  #var: millise tunnuse mõju uurime?
  difs = c()
  for(k in 1:n){
    #rn = 3 #võtame 3 tunnust ja paneme ühe juurde
    rvar = sample(samp_var, rn) #juhuslikult valitud tunnused
    varx = c(test_var,rvar) # ... koos meile huvipakkuva tunnusega
    dif = aic_fun(rvar, liik, data) - aic_fun(varx, liik, data)
    difs = c(difs,dif)
  }
  return(mean(difs))
}

aic_weighted = function(vars, liik, data){
  data1 = data[,c("SID",vars,"cl")]
  pred  = fun_agre_test(data1, data_puud)
  pred = pred  / rowSums(pred)
  true = data_puud_raie_props
  w = colSums(true) / dim(data1)[1] #kaalud vastavalt kui levinud on puuliik
  rsdls = (pred - true)
  rsdls
  cols = colSums(rsdls**2)
  RSS_col = cols*w
  sum(RSS_col)
}


fun_agre_test = function(data, data_puud)
{
  train = data[,2:(dim(data)[2]-1)]
  dists = knn.cv(train, cl = data$cl, k = 10)
  dist1 = attr(dists,"nn.dist")
  dist1 = dist1 + 1e-10
  index1 = attr(dists,"nn.index")
  props = apply(dist1, 1, epa)
  props = t(props)
  indxprops = cbind(index1, props)
  data_puud = data_puud
  retrn = data.frame(t(apply(indxprops, 1, agre)))
  retrn
}

epa <- function(vec){
  props = 3/4*(1-(vec / vec[length(vec)])**2)
  props = props/sum(props)
  props
}



data10 = data; data10[,2:243] = scale(data[,2:243]);
vars_lid = names(data10)[62:243]; #"Int" võib julgesti välja võtta
vars_sat = names(data10)[2:61]
vars_Int = vars_lid[-grep("Int",vars0)]

test_sat = testfun(n = 100, test_var = vars_sat, samp_var = vars_sat,
                   data = data10, rn = 5, liik = 1, aic_fun = aic_weighted)

hist(test_sat)
test_sat[test_sat > quantile(test_sat, 0.5)]
vars_sat1 = vars_sat[test_sat > quantile(test_sat, 0.5)]

test_sat1 = testfun(n = 100, test_var = vars_sat1, samp_var = vars_sat1,
                   data = data10, rn = 5, liik = 1, aic_fun = aic_weighted)
hist(test_sat1)
plot(sort(test_sat1), type = "o")

vars_sat2 = vars_sat1[test_sat1 > quantile(test_sat1, 0.5)]
vars_sat2

test_sat2 = testfun(n = 100, test_var = vars_sat2, samp_var = vars_sat2,
                    data = data10, rn = 5, liik = 1, aic_fun = aic_weighted)
hist(test_sat2)
plot(sort(test_sat2), type = "o")

round(cor(data10[,vars_sat2]),2)
round(rowSums(cor(data10[,vars_sat2]))/length(vars_sat2),2)

#nopime käsitsi, nii et sama kanal kevad1-kevad2 poleks mõlemad
#B07_kevad2 jääb nii välja
ekspert = c("B2_sygis", "B07_kevad1","B4_sygis","B04_kevad1","B5_sygis","B06_kevad2")
eksp_w = c(0.23892375,0.22223079,0.15061512, 0.12824842,0.11369736,0.09852142)
eksp_w1 = eksp_w / sum(eksp_w)

#või võtta siit kõik, v.a 2 esimest (ja korduvad?)

#10. ja 13. kui väga väikesed, 4. kui korduv

ekspert1 = vars_sat2[-c(10,13)]
eksp1_w = test_sat2[-c(10,13)]
eksp1_w1 = eksp1_w / sum(eksp1_w)

#jooksutame siis lidari ka läbi:
test_lidar = testfun(n = 100, test_var = vars_Int, samp_var = ekspert1,
                    data = data10, rn = 5, liik = 1, aic_fun = aic_weighted)

hist(test_lidar)
plot(sort(test_lidar), type = "o")
test_lidar[test_lidar > 0.15]

grande_finale = c(ekspert1, vars_Int[test_lidar > 0.15])
test_finale = testfun(n = 200, test_var = grande_finale, samp_var = grande_finale,
                     data = data10, rn = 5, liik = 1, aic_fun = aic_weighted)

hist(test_finale)
plot(sort(test_finale), type = "o")

tf = data.frame(vars = grande_finale, vals = as.numeric(test_finale))
tf = tf[order(tf$vals, decreasing = T),]

gf1 = grande_finale[test_finale > 0.1]
tf1 = testfun(n = 200, test_var = gf1, samp_var = gf1,
                      data = data10, rn = 5, liik = 1, aic_fun = aic_weighted)

hist(tf1)
plot(sort(tf1), type = "o")

tf11 = data.frame(vars = gf1, vals = as.numeric(tf1))
tf11 = tf11[order(tf11$vals, decreasing = T),]

#kas on stabiilne?
tf1_uuesti = testfun(n = 200, test_var = gf1, samp_var = gf1,
              data = data10, rn = 5, liik = 1, aic_fun = aic_weighted)

tf_all_combos = testfun(n = 1, test_var = gf1, samp_var = gf1,
                        data = data10, rn = 5, liik = 1, aic_fun = aic_weighted)

hist(tf_all_combos - tf1_uuesti)
hist(tf_all_combos)
plot(sort(tf_all_combos), type = "o")

eksp_all = gf1[tf_all_combos > 0.05]
w_all = tf_all_combos[tf_all_combos > 0.05]
w_all1 = w_all / sum(w_all)


#jooksutame uuesti, kasutades juba leitud kaale
#11-st 5 kaupa aint 462 comb ka

data_ekspert1 = data10[,c("SID",eksp_all)]
data_ekspert1[,-1] = t((t(as.matrix(data_ekspert1[,-1])))*w_all1)
data_ekspert1$cl = "cl"

tf_all_combos1 = testfun(n = 1, test_var = eksp_all, samp_var = eksp_all,
                        data = data_ekspert1, rn = 5, liik = 1, aic_fun = aic_weighted)


plot(sort(tf_all_combos1), type = "o")

#negatiivsed välja:

eksp_all2 = eksp_all[tf_all_combos1 > 0]
w2_all = tf_all_combos1[tf_all_combos1 > 0]
w2_all1 = w2_all / sum(w2_all)

data_ekspert2 = data10[,c("SID",eksp_all2)]
data_ekspert2[,-1] = t((t(as.matrix(data_ekspert2[,-1])))*w2_all1)
data_ekspert2$cl = "cl"

tf_all_combos2 = testfun(n = 1, test_var = eksp_all2, samp_var = eksp_all2,
                         data = data_ekspert1, rn = 5, liik = 1, aic_fun = aic_weighted)

plot(sort(tf_all_combos2), type = "o") # 2 väga väikest

eksp_all3 = eksp_all2[tf_all_combos2 > 0.1]
w3_all = tf_all_combos2[tf_all_combos2 > 0.1]
w3_all1 = w3_all / sum(w3_all)

#22.01.2018
#treeningandmete homogeensust suurendatud, uued tunnused

adata = data10; adata$cl = "cl"
a1 = testfun(n = 10, test_var = vars_sat, samp_var = vars_sat,
                   data = adata, rn = 5, liik = 1, aic_fun = aic_weighted)
avars1 = vars_sat[1:45] #vahed ei tööta :S
a2 = testfun(n = 50, test_var = avars1, samp_var = avars1,
             data = adata, rn = 5, liik = 1, aic_fun = aic_weighted)

hist(a2)
plot(sort(a2),type = "o")

avars2 = avars1[a2 > 0.05]
avars2
a2[a2 > 0.05]

a3 = testfun(n = 100, test_var = avars2, samp_var = avars2,
             data = adata, rn = 5, liik = 1, aic_fun = aic_weighted)

hist(a3)
plot(sort(a3),type = "o")

avars3 = avars2[a3 > 0.1]
avars3
a3[a3 > 0.1]

a4 = testfun(n = 100, test_var = avars3, samp_var = avars3,
             data = adata, rn = 5, liik = 1, aic_fun = aic_weighted)

#tee vahepeal epanechnikovi kaalud!
hist(a4)
plot(sort(a4),type = "o")

avars4 = avars3[a4 > 0.08]
avars4
a4[a4 > 0.08]

#nüüd laseks kõik kombod läbi, see on 126 kombot
#"panus" fun on teine:
panus = function(data, test_var, samp_var, n, aic_fun, rn, liik){
  difs = c()
  rvars = combn(samp_var[!(samp_var %in% test_var)], rn)
  n = dim(rvars)[2]
  for(k in 1:n){
    rvar = rvars[,k]
    var1 = c(test_var,rvar)
    dif = aic_fun(rvar, liik, data) - aic_fun(var1, liik, data)
    difs = c(difs,dif)
  }
  return(mean(difs))
}

a_all = testfun(n = 100, test_var = avars4, samp_var = avars4,
             data = adata, rn = 5, liik = 1, aic_fun = aic_weighted)

hist(a_all)
plot(sort(a_all),type = "o")

#sama epa-ga:
a_all_epa = testfun(n = 100, test_var = avars4, samp_var = avars4,
                data = adata, rn = 5, liik = 1, aic_fun = aic_weighted)
a_all; a_all_epa
# [1]  0.06776391  0.15152942  0.27505726  0.17079010  0.19320832  0.22550076 -0.05864777  0.03163898  0.14721893
# [10]  0.38630507
# [1]  0.115407874  0.118389561  0.308034660  0.204232105  0.190499212  0.177325052 -0.038086889 -0.002376533
# [9]  0.143177929  0.299184806

hist(a_all_epa)
plot(sort(a_all_epa),type = "o")



a_all1 = avars4[a_all > 0.1]
a_all1
a_all[a_all > 0.1]

indxNA <- apply(adata, 2, function(x) any(is.na(x)))
names(adata)[indxNA]
lidar_intless = vars_Int[!(vars_Int %in% names(adata)[indxNA])]


a_lidar = testfun(n = 100, test_var = lidar_intless, samp_var = a_all1,
                          data = adata, rn = 5, liik = 1, aic_fun = aic_weighted)
hist(a_lidar)
plot(sort(a_lidar),type = "o")

#laseme need Epa.ga uuesti, mis üle 0.4
alidvar1 = lidar_intless[a_lidar > 0.4]
alid1 = a_lidar[a_lidar > 0.4]
alid1

a_lid_epa = testfun(n = 100, test_var = alidvar1, samp_var = a_all1,
                  data = adata, rn = 5, liik = 1, aic_fun = aic_weighted)

hist(a_lid_epa)
plot(sort(a_lid_epa),type = "o")

epa1 = c(avars4[a_all_epa > 0.1], alidvar1[a_lid_epa > 0.8])
epa1

aepa = testfun(n = 100, test_var = epa1, samp_var = epa1,
        data = adata, rn = 5, liik = 1, aic_fun = aic_weighted)
var_epa = epa1
epa_w = aepa / sum(aepa) * length(aepa)

##########
#Proovin viimast korda hinnata tunnuste kaalusid

#õige "panuse" fun (mitte kõik kombod)
panus = function(data, test_var, samp_var, n, aic_fun, rn, liik){
  #var: millise tunnuse mõju uurime?
  difs = c()
  for(k in 1:n){
    #rn = 3 #võtame 3 tunnust ja paneme ühe juurde
    rvar = sample(samp_var, rn) #juhuslikult valitud tunnused
    varx = c(test_var,rvar) # ... koos meile huvipakkuva tunnusega
    dif = aic_fun(rvar, liik, data) - aic_fun(varx, liik, data)
    difs = c(difs,dif)
  }
  return(mean(difs))
}

adata = data10; adata$cl = "cl"; adata = adata[adata$SID %in% sidxx,]
b1 = testfun(n = 20, test_var = vars_sat, samp_var = vars_sat,
             data = adata, rn = 8, liik = 1, aic_fun = aic_weighted)

hist(b1)
plot(sort(b1), type = "o")

v2 = c(vars_sat[b1 > 0],"B03_vahe_kevad","B07_vahe_kevad") #vahed ei tööta :S, aga testi mõttes mõned jätan

b2 = testfun(n = 50, test_var = v2, samp_var = v2,
             data = adata, rn = 8, liik = 1, aic_fun = aic_weighted)

hist(b2)
plot(sort(b2), type = "o")

v3 = c("B03_vahe_kevad",v2[b2 > 0])

b3 = testfun(n = 100, test_var = v3, samp_var = v3,
             data = adata, rn = 8, liik = 1, aic_fun = aic_weighted)

hist(b3)
plot(sort(b3), type = "o")

v4 = c(v3[b3 > 0])
b4 = testfun(n = 100, test_var = v4, samp_var = v4,
             data = adata, rn = 5, liik = 1, aic_fun = aic_weighted)

hist(b4)
plot(sort(b4), type = "o")

#praegu kippusid enamus negatiivseteks minema
b4_rn5 = testfun(n = 100, test_var = v4, samp_var = v4,
             data = adata, rn = 5, liik = 1, aic_fun = aic_weighted)
#ei saa vahest ikka asja
hist(b4_rn5)
plot(sort(b4_rn5), type = "o") 

v5 = c(v4[b4_rn5 > 0])

b5_rn5 = testfun(n = 100, test_var = v5, samp_var = v5,
                 data = adata, rn = 5, liik = 1, aic_fun = aic_weighted)

b5_rn5
hist(b5_rn5)
plot(sort(b5_rn5), type = "o") 

#kui aint neid nelja tunnust kasutada? Ei töötanud!
bvar = v5[b5_rn5 > 0.04]
bw = b5_rn5[b5_rn5 > 0.04] / sum(b5_rn5[b5_rn5 > 0.04])

#muudame panus
panus = function(data, test_var, samp_var, n, aic_fun, rn, liik){
  difs = c()
  rvars = combn(samp_var[!(samp_var %in% test_var)], rn)
  n = dim(rvars)[2]
  for(k in 1:n){
    rvar = rvars[,k]
    var1 = c(test_var,rvar)
    dif = aic_fun(rvar, liik, data) - aic_fun(var1, liik, data)
    difs = c(difs,dif)
  }
  return(mean(difs))
}


ball = testfun(n = 100, test_var = bvar, samp_var = bvar,
                 data = adata, rn = 5, liik = 1, aic_fun = aic_weighted)


#lidar_intless on vaja uurida

ball
hist(ball)
plot(sort(ball), type = "o")

ball1 = bvar[ball > 0.1]

blid = testfun(n = 100, test_var = lidar_intless, samp_var = ball1,
               data = adata, rn = 3, liik = 1, aic_fun = aic_weighted)

hist(blid)
plot(sort(blid), type = "o")

blidv = lidar_intless[blid > 0.1]
blidv
blid[blid > 0]

ballv = c(ball1, blidv)

blidall = testfun(n = 100, test_var = ballv, samp_var = ballv,
               data = adata, rn = 5, liik = 1, aic_fun = aic_weighted)

ballv1 = ballv[blidall > 0]
ballv1

blidall1 = testfun(n = 100, test_var = ballv1, samp_var = ballv1,
                  data = adata, rn = 4, liik = 1, aic_fun = aic_weighted)

blitz = ballv1[blidall1 > 0.1]
blitzwsq = sqrt(blidall1[blidall1 > 0.1]) / sum(sqrt(blidall1[blidall1 > 0.1])) * 6


########################## 25.01 pärast vigade kõrvaldamist andmestikust #####################

cdata = data10; cdata$cl = "cl"

c0 = testfun(n = 50, test_var = vars_sat, samp_var = vars_sat,
             data = cdata, rn = 5, liik = 1, aic_fun = aic_weighted)
hist(c0)
plot(sort(c0[c0 > 0.27]), type = "o")

cvar1=vars_sat[c0 > 0.27]

c1 = testfun(n = 200, test_var = cvar1, samp_var = cvar1,
             data = cdata, rn = 5, liik = 1, aic_fun = aic_weighted)

hist(c1)
plot(sort(c1), type = "o")

cvar2=cvar1[c1 > 0.2]

#nüüd kõigi kombodega
c2 = testfun(n = 200, test_var = cvar2, samp_var = cvar2,
             data = cdata, rn = 5, liik = 1, aic_fun = aic_weighted)

cexp = cvar2
wexp = c2

lid_uus = testfun(n = 200, test_var = lidar_intless, samp_var = cvar2,
                  data = cdata, rn = 12, liik = 1, aic_fun = aic_weighted)
which.max(lid_uus)

lid_uus2 = testfun(n = 200, test_var = lidar_intless[-84][-100], samp_var = c(cvar2,lidar_intless[84],lidar_intless[-84][100]),
                  data = cdata, rn = 14, liik = 1, aic_fun = aic_weighted)
which.max(lid_uus2); max(lid_uus2)

cexp_lid = c(cvar2,lidar_intless[84],lidar_intless[-84][100])

