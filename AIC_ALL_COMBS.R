#"AIC" kõik kombinatsioonid

panus = function(data, var, samp_var, n, aic_fun, rn, liik){
  difs = c()
  rvars = combn(samp_var, rn)
  n = dim(rvars)[2]
  #print(c("n",n))
  for(k in 1:n){
    #print(c("k",k))
    rvar = rvars[,k]
    var1 = c(var,rvar)
    dif = aic_fun(rvar, liik, data) - aic_fun(var1, liik, data)
    difs = c(difs,dif)
    #print(rvar); print(var1); print(difs)
  }
  return(mean(difs))
  #return(difs)
}


testfun = function(n, test_var, samp_var, data, rn, liik = 1, aic_fun){
  test = c()
  for(i in 1:length(test_var)){
    test[i] = panus(data = data, var = test_var[i], samp_var, n = n, aic_fun, rn, liik)
    print(cbind(i,test_var[i], test[i]))
  }
  test
}


aic_weighted = function(vars, liik){
  data1 = data[,c("SID",vars,"cl")]
  #print(names(data1)); print(dim(data1))
  pred  = fun_agre_test(data1, data_puud)
  pred = pred  / rowSums(pred)
  true = data_puud_raie_props
  w = colSums(true) / dim(data1)[1]
  rsdls = (pred - true)
  rsdls
  cols = colSums(rsdls**2)
  RSS_col = cols*w
  sum(RSS_col)
}

fun_agre_test = function(data, data_puud)
{
  train = data[,2:(dim(data)[2]-1)]
  #print(names(train))
  dists = knn.cv(train, cl = data$cl, k = 10)
  dist1 = attr(dists,"nn.dist")
  dist1 = dist1 + 1e-10
  #print(dist1)
  #print(dist1[11:13,])
  index1 = attr(dists,"nn.index")
  props = apply(dist1, 1, propcumsum1) #erinevad proportsioonide vektorid võimalikud
  props = t(props)
  indxprops = cbind(index1, props)
  #print(indxprops[11:13,])
  data_puud = data_puud
  retr = data.frame(t(apply(indxprops, 1, agre)))
  #print(retr[11:13,])
  #print(dim(data))
  #print(dim(data_puud))
  retr
}



#proovime 8-st 4-kaupa, st 70 kombinatsiooni:

#praegu 12 varem välja vaadatud
#n hetkel ei loe
sampvars = c("B11_sygis1", "B11_sygis2", "B4_kevad1",  "B4_sygis1",  "B5_sygis1") 
#viimanen eist tunduvalt nõrgem: 0.7362693 0.6232957 0.6906128 0.6322121 0.2565842
lidall = testfun(n = 1, vars_Int, sampvars, data0, rn = 3, liik = 1, aic_fun = aic_weighted)
#testime kõigi lidari meetrikuid v.a. intensiivuses

hist(lidall)
lidall[lidall > 0.7]
vars_Int[lidall > 0.7]
plot(sort(lidall), type = "o")

SAT_LID = c(sampvars, vars_Int[lidall > 0.7])#testmiseks kõikid kombide korral!

test_sat_lid = testfun(n = 1, SAT_LID, SAT_LID, data0, rn = 4, liik = 1, aic_fun = aic_weighted)

satlid1 = c(sampvars, "K_First_returns_above_mean", "H_Return_3_count_above_130")
satlid2 = satlid1[-2]

tt_rn2 = test_sat_lid = testfun(n = 1, satlid1, satlid1, data0, rn = 2, liik = 1, aic_fun = aic_weighted)
tt_rn3 = test_sat_lid = testfun(n = 1, satlid1, satlid1, data0, rn = 3, liik = 1, aic_fun = aic_weighted)
tt1 = test_sat_lid = testfun(n = 1, satlid2, satlid2, data0, rn = 3, liik = 1, aic_fun = aic_weighted)

satlid3 = satlid2[-6]
tt2 = test_sat_lid = testfun(n = 1, satlid3, satlid3, data0, rn = 3, liik = 1, aic_fun = aic_weighted)
tt3 = test_sat_lid = testfun(n = 1, satlid3, satlid3, data0, rn = 2, liik = 1, aic_fun = aic_weighted)

#wtf, sygis1 ja sygis2 on täpselt samad asjad? aint natuke nihkes???

