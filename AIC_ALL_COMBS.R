#"AIC" kõik kombinatsioonid

panus = function(data, var, samp_var, n, aic_fun, rn, liik){
  difs = c()
  rvars = combn(samp_var, rn)
  n = dim(rvars)[2]
  print(c("n",n))
  for(k in 1:n){
    print(c("k",k))
    rvar = rvars[,k]
    var1 = c(var,rvar)
    dif = aic_fun(rvar, liik) - aic_fun(var1, liik)
    difs = c(difs,dif)
    print(rvar); print(var1); print(difs)
  }
  #return(mean(difs))
  return(difs)
}


testfun = function(n, test_var, samp_var, data, rn, liik = 1, aic_fun){
  test = c()
  print(test_var)
  for(i in 1:length(test_var)){
    test[i] = panus(data = data, var = test_var[i], samp_var, n = n, aic_fun, rn, liik)
    print(cbind(i,test_var[i], test[i]))
  }
  test
}


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

#proovime 8-st 4-kaupa, st 70 kombinatsiooni:

#praegu 12 varem välja vaadatud
#n hetkel ei loe
sampvars = sat_test250_rn5$final[,1]
lidall = testfun(n = 1, lid1, sampvars, data0, rn = 4, liik = 1, aic_fun = aic_weighted)
lid1 = c("B03_kevad1", lid_vars)

panus(data0, lid1[1], sampvars, 1, aic_weighted, 4, 1)


