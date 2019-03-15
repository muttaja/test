#multinomial_pca;
#ehk järjest PC 1...12 ilma stepita


multinom_pca = function(k,data){
  rtl = vector("list", length = 4)
  ma_list = vector("list", length = k)
  ku_list = vector("list", length = k)
  ks_list = vector("list", length = k)
  mu_list = vector("list", length = k)
  for(j in 1:k){
    pcas = c("PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10","PC11","PC12")
    formulapca = as.formula(paste("cl", paste(c(pcas[1:j],"factor(muld)"), collapse=" + "), sep=" ~ "))
    obsvs = sidxx #601
    probs_pca = vector("list", length = length(obsvs))
    #ILMA stepita!?
    modelpca = multinom(formula = formulapca, data = data) #, weights = weight
      for(i in 1:length(obsvs)){ 
        obs = obsvs[i]
        if(!(obs %in% data$aproovitykk_id)){
          probs_pca[[i]] = predict(modelpca, newdata = dkmpca[dkmpca$aproovitykk_id == obs,], type = "probs")
      }
      else{
        model = multinom(formulapca, data[data$aproovitykk_id != obs,], weights = weight) #, weights = weight
        probs_pca[[i]] = predict(model, newdata = data[data$aproovitykk_id == obs,], type = "probs")
      }
    }

  dfprob <- data.frame(matrix(unlist(probs_pca), nrow=length(probs_pca), byrow=T))
  #KS KU KX MA
  ma_list[[j]] = sqrt(mean((data_puud_raie_props[,1]- dfprob[,4])**2))
  ku_list[[j]] = sqrt(mean((data_puud_raie_props[,2]- dfprob[,2])**2))
  ks_list[[j]] = sqrt(mean((data_puud_raie_props[,3]- dfprob[,1])**2))
  mu_list[[j]] = sqrt(mean((data_puud_raie_props[,4]- dfprob[,3])**2))
  }
  rtl[[1]] = ma_list; rtl[[2]] = ku_list; rtl[[3]] = ks_list; rtl[[4]] = mu_list
  rtl
}

mnpca = multinom_pca(12)


#ilmselgelt ainult 1. PC kasutamine on kõige etem...

#kui kaalusid ka kasutada:
mnpca_w = multinom_pca(12)

#kui võtta vaatlused, kus vähemalt 50% ühe klassi esindajaid
dpc50 = dkmpca[dkmpca$aproovitykk_id %in% dkm50_$aproovitykk_id,]
dpc50$cl = dkm50_$cl
mnpca50 = multinom_pca(12, data = dpc50)



par(mfrow = c(4,2))

#palju üldse viga oleks, kui võtaks prognoosiks keskmise :D?
sqrt(mean((data_puud_raie_props - colMeans(data_puud_raie_props))**2))

tst = data_puud_raie_props - colMeans(data_puud_raie_props)
tst = scale(data_puud_raie_props, scale = F)
sqrt(colMeans(tst**2))


plot(unlist(mnpca[[1]]), type = "o", xlab = "peakomponente", ylab = "sqrt(mean(error**2))")
plot(unlist(mnpca[[2]]), type = "o", xlab = "peakomponente", ylab = "sqrt(mean(error**2))")
plot(unlist(mnpca[[3]]), type = "o", xlab = "peakomponente", ylab = "sqrt(mean(error**2))")
plot(unlist(mnpca[[4]]), type = "o", xlab = "peakomponente", ylab = "sqrt(mean(error**2))")

plot(unlist(mnpca_w[[1]]), type = "o")
plot(unlist(mnpca_w[[2]]), type = "o")
plot(unlist(mnpca_w[[3]]), type = "o")
plot(unlist(mnpca_w[[4]]), type = "o")

plot(unlist(mnpca50[[1]]), type = "o", xlab = "peakomponente", ylab = "sqrt(mean(error**2))")
plot(unlist(mnpca50[[2]]), type = "o", xlab = "peakomponente", ylab = "sqrt(mean(error**2))")
plot(unlist(mnpca50[[3]]), type = "o", xlab = "peakomponente", ylab = "sqrt(mean(error**2))")
plot(unlist(mnpca50[[4]]), type = "o", xlab = "peakomponente", ylab = "sqrt(mean(error**2))")

mean(unlist(mnpca[[1]]) - unlist(mnpca_w[[1]])) #parem :D
mean(unlist(mnpca[[2]]) - unlist(mnpca_w[[2]]))
mean(unlist(mnpca[[3]]) - unlist(mnpca_w[[3]]))
mean(unlist(mnpca[[4]]) - unlist(mnpca_w[[4]]))


pcas = c("PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10","PC11","PC12")
formulapca = as.formula(paste("cl", paste(c(pcas[1:1],"factor(muld)"), collapse=" + "), sep=" ~ "))
obsvs = sidxx #601
probs_pca = vector("list", length = length(obsvs))
data = dpc50
modelpca = multinom(formula = formulapca, data = dpc50) #, weights = weight
for(i in 1:length(obsvs)){ 
  obs = obsvs[i]
  if(!(obs %in% data$aproovitykk_id)){
    probs_pca[[i]] = predict(modelpca, newdata = dkmpca[dkmpca$aproovitykk_id == obs,], type = "probs")
  }
  else{
    model = multinom(formulapca, data[data$aproovitykk_id != obs,], weights = weight) #, weights = weight
    probs_pca[[i]] = predict(model, newdata = data[data$aproovitykk_id == obs,], type = "probs")
  }
}

dfprob1_50 <- data.frame(matrix(unlist(probs_pca), nrow=length(probs_pca), byrow=T))

hist(dfprob1_50[,2])

par(mfrow = c(2,2))
plot(data_puud_raie_props[,1],dfprob1_50[,4], xlab = "osakaal MÄND", ylab = "multinom prognoos: 50%+, PC1+muld")
plot(data_puud_raie_props[,2],dfprob1_50[,2], xlab = "osakaal KUUSK", ylab = "multinom prognoos: 50%+, PC1+muld")
plot(data_puud_raie_props[,3],dfprob1_50[,1], xlab = "osakaal KASK", ylab = "multinom prognoos: 50%+, PC1+muld")
plot(data_puud_raie_props[,4],dfprob1_50[,3], xlab = "osakaal MUU", ylab = "multinom prognoos: 50%+, PC1+muld")

sqrt(mean((data_puud_raie_props[,1]-dfprob1_50[,4])**2))
sqrt(mean((data_puud_raie_props[,2]-dfprob1_50[,2])**2))
sqrt(mean((data_puud_raie_props[,3]-dfprob1_50[,1])**2))
sqrt(mean((data_puud_raie_props[,4]-dfprob1_50[,3])**2))


#PC1, PC2
plot(data_puud_raie_props[,1],dfprob2[,3])
plot(data_puud_raie_props[,2],dfprob2[,2])
plot(data_puud_raie_props[,3],dfprob2[,1])
plot(data_puud_raie_props[,4],dfprob2[,3])


#proovi kaaludega ja ilma
#proovi teisi väärtusi kui 80%
