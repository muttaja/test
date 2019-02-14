#cut_low
#palju alt "lõigata vöikseid prognoose?

rss_cut = function(cut,data){
  data1 = data;
  data1[data < cut] = 0;
  data1 = data1/ rowSums(data1)
  WRSS(data1)
}
cutrss = c()
for(i in 1:200){
  cutrss[i] = rss_cut(i / 1000, lidlessB)}
par(mfrow = c(1,1))
plot(cutrss, type = "o")
which.min(cutrss) #0.01 seega :/ pole mõtet neid kõrvaldada
min(cutrss)

C35 = HPMB0; C35[C35 < 0.035] = 0; C35 = C35 / rowSums(C35)

par(mfrow = c(3,3))
plot(C35$MA, HPM1$T)
plot(C35$KU, HPM2$T)
plot(C35$KS, HPM3$T)
plot(C35$HB, HPM4$T)
plot(C35$LM, HPM5$T)
plot(C35$LV, HPM6$T)
plot(C35$KX, HPM7$T)
