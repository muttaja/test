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

data.cut = cbind(puud_true, dp[,1:4])
dim(data.cut)
MRSS(data.cut)

data.cut = cbind(dats[,c(11:14)],dats[,c(2:5)])


MRSS = function(data){
  sqrt(sum((data[,1:4]-data[,5:8])**2)/(dim(data)[1]*4))
}

rss_cut_hilow = function(cut,data){
  #data = data.cut
  #cut = 0.1
  data1 = data[,5:8];
  data1[data1 < cut] = 0;data1[data1 > (1-cut)] = 1;
  data1 = data1/ rowSums(data1)
  data2 = cbind(data[,1:4],data1)
  MRSS(data2)
}


#kas see hui-low oli mul ikka õige!?
require(scales)
rss_cut_hilow2 = function(cut,data){
  #data = data.cut
  #cut = 0.1
  data1 = data[,5:8];
  data1[data1 < cut] = cut;data1[data1 > (1-cut)] = 1-cut;
  #nüüd scale veergude kaupa
  data1[,1] = rescale(data1[,1], to = c(0,1))
  data1[,2] = rescale(data1[,2], to = c(0,1))
  data1[,3] = rescale(data1[,3], to = c(0,1))
  data1[,4] = rescale(data1[,4], to = c(0,1))
  
  data1 = data1/ rowSums(data1)
  data2 = cbind(data[,1:4],data1)
  MRSS(data2)
}

cutrss = c()
for(i in 1:200){
  cutrss[i] = rss_cut_hilow2(i / 1000, data.cut)}
par(mfrow = c(1,1))
plot(cutrss, type = "o")
which.min(cutrss)
min(cutrss)

#0.1663449 -->> 0.165294 Knn n = 5, 455 (ehk tm 100+)

rss_cut_hilow2_data = function(cut,data){
  #data = data.cut
  #cut = 0.1
  data1 = data[,5:8];
  data1[data1 < cut] = cut;data1[data1 > (1-cut)] = 1-cut;
  #nüüd scale veergude kaupa
  data1[,1] = rescale(data1[,1], to = c(0,1))
  data1[,2] = rescale(data1[,2], to = c(0,1))
  data1[,3] = rescale(data1[,3], to = c(0,1))
  data1[,4] = rescale(data1[,4], to = c(0,1))
  
  data1 = data1/ rowSums(data1)
  data2 = cbind(data[,1:4],data1)
}

dats1 = rss_cut_hilow2_data(0.008, data.cut)

dev.off()
par(mfrow = c(2,2))
plot(dats1[,1],dats1[,5], xlab = "MÄND", ylab = "Hinnang KNN: K=5", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dats1[,2],dats1[,6], xlab = "KUUSK", ylab = "Hinnang KNN: K=5", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dats1[,3],dats1[,7], xlab = "KASK", ylab = "Hinnang KNN: K=5", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))
plot(dats1[,4],dats1[,8], xlab = "MUU", ylab = "Hinnang KNN: K=5", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369))





