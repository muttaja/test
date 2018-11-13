### kNN, distantsid ###

require(FNN)
dists = knn.cv(train = data4all[,3:6], cl = data4all$ENAMUS, k = 10)

#tore oleks kuskil mignit põlve näha
dist1 = attr(dists,"nn.dist")
index1 = attr(dists,"nn.index")

#https://stackoverflow.com/questions/2018178/finding-the-best-trade-off-point-on-a-curve
#põhjal 
elbow <- function(arg){
  y_values = cumsum(arg)
  x_values = 1:length(arg)
  max_df = data.frame(x = c(1,10), y = c(y_values[1],y_values[10])) 
  fit <- lm(max_df$y ~ max_df$x)
  distances = c()
  for(i in 1:10){
    distances <- c(distances, abs(coef(fit)[2]*x_values[i] - y_values[i] + coef(fit)[1]) / sqrt(coef(fit)[2]^2 + 1^2))
  }
  which.is.max(distances)
}

maxpoint = apply(dist1, 1, elbow)
hist(maxpoint)

#nüüd vastavate kaaludega cumsumist

propcumsum <- function(arg){
  maxpoint = elbow(arg)
  data = arg
  if(maxpoint != length(arg)){
    data[(maxpoint+1):length(arg)] = data[maxpoint+1]
  }
  props = 1 - (data/ data[maxpoint+1])
  props = props/(sum(props))
  props
}

props = apply(dist1, 1, propcumsum)
props = t(props)

#nüüd korrutame lihtsalt läbi :)
#dataw
sum (dataw[index1[1,],]$ARV_VMA * props[1,])
sum (dataw[index1[1,],]$ARV_VKU * props[1,])

colSums(dataw[index1[1,],c("ARV_VMA", "ARV_VKU")] * props[1,])
#1. sid mänd ja kuusk

(dataw[index1,c("ARV_VMA", "ARV_VKU", "ARV_VKS", "ARV_VHB", "ARV_VLM", "ARV_VLV", "ARV_VXX")] * props)

#agr_data = 
puud = c("ARV_VMA", "ARV_VKU", "ARV_VKS", "ARV_VHB", "ARV_VLM", "ARV_VLV", "ARV_VXX")
indxprops = cbind(index1, props)
agre <- function(arg){
  #"arg"-ist osa indexid, osa neile vastavad propsid
  indx = arg[1:10]; props = arg[11:20]
  colsums = colSums(dataw[indx, puud]*props)
}

agre1 <- function(arg, data1){
  #"arg"-ist osa indexid, osa neile vastavad propsid
  indx = arg[1:10]; props = arg[11:20]
  colsums = colSums(data1[indx, puud]*props)
}

MAND_KUUSK_HAAB = apply(indxprops, 1, agre)
vecs1 = apply(indxprops, 1, function(x) agre1(x, dataw))

###sama asi, aga terve andmestiku peal?
#dataw1 see andmestik, kust pole midagi välja võetud

PCA100 = prcomp(dataw1[,c(2:31,56:60)])
summary(PCA100)
cumvar = cumsum(sat.all$sdev**2)/sum(sat.all$sdev**2)
cumvar

pca4_100 = PCA100$x[,1:4]
data100 = cbind(dataw1[,c("SID", "ENAMUS")], pca4_100)
data100 = merge(data100, muld1, by = "SID", all.x = T)



KNN100 = knn.cv(train = data100[,3:6], cl = data100$ENAMUS, k = 10)

indx100 = attr(KNN100, "nn.index"); props100 = t(apply(attr(KNN100, "nn.dist"), 1, propcumsum))
indx100; props100

indxprops_all = cbind(indx100, props100)

KOIK_VEKTORID = t(apply(indxprops_all, 1, function(x) agre1(x,dataw1)))

ERINEVUSED = abs(KOIK_VEKTORID - dataw1[,puud]) #mõttetu töö

#proovime erinevat prop vektorit?
propcumsum1 <- function(arg){
  maxpoint = elbow(arg)
  data = arg
  props = c()
  if(maxpoint != length(arg)){
    data[(maxpoint+1):length(arg)] = 0
  }
  props[1:maxpoint] = 1 - (data/ sum(data)); props[(maxpoint+1):length(arg)] = 0
  props = props/(sum(props))
  props
}
  
props100x = t(apply(attr(KNN100, "nn.dist"), 1, propcumsum1))
round(props100x,3)


