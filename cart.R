####CART pythonist
setwd("A:/MAKA/TEST")
##

#teeme ikka loopiga :)
carts = vector("list", length = 12)
mds = seq(10,30, by = 2)
for(j in 2:13){
  vec = vector("list", length = length(mds))
  for(k in 1:length(mds)){
    md = mds[k]
    name <- paste("CART_valitud",j,md,".csv", sep = "")
    vec[[k]] <- read.csv(name)
  }
  carts[[j-1]] = vec
}

#carts[[1]][[1]][,1]

mav = matrix(NA, nrow = 12, ncol = length(mds))
kuv = matrix(NA, nrow = 12, ncol = length(mds))
ksv = matrix(NA, nrow = 12, ncol = length(mds))
muv = matrix(NA, nrow = 12, ncol = length(mds))
koos = matrix(NA, nrow = 12, ncol = length(mds))
for(j in 1:12){
  for(k in 1:length(mds)){
    mav[j,k] = sqrt(mean((unlist(carts[[j]][[k]][,1]) - dpp[,2])**2))
    kuv[j,k] = sqrt(mean((unlist(carts[[j]][[k]][,2]) - dpp[,3])**2))
    ksv[j,k] = sqrt(mean((unlist(carts[[j]][[k]][,3]) - dpp[,4])**2))
    muv[j,k] = sqrt(mean((unlist(carts[[j]][[k]][,4]) - dpp[,5])**2))
    koos[j,k]= sqrt((sum((unlist(carts[[j]][[k]][,1:4]) - dpp[,2:5])**2))/(dim(dpp)[1]*4))
  }
}

require(pheatmap)

pheatmap(mav, cluster_rows = F, cluster_cols = F)
pheatmap(kuv, cluster_rows = F, cluster_cols = F)
pheatmap(ksv, cluster_rows = F, cluster_cols = F)
pheatmap(muv, cluster_rows = F, cluster_cols = F)
pheatmap(koos, cluster_rows = F, cluster_cols = F)

require(gridExtra)
require(grid)
require(ggplot2)
require(lattice)

vv = vector("list", length = 4)
vv[[1]] = mav;vv[[2]] = kuv;vv[[3]] = ksv;vv[[4]] = muv;
plot_list=list()
for (a in 1:4){
  x=pheatmap(vv[[a]],cluster_rows = F, cluster_cols = F)
  plot_list[[a]] = x[[4]]     ##to save each plot into a list. note the [[4]]
}
g<-do.call(grid.arrange,plot_list)
#ggsave("CART_heatmap.pdf",g)

which(mav == min(mav), arr.ind = TRUE)
which(kuv == min(kuv), arr.ind = TRUE)
which(ksv == min(ksv), arr.ind = TRUE)
which(muv == min(muv), arr.ind = TRUE)
which(koos == min(koos), arr.ind = TRUE)#12 ja 5; ehk 5 ja 32! kuidas ma varem sain 10 ja 40?
min(koos) #0.2175329 valitud ja kaalud; 0.2073732 ilma kaaludeta
mds[5] #või md 18 ja msl 12???

#võtame siis 10 ja 40

setwd("A:/MAKA/TEST")
cart_muld40msl10 = read.csv("cart_max1040.csv")
cart. = read.csv("cart_uuscatse1040.csv")
cart. = read.csv("cart_uuscatse1218.csv")
cart. = read.csv("cart_max530.csv")
cart. = read.csv("CART_valitud1128.csv") 

#taks.info, kus on ka id-d olemas
setwd("A:/MAKA/TEST/test");load("taks_info.RData") #taks.info
dats = merge(cart.,taks.info, by = "aproovitykk_id", all.x = T)

#uuesti jooksutatud asi, kus id-d peaks kindlasti klappima:
dev.off()
par(mfrow = c(2,2))
plot(dats[,11],dats[,2], xlab = "MÄND", ylab = "CART: msl=12, max.depth=18", xlim = c(0,1), ylim = c(0,1))
plot(dats[,12],dats[,3], xlab = "KUUSK", ylab = "CART: msl=12, max.depth=18", xlim = c(0,1), ylim = c(0,1))
plot(dats[,13],dats[,4], xlab = "KASK", ylab = "CART: msl=12, max.depth=18", xlim = c(0,1), ylim = c(0,1))
plot(dats[,14],dats[,5], xlab = "MUU", ylab = "CART: msl=12, max.depth=18", xlim = c(0,1), ylim = c(0,1))
#nüüd on õige asi. aga selles valguses: kas näiteks rf-i plotid on õiged?

sqrt(mean((dats[,11] - dats[,2])**2))
sqrt(mean((dats[,12] - dats[,3])**2))
sqrt(mean((dats[,13] - dats[,4])**2))
sqrt(mean((dats[,14] - dats[,5])**2))
sqrt(sum((dats[,11:14] - dats[,2:5])**2) /(dim(dats)[1]*4)) #0.2121045 msl 12 md 18; 0.22573 kui msl 5, md 32


#mis vaatlusel suurim viga nt männil?
v2 = (dpp[,1] - cart_muld40msl10[,1])**2; 
enn = 1
mts = match(v2[order(-v2)][enn],v2);id = sidxx[mts]
koos[koos$aproovitykk_id == id,c("aproovitykk_id","arv_maht_es","MA", "KU","KS","HB","LV","LM")]; cart_muld40msl10[mts,]
lnk = raied[raied$aproovitykk_id == id,]$link
browseURL(lnk, browser = getOption("browser"),encodeIfNeeded = FALSE)

#82757 MARU KAHTLANE!
raied[raied$aproovitykk_id == 82757,]


#võrdluseks siia, kui tõin "määramatuse" sisse:
#parameertrid samad: msl 110, md 40
setwd("A:/MAKA/TEST")
cart_unc = read.csv("cart_koos_maaramatus.csv")
dats = merge(cart_unc,taks.info, by = "aproovitykk_id", all.x = T)
dev.off()
par(mfrow = c(2,2))
plot(dats[,11],dats[,2], xlab = "MÄND", ylab = "CART: msl=10, max.depth=40", xlim = c(0,1), ylim = c(0,1))
plot(dats[,12],dats[,3], xlab = "KUUSK", ylab = "CART: msl=10, max.depth=40", xlim = c(0,1), ylim = c(0,1))
plot(dats[,13],dats[,4], xlab = "KASK", ylab = "CART: msl=10, max.depth=40", xlim = c(0,1), ylim = c(0,1))
plot(dats[,14],dats[,5], xlab = "MUU", ylab = "CART: msl=10, max.depth=40", xlim = c(0,1), ylim = c(0,1))

sqrt(mean((dats[,11] - dats[,2])**2))
sqrt(mean((dats[,12] - dats[,3])**2))
sqrt(mean((dats[,13] - dats[,4])**2))
sqrt(mean((dats[,14] - dats[,5])**2))


#0.2124675
#0.1878136
#0.2418231
#0.1987057
#pmst on samad, aga äkki siis on võimalik siiski mingi võit saavutada???


#cart määramatusega!
setwd("A:/MAKA/TEST")
#mds = c(c(1:4),seq(5,38, by = 3))
mds = c(1:10)
msls = seq(15,141, by = 3)
carts = vector("list", length = length(msls))
for(j in 1:length(msls)){
  vec = vector("list", length = length(mds))
  for(k in 1:length(mds)){
    md = mds[k]
    msl = msls[j]
    name <- paste("cart_maaramatus_valitud_mss",msl,"_",md,".csv", sep = "")
    vec[[k]] <- read.csv(name)
  }
  carts[[j]] = vec
}

#kas järjekord ikka õige?
setwd("A:/MAKA/TEST/test")
load("taks_info.RData")
dpp = taks.info[taks.info$aproovitykk_id %in% sidxx,]
dpp = dpp[,c(1,7:10)]

sid.order = unlist(carts[[j]][[k]][,5])
#dpp = dpp[order(sid.order,dpp$aproovitykk_id),]

dpp = dpp[match(sid.order, dpp$aproovitykk_id),]



mav = matrix(NA, nrow = length(msls), ncol = length(mds))
kuv = matrix(NA, nrow = length(msls), ncol = length(mds))
ksv = matrix(NA, nrow = length(msls), ncol = length(mds))
muv = matrix(NA, nrow = length(msls), ncol = length(mds))
koos = matrix(NA, nrow = length(msls), ncol = length(mds))

for(j in 1:length(msls)){
  for(k in 1:length(mds)){
    mav[j,k] = sqrt(mean((unlist(carts[[j]][[k]][,1]) - dpp[,2])**2))
    kuv[j,k] = sqrt(mean((unlist(carts[[j]][[k]][,2]) - dpp[,3])**2))
    ksv[j,k] = sqrt(mean((unlist(carts[[j]][[k]][,3]) - dpp[,4])**2))
    muv[j,k] = sqrt(mean((unlist(carts[[j]][[k]][,4]) - dpp[,5])**2))
    koos[j,k]= sqrt((sum((unlist(carts[[j]][[k]][,1:4]) - dpp[,2:5])**2))/(dim(dpp)[1]*4))
  }
}

pheatmap(koos, cluster_rows = F, cluster_cols = F)


vv = vector("list", length = 4)
vv[[1]] = mav;vv[[2]] = kuv;vv[[3]] = ksv;vv[[4]] = muv;
plot_list=list()
for (a in 1:4){
  x=pheatmap(vv[[a]],cluster_rows = F, cluster_cols = F)
  plot_list[[a]] = x[[4]]
}
g<-do.call(grid.arrange,plot_list)

#ggsave("CART_heatmap.pdf",g)

which(mav == min(mav), arr.ind = TRUE)
which(kuv == min(kuv), arr.ind = TRUE)
which(ksv == min(ksv), arr.ind = TRUE)
which(muv == min(muv), arr.ind = TRUE)

which(koos == min(koos), arr.ind = TRUE)
min(koos)
msls[7];mds[7] #33, 23

#plotime selle 33,23

setwd("A:/MAKA/TEST")
cart_unc = read.csv("cart_koos_maaramatus33_23.csv")
dats = merge(cart_unc,taks.info, by = "aproovitykk_id", all.x = T)
dev.off()
par(mfrow = c(2,2))
plot(dats[,11],dats[,2], xlab = "MÄND", ylab = "CART: msl=10, max.depth=40", xlim = c(0,1), ylim = c(0,1))
plot(dats[,12],dats[,3], xlab = "KUUSK", ylab = "CART: msl=10, max.depth=40", xlim = c(0,1), ylim = c(0,1))
plot(dats[,13],dats[,4], xlab = "KASK", ylab = "CART: msl=10, max.depth=40", xlim = c(0,1), ylim = c(0,1))
plot(dats[,14],dats[,5], xlab = "MUU", ylab = "CART: msl=10, max.depth=40", xlim = c(0,1), ylim = c(0,1))

sqrt(mean((dats[,11] - dats[,2])**2))
sqrt(mean((dats[,12] - dats[,3])**2))
sqrt(mean((dats[,13] - dats[,4])**2))
sqrt(mean((dats[,14] - dats[,5])**2))

#tee CART nii, et ainult olulised tunnused (NT knn põhjal? on mudelis);
#ehk mingi eelvalik tunnustest

#kasuta ka kaale!?







#########CART 506 #####################

setwd("A:/MAKA/TEST")
carts = vector("list", length = length(msls))
mds = seq(1,15, by = 1)
msls = c(5:20)
for(j in 1:length(msls)){
  vec = vector("list", length = length(mds))
  for(k in 1:length(mds)){
    jj = msls[j]
    md = mds[k]
    name <- paste("CART506_",jj,md,".csv", sep = "")
    vec[[k]] <- read.csv(name)
  }
  carts[[j]] = vec
}

#carts[[1]][[1]][,1]

mav = matrix(NA, nrow = length(msls), ncol = length(mds))
kuv = matrix(NA, nrow = length(msls), ncol = length(mds))
ksv = matrix(NA, nrow = length(msls), ncol = length(mds))
muv = matrix(NA, nrow = length(msls), ncol = length(mds))
koos = matrix(NA, nrow = length(msls), ncol = length(mds))

setwd("A:/MAKA/TEST/test")
load("taks_info.RData")
sid.order = unlist(carts[[1]][[1]][,5])
dpp = taks.info[taks.info$aproovitykk_id %in% sid.order,]
dpp = dpp[match(sid.order, dpp$aproovitykk_id),]



for(j in 1:length(msls)){
  for(k in 1:length(mds)){
    mav[j,k] = sqrt(mean((unlist(carts[[j]][[k]][,1]) - dpp[,7])**2))
    kuv[j,k] = sqrt(mean((unlist(carts[[j]][[k]][,2]) - dpp[,8])**2))
    ksv[j,k] = sqrt(mean((unlist(carts[[j]][[k]][,3]) - dpp[,9])**2))
    muv[j,k] = sqrt(mean((unlist(carts[[j]][[k]][,4]) - dpp[,10])**2))
    koos[j,k]= sqrt((sum((unlist(carts[[j]][[k]][,1:4]) - dpp[,7:10])**2))/(dim(dpp)[1]*4))
  }
}

require(pheatmap)

pheatmap(mav, cluster_rows = F, cluster_cols = F)
pheatmap(kuv, cluster_rows = F, cluster_cols = F)
pheatmap(ksv, cluster_rows = F, cluster_cols = F)
pheatmap(muv, cluster_rows = F, cluster_cols = F)
pheatmap(koos, cluster_rows = F, cluster_cols = F)

require(gridExtra)
require(grid)
require(ggplot2)
require(lattice)

vv = vector("list", length = 4)
vv[[1]] = mav;vv[[2]] = kuv;vv[[3]] = ksv;vv[[4]] = muv;
plot_list=list()
for (a in 1:4){
  x=pheatmap(vv[[a]],cluster_rows = F, cluster_cols = F)
  plot_list[[a]] = x[[4]]     ##to save each plot into a list. note the [[4]]
}
g<-do.call(grid.arrange,plot_list)
#ggsave("CART_heatmap.pdf",g)

which(mav == min(mav), arr.ind = TRUE)
which(kuv == min(kuv), arr.ind = TRUE)
which(ksv == min(ksv), arr.ind = TRUE)
which(muv == min(muv), arr.ind = TRUE)
which(koos == min(koos), arr.ind = TRUE) #11 5 ehk msl16 ja md 5.

setwd("A:/MAKA/TEST")
cart = read.csv("CART506_165.csv")
dats = merge(cart,taks.info, by = "aproovitykk_id", all.x = T)
dev.off()
par(mfrow = c(2,2))
plot(dats[,11],dats[,2], xlab = "MÄND", ylab = "CART: msl=16, max.depth=5", xlim = c(0,1), ylim = c(0,1))
plot(dats[,12],dats[,3], xlab = "KUUSK", ylab = "CART: msl=16, max.depth=5", xlim = c(0,1), ylim = c(0,1))
plot(dats[,13],dats[,4], xlab = "KASK", ylab = "CART: msl=16, max.depth=5", xlim = c(0,1), ylim = c(0,1))
plot(dats[,14],dats[,5], xlab = "MUU", ylab = "CART: msl=16, max.depth=5", xlim = c(0,1), ylim = c(0,1))


#proovi bagging ja boosting (adaboost sklearn-is)






