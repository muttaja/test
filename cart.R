####CART pythonist
setwd("A:/MAKA/TEST")
cart = read.csv("cart.csv")
cart_muld = read.csv("cart_muld.csv")
cart_muld2 = read.csv("cart_max2.csv")
cart_muld5 = read.csv("cart_max5.csv")
cart_muld10 = read.csv("cart_max10.csv")
cart_muld15 = read.csv("cart_max15.csv")
cart_muld20 = read.csv("cart_max20.csv")
cart_muld25 = read.csv("cart_max25.csv")
cart_muld30 = read.csv("cart_max30.csv")
cart_muld35 = read.csv("cart_max35.csv")
cart_muld40 = read.csv("cart_max40.csv")

cart_muld40msl2 = read.csv("cart_max40_msl2.csv")
cart_muld40msl4 = read.csv("cart_max40_msl4.csv")
cart_muld40msl8 = read.csv("cart_max40_msl8.csv")

par(mfrow = c(2,2))
plot(dpp[,1],cart[,1])
plot(dpp[,2],cart[,2])
plot(dpp[,3],cart[,3])
plot(dpp[,4],cart[,4])

plot(dpp[,1],cart_muld[,1], xlab = "MÄND", ylab = "CART prognoos", xlim = c(0,1), ylim = c(0,1))
plot(dpp[,2],cart_muld[,2], xlab = "KUUSK", ylab = "CART prognoos", xlim = c(0,1), ylim = c(0,1))
plot(dpp[,3],cart_muld[,3], xlab = "KASK", ylab = "CART prognoos", xlim = c(0,1), ylim = c(0,1))
plot(dpp[,4],cart_muld[,4], xlab = "MUU", ylab = "CART prognoos", xlim = c(0,1), ylim = c(0,1))

plot(dpp[,1],cart_muld2[,1])
plot(dpp[,2],cart_muld2[,2])
plot(dpp[,3],cart_muld2[,3])
plot(dpp[,4],cart_muld2[,4])

plot(dpp[,1],cart_muld5[,1])
plot(dpp[,2],cart_muld5[,2])
plot(dpp[,3],cart_muld5[,3])
plot(dpp[,4],cart_muld5[,4])

plot(dpp[,1],cart_muld10[,1])
plot(dpp[,2],cart_muld10[,2])
plot(dpp[,3],cart_muld10[,3])
plot(dpp[,4],cart_muld10[,4])

plot(dpp[,1],cart_muld15[,1])
plot(dpp[,2],cart_muld15[,2])
plot(dpp[,3],cart_muld15[,3])
plot(dpp[,4],cart_muld15[,4])

plot(dpp[,1],cart_muld20[,1])
plot(dpp[,2],cart_muld20[,2])
plot(dpp[,3],cart_muld20[,3])
plot(dpp[,4],cart_muld20[,4])

plot(dpp[,1],cart_muld25[,1])
plot(dpp[,2],cart_muld25[,2])
plot(dpp[,3],cart_muld25[,3])
plot(dpp[,4],cart_muld25[,4])

plot(dpp[,1],cart_muld30[,1])
plot(dpp[,2],cart_muld30[,2])
plot(dpp[,3],cart_muld30[,3])
plot(dpp[,4],cart_muld30[,4])

plot(dpp[,1],cart_muld35[,1])
plot(dpp[,2],cart_muld35[,2])
plot(dpp[,3],cart_muld35[,3])
plot(dpp[,4],cart_muld35[,4])

plot(dpp[,1],cart_muld40msl2[,1])
plot(dpp[,2],cart_muld40msl2[,2])
plot(dpp[,3],cart_muld40msl2[,3])
plot(dpp[,4],cart_muld40msl2[,4])

plot(dpp[,1],cart_muld40msl4[,1])
plot(dpp[,2],cart_muld40msl4[,2])
plot(dpp[,3],cart_muld40msl4[,3])
plot(dpp[,4],cart_muld40msl4[,4])

plot(dpp[,1],cart_muld40msl8[,1])
plot(dpp[,2],cart_muld40msl8[,2])
plot(dpp[,3],cart_muld40msl8[,3])
plot(dpp[,4],cart_muld40msl8[,4])


sqrt(mean((dpp[,1] - cart_muld40msl4[,1])**2))
sqrt(mean((dpp[,2] - cart_muld40msl4[,2])**2))
sqrt(mean((dpp[,3] - cart_muld40msl4[,3])**2))
sqrt(mean((dpp[,4] - cart_muld40msl4[,4])**2))



sqrt(mean((dp00[,1] - cart[,1])**2))
sqrt(mean((dp00[,2] - cart[,2])**2))
sqrt(mean((dp00[,3] - cart[,3])**2))
sqrt(mean((dp00[,4] - cart[,4])**2))

sqrt(mean((dp00[,1] - cart_muld[,1])**2))
sqrt(mean((dp00[,2] - cart_muld[,2])**2))
sqrt(mean((dp00[,3] - cart_muld[,3])**2))
sqrt(mean((dp00[,4] - cart_muld[,4])**2))

##

#teeme ikka loopiga :)
carts = vector("list", length = 12)
mds = seq(10,40, by = 2)
for(j in 1:12){
  vec = vector("list", length = length(mds))
  for(k in 1:length(mds)){
    md = mds[k]
    name <- paste("cart_max",j,md,".csv", sep = "")
    vec[[k]] <- read.csv(name)
  }
  carts[[j]] = vec
}

#carts[[1]][[1]][,1]

mav = matrix(NA, nrow = 12, ncol = 16)
kuv = matrix(NA, nrow = 12, ncol = 16)
ksv = matrix(NA, nrow = 12, ncol = 16)
muv = matrix(NA, nrow = 12, ncol = 16)
for(j in 1:12){
  for(k in 1:length(mds)){
    mav[j,k] = sqrt(mean((unlist(carts[[j]][[k]][,1]) - dpp[,1])**2))
    kuv[j,k] = sqrt(mean((unlist(carts[[j]][[k]][,2]) - dpp[,2])**2))
    ksv[j,k] = sqrt(mean((unlist(carts[[j]][[k]][,3]) - dpp[,3])**2))
    muv[j,k] = sqrt(mean((unlist(carts[[j]][[k]][,4]) - dpp[,4])**2))
  }
}

require(pheatmap)

pheatmap(mav, cluster_rows = F, cluster_cols = F)
pheatmap(kuv, cluster_rows = F, cluster_cols = F)
pheatmap(ksv, cluster_rows = F, cluster_cols = F)
pheatmap(muv, cluster_rows = F, cluster_cols = F)


vv = vector("list", length = 4)
vv[[1]] = mav;vv[[2]] = kuv;vv[[3]] = ksv;vv[[4]] = muv;
plot_list=list()
for (a in 1:4){
  x=pheatmap(vv[[a]],cluster_rows = F, cluster_cols = F)
  plot_list[[a]] = x[[4]]     ##to save each plot into a list. note the [[4]]
}
g<-do.call(grid.arrange,plot_list)
ggsave("CART_heatmap.pdf",g)

which(mav == min(mav), arr.ind = TRUE)
which(kuv == min(kuv), arr.ind = TRUE)
which(ksv == min(ksv), arr.ind = TRUE)
which(muv == min(muv), arr.ind = TRUE)

#võtame siis 10 ja 40

cart_muld40msl10 = read.csv("cart_max1040.csv")
plot(dpp[,1],cart_muld40msl10[,1], xlab = "MÄND", ylab = "CART: msl=10, max.depth=40", xlim = c(0,1), ylim = c(0,1))
plot(dpp[,2],cart_muld40msl10[,2], xlab = "KUUSK", ylab = "CART: msl=10, max.depth=40", xlim = c(0,1), ylim = c(0,1))
plot(dpp[,3],cart_muld40msl10[,3], xlab = "KASK", ylab = "CART: msl=10, max.depth=40", xlim = c(0,1), ylim = c(0,1))
plot(dpp[,4],cart_muld40msl10[,4], xlab = "MUU", ylab = "CART: msl=10, max.depth=40", xlim = c(0,1), ylim = c(0,1))


sqrt(mean((dpp[,1] - cart_muld40msl10[,1])**2))
sqrt(mean((dpp[,2] - cart_muld40msl10[,2])**2))
sqrt(mean((dpp[,3] - cart_muld40msl10[,3])**2))
sqrt(mean((dpp[,4] - cart_muld40msl10[,4])**2))
