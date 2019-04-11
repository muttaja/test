#prognooside miximine

#dp ja dats

dats1 = merge(dats, dp, by = "aproovitykk_id")
dats2 = (dats1[,2:5] + dats1[,15:18]) / 2
dats2 = dats2 / rowSums(dats2) #kuigi on juba 1!

dev.off()
par(mfrow = c(2,2))
plot(dats1[,11],dats2[,1])
plot(dats1[,12],dats2[,2])
plot(dats1[,13],dats2[,3])
plot(dats1[,14],dats2[,4])

sqrt(mean((dats1[,11] - dats2[,1])**2))
sqrt(mean((dats1[,12] - dats2[,2])**2))
sqrt(mean((dats1[,13] - dats2[,3])**2))
sqrt(mean((dats1[,14] - dats2[,4])**2))
sqrt(sum((dats1[,11:14] - dats2[,1:4])**2) /(dim(dats1)[1]*4))
#mnjaa, see oli küll kehv mõte