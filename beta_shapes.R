#beta shapes!

xx = seq(0,1, 0.001)
shapes0 = c(0.5,1,1.1,1.5,12)
shapes = CombSet(shapes0,m = 2, repl = T, ord = F, as.list = T)

dev.off();par(mfrow=c(5,3))
for(s in shapes){
  plot(xx,dbeta(xx, shape1 = s[1], shape2 = s[2]), main = paste(s[1],s[2], sep =", "), ylab = "", type = "l", xlab = "")
}

rbeta(10, 1,8);
rbeta(10, 1.2,8);


### knn_pbp_506.R;
load(file ="KNN_pbp_tv100_v2.RData")
#.. siit kp_data
require(EnvStats)

sits = KNN_PBP_BETA$aproovitykk_id[325]
pdid1 = kp_data[kp_data$aproovitykk_id == sits,]; 
pd = pdid1[,2:5]
eb = ebeta(pd[,2], method = "mle");a = eb$parameters[1]; b = eb$parameters[2]
xx = seq(0,1,0.001)
yy = dbeta(pd[,2],a,b)

par(mfrow = c(2,2))
d = density(pd[,1], kernel = "epanechnikov", from = 0, to = 1)
plot(d)
d = density(pd[,2], kernel = "epanechnikov", from = 0, to = 1)
plot(d)
d = density(pd[,3], kernel = "epanechnikov", from = 0, to = 1)
plot(d)
d = density(pd[,4], kernel = "epanechnikov", from = 0, to = 1)
plot(d)

par(mfrow = c(2,2))
eb = ebeta(pd[,1], method = "mle")
a = eb$parameters[1]; b = eb$parameters[2];xx = seq(0,1,0.001);yy = dbeta(xx, shape1 = a, shape2 = b)
plot(xx,yy, type = "l")
text(x = mean(pd[,1]), y = 3, labels=paste("mean = ",round(mean(pd[,1]),3)),col="red")

eb = ebeta(pd[,2], method = "mle")
a = eb$parameters[1]; b = eb$parameters[2];xx = seq(0,1,0.001);yy = dbeta(xx, shape1 = a, shape2 = b)
plot(xx,yy, type = "l")
eb = ebeta(pd[,3], method = "mle")
a = eb$parameters[1]; b = eb$parameters[2];xx = seq(0,1,0.001);yy = dbeta(xx, shape1 = a, shape2 = b)
plot(xx,yy, type = "l")
eb = ebeta(pd[,4], method = "mle")
a = eb$parameters[1]; b = eb$parameters[2];xx = seq(0,1,0.001);yy = dbeta(xx, shape1 = a, shape2 = b)
plot(xx,yy, type = "l")







par(mfrow=c(1,2))
plot(d)
points(x = pd[,1], y = rep(0, length(pd[,1])), pch = 19, cex = 1.5,col = rgb(red = 0, green = 0, blue = 0, alpha = 0.169)); abline(h = 0)
abline(v = mean(pd[,1]))




par(mfrow=c(2,2))
vec = pd[,1]
eb = ebeta(vec, method = "mle");a = eb$parameters[1]; b = eb$parameters[2]
d = density(vec, kernel = "epanechnikov", from = 0, to = 1)

plot(x = xx, y = dbeta(xx, shape1 = a, shape2 = b), type = "l", ylim = c(0,4), col = "blue", lwd = 2, ylab = "Tihedus", xlab = "Männi osakaalu hinnangud piltide kaupa") 
points(x = pd[,1], y = rep(0, length(pd[,1])), pch = 19, cex = 1.5,col = rgb(red = 0, green = 0, blue = 0, alpha = 0.169))
abline(h = 0)
abline(v = mean(pd[,1]), lty = 2, col = "red", lwd = 2)
lines(d, col = 139, lty = 2, lwd = 2)
#text(mean(pd[,1]), "keskmine")
#text(locator(), labels = c("mean", "beta max)"))
text(x = 0.35, y = 3, labels="mean = 0.234",col="red")
text(x = 0.125, y = 4, labels="beta max = 0",col="blue")
text(x = 0.08, y = 1.1, labels="Epa max = 0.102",col=139)

vec = pd[,2]
eb = ebeta(vec, method = "mle");a = eb$parameters[1]; b = eb$parameters[2]
d = density(vec, kernel = "epanechnikov", from = 0, to = 1)
plot(x = xx, y = dbeta(xx, shape1 = a, shape2 = b), type = "l", ylim = c(0,12), col = "blue", lwd = 2, ylab = "Tihedus", xlab = "Kuuse osakaalu hinnangud piltide kaupa") 
points(x = vec, y = rep(0, length(vec)), pch = 19, cex = 1.5,col = rgb(red = 0, green = 0, blue = 0, alpha = 0.169))
abline(h = 0)
abline(v = mean(vec), lty = 2, col = "red", lwd = 2)
lines(d, col = 139, lty = 2, lwd = 2)
#text(mean(pd[,1]), "keskmine")
#text(locator(), labels = c("mean", "beta max)"))
text(x = 0.25, y = 12, labels=paste("mean = ",round(mean(vec),3)),col="red")
text(x = 0.295, y = 6, labels="beta max = 0.089",col="blue")
text(x = 0.28, y = 10, labels="Epa max = 0.112",col=139)

vec = pd[,3]
eb = ebeta(vec, method = "mle");a = eb$parameters[1]; b = eb$parameters[2]
d = density(vec, kernel = "epanechnikov", from = 0, to = 1)
plot(x = xx, y = dbeta(xx, shape1 = a, shape2 = b), type = "l", ylim = c(0,4), col = "blue", lwd = 2, ylab = "Tihedus", xlab = "Kase osakaalu hinnangud piltide kaupa") 
points(x = vec, y = rep(0, length(vec)), pch = 19, cex = 1.5,col = rgb(red = 0, green = 0, blue = 0, alpha = 0.169))
abline(h = 0)
abline(v = mean(vec), lty = 2, col = "red", lwd = 2)
lines(d, col = 139, lty = 2, lwd = 2)
#text(mean(pd[,1]), "keskmine")
#text(locator(), labels = c("mean", "beta max)"))
text(x = 0.52, y = 3.5, labels=paste("mean = ",round(mean(vec),3)),col="red")
text(x = 0.2, y = 2, labels="beta max = 0.296",col="blue")
text(x = 0.58, y = 2.3, labels="Epa max = 0.509",col=139)


vec = pd[,4]
eb = ebeta(vec, method = "mle");a = eb$parameters[1]; b = eb$parameters[2]
d = density(vec, kernel = "epanechnikov", from = 0, to = 1)
plot(x = xx, y = dbeta(xx, shape1 = a, shape2 = b), type = "l", ylim = c(0,4), col = "blue", lwd = 2, ylab = "Tihedus", xlab = "Muude liikide osakaalu hinnangud piltide kaupa") 
points(x = vec, y = rep(0, length(vec)), pch = 19, cex = 1.5,col = rgb(red = 0, green = 0, blue = 0, alpha = 0.169))
abline(h = 0)
abline(v = mean(vec), lty = 2, col = "red", lwd = 2)
lines(d, col = 139, lty = 2, lwd = 2)
#text(mean(pd[,1]), "keskmine")
#text(locator(), labels = c("mean", "beta max)"))
text(x = 0.37, y = 3, labels=paste("mean = ",round(mean(vec),3)),col="red")
text(x = 0.12, y = 4, labels="beta max = 0",col="blue")
text(x = 0.48, y = 2, labels="Epa max = 0.370",col=139)







which.max(dbeta(xx, shape1 = a, shape2 = b))






mean(pd[,1])
max.epa = d$x[which.max(d$y)]
max.epa
