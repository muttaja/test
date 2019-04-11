#beta shapes!

xx = seq(0,1, 0.001)
shapes0 = c(0.5,1,1.1,1.5,12)
shapes = CombSet(shapes0,m = 2, repl = T, ord = F, as.list = T)

dev.off();par(mfrow=c(5,3))
for(s in shapes){
  plot(xx,dbeta(xx, shape1 = s[1], shape2 = s[2]), main = paste(s[1],s[2], sep =", "), ylab = "")
}

rbeta(10, 1,8);
rbeta(10, 1.2,8);
