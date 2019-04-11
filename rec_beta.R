#rectangular beta log likehood!?


library(stats4)


betaG = function(y,mu,phi){
  gamma(phi)/(gamma(phi*mu)*gamma((1-mu)*phi))*y**(mu*phi-1)*(1-y)**((1-mu)*phi-1)
}


xx = t1m

xx = c(rep(0.01,10), seq(0.75,0.84,0.01),rep(0.99,10))
xx = runif(15)



xx = runif(15)
LL <- function(mu, phi, theta) {
  #beta = dbeta(xx, mu, phi)
  beta = betaG(xx, mu, phi)
  -sum(log(theta + (1-theta)*beta))
}



fit1 = mle(LL, start = list(mu = 0.5, phi=1, theta = 0.69), method = "L-BFGS-B", lower = c(0.0001,0,0.0001),
    upper = c(0.9999, Inf,0.9999))
fit1



fit2 = optim(par = c(0.5,0.5,0.5), fn = LL, method = "L-BFGS-B", lower = c(0.0001,0,0.0001),
             upper = c(0.9999, Inf,0.9999))
fit2

#tim ja tik