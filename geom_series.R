#geom

ser= c(0.4,0.3,0.23,0.18,0.17)
asym = ?
  
find.geom = function(params,pos1,N,ser){
  a = params[1]
  r = params[2]
  asym = params[3]
  #pos1 = params[4]
  kk = 1:N
  points = a*r**kk + asym
  mse = mean((points[pos1:(pos1+length(ser)-1)] - ser)**2)
  mse
}

tst = find.geom(c(3,0.6,0.12,4),20,ser)

plot(tst)

ops = optim(par = c(3,0.6,0.12,4),fn = find.geom, N = 100, ser = ser, method = "BFGS")
#r=2,r=0.5,asym=0.1,pos1=1,

N = 100
values = c()
for(NN in 1:(N-length(ser))){
  ops = optim(par = c(3,0.6,0.12),pos1 = NN,fn = find.geom, N = N, ser = ser, method = "BFGS")
  values[NN] = ops$value
}

pos1 = which.min(values)
ops = optim(par = c(3,0.6,0.12),pos1 = pos1,fn = find.geom, N = N, ser = ser, method = "BFGS")

gs=function(a,r,as,N){
  kk = 1:N
  points = a*r**kk + asym
  points
}
