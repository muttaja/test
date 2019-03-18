#tekitame natuke segadust :)

segadus = function(row,n){
  ret = matrix(, nrow = n+1, ncol = 4)
  ret[1,] = row
  rmat = matrix(data = rnorm(4*n, sd = 0.1), nrow = n)
  rowmat = matrix(rep(row, each = n), nrow = n)
  retmat = rmat + rowmat
  retmat[rowmat == 0] = 0
  ret[2:(n+1),] = retmat
  
  if(1 %in% row){
    ret[2:(n+1),] = rep(row, each = n)
  }
  ret[ret < 0] = 0
  ret[ret > 1] = 1
  ret = ret / rowSums(ret)
  return(ret)
}

tw = segadus(c(0.35,0.3,0.15,0.1),10)
tw
colMeans(tw)
