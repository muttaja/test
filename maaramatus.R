#tekitame natuke segadust :)

segadus = function(row,n){
  row = row[2:5]
  ret = matrix(NA, nrow = (n+1)*15, ncol = 4)
  ind1 = seq(1,15*11,11)
  ind2 = c(1:(15*11))[-ind1]
  ret[ind1,] = row
  rmat = matrix(data = rnorm(4*n*15, sd = 0.1), nrow = n*15)
  rowmat = matrix(rep(row, each = n*15), nrow = n*15)
  retmat = rmat + rowmat
  retmat[rowmat == 0] = 0
  ret[ind2,] = retmat
  
  if(1 %in% row){
    ret[ind2,] = rep(row, each = n)
  }
  ret[ret < 0] = 0
  ret[ret > 1] = 1
  ret = ret / rowSums(ret)
  return(t(ret))
}

#tw = segadus(c(0.35,0.3,0.15,0.1),10)
#tw

#id599 = ppp$aproovitykk_id
#save(id599, file = "id599.RData")
#esmalt aint proportsioonid:
#koos_wide = sat_sep_koos %>% group_by(aproovitykk_id, band, kp) %>% dcast(aproovitykk_id + satel ~ band + kp, value.var="value")
#kw599 = koos_wide[koos_wide$aproovitykk_id %in% id599,]

dat599 = data.frame(aproovitykk_id = id599)
dat599 = merge(dat599, koos[,c("aproovitykk_id","MA","KU","KS", "KX1")], by = "aproovitykk_id", all.x = T)
dat599[,2:5] = dat599[,2:5]/100

#ei oska hetkel ilma for tsüklita

n = 10
#dfn10 = data.frame(aproovitykk_id = rep(dat599$aproovitykk_id, each = n))
dfn10 = matrix(NA, nrow = 599*(n+1)*15, ncol = 5)
dfn10[,1] = rep(dat599$aproovitykk_id, each = (n+1)*15)
dfn10[,2:5] = matrix(apply(dat599,1,segadus,n = n), ncol = 4,byrow = T)

# is = seq(1,599*n*15, by = n*15)
# for(i in is){
#   dfn10[(i:i+9),2:5] = matrix(apply(dat599,1,segadus,n = n*15), ncol = 4,byrow = T)
# }

dfn10 = data.frame(dfn10); names(dfn10) = c("aproovitykk_id", "MA", "KU", "KS", "MUU")

sentinel599sc = sentinel601sc[sentinel601sc$aproovitykk_id %in% id599,]
sentinel599sc = sentinel599sc[,!(names(sentinel601sc) %in% c("B2","B3","B4","B5","B6","B7","B9"))]

#sent.expanded  <- sentinel599sc[rep(row.names(sentinel599sc), each =11),]
#dfn10.expanded <- dfn10 %>% group_by(aproovitykk_id) %>% .[rep(row.names(.), times = 15),]

#dfn10.expanded = dfn10.expanded[order(rep(id599, each = 11*15)),"aproovitykk_id", drop=FALSE]

#dfn10.expanded = dfn10.expanded[order(rep(id599, each = 11*15)),]

datx10 = sent.expanded; datx10[,c("MA", "KU", "KS", "MUU")] = dfn10[,c("MA", "KU", "KS", "MUU")]
#write.csv(datx10, file = "datx10_599.csv")



#määramatus d601 failiga:
segadus1 = function(row,n){
  row = as.numeric(row[53:56])
  ret = matrix(NA, nrow = (n+1), ncol = 4)
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
  return(t(ret))
}

d601 = read.csv("d601.csv")
n = 10
d601_10 = matrix(NA, nrow = 601*(n+1), ncol = 5)
d601_10[,1] = rep(d601$aproovitykk_id, each = (n+1))
d601_10[,2:5] = matrix(apply(d601,1,segadus1,n = n), ncol = 4,byrow = T)
d601x10 = d601[rep(row.names(d601), each =11),]
d601x10[,c( "ARV_VMA","ARV_VKU","ARV_VKS","ARV_VXX" )] = d601_10[,2:5]
#write.csv(d601x10, file = "d601x10.csv")
