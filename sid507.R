#sid 507

d601 = read.csv("d601.csv")
load("sid507.RData")
out = c(111181,111185) #nende kohta polnud ühtegi satelliidipilti!
sid = sid507[!(sid507 %in% out)]
d506 = d601[d601$aproovitykk_id %in% sid,]
save(d506, file = "d506.RData")
write.csv(d506, file = "d506.csv")
save(sid, file = "sid506.RData")

#kontrollime mulla

muld1 = data.frame(muld = koos[koos$aproovitykk_id %in% sid,]$muld)
muld1[is.na(muld1$muld),]$muld = 999


#võtan alla 30 välja

muld1[muld1$muld %in% c(10,11,16,73,200,999,31,37,53,57,63,NA), "muld"] = 999 #24 hetkel piiriks
table(muld1$muld)
