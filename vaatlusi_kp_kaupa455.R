data0 = read.csv("sentinel455.csv")
data0 = data0[,-c(14:20)]

compcases = rowSums(table(data0$aproovitykk_id, data0$kp)) - rowSums(table(na.omit(data0)$aproovitykk_id, na.omit(data0)$kp)) 
table(compcases)

d.sent = data0[,c("aproovitykk_id","kp","satel")]
d.sent.naomit = na.omit(data0)[,c("aproovitykk_id","kp","satel")]


tc = data.frame(table((compcases)))
tc = tc[order(tc$Var1, tc$Freq, decreasing = T),]

# compcases
# 1   2   3   4   5   6   7 
# 41 117 170  96  20  10   1 

#41 juhul 1 pilt .... 1 juhul 7 pilti!



#aga piltide kaupa?
comppilt = rowSums(table(na.omit(data0)$aproovitykk_id,na.omit(data0)$kp))
sort(table((comppilt)), decreasing = T)

tc = data.frame(table((comppilt)))
tc = tc[order(tc$Var1, decreasing = T),]

# comppilt
# 86 150 285 294 360 386 397 399 415 443 454 455 
# 1   1   1   1   1   1   1   1   1   1   1   4

data0 = read.csv("landsat455.csv")
data0 = data0[,-c(14:20)]

compcases = rowSums(table(data0$aproovitykk_id, data0$kp)) - rowSums(table(na.omit(data0)$aproovitykk_id, na.omit(data0)$kp)) 
table(compcases)

d.land = data0[,c("aproovitykk_id","kp","satel")]
d.land.naomit = na.omit(data0)[,c("aproovitykk_id","kp","satel")]

tc = data.frame(table((compcases)))
tc = tc[order(tc$Var1, tc$Freq, decreasing = T),]



compcases[compcases == 7]
#15 erinevat pilti
#47 juhul vaid 1 pilt, neeb võibolla välja võtta?

#aga piltide kaupa?
comppilt = rowSums(table(na.omit(data0)$aproovitykk_id,na.omit(data0)$kp))
sort(table((comppilt)), decreasing = T)

tc = data.frame(table((comppilt)))
tc = tc[order(tc$Var1, decreasing = T),]


d.sl = rbind(d.sent,d.land); d.sl.naomit = rbind(d.sent.naomit, d.land.naomit)
d.sl$kpsatel = paste(d.sl$kp,d.sl$satel)
d.sl.naomit$kpsatel = paste(d.sl.naomit$kp,d.sl.naomit$satel)

compcases = rowSums(table(d.sl$aproovitykk_id, d.sl$kpsatel)) - rowSums(table(d.sl.naomit$aproovitykk_id, d.sl.naomit$kpsatel)) 
table(compcases)
tc = data.frame(table((compcases)))
tc = tc[order(tc$Var1, tc$Freq, decreasing = T),]


comppilt = rowSums(table(d.sl.naomit$aproovitykk_id, d.sl.naomit$kpsatel))
tc = data.frame(table((comppilt)))
tc = tc[order(tc$Var1, decreasing = T),]





