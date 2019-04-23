#pbp 455

koos_wide = sat_sep_koos %>% group_by(aproovitykk_id, band, kp) %>% dcast(aproovitykk_id + kp + satel ~ band, value.var="value")
kw455 = koos_wide[koos_wide$aproovitykk_id %in% sidxx,]
length(unique(kw455$aproovitykk_id))

koos = koos %>% mutate(KX1 = HB + LV + LM + KX)

m455 = data.frame(muld = koos[koos$aproovitykk_id %in% sidxx,]$muld)
m455$aproovitykk_id = koos[koos$aproovitykk_id %in% sidxx,]$aproovitykk_id
m455$MA = koos[koos$aproovitykk_id %in% sidxx,]$MA
m455$KU = koos[koos$aproovitykk_id %in% sidxx,]$KU
m455$KS = koos[koos$aproovitykk_id %in% sidxx,]$KS
m455$MUU= koos[koos$aproovitykk_id %in% sidxx,]$KX1

length(unique(m455$aproovitykk_id))


m455[m455$muld %in% names(table(m455$muld) < 15)[table(m455$muld) < 15],"muld"] = 999
m455[m455$aproovitykk_id == 114753,]

length(unique(m455$aproovitykk_id))
m455[is.na(m455$muld),"muld"] = 999
length(unique(m455$aproovitykk_id))
table(m455$muld)

muld2 = dcast(m455[,c("aproovitykk_id", "muld")],aproovitykk_id~muld,fun.aggregate = function(x){as.integer(length(x) > 0)})
length(unique(muld2$aproovitykk_id))

kw455 = merge(kw455,m455, all.x = T)
length(unique(kw455$aproovitykk_id))
kw455 = merge(kw455,muld2, by = "aproovitykk_id")
length(unique(kw455$aproovitykk_id))

kw455 = kw455[,-11] #B10 on väärakas
length(unique(kw455$aproovitykk_id))
sentinel455 = kw455[kw455$satel == "S2",]
land455 = kw455[kw455$satel == "LC08",]
land455 = land455[,c(1:3,13:37)]
land455 = land455[,-10]

length(unique(sentinel455$aproovitykk_id))

setwd("A:/MAKA/TEST/test")
save(kw455, file = "kw455.RData")
write.csv(kw455, file = "kw455.csv")
write.csv(sentinel455, file = "sentinel455.csv")
write.csv(land455, file = "landsat455.csv")

sc = sentinel455[,c(2,4:12)] %>% group_by(kp) %>% mutate_all(funs(scale))
sentinel455sc = sentinel455; sentinel455sc[,4:12] = sc[,2:10]

write.csv(sentinel455sc, file = "sentinel455sc.csv")

#sentinel ja landsat koos?
sc1 = kw455[,c(2,4:19)] %>% group_by(kp) %>% mutate_all(funs(scale))

sl455sc = kw455; sl455sc[,4:19] = sc1[,2:17]
sl455sc = sl455sc[,-19]

write.csv(sl455sc, file = "sl455sc.csv")

land455sc = sl455sc[sl455sc$satel == "LC08",]
land455sc = land455sc[,c(1:3,13:37)]; 
land455sc = land455sc[,-10] #B9 ehk cirrus
write.csv(land455sc, file = "landsat455sc.csv")



