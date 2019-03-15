to_test = dcl[format(as.Date(dcl$kp), "%y") < 18,]
to_test1 = dcl[format(as.Date(dcl$kp), "%y") == 18,]

to_test = unique(to_test$aproovitykk_id)
to_test1 = unique(to_test1$aproovitykk_id)
length(to_test1)

length(unique(dcl$aproovitykk_id))

uqu = head(unique(dcl$aproovitykk_id),10)

uqu1 = dcl[dcl$aproovitykk_id %in% uqu,]
uqu2 = sat_sep[sat_sep$aproovitykk_id %in% 34087,]
uqu2 = na.omit(uqu2)


table(uqu2$aproovitykk_id, uqu2$kp)

hist(uqu2[uqu2$band == "B2",]$value)
unique(uqu2$cat)

a531 = uqu2[uqu2$cat == 531,]
a532 = uqu2[uqu2$cat == 532,]

a531 = a531[order(as.Date(a531$kp)),]
plot(a531[a531$band == "B2",]$value, type = "o")
plot(a531[a531$band == "B3",]$value, type = "o")
plot(a531[a531$band == "B4",]$value, type = "o")

#plot 2016-05-10 ja 2018-05-10

x16 = dcl[dcl$kp == "2016-05-10", ]
x18 = dcl[dcl$kp == "2018-05-10", ]

mets_id = unique(koos[koos$maakatsgrp == "M",]$aproovitykk_id)

xx16 = sat_sep[sat_sep$kp == "2016-05-10", ]; xx16 = xx16[xx16$aproovitykk_id %in% mets_id,]
xx18 = sat_sep[sat_sep$kp == "2018-05-10", ]; xx18 = xx18[xx18$aproovitykk_id %in% mets_id,]

par(mfrow = c(2,3))

plot(x16[x16$band == "B02",]$value,x18[x18$band == "B02",]$value)
plot(x16[x16$band == "B03",]$value,x18[x18$band == "B03",]$value)
plot(x16[x16$band == "B04",]$value,x18[x18$band == "B04",]$value)

par(mfrow = c(3,3))
plot(xx16[xx16$band == "B02",]$value,xx18[xx18$band == "B02",]$value)
plot(xx16[xx16$band == "B03",]$value,xx18[xx18$band == "B03",]$value)
plot(xx16[xx16$band == "B04",]$value,xx18[xx18$band == "B04",]$value)
plot(xx16[xx16$band == "B05",]$value,xx18[xx18$band == "B05",]$value)
plot(xx16[xx16$band == "B06",]$value,xx18[xx18$band == "B06",]$value)
plot(xx16[xx16$band == "B07",]$value,xx18[xx18$band == "B07",]$value)
plot(xx16[xx16$band == "B08",]$value,xx18[xx18$band == "B08",]$value)
plot(xx16[xx16$band == "B11",]$value,xx18[xx18$band == "B11",]$value)
plot(xx16[xx16$band == "B12",]$value,xx18[xx18$band == "B12",]$value)

#tee uus ladsat to sentinel, aga mitte id vaid cat põhjal!!!


