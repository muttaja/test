#kuupäevad:

#https://hls.gsfc.nasa.gov/algorithms/bandpass-adjustment/

# LC08 S2AL1C S2BL1C
# 2015-08-04     0  56400      0
# 2015-08-24     0  56400      0
# 2015-09-30     0  56400      0
# 2016-04-27     0  56400      0
# 2016-05-10     0  56400      0
# 2017-05-02     0  50760      0
# 2017-05-05     0  50760      0
# 2017-06-17 33840      0      0
# 2017-08-30     0  50760      0
# 2017-09-24     0      0  50760
# 2018-05-10 33840  50760      0
# 2018-05-12 33840      0      0
# 2018-05-26 33840      0      0
# 2018-05-27     0  50760      0
# 2018-05-28 33840      0      0
# 2018-05-30     0  50760      0
# 2018-06-09     0  50760      0
# 2018-08-23 33840      0  50760
# 2018-09-19     0      0  50760

#Landsati kanalid B2, B3 jne, Sentinelil B02, B03 jne
#Landsatul 6 kanalit; Sentinelil 9

#Sentinel:

# Band 2 ??? Blue	492.4
# Band 3 ??? Green	559.8
# Band 4 ??? Red	664.6
# Band 5 ??? Vegetation red edge	704.1
# Band 6 ??? Vegetation red edge	740.5	15
# Band 7 ??? Vegetation red edge	782.8	20
# Band 8 ??? NIR	832.8
# Band 11 ??? SWIR	1613.7
# Band 12 ??? SWIR 2202.4

#Landsat
#Band 2 - Blue	0.452 - 0.512
#Band 3 - Green	0.533 - 0.590
#Band 4 - Red	0.636 - 0.673
#Band 5 - Near Infrared (NIR)	0.851 - 0.879
#Band 6 - Shortwave Infrared (SWIR) 1	1.566 - 1.651
#Band 7 - Shortwave Infrared (SWIR) 2	2.107 - 2.294

#NIR-id ja SWIR-id ei lange päris kokku, aga mis seal ikka...

#kuupäevad mudeli jaoks: 

# 2018-05-10

# siin kasutame LC kevad2 prognoosväärtusi?
# 2018-05-26 33840      0      0
# 2018-05-27     0  50760      0
# 2018-05-28 33840 

#2018-08-23 33840      0  50760

#andmestik sat_set

sat_2018_05_10 = sat_sep[sat_sep$kp == "2018-05-10",]
sat_2018_08_23 = sat_sep[sat_sep$kp == "2018-08-23",]
data_str = data1; data_str$kp = as.character(data_str$kp)
sat_LC_kevad2  = data_str[data_str$kp %in% c("2018-05-26","2018-05-27", "2018-05-28"),]

sat_2018_05_27 = sat_sep[sat_sep$kp == "2018-05-27",]
sat_LC_kevad2[sat_LC_kevad2$kp == "2018-05-27",]$pred_glmer = sat_2018_05_27$value
sat_LC_kevad2 = sat_LC_kevad2[,-(7:8)]
names(sat_LC_kevad2) = names(sat_sep)

#see ju vigane, peab olema cat põhjal!!!

wide1 = sat_2018_05_10 %>% dcast(aproovitykk_id + cat + kp  ~ band , value.var="value")
wide2 = sat_2018_08_23 %>% dcast(aproovitykk_id + cat + kp  ~ band , value.var="value")
wide3 = sat_LC_kevad2 %>% dcast(aproovitykk_id + cat + kp ~ band , value.var="value")

# which.sent1 = sat_2018_05_10[sat_2018_05_10$satel != "LC08",]
# which.sent2 = sat_2018_08_23[sat_2018_08_23$satel != "LC08",]
# 
# wx1 = merge(wide1,which.sent1[,2:5], by = "kp")
# wide2 = merge(wide2,which.sent2[,2:5], by = c("aproovitykk_id","kp"), all.x = T)
wide1$satel = "S2AL1C"
wide2$satel = "S2BL1C"


ww0 = rbind(wide1,wide2) #,wide3, #äkki see kolmas rikub ära?


#"koos" andmestikust siia juurde:

koos_2018_05_10 = sat_sep_koos[sat_sep_koos$kp == "2018-05-10",]
koos_2018_08_23 = sat_sep_koos[sat_sep_koos$kp == "2018-08-23",]
kwide1 = koos_2018_05_10 %>% dcast(aproovitykk_id + cat + kp ~ band , value.var="value")
kwide2 = koos_2018_08_23 %>% dcast(aproovitykk_id + cat + kp ~ band , value.var="value")
kwide1$satel = "S2AL1C"
kwide2$satel = "S2BL1C"


test1 = na.omit(kwide1)
cor(test1$B02, test1$B2)#tegelt 0.9778, tuli 0.9713435

test1[,13:18] = test1[,13:18] / 10

plot(test1$B02, test1$B2)
plot(test1$B03, test1$B3)
plot(test1$B04, test1$B4)
plot(test1$B08, test1$B5)

lm1 = lm(B02 ~ B2, data = test1)
lm2 = lm(B03 ~ B3, data = test1)
lm3 = lm(B04 ~ B4, data = test1)
lm4 = lm(B05 ~ B5, data = test1)

#kas lm ja lmer, kus piksli mõju on juhuslik, kuna pikslid ei kattu, annavad erineva tulemuse?
lmer1 = lmer(B02 ~ B2 + 1|cat, data = test1, na.action = na.exclude)




kww = rbind(kwide1, kwide2)
kww = kww[!(kww$aproovitykk_id %in% ww$aproovitykk_id),]

ww = rbind(ww0,kww)


names(ww);
ww$B2_2 = ww$B2**2
ww$B3_2 = ww$B3**2
ww$B4_2 = ww$B4**2
ww$B5_2 = ww$B5**2
ww$B6_2 = ww$B6**2
ww$B7_2 = ww$B7**2


ww$B2_3 = ww$B2**3
ww$B3_3 = ww$B3**3
ww$B4_3 = ww$B4**3
ww$B5_3 = ww$B5**3
ww$B6_3 = ww$B6**3
ww$B7_3 = ww$B7**3


ww$B2_4 = ww$B2**4
ww$B3_4 = ww$B3**4
ww$B4_4 = ww$B4**4
ww$B5_4 = ww$B5**4
ww$B6_4 = ww$B6**4
ww$B7_4 = ww$B7**4

ww$B2_sq = sqrt(ww$B2)
ww$B3_sq = sqrt(ww$B3)
ww$B4_sq = sqrt(ww$B4)
ww$B5_sq = sqrt(ww$B5)
ww$B6_sq = sqrt(ww$B6)
ww$B7_sq = sqrt(ww$B7)





#tahan prognoosida sentineli kanali väärtust lansati kanalite põhjal:
#erindid võivad tekkida sellest, et pikslid on erineva suurusega?
#eeldame, et väiksema piksilga sentinel on "tõde", ehk landsat on mõõdetud veaga?
#how do deal with measurement uncerainty?

############### väga lihtsad mudelid ###############

#kas võtta ainult mets?
ww = ww[ww$aproovitykk_id %in% mets_id,]


#jagame läbi vastavalt piksi suuruse erinevusega???
#ww[,13:15] = ww[,13:15]/9; ww[,16:18] = ww[,16:18]/(9/4)



require(lme4)
m2 = lmer(B02 ~ B2 + (1|cat), data = ww, na.action = na.exclude)
m2id = lmer(B02 ~ B2 + (1|aproovitykk_id), data = ww, na.action = na.exclude)
m3 = lmer(B03 ~ B3 + (1|cat), data = ww, na.action = na.exclude)
m4 = lmer(B04 ~ B4 + (1|cat), data = ww, na.action = na.exclude)
m5 = lmer(B08 ~ B5 + (1|cat), data = ww, na.action = na.exclude)
m6 = lmer(B11 ~ B6 + (1|cat), data = ww, na.action = na.exclude)
m7 = lmer(B12 ~ B7 + (1|cat), data = ww, na.action = na.exclude)

#mis krdi cat!??? id ikka!

#####################################################


hist(ww$B2)
mb2 = lm(B02 ~ B2 + B2_2 + B2_3 + B2_sq, data = ww) #3. aste pole enam oluline, ja siis pole ka eelmised enam olulised :O?
mb2
summary(mb2) #0.8981 Rsq


par(mfrow = c(1,2))
plot(ww$B02, predict(mb2, newdata = ww))
plot(ww$B02, ww$B2)

#aga kui glmer?

mb2g = lmer(B02 ~ B2 + (1|cat), data = ww, na.action = na.exclude)
mb2g
summary(mb2g)
plot(ww$B02, predict(mb2g, newdata = ww,allow.new.levels = TRUE, re.form = NA))
plot(ww$B02, ww$B2)


bb2 = ww[,c(1:4,13)]; bb2$pred = predict(mb2g, newdata = ww,allow.new.levels = TRUE, re.form = NA); bb2$vahe = bb2$B02 - bb2$pred
bb2 = bb2[order(abs(bb2$vahe), decreasing = T),]

#PIKSLID ON ERINEVA SUURUSEGA!
#seetõttu ka random effecte vaja
lnk = koos[koos$aproovitykk_id == 34094,]$link; browseURL(lnk, browser = getOption("browser"),encodeIfNeeded = FALSE);
hist(bb2$vahe)



#B03
mb3 = lm(B03 ~ B3, data = ww)
mb3
summary(mb3)

plot(ww$B03, predict(mb3, newdata = ww))
plot(ww$B03, ww$B3)

mb3g = lmer(B03 ~ B3 + (1|cat), data = ww, na.action = na.exclude)
mb3g
summary(mb3g)
plot(ww$B03, predict(mb3g, newdata = ww,allow.new.levels = TRUE, re.form = NA))
plot(ww$B03, ww$B3)

plot(ww$B03, predict(mb3, newdata = ww))
plot(ww$B03, predict(mb3g, newdata = ww,allow.new.levels = TRUE, re.form = NA))


bb3 = ww[,c(1:3,5,14)]; bb3$pred = predict(mb3g, newdata = ww,allow.new.levels = TRUE, re.form = NA); bb3$vahe = bb3$B03 - bb3$pred
bb3 = bb3[order(abs(bb3$vahe), decreasing = T),]


abs3 = data.frame(sid = ww$aproovitykk_id, cat = ww$cat, kp = ww$kp, B03 = ww$B03, vahe = abs(ww$B03 - predict(mb3g, newdata = ww,allow.new.levels = TRUE, re.form = NA)))
abs3 = abs3[order(abs3$vahe, decreasing = T),]
sid3 = unique(head(abs3,100)$sid)

count = 1;
lnk = koos[koos$aproovitykk_id == sid3[count],]$link; browseURL(lnk, browser = getOption("browser"),encodeIfNeeded = FALSE); count = count +1


#B04
mb4 = lm(B04 ~ B4, data = ww)
mb4
summary(mb4)

plot(ww$B04, predict(mb4, newdata = ww))

#B08
mb5 = lm(B08 ~ B5, data = ww)
mb5
summary(mb5)

plot(ww$B08, predict(mb5, newdata = ww))

#B11
mb6 = lm(B11 ~ B6, data = ww)
mb6
summary(mb6)

plot(ww$B11, predict(mb6, newdata = ww))

#B12
mb7 = lm(B12 ~ B7, data = ww)
mb7
summary(mb7)

plot(ww$B12, predict(mb7, newdata = ww))


#nüüd saaks 2017 aasta info ka kasutusele võtta?
#kasuta andmestikku, kus erinevad sentinelid
# data = sat_sep_koos %>% dcast(aproovitykk_id + kp + cat  ~ band, value.var="value")
# 
# ww = data;
# 
# ww$B2_2 = ww$B2**2
# ww$B3_2 = ww$B3**2
# ww$B4_2 = ww$B4**2
# ww$B5_2 = ww$B5**2
# ww$B6_2 = ww$B6**2
# ww$B7_2 = ww$B7**2
# 
# 
# ww$B2_3 = ww$B2**3
# ww$B3_3 = ww$B3**3
# ww$B4_3 = ww$B4**3
# ww$B5_3 = ww$B5**3
# ww$B6_3 = ww$B6**3
# ww$B7_3 = ww$B7**3
# 
# 
# ww$B2_4 = ww$B2**4
# ww$B3_4 = ww$B3**4
# ww$B4_4 = ww$B4**4
# ww$B5_4 = ww$B5**4
# ww$B6_4 = ww$B6**4
# ww$B7_4 = ww$B7**4
# 
# ww$B2_sq = sqrt(ww$B2)
# ww$B3_sq = sqrt(ww$B3)
# ww$B4_sq = sqrt(ww$B4)
# ww$B5_sq = sqrt(ww$B5)
# ww$B6_sq = sqrt(ww$B6)
# ww$B7_sq = sqrt(ww$B7)
# 
# data = ww;

#mis krdi "cat" mul seal varem oli? tavaline lineaarne mudel1?
#nb, siin "ww" mis ei ole mudeli koostamiseks!
m2l = lm(B02 ~ B2 , data = ww, na.action = na.exclude)
m3l = lm(B03 ~ B3 , data = ww, na.action = na.exclude)
m4l = lm(B04 ~ B4 , data = ww, na.action = na.exclude)
m5l = lm(B08 ~ B5 , data = ww, na.action = na.exclude)
m6l = lm(B11 ~ B6 , data = ww, na.action = na.exclude)
m7l = lm(B12 ~ B7 , data = ww, na.action = na.exclude)


m2lin = lm(B02 ~ B2 , data = ww, na.action = na.exclude)

mdls = list(m2, m3, m4, m5, m6, m7, m2l, m3l, m4l, m5l, m6l, m7l) #lineaarfunktsioon + cati juhuslik mõju
bb = c(2,3,4,8,11,12,102,103,104,108,111,112)

data = sat_sep_koos %>% dcast(aproovitykk_id + kp + cat  ~ band, value.var="value")
#aga kui jagaks 14:16 9-ga (kuna ala on 9x suurem!?); 8a, 11 ja 12 sent 20x20, 5,6,7 ladnsat: 30x, ehk jagame (9/4)-ga
#data[,14:16] = data[14:16]/9; data[,17:20] = data[,17:20] /(9/4)

for(b in c(1:12)){
  bbb = bb[b]
  assign(paste("band",bbb, sep = "_"), predict(mdls[[b]], newdata = data, allow.new.levels = TRUE, re.form = NA))
}

data$predB02 = band_2
data$predB03 = band_3
data$predB04 = band_4
data$predB08 = band_8
data$predB11 = band_11
data$predB12 = band_12
data$predB02l = band_102
data$predB03l = band_103
data$predB04l = band_104
data$predB08l = band_108
data$predB11l = band_111
data$predB12l = band_112


plot(data$predB02, data$predB02l)
max(na.omit(data$predB02-data$predB02l)) #id ja cat annavad sama tulemuse, tõenäoliselt optimeerimisel erinevus
#linear ja juhuslik mõju siiski erinev nats! 6.838033 tulex max erinevus, kui enne on landsati väärtused kohendatud sentineli piksli suurustele


data = data[data$aproovitykk_id %in% mets_id,]

mse1 = function(arg1,arg2){
  df = arg1-arg2; df = na.omit(df)
  mn = mean( (arg1 - arg2)**2, na.rm = T)
  sqrt(mn)
  }


ms2 = mse1(data$B02, data$predB02)
ms3 = mse1(data$B03, data$predB03)
ms4 = mse1(data$B04, data$predB04)
ms8 = mse1(data$B08, data$predB08)
ms11 =mse1(data$B11, data$predB11)
ms12 =mse1(data$B12, data$predB12)

ms2l = mse1(data$B02, data$predB02l)
ms3l = mse1(data$B03, data$predB03l)
ms4l = mse1(data$B04, data$predB04l)
ms8l = mse1(data$B08, data$predB08l)
ms11l =mse1(data$B11, data$predB11l)
ms12l =mse1(data$B12, data$predB12l)

ms2;ms2l
ms3;ms3l
ms4;ms4l
ms8;ms8l
ms11;ms11l
ms12;ms12l

# > ms2;ms2l
# [1] 1171.352
# [1] 1356.889
# > ms3;ms3l
# [1] 1462.471
# [1] 1770.888
# > ms4;ms4l
# [1] 2464.622
# [1] 2848.044
# > ms8;ms8l
# [1] 33183.1
# [1] 34210.05
# > ms11;ms11l
# [1] 23962.27
# [1] 26470.07
# > ms12;ms12l
# [1] 9172.517
# [1] 10419.05
# > 

#teisel katsel... nüüd rmse muidugi, aga lineaarne parem!
# [1] 55.10601
# [1] 53.90756
# > ms3;ms3l
# [1] 67.1406
# [1] 63.45865
# > ms4;ms4l
# [1] 97.04917
# [1] 93.77613
# > ms8;ms8l
# [1] 204.2233
# [1] 203.8076
# > ms11;ms11l
# [1] 227.2503
# [1] 217.6539
# > ms12;ms12l
# [1] 170.0478
# [1] 166.124



#NB siin teine satsepkoos, kus sentinelid pole veel kokku pandud!
dxx = sat_sep_koos[sat_sep_koos$satel !="LC",] %>% dcast(aproovitykk_id + kp + cat + satel + ylelend  ~ band, value.var="value")
dxx = dxx[dxx$aproovitykk_id %in% mets_id,]
dxx1 = dxx[,c("aproovitykk_id","kp","satel", "ylelend")]
dxx2 = merge(data, dxx1, by =c("aproovitykk_id","kp"),all.x = T)



par(mfrow = c(3,2))

#dnomit = na.omit(data)
plot(data$B02, data$predB02,col = rgb(red = 0, green = 0, blue = 0, alpha = 0.09))
plot(data$B03, data$predB03,col = rgb(red = 0, green = 0, blue = 0, alpha = 0.09))
plot(data$B04, data$predB04,col = rgb(red = 0, green = 0, blue = 0, alpha = 0.09))
plot(data$B08, data$predB08,col = rgb(red = 0, green = 0, blue = 0, alpha = 0.09))
plot(data$B11, data$predB11,col = rgb(red = 0, green = 0, blue = 0, alpha = 0.09))
plot(data$B12, data$predB12,col = rgb(red = 0, green = 0, blue = 0, alpha = 0.09))

require(ggplot2)
data = dxx2
data = data[data$satel %in% c("S2AL1C", "S2BL1C"),]
p1 =ggplot(data = data, aes(x = B02, y = predB02l)) + geom_point(aes(colour = satel), alpha = 0.2) 
p2 =ggplot(data = data, aes(x = B03, y = predB03l)) + geom_point(aes(colour = satel), alpha = 0.2) 
p3 =ggplot(data = data, aes(x = B04, y = predB04l)) + geom_point(aes(colour = satel), alpha = 0.2) 
p4 =ggplot(data = data, aes(x = B08, y = predB08l)) + geom_point(aes(colour = satel), alpha = 0.2) 
p5 =ggplot(data = data, aes(x = B11, y = predB11l)) + geom_point(aes(colour = satel), alpha = 0.2) 
p6 =ggplot(data = data, aes(x = B12, y = predB12l)) + geom_point(aes(colour = satel), alpha = 0.2) 

grid.arrange(p1,p2,p3,p4,p5,p6,nrow = 3)
#nonii, kohe näha, et erinevad satid annavad erineva tulemuse
#sentineli trajektoorid mõlemal kuupäeval samad, aga landsat?

koos_2018_05_10 = sat_sep_koos[sat_sep_koos$kp == "2018-05-10",]
koos_2018_08_23 = sat_sep_koos[sat_sep_koos$kp == "2018-08-23",]

table(koos_2018_05_10$satel, koos_2018_05_10$ylelend)
table(koos_2018_08_23$satel, koos_2018_08_23$ylelend)

#ka samad trajektoorid :S

#aga kuidas nüüd pärast teisendust sentineli ja landsati kanalid neil kahel kuupäeval plottuvad?
p1 =ggplot(data = ww, aes(x = B02, y = B2)) + geom_point(aes(colour = satel), alpha = 0.2) 
p2 =ggplot(data = ww, aes(x = B03, y = B3)) + geom_point(aes(colour = satel), alpha = 0.2) 
p3 =ggplot(data = ww, aes(x = B04, y = B4)) + geom_point(aes(colour = satel), alpha = 0.2) 
p4 =ggplot(data = ww, aes(x = B08, y = B5)) + geom_point(aes(colour = satel), alpha = 0.2) 
p5 =ggplot(data = ww, aes(x = B11, y = B6)) + geom_point(aes(colour = satel), alpha = 0.2) 
p6 =ggplot(data = ww, aes(x = B12, y = B7)) + geom_point(aes(colour = satel), alpha = 0.2) 

grid.arrange(p1,p2,p3,p4,p5,p6,nrow = 3)
p6






m2 = lm(B02 ~ B2 , data = ww, na.action = na.exclude)


mse1 = function(arg1,arg2){mean((arg1 - arg2)**2, na.rm = T)}
ms2 = mse1(data$B02, data$predB02)
ms2.linear = mse1(data$B02, data$predB02_linear)


ms2;ms3;ms4;ms8;ms11;ms12
mean(c(ms2,ms3,ms4,ms8,ms11,ms12)) #11902.72 mudelil, mis ainult metsa põhjal

#metsa põhjal mudel:
# [1] 1171.352
# [1] 1462.471
# [1] 2464.622
# [1] 33183.1
# [1] 23962.27
# [1] 9172.517
# > mean(c(ms2,ms3,ms4,ms8,ms11,ms12))
# [1] 11902.72

#kõigi andmete põhjal mudel:
# [1] 1354.011
# [1] 1764.456
# [1] 2836.352
# [1] 34155.37
# [1] 26502.56
# [1] 10433.19
# > mean(c(ms2,ms3,ms4,ms8,ms11,ms12))
# [1] 12840.99

#kus on suurimad vead?

require(ggplot2)
require(plotly)
dfb2 = data.frame(SID = data$aproovitykk_id, true = data$B02, pred = data$predB02)
p = ggplot(dfb2, aes(x=true, y=pred)) + geom_point(aes(text = SID))# + geom_text(aes(label = SID), size = 2.5, vjust = 1.2)
ggplotly(p, tooltip = c("color","text"))

lnk = koos[koos$aproovitykk_id == 115191,"link"]; browseURL(lnk, browser = getOption("browser"),encodeIfNeeded = FALSE)
lnk = koos[koos$aproovitykk_id == 76150,"link"]; browseURL(lnk, browser = getOption("browser"),encodeIfNeeded = FALSE)
lnk = koos[koos$aproovitykk_id == 35293,"link"]; browseURL(lnk, browser = getOption("browser"),encodeIfNeeded = FALSE)


dfb3 = data.frame(SID = data$aproovitykk_id, true = data$B03, pred = data$predB03)
p = ggplot(dfb3, aes(x=true, y=pred)) + geom_point(aes(text = SID))# + geom_text(aes(label = SID), size = 2.5, vjust = 1.2)
ggplotly(p, tooltip = c("color","text"))

dfb4 = data.frame(SID = data$aproovitykk_id, true = data$B04, pred = data$predB04)
p = ggplot(dfb4, aes(x=true, y=pred)) + geom_point(aes(text = SID))# + geom_text(aes(label = SID), size = 2.5, vjust = 1.2)
ggplotly(p, tooltip = c("color","text"))

dfb11 = data.frame(SID = data$aproovitykk_id, true = data$B11, pred = data$predB11)
p = ggplot(dfb11, aes(x=true, y=pred)) + geom_point(aes(text = SID))# + geom_text(aes(label = SID), size = 2.5, vjust = 1.2)
ggplotly(p, tooltip = c("color","text"))
lnk = koos[koos$aproovitykk_id == 72444,"link"]; browseURL(lnk, browser = getOption("browser"),encodeIfNeeded = FALSE)

#suurimad absoluutsed vead:

data$abs = abs(data$B02 - data$predB02) + abs(data$B03 - data$predB03) + abs(data$B04 - data$predB04) + abs(data$B08 - data$predB08) + abs(data$B11 - data$predB11) + abs(data$B12 - data$predB12)
data$abs2 = abs(data$B02 - data$predB02)
data$abs3 = abs(data$B03 - data$predB03)
data$abs4 = abs(data$B04 - data$predB04)
data$abs8 = abs(data$B08 - data$predB08)
data$abs11 = abs(data$B11 - data$predB11)
data$abs12 = abs(data$B12 - data$predB12)


data = data[order(data$abs, decreasing = T),]
count = 1;
lnk = koos[koos$aproovitykk_id == data$aproovitykk_id[count],"link"]; browseURL(lnk, browser = getOption("browser"),encodeIfNeeded = FALSE); count = count +1

#korrelatsioon vigade homogeensuse puudumise vahel?

data_ints = data[data$aproovitykk_id %in% ints,]
#vaja oleks keskmisi erinevuse üle kuupäevade


dints_w = data_ints %>% group_by(aproovitykk_id) %>%  summarise_all(funs(mean(.,na.rm = T)))
dints = merge(dints_w, hh4, by = "aproovitykk_id")
plot(dints$abs, dints$score)
cor(dints$abs, dints$score, use = "complete.obs")


par(mfrow=c(3,2))
plot(dints$abs2, dints$score)
plot(dints$abs3, dints$score)
plot(dints$abs4, dints$score)
plot(dints$abs8, dints$score)
plot(dints$abs11, dints$score)
plot(dints$abs12, dints$score)


cor(dints$abs2, dints$score, use = "complete.obs")
cor(dints$abs3, dints$score, use = "complete.obs")
cor(dints$abs4, dints$score, use = "complete.obs")
cor(dints$abs8, dints$score, use = "complete.obs")
cor(dints$abs11, dints$score, use = "complete.obs")
cor(dints$abs12, dints$score, use = "complete.obs")

#############################

#asendame puuduvad väärtused prognoosidega

data$B02[is.na(data$B02)] = data$predB02l[is.na(data$B02)] #47 ---> 20 NA-d
data$B03[is.na(data$B03)] = data$predB03l[is.na(data$B03)] 
data$B04[is.na(data$B04)] = data$predB04l[is.na(data$B04)] 
data$B08[is.na(data$B08)] = data$predB08l[is.na(data$B08)] 
data$B11[is.na(data$B11)] = data$predB11l[is.na(data$B11)] 
data$B12[is.na(data$B12)] = data$predB12l[is.na(data$B12)] 

#
#kui nüüd agregeerida nii, et arvutab iga id kohta kõikide kanalite keskmise varieervuse üle "cat", siis see oleks inforks homogeensuse kohta!?

#rsd - relative standard deviation

rsd = function(vec){
  var(vec, na.rm = T) / mean(vec, na.rm = T)
}

require(reshape2)
#NB! see homo töötab vaid selle andmestiku peal, kus on iga eraldise kohta mitu piksli väärtust
homo = sat_sep %>% dcast(aproovitykk_id + kp ~ band, value.var="value", fun.aggregate = rsd)

var(dd34087[dd34087$kp == "2015-08-04",]$B11, na.rm = T)/mean(dd34087[dd34087$kp == "2015-08-04",]$B11, na.rm = T)

#nüüd nii kokku agregeerida, et võtta uuesti arvesse erinevate bandide var.coeffid

nmh = names(homo); nmh = nmh[nmh != "B10"]; homo = homo[,nmh]
hh1 = homo %>% group_by(aproovitykk_id) %>%  summarise_all(funs(mean(.,na.rm = T)))
#jei, siit saabki välja võtta juba :)
#B10 pole vaja


par(mfrow = c(3,3))
hist(hh1$B02);hist(hh1$B03);hist(hh1$B04)
hist(hh1$B05);hist(hh1$B06);hist(hh1$B07)
hist(hh1$B08);hist(hh1$B11);hist(hh1$B12)

hh10 = hh1[hh1$B05 > 100,]


link = "https://xgis.maaamet.ee/maps/XGis?app_id=MA29&user_id=at&LANG=1&WIDTH=1220&HEIGHT=1263&zlevel=12,688213.00000001,6446066.9999998&setlegend=HMAMULD_YLD=0,HMAHYBR_ALUS01_29=1,HMAHYBR_ALUS02_29=0"
koos$link = str_replace(link, str_sub(link, 98, 103),as.character(koos$koord_e))
koos$link = str_replace(koos$link, str_sub(link, 114,120),as.character(koos$koord_n -1))




hh2 = hh1; hh2[,-c(1,2)] = hh2[,-c(1,2)] /  colMeans(hh2[,-c(1,2)], na.rm = T)
hh2 = na.omit(hh2)

hh2$score = rowSums(hh2[,-c(1,2)])
hist(hh2$score)

hh3 = hh2[,c("aproovitykk_id", "score")]
hh3 = hh3[order(hh3$score, decreasing = F),]
mets_id = koos[koos$maakatsgrp == "M",]

mets_id = koos[koos$maakatsgrp == "M",]$aproovitykk_id
ints = intersect(hh3$aproovitykk_id,mets_id)
hh4 = hh3[hh3$aproovitykk_id %in% ints,]


count = 1;
lnk = koos[koos$aproovitykk_id == hh4$aproovitykk_id[count],"link"]; browseURL(lnk, browser = getOption("browser"),encodeIfNeeded = FALSE); count = count +1
#logaritmitud skooride histogramm? siis tuleks ka ebanormaalselt väiksed väärtused välja?

#### edasi mudeliga ####
#data

dc = data[,c(1:10,12,13)]

dcl = dc  %>% gather(band, value, -aproovitykk_id, -cat, -kp)

kpd = sort(format(as.Date(names(table(dcl$kp))), "%m-%d"))
dcl$kp1 = format(as.Date(dcl$kp), "%m-%d")

dcl$aa = ifelse(dcl$kp1 %in% kpd[1:6], "kevad1",
                  ifelse(dcl$kp1 %in% kpd[7:14], "kevad2", "sygis"))

plot(dcl$value, dcl$pred)
p = ggplot(dcl, aes(x=value, y=pred, colour = aa)) + geom_point(aes(text = aproovitykk_id))# + geom_text(aes(label = SID), size = 2.5, vjust = 1.2)
ggplotly(p, tooltip = c("color","text"))

#praegu ilma trajektoorita. landsati põhjal prognoositud väärtused võtta ka eraldi "trajektooriks"?

#mudel silumiseks ja puuduvate väärtuste kõrvaldamiseks:

bands = unique(dcl$band)

start = Sys.time()
start
for(band in bands){
  print(Sys.time())
  data_band = dcl[dcl$band == band,]
  mm1 = lm(value ~ factor(aproovitykk_id) + factor(aproovitykk_id)*aa + aa, data = data_band, na.action = na.exclude)
  dcl$pred[dcl$band == band] = predict(mm1, newdata = data_band)
}
end = Sys.time()
end
end - start

save(dcl, file = "data_xxx_v16_04_2019_linear_landsat_to_sentinel.RData")

# Warning messages:
#   1: In predict.lm(mm1, data_band) :
#   prediction from a rank-deficient fit may be misleading

#ID*aastaeg ei tööta tõenäoliselt
#kuidas mudel selle olukorra lahendab? - võtab üldkeskmise

band = "B12";data_band = dcl[dcl$band == band,]; hist(data_band$value)
db5 = data_band
db5 = db5[order(db5$aproovitykk_id),]
par(mfrow = c(1,1))
hist(db5[db5$aa == "kevad1",]$value)
hist(db5[db5$aa == "kevad2",]$value)
hist(db5[db5$aa == "sygis",]$value)

db5w = db5 %>% group_by(aproovitykk_id, band, aa) %>% sample_n(1)

hist(db5w[db5w$aa == "kevad2",]$pred - db5w[db5w$aa == "kevad1",]$pred)
table(table(db5w[db5w$aa == "kevad2",]$pred - db5w[db5w$aa == "kevad1",]$pred))
#15 korral sama väärtus
table(db5w[db5w$aa == "kevad2",]$pred - db5w[db5w$aa == "kevad1",]$pred)[table(db5w[db5w$aa == "kevad2",]$pred - db5w[db5w$aa == "kevad1",]$pred) == 15]
#-287.666666665294

db5w = db5 %>% group_by(aproovitykk_id, band, aa) %>% sample_n(1) %>%
  dcast(aproovitykk_id ~ band + aa , value.var="pred")
db5w$vahe = db5w$B05_kevad2 - db5w$B05_kevad1
rank_def = db5w[round(db5w$vahe,3) == -287.667,]

#34092 nt rank def.?
d5 = data_band[data_band$aproovitykk_id ==34092,]
d5 = d5[order(d5$aa),]
#ei tohiks olla probleeme

#72605
d5 = data_band[data_band$aproovitykk_id ==72605,]
d5 = d5[order(d5$aa),]
#mnjaa, siin küll astakudefitsiit...
mean(data_band[data_band$aa == "kevad2",]$value, na.rm = T)
#kuidas 463 saab mudel?
#intercept + 72605 + aa?
summary(mm1)
#1091.000 -340.000 -287.66667 #ok!

dk = dcl %>% group_by(aproovitykk_id, band, aa) %>% sample_n(1) %>%
  dcast(aproovitykk_id ~ band + aa , value.var="pred")

names1 = names(dk)
nk1 = grep("kevad1", names1, value = T) #kevad 1
nk2 = grep("kevad2", names1, value = T) #kevad 2
ns = grep("sygis", names1, value = T)
nkv = paste(gsub("_","",str_sub(nk1,1,3)), "vahe_kevad", sep = "_")
vahed_kevad = data.frame(dk[,nk2] - dk[,nk1])
names(vahed_kevad) = nkv

dk = cbind(dk, vahed_kevad)
save(dk, file = "lnds_to_sent_linear_model_16_04_2019.RData")

load(file = "lnds_to_sent.RData")

#load(file = "koond455_uus_landsat_to_sentinel_mudel.RData") #nimi: data
#lõpuks sai selline andmestik koos mullaga

