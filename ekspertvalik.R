#ekspertvalik

#mis nende "kõrguse" ja "katvuse" erinevus on?

# H_Elev_Var, H_Elev_P50, H_Elev_P90
# B2, B3, B4 - landsat
# B02, B03, B04, B05 - sentinel, 08 NIR. võtaks ainult kevadesid sentinelid
# erinevused?
#   
#landsatilt B5 - NIR
dat1 = sat18w
names(dat1)
ekspert = c("SID","B02_kevad1", "B02_kevad2", "B02_vahe_kevad", "B03_kevad1", "B03_kevad2", "B03_vahe_kevad",
                  "B04_kevad1", "B04_kevad2", "B04_vahe_kevad","B05_kevad1", "B05_kevad2", "B05_vahe_kevad",
                  "B08_kevad1", "B08_kevad2", "B08_vahe_kevad", 
                  "H_Elev_P50", "H_Elev_P90", "H_Elev_variance")

data_ex = sat18_lidar[,ekspert]

#selle peal KNN
fun_agre_ex = function(data, data_puud)
{
  dists = knn.cv(train = data[,2:(dim(data)[2]-1)], cl = data$cl, k = 10)
  dist1 = attr(dists,"nn.dist")
  index1 = attr(dists,"nn.index")
  props = apply(dist1, 1, propcumsum1) #erinevad proportsioonide vektorid võimalikud
  props = t(props)
  indxprops = cbind(index1, props)
  data_puud = data_puud
  t(apply(indxprops, 1, agre))
}


fun_agre_test = function(data, data_puud)
{
  train = data[,2:(dim(data)[2]-1)]
  dists = knn.cv(train, cl = data$cl, k = 10)
  dist1 = attr(dists,"nn.dist")
  dist1 = dist1 + 1e-10
  index1 = attr(dists,"nn.index")
  props = apply(dist1, 1, propcumsum1) #siia vaja epanechnikov
  props = t(props)
  indxprops = cbind(index1, props)
  data_puud = data_puud
  retrn = data.frame(t(apply(indxprops, 1, agre)))
  retrn
}




SID_temp = SID_temp #need 235
SID_temp = sidxx #207
intsct = intersect(mets_raie$SID,koos$SID); intsct = intersect(intsct, SID_temp); intsct = intersect(intsct, taks_uus$SID)

data_puud = taks_uus[taks_uus$SID %in% intsct, puud]
SID_raie1 = taks_uus[taks_uus$SID %in% intsct,"SID"]
mets_raie = data_ex
mets_raie$cl = "cl"
mets_raie1 = mets_raie[mets_raie$SID %in% intsct,]
HINNANG_RAIE = fun_agre_ex(mets_raie1, data_puud)


#data_puud_raie = koos[koos$SID %in% intsct,c("MA", "KS", "KU", "HB", "LM", "LV", "KX")]
intsct = sidxx
EKSPERTHINNANG_RAIE_JA_TODE = data.frame(cbind(intsct, HINNANG_RAIE, round(data_puud,0)))
par(mfrow=c(1,1))
plot(EKSPERTHINNANG_RAIE_JA_TODE$ARV_VMA, HINNANG_RAIE_JA_TODE$ARV_VMA.1)
plot(EKSPERTHINNANG_RAIE_JA_TODE$ARV_VKU, HINNANG_RAIE_JA_TODE$ARV_VKU.1)
plot(EKSPERTHINNANG_RAIE_JA_TODE$ARV_VKS, HINNANG_RAIE_JA_TODE$ARV_VKS.1)
plot(EKSPERTHINNANG_RAIE_JA_TODE$ARV_VHB, HINNANG_RAIE_JA_TODE$ARV_VHB.1)

EKSPERTHINNANG_RAIE_JA_TODE1 = HINNANG_RAIE / rowSums(HINNANG_RAIE)

#ekspert, mille saime RSS põhjal:

ekspert = c("SID", "B2_sygis", "B07_kevad1","B4_sygis","B04_kevad1","B5_sygis","B06_kevad2")
eksp_w = c(0.23892375,0.22223079,0.15061512, 0.12824842,0.11369736,0.09852142)
eksp_w1 = eksp_w / sum(eksp_w)

data_ekspert = data10[,ekspert]
data_ekspert[,-1] = t((t(as.matrix(data_ekspert[,-1])))*eksp_w1)
data_ekspert$cl = "cl"

H_eks1 = fun_agre_ex(data_ekspert, data_puud)



mets_raie = data_ex
mets_raie$cl = "cl"
mets_raie1 = mets_raie[mets_raie$SID %in% sidxx,]
H_eks0 = fun_agre_ex(mets_raie1, data_puud)
H_eks01 = fun_agre_test(mets_raie1, data_puud)#algne ekspert võrdluseks
H0 = H_eks0 / rowSums(H_eks0)
H01 = H_eks01 / rowSums(H_eks01)

EKSPERTHINNANG_RAIE_JA_TODE = data.frame(cbind(sidxx, H, data_puud_raie_props))
names(EKSPERTHINNANG_RAIE_JA_TODE) = c("SID",paste(rep(c("MA", "KU", "KS", "HB", "LV", "LM", "KX"),2),
                                                  rep(c("EKS_VALIK","TODE"), each = 7), sep = "."))

plot(EKSPERTHINNANG_RAIE_JA_TODE$MA.EKS_VALIK, EKSPERTHINNANG_RAIE_JA_TODE$MA.TODE)
plot(EKSPERTHINNANG_RAIE_JA_TODE$KU.EKS_VALIK, EKSPERTHINNANG_RAIE_JA_TODE$KU.TODE)
plot(EKSPERTHINNANG_RAIE_JA_TODE$KS.EKS_VALIK, EKSPERTHINNANG_RAIE_JA_TODE$KS.TODE)
plot(EKSPERTHINNANG_RAIE_JA_TODE$HB.EKS_VALIK, EKSPERTHINNANG_RAIE_JA_TODE$HB.TODE)

#sellise valiku kaalutud "viga", kus olid:
# ekspert = c("SID", "B2_sygis", "B07_kevad1","B4_sygis","B04_kevad1","B5_sygis","B06_kevad2")
# eksp_w = c(0.23892375,0.22223079,0.15061512, 0.12824842,0.11369736,0.09852142)
# eksp_w1 = eksp_w / sum(eksp_w)
#skaleeritud andmete peal tehtud oli ka

rsdls = (EKSPERTHINNANG_RAIE_JA_TODE[,2:8] - EKSPERTHINNANG_RAIE_JA_TODE[,9:15])
w = colSums(EKSPERTHINNANG_RAIE_JA_TODE[,9:15]) / dim(EKSPERTHINNANG_RAIE_JA_TODE[,9:15])[1]
cols = colSums(rsdls**2)
RSS_col = cols*w
sum(RSS_col) #11.73075

#kui nüüd:
# ekspert1
# #[1] "B04_kevad1" "B06_kevad2" "B07_kevad1" "B07_kevad2" "B08_kevad1" "B08_kevad2" "B08_sygis"  "B11_kevad1"
# #[9] "B11_kevad2" "B2_sygis"   "B3_sygis"   "B4_sygis"   "B5_sygis"  
# eksp1_w1
# #[1] 0.08728507 0.06705306 0.15124888 0.10709088 0.04942480 0.03087296 0.03501200 0.02711928 0.03994385 0.16261000
# #[11] 0.06244980 0.10250770 0.07738171

data_ekspert1 = data10[,c("SID",ekspert1)]
e13 = rep(1,13)
data_ekspert1[,-1] = t((t(as.matrix(data_ekspert1[,-1])))*eksp1_w1) #eksp1_w1
data_ekspert1$cl = "cl"

H_eks1 = fun_agre_ex(data_ekspert1, data_puud)
H1 = H_eks1 / rowSums(H_eks1)

EH1 = data.frame(cbind(sidxx, H1, data_puud_raie_props))
names(EH1) = c("SID",paste(rep(c("MA", "KU", "KS", "HB", "LV", "LM", "KX"),2),
                                                   rep(c("EKS_VALIK","TODE"), each = 7), sep = "."))

rsdls = (EH1[,2:8] - EH1[,9:15])
w = colSums(EH1[,9:15]) / dim(EH1[,9:15])[1]
cols = colSums(rsdls**2)
RSS_col = cols*w
sum(RSS_col) #10.79559
#aga kui kaalud oleksid samad, siis tulemus: 10.83907. Ehk vahet pole...

plot(EH1$MA.EKS_VALIK, EH1$MA.TODE)
plot(EH1$KU.EKS_VALIK, EH1$KU.TODE)
plot(EH1$KS.EKS_VALIK, EH1$KS.TODE)
plot(EH1$HB.EKS_VALIK, EH1$HB.TODE)

#nüüd kaalud, mis sain kõik kombod läbi jooksutades:

data_ekspert1 = data10[,c("SID",eksp_all)]
data_ekspert1[,-1] = t((t(as.matrix(data_ekspert1[,-1])))*w_all1)
data_ekspert1$cl = "cl"

H_eks1 = fun_agre_ex(data_ekspert1, data_puud)
H1 = H_eks1 / rowSums(H_eks1)

EH1 = data.frame(cbind(sidxx, H1, data_puud_raie_props))
names(EH1) = c("SID",paste(rep(c("MA", "KU", "KS", "HB", "LV", "LM", "KX"),2),
                           rep(c("EKS_VALIK","TODE"), each = 7), sep = "."))

rsdls = (EH1[,2:8] - EH1[,9:15])
w = colSums(EH1[,9:15]) / dim(EH1[,9:15])[1]
cols = colSums(rsdls**2)
RSS_col = cols*w
sum(RSS_col) #10.36026

plot(EH1$MA.EKS_VALIK, EH1$MA.TODE)
plot(EH1$KU.EKS_VALIK, EH1$KU.TODE)
plot(EH1$KS.EKS_VALIK, EH1$KS.TODE)
plot(EH1$HB.EKS_VALIK, EH1$HB.TODE)


#uuel ringil:
w2_all1
#0.09601557 0.12703677 0.19778564 0.18242163 0.09794138 0.02075639 0.01382082 0.08665381 0.03561882 0.14194918
eksp_all2
# [1] "B04_kevad1"                 "B06_kevad2"                 "B07_kevad1"                 "B07_kevad2"                
# [5] "B2_sygis"                   "B3_sygis"                   "B4_sygis"                   "K_Elev_variance"           
# [9] "K_First_returns_above_mean" "H_Return_2_count_above_130"

data_ekspert2 = data10[,c("SID",eksp_all2)]
data_ekspert2[,-1] = t((t(as.matrix(data_ekspert2[,-1])))*w2_all1)
data_ekspert2$cl = "cl"

H_eks1 = fun_agre_ex(data_ekspert2, data_puud)
H1 = H_eks1 / rowSums(H_eks1)

EH1 = data.frame(cbind(sidxx, H1, data_puud_raie_props))
names(EH1) = c("SID",paste(rep(c("MA", "KU", "KS", "HB", "LV", "LM", "KX"),2),
                           rep(c("EKS_VALIK","TODE"), each = 7), sep = "."))

rsdls = (EH1[,2:8] - EH1[,9:15])
w = colSums(EH1[,9:15]) / dim(EH1[,9:15])[1]
cols = colSums(rsdls**2)
RSS_col = cols*w
sum(RSS_col) #10.11523, läheb ikka nats paremaks

plot(EH1$MA.EKS_VALIK, EH1$MA.TODE)
plot(EH1$KU.EKS_VALIK, EH1$KU.TODE)
plot(EH1$KS.EKS_VALIK, EH1$KS.TODE)
plot(EH1$HB.EKS_VALIK, EH1$HB.TODE)

###
eksp_all3;w3_all1
# [1] "B04_kevad1"                 "B06_kevad2"                 "B07_kevad1"                 "B07_kevad2"                
# [5] "B2_sygis"                   "B3_sygis"                   "K_Elev_variance"            "H_Return_2_count_above_130"
# [1] 0.11936826 0.13037482 0.15897297 0.15710982 0.10692260 0.06264722 0.10454846 0.16005585
data_ekspert3 = data10[,c("SID",eksp_all3)]
data_ekspert3[,-1] = t((t(as.matrix(data_ekspert3[,-1])))*w3_all1)
data_ekspert3$cl = "cl"

H_eks1 = fun_agre_ex(data_ekspert3, data_puud)
H1 = H_eks1 / rowSums(H_eks1)

EH1 = data.frame(cbind(sidxx, H1, data_puud_raie_props))
names(EH1) = c("SID",paste(rep(c("MA", "KU", "KS", "HB", "LV", "LM", "KX"),2),
                           rep(c("EKS_VALIK","TODE"), each = 7), sep = "."))

rsdls = (EH1[,2:8] - EH1[,9:15])
w = colSums(EH1[,9:15]) / dim(EH1[,9:15])[1]
cols = colSums(rsdls**2)
RSS_col = cols*w
sum(RSS_col) #9.990812


plot(EH1$MA.EKS_VALIK, EH1$MA.TODE)
plot(EH1$KU.EKS_VALIK, EH1$KU.TODE)
plot(EH1$KS.EKS_VALIK, EH1$KS.TODE)
plot(EH1$HB.EKS_VALIK, EH1$HB.TODE)

require(ggplot2)
ggplot(EH1, aes(x = MA.EKS_VALIK, y = MA.TODE)) + geom_text(aes(label = SID), size = 2)
ggplot(EH1, aes(x = KU.EKS_VALIK, y = KU.TODE)) + geom_text(aes(label = SID), size = 2)
ggplot(EH1, aes(x = KS.EKS_VALIK, y = KS.TODE)) + geom_text(aes(label = SID), size = 2)

#Mänd: 73797 ja 82448 väga kahtlased, 35333
#Kuusk: 125051, 75958, 109187
#Kask: 82489, 80837, 73706, prognoos suur 73797 (männil väike sama!), 76496

lnk = raied50[raied50$aproovitykk_id == 82448,]$link
browseURL(lnk, browser = getOption("browser"),encodeIfNeeded = FALSE)

#73797 - väga problemaatiline. Peaks justkui olema 71% mändi. Vb koordinaadid mööda. Pildi pealt mändi ei paista. Raiesmik ka mitmes suunas.
#76496 - jällegi probleematiline. Peaks olema 60% leppa ja 35% kuuske. Pildil ühtki kuuske ei paista. Lepavõsa võib olla. Suurem probleem muidugi põld.
#82448 - männi enamusega segamets. Ei näe klassiklaine "männik" välja. Mõistetav, et valesti klassiftseeritud. Samas raiesmik ja põld kõrval.
#35333 - tundub kena männik. Põld mitmes suunas.
#125051 - väga problemaatiline. Väike tüvemaht, raiesmikud ja lagendikud mitmes suunas. Miks seda varem pole välja võetud? Seda polegi võimalik täpselt prognoosida.
#75958 - raismik kõrval + harvendus
#109187 - ei ole homogeenne. Loogiline, et pole täpselt prognoositud.
#82489 - ei ole homogeenne. Taks. põhjal 80% kask, pildi põhjal keset männikuid madalam ala / ojaorg? Tüvemahu hinnangu annavad tõenäoliselt mõned männid.
#80837 - ei ole homogeenne. Majad ja põllud kõrval. Talvine pilt, liikidest ei saa aru.
#73706 - läheduses tiik, kraav+kallas. Võibolla metsa ka harvendatud.

v2lja =  c(73797,76496,82448, 35333, 125051,75958,109187,82489,80837)
sidxx = sidxx[!(sidxx %in% v2lja)]
data_puud = taks_uus[taks_uus$SID %in% sidxx, puud]
data = sat18_lidar[sat18_lidar$SID %in% sidxx,]
data10 = data; data10[,2:243] = scale(data[,2:243]);

dex = data10[,c("SID",eksp_all3)]
dex[,-1] = t((t(as.matrix(dex[,-1])))*w3_all1)
dex$cl = "cl"

H_eks1 = fun_agre_ex(dex, data_puud); H1 = H_eks1 / rowSums(H_eks1)
data_puud_raie_props = data_puud / rowSums(data_puud)

EH1 = data.frame(cbind(sidxx, H1, data_puud_raie_props))
names(EH1) = c("SID",paste(rep(c("MA", "KU", "KS", "HB", "LV", "LM", "KX"),2),
                           rep(c("EKS_VALIK","TODE"), each = 7), sep = "."))

rsdls = (EH1[,2:8] - EH1[,9:15])
w = colSums(EH1[,9:15]) / dim(EH1[,9:15])[1]
cols = colSums(rsdls**2)
RSS_col = cols*w
sum(RSS_col) #9.990812 --->>> 8.153395 pärast vigaste eemaldamist.


plot(EH1$MA.EKS_VALIK, EH1$MA.TODE)
plot(EH1$KU.EKS_VALIK, EH1$KU.TODE)
plot(EH1$KS.EKS_VALIK, EH1$KS.TODE)
plot(EH1$HB.EKS_VALIK, EH1$HB.TODE)

require(ggplot2)
ggplot(EH1, aes(x = MA.EKS_VALIK, y = MA.TODE)) + geom_text(aes(label = SID), size = 2, angle = 45)
ggplot(EH1, aes(x = KU.EKS_VALIK, y = KU.TODE)) + geom_text(aes(label = SID), size = 2, angle = 45)
ggplot(EH1, aes(x = KS.EKS_VALIK, y = KS.TODE)) + geom_text(aes(label = SID), size = 2, angle = 45)

#uuesti vaatame "vigaseid":
#Mänd: 82493, 82446, 124301, 109438
#Kuusk: 82493, 82482, 82472, 124831, 124475
#Kask: 73706 - vana viga juba. Jätan sisse
lnk = raied50[raied50$aproovitykk_id == 124475,]$link
browseURL(lnk, browser = getOption("browser"),encodeIfNeeded = FALSE)

#82493 - harvendus + raiesmik kõrval
#82446 - ei ole homogeenne. Raiesmikud kõrval. Taks. andmetel 60% kuusk - ei näe.
#124301 - pildi järgi üleminekuala männi- ja kuusemetsa vahel. Viga mõistetav. Ei peaks treeningandmetes kasutama?
#109438 - segamets, aga mitte homogeenne vaid saludena. Ei tea ju, kus takseerala täpselt paikneb. Probleem mõistetav.
#82493 - raisemiku piiril. Välja!
#82482 - raiesmikud, erineva vanusega mets. Ei tundu ka liigiliselt homogeenne. Ei peaks treenima selle põhjal.
#82472 - kuuse- ja mingi lehtmetsa piiril. Soostunud ala kõrval. Ei peaks treenima?
#124831 - midagi raitud. Talvine pilt, ei saa hästi aru. Teed-põllud lähedal. Oleneb väga sellest, kus täpselt takseerala asub, ehk kui täpsed on koordinaadid.
#124475 - raiesmik kõrval. Pigem välja.

v2lja1 = c(82493, 82446 ,109438,82493,82482,82472,124831,124475)
sidxx = sidxx[!(sidxx %in% v2lja1)]
data_puud = taks_uus[taks_uus$SID %in% sidxx, puud]
data = sat18_lidar[sat18_lidar$SID %in% sidxx,]
data10 = data; data10[,2:243] = scale(data[,2:243]);

dex = data10[,c("SID",eksp_all3)]
dex[,-1] = t((t(as.matrix(dex[,-1])))*w3_all1)
dex$cl = "cl"

H_eks1 = fun_agre_ex(dex, data_puud); H1 = H_eks1 / rowSums(H_eks1)
data_puud_raie_props = data_puud / rowSums(data_puud)

EH1 = data.frame(cbind(sidxx, H1, data_puud_raie_props))
names(EH1) = c("SID",paste(rep(c("MA", "KU", "KS", "HB", "LV", "LM", "KX"),2),
                           rep(c("EKS_VALIK","TODE"), each = 7), sep = "."))

rsdls = (EH1[,2:8] - EH1[,9:15])
w = colSums(EH1[,9:15]) / dim(EH1[,9:15])[1]
cols = colSums(rsdls**2)
RSS_col = cols*w
sum(RSS_col) #9.990812 --->>> 8.153395 pärast vigaste eemaldamist. --->>> 7.18667;

plot(EH1$MA.EKS_VALIK, EH1$MA.TODE)
plot(EH1$KU.EKS_VALIK, EH1$KU.TODE)
plot(EH1$KS.EKS_VALIK, EH1$KS.TODE)
plot(EH1$HB.EKS_VALIK, EH1$HB.TODE)

require(ggplot2)
ggplot(EH1, aes(x = MA.EKS_VALIK, y = MA.TODE)) + geom_text(aes(label = SID), size = 3, angle = 90)
ggplot(EH1, aes(x = KU.EKS_VALIK, y = KU.TODE)) + geom_text(aes(label = SID), size = 3, angle = 90)
ggplot(EH1, aes(x = KS.EKS_VALIK, y = KS.TODE)) + geom_text(aes(label = SID), size = 3, angle = 90)

#Mänd: 124301, 112479 ... 76162, 109177, 71281, 115167
#Kuusk: 79880, 109177, 109143, 114723 ... 114443, 34264, 34245, 80861, 79878, 124301
#Kask: 73706, 114427 ... 114081, 112499, 34262, 124825, 125055, 114069, 35079, 114405

lnk = raied50[raied50$aproovitykk_id == 114405,]$link
browseURL(lnk, browser = getOption("browser"),encodeIfNeeded = FALSE)

#124301 - oli juba varem: männi- ja kuusemetsa piiril
#112479 - ei ole homogeenne. Raiesmik lähedal.
#76162 - ei ole homogeenne: kuuse ja männi? piiril, raiesmik ka lähedal.
#109177 - ok.
#71281 - ei ole homogeenne. Väike lapike keset sood.
#115167 - segamets, ei ole homogeenne. Pole samas otsest põhjust välja võtta.

#79880 - ei ole homogeenne. Väike kuusesalu keset kaski?. Põld kõrval. Välja!
#109177 - ok.
#109143 - segamets. Ongi raske prognoosida.
#114723 - ok, kui takseer on ikka õige.
#114443 - segamets
#34264 - segamets
#34245 - segamets. Homog?
#80861 - erinevad metsad, pole homog? Peaks olema 55% kuuske, silma järgi küll ei tundu.
#79878 - raiesmik lähedal. Kasvukoha põhjal võiks olla küll must lepp.
#124301 - männi- ja kuusemetsa piiril

#73706 - vana probleemne vaatlus
#114427 - pole homogeenne, raiesmik lähedal. Üldiselt tundub aga kask olema.
#114081 - kena homogeenne lepik!
#112499 - ei kujuta ette, mis olla võiks. Mingi istutatud mets. Liiga erandlik, et treeningandmetes kasutada? Astelpaju!?
#34262 - ei ole homog. Välja!
#124825 - võib silma järgi küll haavik olla
#125055 - raiesmiku piiril, pole homog. Välja!
#114069 - pole homog. Välja!
#35079 - kena haavik. Tahaks jätta. Samas tee kohe kõrval ja teisel pool teed kuusik.
#114405 - Liigid vastavad ilmselt takseerile. Imelikult suur tüvemaht. Kas on õiged koordiaadid?

v2lja2 = c(112479,76162,71281,79880,80861,114427,112499,34262,125055,114069)

sidxx = sidxx[!(sidxx %in% v2lja2)]
data_puud = taks_uus[taks_uus$SID %in% sidxx, puud]
data = sat18_lidar[sat18_lidar$SID %in% sidxx,]
data10 = data; data10[,2:243] = scale(data[,2:243]);

dex = data10[,c("SID",eksp_all3)]
dex[,-1] = t((t(as.matrix(dex[,-1])))*w3_all1)
dex$cl = "cl"

H_eks1 = fun_agre_ex(dex, data_puud); H1 = H_eks1 / rowSums(H_eks1)
data_puud_raie_props = data_puud / rowSums(data_puud)

EH1 = data.frame(cbind(sidxx, H1, data_puud_raie_props))
names(EH1) = c("SID",paste(rep(c("MA", "KU", "KS", "HB", "LV", "LM", "KX"),2),
                           rep(c("EKS_VALIK","TODE"), each = 7), sep = "."))

rsdls = (EH1[,2:8] - EH1[,9:15])
w = colSums(EH1[,9:15]) / dim(EH1[,9:15])[1]
cols = colSums(rsdls**2)
RSS_col = cols*w
sum(RSS_col) #9.990812 --->>> 8.153395 pärast vigaste eemaldamist. --->>> 7.18667 ---- >>> 6.259736;

plot(EH1$MA.EKS_VALIK, EH1$MA.TODE)
plot(EH1$KU.EKS_VALIK, EH1$KU.TODE)
plot(EH1$KS.EKS_VALIK, EH1$KS.TODE)
plot(EH1$HB.EKS_VALIK, EH1$HB.TODE)

require(ggplot2)
ggplot(EH1, aes(x = MA.EKS_VALIK, y = MA.TODE)) + geom_text(aes(label = SID), size = 3, angle = 90)
ggplot(EH1, aes(x = KU.EKS_VALIK, y = KU.TODE)) + geom_text(aes(label = SID), size = 3, angle = 90)
ggplot(EH1, aes(x = KS.EKS_VALIK, y = KS.TODE)) + geom_text(aes(label = SID), size = 3, angle = 90)

#viimane kord erindid üle vaadata ja siis uuesti tunnused läbi jooksutada!
#Mänd: 124301,124477,124267,80853 ... 109143,115167,82484
#Kuusk: 109143,114723,109177,78148 ... 79878,124301,114443,34264,34245
#Kask: 72407,73706,70230,73779 ... 114081!!!, 114405,124825,35079,78150


lnk = raied50[raied50$aproovitykk_id == 78150,]$link
browseURL(lnk, browser = getOption("browser"),encodeIfNeeded = FALSE)

##124301 - oli juba varem: männi- ja kuusemetsa piiril
#124477 - ei ole homogeenne. Mitmes vanuses istutatud mets, raiesmik. Taks. põhjal 25 aastane.
#124267 - ei ole homogeenne. Männi- ja lehtmetsa piiril. Põld võib ka mõjutada.
#80853 - ei ole homogenne.Rabasaare servas
#109143 - segamets. Tee ja õueala kõrval. ?
#115167 - segamets, ei ole homogeenne. Pole samas otsest põhjust välja võtta. ?
#82484 - raudtee kõrval. Raiesmik lähedal. Muidu kena segamets. ?

#109143 - vt mänd.
#114723 - kehv kasutada klassifitseerimiseks, kuna tüvemaht väike ja pole ka väga homog. ?
#109177 - suht ok, raiesmik küll üsna lähedal.
#78148 - pole homogeenne. Raiesmik kõrval. Välja!
#79879 - raiesmik ja kraav kõrval. ?
#124301 - vt mänd
#114443 - tüvemaht 53! Noor kaasik? Välja?
#34264 - segamets, samas pole väga homogeenne
#34245 - segamets, pole homogeenne. ?

#72407 - vana raiutud ala piiril. Seega pole homogeenne?
#73706 - vt eespool, vana probleem
#70230 - ?
#73779 - ei ole väga homogeenne, aga ...?
#114081 - ilus lepik, vt eespool
#114405 - Liigid vastavad ilmselt takseerile. Imelikult suur tüvemaht. Kas on õiged koordiaadid?
#124825 - haavik
#35079 - kena haavik. Tahaks jätta. Samas tee kohe kõrval ja teisel pool teed kuusik. Võtan välja, ei ole homog.
#78150 - kena mets.

v2lja3 = c(124301, 124267, 80853, 78148, 35079)
nms = names(sat18_lidar);nms[1] = "SID";names(sat18_lidar) = nms
sidxx = sidxx[!(sidxx %in% v2lja3)]
data_puud = taks_uus[taks_uus$SID %in% sidxx, puud]
data10 = sat18_lidar[sat18_lidar$SID %in% sidxx,]
data10[,2:243] = scale(data10[,2:243]);

dex = data10[,c("SID",eksp_all3)]
dex[,-1] = t((t(as.matrix(dex[,-1])))*w3_all1)
dex$cl = "cl"

H_eks1 = fun_agre_ex(dex, data_puud); H1 = H_eks1 / rowSums(H_eks1)
data_puud_raie_props = data_puud / rowSums(data_puud)

EH1 = data.frame(cbind(sidxx, H1, data_puud_raie_props))
names(EH1) = c("SID",paste(rep(c("MA", "KU", "KS", "HB", "LV", "LM", "KX"),2),
                           rep(c("EKS_VALIK","TODE"), each = 7), sep = "."))

rsdls = (EH1[,2:8] - EH1[,9:15])
w = colSums(EH1[,9:15]) / dim(EH1[,9:15])[1]
cols = colSums(rsdls**2)
RSS_col = cols*w
sum(RSS_col) #9.990812 --->>> 8.153395 pärast vigaste eemaldamist. --->>> 7.18667 ---- >>> 6.259736 --->>> 5.949309;

plot(EH1$MA.EKS_VALIK, EH1$MA.TODE)
plot(EH1$KU.EKS_VALIK, EH1$KU.TODE)
plot(EH1$KS.EKS_VALIK, EH1$KS.TODE)
plot(EH1$HB.EKS_VALIK, EH1$HB.TODE)

require(ggplot2)
ggplot(EH1, aes(x = MA.EKS_VALIK, y = MA.TODE)) + geom_text(aes(label = SID), size = 3, angle = 90)
ggplot(EH1, aes(x = KU.EKS_VALIK, y = KU.TODE)) + geom_text(aes(label = SID), size = 3, angle = 90)
ggplot(EH1, aes(x = KS.EKS_VALIK, y = KS.TODE)) + geom_text(aes(label = SID), size = 3, angle = 90)

#Kui kasutada Epanechnikovi kaale, siis viimane tulemus oleks...

fun_agre_epa = function(data, data_puud, k)
{
  kk = k+1
  dists = knn.cv(train = data[,2:(dim(data)[2]-1)], cl = data$cl, k = kk)
  dist1 = attr(dists,"nn.dist")
  dist1 = dist1 + 1e-8 #kui lähim naaber on kaugusel 0
  index1 = attr(dists,"nn.index")
  props = apply(dist1, 1, epa)
  props = t(props)
  indxprops = cbind(index1, props)
  data_puud = data_puud
  t(apply(indxprops, 1, agre))
}

agre <- function(arg){
  kk = length(arg) / 2
  indx = arg[1:kk]; props = arg[(kk+1):(2*kk)]
  colsums = colSums(data_puud[indx,]*props)
  colsums
}

H_eks1 = fun_agre_epa(dex, data_puud); H1 = H_eks1 / rowSums(H_eks1)
data_puud_raie_props = data_puud / rowSums(data_puud)

EH1 = data.frame(cbind(sidxx, H1, data_puud_raie_props))
names(EH1) = c("SID",paste(rep(c("MA", "KU", "KS", "HB", "LV", "LM", "KX"),2),
                           rep(c("EKS_VALIK","TODE"), each = 7), sep = "."))

rsdls = (EH1[,2:8] - EH1[,9:15])
w = colSums(EH1[,9:15]) / dim(EH1[,9:15])[1]
cols = colSums(rsdls**2)
RSS_col = cols*w
sum(RSS_col) #9.990812 --->>> 8.153395 pärast vigaste eemaldamist. --->>> 7.18667 ---- >>> 6.259736 --->>> 5.949309;
#Epanechnikov: 5.897734. Suht sama. Aga nõks parem :D

plot(EH1$MA.EKS_VALIK, EH1$MA.TODE)
plot(EH1$KU.EKS_VALIK, EH1$KU.TODE)
plot(EH1$KS.EKS_VALIK, EH1$KS.TODE)
plot(EH1$HB.EKS_VALIK, EH1$HB.TODE)

require(ggplot2)
ggplot(EH1, aes(x = MA.EKS_VALIK, y = MA.TODE)) + geom_text(aes(label = SID), size = 3, angle = 90)
ggplot(EH1, aes(x = KU.EKS_VALIK, y = KU.TODE)) + geom_text(aes(label = SID), size = 3, angle = 90)
ggplot(EH1, aes(x = KS.EKS_VALIK, y = KS.TODE)) + geom_text(aes(label = SID), size = 3, angle = 90)


####### PRMPERMRMRRMRMRMR! Epanechnikovi kaaludega saadud olulised tunnused!!!
var_epa; epa_w

dex = data10[,c("SID",var_epa)]
dex[,-1] = t((t(as.matrix(dex[,-1])))*epa_w)
dex$cl = "cl"

H_eks1 = fun_agre_epa(dex, data_puud); H1 = H_eks1 / rowSums(H_eks1)
data_puud_raie_props = data_puud / rowSums(data_puud)

EH1 = data.frame(cbind(sidxx, H1, data_puud_raie_props))
names(EH1) = c("SID",paste(rep(c("MA", "KU", "KS", "HB", "LV", "LM", "KX"),2),
                           rep(c("EKS_VALIK","TODE"), each = 7), sep = "."))

rsdls = (EH1[,2:8] - EH1[,9:15])
w = colSums(EH1[,9:15]) / dim(EH1[,9:15])[1]
cols = colSums(rsdls**2)
RSS_col = cols*w
sum(RSS_col) #6.3 :/

#järgmine samm:
aepa1 = testfun(n = 100, test_var = epa1, samp_var = epa1,
               data = dex, rn = 5, liik = 1, aic_fun = aic_weighted)

var_epa = epa1
epa_w1 = aepa1 / sum(aepa1) * length(aepa1)
epa_w2 = aepa2 / sum(aepa2) * length(aepa2)


#dex0 = dex
dex[,2:length(epa1)] = t((t(as.matrix(dex[,2:length(epa1)])))*epa_w2)

H_eks1 = fun_agre_epa(dex, data_puud); H1 = H_eks1 / rowSums(H_eks1)
data_puud_raie_props = data_puud / rowSums(data_puud)

EH1 = data.frame(cbind(sidxx, H1, data_puud_raie_props))
names(EH1) = c("SID",paste(rep(c("MA", "KU", "KS", "HB", "LV", "LM", "KX"),2),
                           rep(c("EKS_VALIK","TODE"), each = 7), sep = "."))

rsdls = (EH1[,2:8] - EH1[,9:15])
w = colSums(EH1[,9:15]) / dim(EH1[,9:15])[1]
cols = colSums(rsdls**2)
RSS_col = cols*w
sum(RSS_col) #7.85 - kasvab raip! --- >> 8.4 nüüd juba. Ei koondu need asjad kuhugi, väiksemad lihtsalt taanduvad välja.

### Proovime erinevate naabrite arvuga:
nr_neigh = function(k){
H_eks1 = fun_agre_epa(dex0, data_puud, k = k); H1 = H_eks1 / rowSums(H_eks1)
data_puud_raie_props = data_puud / rowSums(data_puud)

EH1 = data.frame(cbind(sidxx, H1, data_puud_raie_props))
names(EH1) = c("SID",paste(rep(c("MA", "KU", "KS", "HB", "LV", "LM", "KX"),2),
                           rep(c("EKS_VALIK","TODE"), each = 7), sep = "."))

rsdls = (EH1[,2:8] - EH1[,9:15])
w = colSums(EH1[,9:15]) / dim(EH1[,9:15])[1]
cols = colSums(rsdls**2)
RSS_col = cols*w
sum(RSS_col)
}

rss = c()
for(k in 1:20){
  rss[k] = nr_neigh(k)
}

plot(rss, type = "o")
rss
which.min(rss) #8

############
H_eks1 = fun_agre_epa(dex0, data_puud, k = 8); H1 = H_eks1 / rowSums(H_eks1)
data_puud_raie_props = data_puud / rowSums(data_puud)

EH1 = data.frame(cbind(sidxx, H1, data_puud_raie_props))
names(EH1) = c("SID",paste(rep(c("MA", "KU", "KS", "HB", "LV", "LM", "KX"),2),
                           rep(c("EKS_VALIK","TODE"), each = 7), sep = "."))

rsdls = (EH1[,2:8] - EH1[,9:15])
w = colSums(EH1[,9:15]) / dim(EH1[,9:15])[1]
cols = colSums(rsdls**2)
RSS_col = cols*w
sum(RSS_col)


plot(EH1$MA.EKS_VALIK, EH1$MA.TODE)
plot(EH1$KU.EKS_VALIK, EH1$KU.TODE)
plot(EH1$KS.EKS_VALIK, EH1$KS.TODE)
plot(EH1$HB.EKS_VALIK, EH1$HB.TODE)


#mis erinevused vrdl eelmise "ekspertvalikuga" olid:
eksp_all3
# [1] "B04_kevad1"                 "B06_kevad2"                 "B07_kevad1"                 "B07_kevad2"                
# [5] "B2_sygis"                   "B3_sygis"                   "K_Elev_variance"            "H_Return_2_count_above_130"
w3_all1
# 0.11936826 0.13037482 0.15897297 0.15710982 0.10692260 0.06264722 0.10454846 0.16005585

var_epa;epa_w
# [1] "B02_kevad1"                 "B03_kevad1"                 "B04_kevad1"                 "B06_sygis"                 
# [5] "B07_kevad1"                 "B11_kevad1"                 "B5_kevad1"                  "B5_sygis"                  
# [9] "H_Return_2_count_above_130" "H_Return_3_count_above_130" "H_Elev_IQ"                 
# [1] 0.6706090 0.3797757 1.4468763 1.1849980 1.2911145 0.6190509 1.3812368 1.5294504 0.5493458 1.1051050 0.8424376
#päris erinevad!

#kas mõni uus erind?
ggplot(EH1, aes(x = MA.EKS_VALIK, y = MA.TODE)) + geom_text(aes(label = SID), size = 3, angle = 90)
ggplot(EH1, aes(x = KU.EKS_VALIK, y = KU.TODE)) + geom_text(aes(label = SID), size = 3, angle = 90)
ggplot(EH1, aes(x = KS.EKS_VALIK, y = KS.TODE)) + geom_text(aes(label = SID), size = 3, angle = 90)

#Mänd: ... 115137, 109177, 109143, 124283
#109177 - kontrollitud
#109143 - vt varasem. Problemaatiline vist ikka.
#124283 - ei saa pildist aru: aerofotode kleepekoht
#115137 - aus kuuskil, kuid tee läheduses

lnk = raied50[raied50$aproovitykk_id == 114421,]$link
browseURL(lnk, browser = getOption("browser"),encodeIfNeeded = FALSE)

#Kuusk:
#34245 - vana tuttav.VÄLJA! Miks varem pold... Ei ole homogeene ju.
#114419 - metsa ja põllu piir, pole homog. Välja!
#124529 - lage ala natuke lähedal, muidu ok.

#kask:
#70230 - no on jama, aga kas piisav alus välja võtta?
#78150 - vt varasem
#114693 - ilus haavik ja kaasik, aga kahjuks pole homogeenne :/
#114421 - ei ole väga homog. Põld kõrval

v2lja4 = c(34245,114419)
sidxx = sidxx[!(sidxx %in% v2lja4)]

dex0 = dex0[dex0$SID %in% sidxx,]
data_puud = taks_uus[taks_uus$SID %in% sidxx, puud]

H_eks1 = fun_agre_epa(dex0, data_puud, k = 8); H1 = H_eks1 / rowSums(H_eks1)
data_puud_raie_props = data_puud / rowSums(data_puud)

EH1 = data.frame(cbind(sidxx, H1, data_puud_raie_props))
names(EH1) = c("SID",paste(rep(c("MA", "KU", "KS", "HB", "LV", "LM", "KX"),2),
                           rep(c("EKS_VALIK","TODE"), each = 7), sep = "."))

rsdls = (EH1[,2:8] - EH1[,9:15])
w = colSums(EH1[,9:15]) / dim(EH1[,9:15])[1]
cols = colSums(rsdls**2)
RSS_col = cols*w
sum(RSS_col)


plot(EH1$MA.EKS_VALIK, EH1$MA.TODE)
plot(EH1$KU.EKS_VALIK, EH1$KU.TODE)
plot(EH1$KS.EKS_VALIK, EH1$KS.TODE)
plot(EH1$HB.EKS_VALIK, EH1$HB.TODE)


dex = data10[,c("SID",blitz)]
dex = dex[dex$SID %in% sidxx,]
dex[,-1] = t((t(as.matrix(dex[,-1])))  )#*bw - ta
dex$cl = "cl"

nr_neigh = function(k){
  H_eks1 = fun_agre_epa(dex, data_puud, k = k); H1 = H_eks1 / rowSums(H_eks1)
  data_puud_raie_props = data_puud / rowSums(data_puud)
  
  EH1 = data.frame(cbind(sidxx, H1, data_puud_raie_props))
  names(EH1) = c("SID",paste(rep(c("MA", "KU", "KS", "HB", "LV", "LM", "KX"),2),
                             rep(c("EKS_VALIK","TODE"), each = 7), sep = "."))
  
  rsdls = (EH1[,2:8] - EH1[,9:15])
  w = colSums(EH1[,9:15]) / dim(EH1[,9:15])[1]
  cols = colSums(rsdls**2)
  RSS_col = cols*w
  sum(RSS_col)
}

rss= c()
for(k in 1:20){
  rss[k] = nr_neigh(k)
}

plot(rss, type = "o")
rss_sqw
which.min(rss_sqw); min(rss_sqw) #6.024464
which.min(rss); min(rss) #5.906907, parem ja selgem graafik ka!


#
H_eks1 = fun_agre_epa(dex, data_puud, k = 6); H1 = H_eks1 / rowSums(H_eks1)
data_puud_raie_props = data_puud / rowSums(data_puud)

EH1 = data.frame(cbind(sidxx, H1, data_puud_raie_props))
names(EH1) = c("SID",paste(rep(c("MA", "KU", "KS", "HB", "LV", "LM", "KX"),2),
                           rep(c("EKS_VALIK","TODE"), each = 7), sep = "."))

rsdls = (EH1[,2:8] - EH1[,9:15])
w = colSums(EH1[,9:15]) / dim(EH1[,9:15])[1]
cols = colSums(rsdls**2)
RSS_col = cols*w
sum(RSS_col)


plot(EH1$MA.EKS_VALIK, EH1$MA.TODE)
plot(EH1$KU.EKS_VALIK, EH1$KU.TODE)
plot(EH1$KS.EKS_VALIK, EH1$KS.TODE)
plot(EH1$HB.EKS_VALIK, EH1$HB.TODE)

getwd()
png(filename="Hinnang_nopitud_tunnused.png")
par(mfrow=c(2,2))
plot(EH1$MA.EKS_VALIK, EH1$MA.TODE)
plot(EH1$KU.EKS_VALIK, EH1$KU.TODE)
plot(EH1$KS.EKS_VALIK, EH1$KS.TODE)
plot(EH1$HB.EKS_VALIK, EH1$HB.TODE)
dev.off()

png(filename="Kaalumata_ja_kaalutud.png")
par(mfrow=c(1,2))
plot(rss, type = "o", xlab = " naabreid")
plot(rss_sqw, type = "o", xlab = "naareid")
dev.off()

############ esimene katsetus mitte-vigase andmestikugam, ehk vahed ka sees! #############

dex = data10[,c("SID","B02_kevad1")]
dex = dex[dex$SID %in% sidxx,]
dex[,-1] = t((t(as.matrix(dex[,-1]))))#*bw - ta
dex$cl = "cl"

nr_neigh = function(k){
  H_eks1 = fun_agre_epa(dex, data_puud, k = k); H1 = H_eks1 / rowSums(H_eks1)
  data_puud_raie_props = data_puud / rowSums(data_puud)
  
  EH1 = data.frame(cbind(sidxx, H1, data_puud_raie_props))
  names(EH1) = c("SID",paste(rep(c("MA", "KU", "KS", "HB", "LV", "LM", "KX"),2),
                             rep(c("EKS_VALIK","TODE"), each = 7), sep = "."))
  
  rsdls = (EH1[,2:8] - EH1[,9:15])
  w = colSums(EH1[,9:15]) / dim(EH1[,9:15])[1]
  cols = colSums(rsdls**2)
  RSS_col = cols*w
  sum(RSS_col)
}

rss= c()
for(k in 1:20){
  rss[k] = nr_neigh(k)
}

plot(rss, type = "o")
which.min(rss); min(rss)
#kaalud ikka ei aita

H_eks1 = fun_agre_epa(dex, data_puud, k = 10); H1 = H_eks1 / rowSums(H_eks1)
data_puud_raie_props = data_puud / rowSums(data_puud)

EH1 = data.frame(cbind(sidxx, H1, data_puud_raie_props))
names(EH1) = c("SID",paste(rep(c("MA", "KU", "KS", "HB", "LV", "LM", "KX"),2),
                           rep(c("EKS_VALIK","TODE"), each = 7), sep = "."))

rsdls = (EH1[,2:8] - EH1[,9:15])
w = colSums(EH1[,9:15]) / dim(EH1[,9:15])[1]
cols = colSums(rsdls**2)
RSS_col = cols*w
sum(RSS_col) #cexp: 5.430137;  4.702094 koos lidariga;


plot(EH1$MA.EKS_VALIK, EH1$MA.TODE)
plot(EH1$KU.EKS_VALIK, EH1$KU.TODE)
plot(EH1$KS.EKS_VALIK, EH1$KS.TODE)
plot(EH1$HB.EKS_VALIK, EH1$HB.TODE)


