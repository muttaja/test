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
  data.frame(round(t(apply(indxprops, 1, agre)),0))
}


SID_temp = SID_temp #need 235
intsct = intersect(mets_raie$SID,koos$SID); intsct = intersect(intsct, SID_temp); intsct = intersect(intsct, taks_uus$SID)

data_puud = taks_uus[taks_uus$SID %in% intsct, puud]
SID_raie1 = taks_uus[taks_uus$SID %in% intsct,"SID"]
mets_raie = data_ex
mets_raie$cl = "cl"
mets_raie1 = mets_raie[mets_raie$SID %in% intsct,]
HINNANG_RAIE = fun_agre_ex(mets_raie1, data_puud)


#data_puud_raie = koos[koos$SID %in% intsct,c("MA", "KS", "KU", "HB", "LM", "LV", "KX")]

EKSPERTHINNANG_RAIE_JA_TODE = data.frame(cbind(intsct, HINNANG_RAIE, round(data_puud,0)))
par(mfrow=c(1,1))
plot(EKSPERTHINNANG_RAIE_JA_TODE$ARV_VMA, HINNANG_RAIE_JA_TODE$ARV_VMA.1)
plot(EKSPERTHINNANG_RAIE_JA_TODE$ARV_VKU, HINNANG_RAIE_JA_TODE$ARV_VKU.1)
plot(EKSPERTHINNANG_RAIE_JA_TODE$ARV_VKS, HINNANG_RAIE_JA_TODE$ARV_VKS.1)
plot(EKSPERTHINNANG_RAIE_JA_TODE$ARV_VHB, HINNANG_RAIE_JA_TODE$ARV_VHB.1)

EKSPERTHINNANG_RAIE_JA_TODE1 = HINNANG_RAIE / rowSums(HINNANG_RAIE)






