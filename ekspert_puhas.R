#ekspert_puhas
#epanechnikovi kaalud

fun_agre_epa = function(data, data_puud, k)
{
  kk = k+1
  dists = knn.cv(train = data[,2:(dim(data)[2]-1)], cl = data$cl, k = kk)
  dist1 = attr(dists,"nn.dist")
  dist1 = dist1 + 1e-5 #et kaugused ei saaks 0 olla. on see vajalik?
  index1 = attr(dists,"nn.index")
  props = apply(dist1, 1, epa)
  props = t(props)
  indxprops = cbind(index1, props)
  t(apply(indxprops, 1, agre, data_puud))
}

epa = function(vec){
  props = 3/4*(1-(vec / vec[length(vec)])**2)
  props = props/sum(props)
  props
}

agre <- function(arg){
  kk = length(arg) / 2
  indx = arg[1:kk]; props = arg[(kk+1):(2*kk)]
  colsums = colSums(data_puud[indx,]*props)
  colsums
}

#data_puud vaja
#data_puud_raie_props vaja
#sidxx vaja
load(file = "SID_OK.RData")
puud = c("ARV_VMA", "ARV_VKU", "ARV_VKS", "ARV_VHB", "ARV_VLM", "ARV_VLV", "ARV_VKX")


names_taks = names(koos)[c(2,19,25:31)]
taks_uus = koos[sidxx,names_taks]
taks_uus = na.omit(taks_uus)
taks_uus$ARV_VMA = taks_uus$arv_maht_es*taks_uus$MA / 100
taks_uus$ARV_VKU = taks_uus$arv_maht_es*taks_uus$KU / 100
taks_uus$ARV_VKS = taks_uus$arv_maht_es*taks_uus$KS / 100
taks_uus$ARV_VHB = taks_uus$arv_maht_es*taks_uus$HB / 100
taks_uus$ARV_VLM = taks_uus$arv_maht_es*taks_uus$LM / 100
taks_uus$ARV_VLV = taks_uus$arv_maht_es*taks_uus$LV / 100
taks_uus$ARV_VKX = taks_uus$arv_maht_es*taks_uus$KX / 100
data_puud = taks_uus[, puud]
data_puud_raie_props = data_puud / rowSums(data_puud)


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