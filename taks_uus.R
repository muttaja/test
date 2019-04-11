names_taks = names(koos)[c(2,19,25:31)]
taks_uus = koos[,names_taks]

taks_uus$ARV_VMA = taks_uus$arv_maht_es*taks_uus$MA / 100
taks_uus$ARV_VKU = taks_uus$arv_maht_es*taks_uus$KU / 100
taks_uus$ARV_VKS = taks_uus$arv_maht_es*taks_uus$KS / 100
taks_uus$ARV_VHB = taks_uus$arv_maht_es*taks_uus$HB / 100
taks_uus$ARV_VLM = taks_uus$arv_maht_es*taks_uus$LM / 100
taks_uus$ARV_VLV = taks_uus$arv_maht_es*taks_uus$LV / 100
taks_uus$ARV_VKX = taks_uus$arv_maht_es*taks_uus$KX / 100

taks_uus$ARV_VXX = taks_uus$ARV_VHB + taks_uus$ARV_VLM+ taks_uus$ARV_VLV+ taks_uus$ARV_VKX
taks_uus$XX = taks_uus$HB + taks_uus$LM+ taks_uus$LV+ taks_uus$KX

taks.info = taks_uus[,c("aproovitykk_id","arv_maht_es","ARV_VMA","ARV_VKU","ARV_VKS","ARV_VXX","MA","KU","KS","XX")]
taks.info[,7:10] = taks.info[,7:10]  / 100
save(taks.info, file = "taks_info.RData")
