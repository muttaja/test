#raied
#prbleem pole mitte nendega, kus raie oleks märgitud, vaid nendega, kus ei ole!
#need tuleb kõik käsitsi üle kontrollida

raied = koos[koos$raie_aeg1 == -1 & koos$maakatgrp == "ME",c("SID", "vanus", "arv_maht_es", "koord_n", "koord_e")]
#vanus -1?
hist(raied[raied$vanus == -1 & raied$arv_maht_es > 0, "arv_maht_es"], breaks = 20)
length(raied$SID) #927 - nende kohta lidari info?
length(mets$SID) #966 - nii paljude kohta mullainfo?
length(intersect(raied$SID, mets$SID)) #... aga vaid 354 kohta mõlemad

raied1 = raied[raied$SID %in% intersect(raied$SID, mets$SID),]
link = "https://xgis.maaamet.ee/maps/XGis?app_id=MA29&user_id=at&LANG=1&WIDTH=1220&HEIGHT=1263&zlevel=12,688213.00000001,6446066.9999998&setlegend=HMAMULD_YLD=0,HMAHYBR_ALUS01_29=1,HMAHYBR_ALUS02_29=0"
link1 = str_sub(link, 1,3)


str_replace(link, str_sub(link, 98,103),koord_e)
str_replace(link, str_sub(link, 114,120),koord_n-1)

raied1$link = str_replace(link, str_sub(link, 98, 103),as.character(raied1$koord_e))
raied1$link = str_replace(raied1$link, str_sub(link, 114,120),as.character(raied1$koord_n -1))

count = 1
raied1$comment = ""


raied1$comment[count] = "midagi raiutud?"
raied1$raie[count] = 0;count;count=count +1;browseURL(raied1$link[count], browser = getOption("browser"),encodeIfNeeded = FALSE)
#0 kui pole toimunud, 1 kui on, 2 kui ei saa aru, mis toimub, 3 pole homogeenne, 4 põld jms;

setwd("A:/MAKA/Raie_pildid")
load("raied_k6ik.RData")

#kus leidsin raie, aga tüvemaht üle 50. vaatan uuesti üle:
raied50 = raied1[raied1$arv_maht_es > 50 & raied1$raie == 1,]

count = 0
#raied1$raie[count] = 0;count;
count=count +1;browseURL(raied50$link[count], browser = getOption("browser"),encodeIfNeeded = FALSE)

#need, kus vanus < 10, on raiet arvestatud
kontrollitud_SID = raied1[raied1$raie == 0 | (raied1$raie == 0 & raied1$vanus < 10),"SID"]
length(kontrollitud_SID)


