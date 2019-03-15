#vt sid-id "knn_KOOS"

sidtotest = sidxx[!(sidxx %in% id_ok)]

koos0 = koos[koos$aproovitykk_id %in% sidtotest,]

raied = koos0[koos0$raie_aeg1 == -1,c("aproovitykk_id", "vanus", "arv_maht_es", "koord_n", "koord_e")]
link = "https://xgis.maaamet.ee/maps/XGis?app_id=MA29&user_id=at&LANG=1&WIDTH=1220&HEIGHT=1263&zlevel=11,688213.00000001,6446066.9999998&setlegend=HMAMULD_YLD=0,HMAHYBR_ALUS01_29=1,HMAHYBR_ALUS02_29=0"
link1 = str_sub(link, 1,3)

raied$link = str_replace(link, str_sub(link, 98, 103),as.character(raied$koord_e))
raied$link = str_replace(raied$link, str_sub(link, 114,120),as.character(raied$koord_n -1))

count = 1
raied$comment = ""

#kas siin peab ikka "-1" olema?
count; raied$raie[count -1] = 0; raied$vanus[count]; raied$arv_maht_es[count]; browseURL(raied$link[count], browser = getOption("browser"),encodeIfNeeded = FALSE);count=count +1
raied$comment[count-1] = "homog!? tormimurd!?"
raied$comment[count-1] = "oja risti läbi"
raied$comment[count-1] = "erinevate metsade piiril"
#0 kui pole toimunud raiet (või on, aga see kajastub ka tüvemah hinanngus),
#1 kui on, 2 kui ei saa aru, mis toimub, 3 pole homogeenne, 4 põld jms;

#save(raied, file = "raied_koos.RData")
