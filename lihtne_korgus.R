#metsa kırgus


koos_mets = koos[(koos$maakatsgrp == "M" & koos$maakatgrp == "ME"),] #miks NA-d sisse j‰‰vad kui panna aint 1. tingimus?
koos_mets$aasta_erinevus = 2018 - koos_mets$aasta

load(file = "korgus_valja.RData")
k1 = koos_mets[!(koos_mets$aproovitykk_id %in% korgus_valja),]
km1 = lm(inv_korgus ~ K_Elev_P90 + H_Elev_P90 + aasta_erinevus, data = k1)
summary(km1) #aasta_erinevus negatiivse kordajaga, sest praegu n‰itaks lidar muidu liiga kırget metsa. kordaja 3.3, ehk 33cm aastas juurdekasv? palju natuke!
pk1 = predict(km1, data = k1)
res1 = pk1 - k1$inv_korgus
resdf1 = data.frame(SID = k1$aproovitykk_id, res = res1)
plot(k1$inv_korgus, pk1)

rs1 = resdf1[resdf1$res < -100,]

link = "https://xgis.maaamet.ee/maps/XGis?app_id=MA29&user_id=at&LANG=1&WIDTH=1220&HEIGHT=1263&zlevel=12,688213.00000001,6446066.9999998&setlegend=HMAMULD_YLD=0,HMAHYBR_ALUS01_29=1,HMAHYBR_ALUS02_29=0"
koos_mets$link = str_replace(link, str_sub(link, 98, 103),as.character(koos_mets$koord_e))
koos_mets$link = str_replace(koos_mets$link, str_sub(link, 114,120),as.character(koos_mets$koord_n -1))

lnk = koos_mets[koos_mets$aproovitykk_id == rs1[2,1],]$link; browseURL(lnk, browser = getOption("browser"),encodeIfNeeded = FALSE)


#kırguse prognoos ~0?
hist(koos_mets[koos_mets$arv_maht_es < 10,]$inv_korgus) #siit mets kırgugusega ¸le 150 v‰lja?
valja = koos_mets[koos_mets$arv_maht_es < 10 & koos_mets$inv_korgus > 150 ,] 


#step
frm = as.formula(paste("inv_korgus", paste(c(lidar_intless, "aasta_erinevus"), collapse = "+"), sep = " ~ "))
m1 = lm(frm, k1)
m_step = step(m1)
summary(m_step)
pstep = predict(m_step, data = k1)
plot(k1$inv_korgus, pstep)
resstep = pstep - k1$inv_korgus
stepres = data.frame(SID = k1$aproovitykk_id, res = resstep)
st1 = stepres[stepres$res < - 100,]
lnk = koos_mets[koos_mets$aproovitykk_id == st1[5,1],]$link; browseURL(lnk, browser = getOption("browser"),encodeIfNeeded = FALSE)
#1 - midagi raiutud, 2 - piiril, 3 - midagi raiutud, 4. kena


#kuidas minu v‰ljavalituid prognoosib?
load(file = "SID_OK.RData")
ksx = koos[koos$aproovitykk_id %in% sidxx,]
ksx$aasta_erinevus = 2018 - ksx$aasta
pk = predict(km1, newdata = ksx) #wtf, kuidas siin 905?
res = pk - ksx$inv_korgus
dres = data.frame(SID = ksx$aproovitykk_id, res = res)
plot(ksx$inv_korgus, pk)
ch1 = dres[dres$res < - 50,]
lnk = koos_mets[koos_mets$aproovitykk_id == ch1[6,1],]$link; browseURL(lnk, browser = getOption("browser"),encodeIfNeeded = FALSE)

#vaatame, millised vaatlused ma olen v‰lja korjanud:

#save(sidxx, file = "SID_OK.RData")
#load(file = "SID_OK.RData")
kms18 = koos_mets[koos_mets$aproovitykk_id %in% sat18_mets$aproovitykk_id,]
kms18$kasutan = "ei"
kms18[kms18$aproovitykk_id %in% sidxx,]$kasutan = "jah"


kms18$aasta_erinevus = 2018 - kms18$aasta
pk = predict(km1, newdata = kms18) #wtf, kuidas siin 905?
#res = pk - kms18$inv_korgus
df18 = data.frame(SID = kms18$aproovitykk_id, pred = pk)
df18$true = kms18$inv_korgus
df18$kasutan = kms18$kasutan

require(plotly)
p = ggplot(df18, aes(x=true, y=pred, color=kasutan)) + geom_point(aes(text = SID))# + geom_text(aes(label = SID), size = 2.5, vjust = 1.2)
ggplotly(p, tooltip = c("color","text"))
#miks mında kena vaatlust ei kasuta?
lnk = koos_mets[koos_mets$aproovitykk_id == 124499,]$link; browseURL(lnk, browser = getOption("browser"),encodeIfNeeded = FALSE)
#82493 raiesmiku piiril
#79876 ei ole homog

#t¸vemaht vs viga?
plot(kms18$arv_maht_es,(df18$pred - df18$true))


#kırguse prognoosimudelist v‰lja j‰etud
#korgus_valja = c(dres1$SID, dres2$SID[-12],dres3$SID[-7],dres4$SID,dres5$SID[1:2],valja$aproovitykk_id)
#save(korgus_valja, file = "korgus_valja.RData")

#log teisendus
load(file = "korgus_valja.RData")
k1 = koos_mets[!(koos_mets$aproovitykk_id %in% korgus_valja),]
k1$log_korgus = log(k1$inv_korgus)
k1$poord_korgus = 1 / k1$inv_korgus
km_log = lm(log_korgus ~ K_Elev_P90 + H_Elev_P90 + aasta_erinevus, data = k1)
summary(km_log) #aasta_erinevus negatiivse kordajaga, sest praegu n‰itaks lidar muidu liiga kırget metsa. kordaja 3.3, ehk 33cm aastas juurdekasv? palju natuke!
pk1 = exp(predict(km_log, data = k1))
res1 = pk1 - k1$inv_korgus
resdf1 = data.frame(SID = k1$aproovitykk_id, res = res1)
plot(exp(k1$log_korgus), pk1)

#step
#log ei tˆˆta, pˆˆrd?

indxNA <- apply(koos[,233:416], 2, function(x) any(is.na(x)))
names(koos[,233:416])[indxNA]
vars_Int = names(koos[,233:416])[-grep("Int",names(koos[,233:416]))]
#lidar_intless = vars_Int[!(vars_Int %in% names(adata)[indxNA])]


frm_poord = as.formula(paste("poord_korgus", paste(c(vars_Int, "aasta_erinevus"), collapse = "+"), sep = " ~ "))
m1_log = lm(frm_poord, k1)
m_step_log = step(m1_log)
summary(m_step_log)
pstep_log = exp(predict(m_step_log, data = k1))
plot(k1$inv_korgus, pstep_log)
