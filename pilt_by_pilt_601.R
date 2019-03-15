#cart prognoosid vaid ühe pildi põhjal, seejärel keskmine
#sat_sep_koos failist andmestike_sisselugemine_koos

koos601 = sat_sep_koos[sat_sep_koos$aproovitykk_id %in% sidxx,] #test
koos601 = koos601[koos601$band == "B02",] #kui on üks kanal, siis on kõik

compcases = rowSums(table(koos601$aproovitykk_id, koos601$kp)) - rowSums(table(na.omit(koos601)$aproovitykk_id, na.omit(koos601)$kp)) 
table(compcases)
compcases[compcases == 7]
#15 erinevat pilti
#47 juhul vaid 1 pilt, neeb võibolla välja võtta?

#aga piltide kaupa?
comppilt = rowSums(table(na.omit(koos601)$kp, na.omit(koos601)$aproovitykk_id))
table(comppilt)
comppilt[comppilt == 601]
#2017-05-02 2017-09-24 2018-05-10 2018-09-19  neil kuupäevadel KÕIK pildid olemas!

#kui 2 kehvemat pilti välja võtta, siis palju vaatlusi jääb?
k1 = koos601[!(koos601$kp %in% comppilt[comppilt %in% c(100,182)]),]

ck1 = rowSums(table(k1$aproovitykk_id, k1$kp)) - rowSums(table(na.omit(k1)$aproovitykk_id, na.omit(k1)$kp)) 
table(ck1)

#227 + 135 + 30 + 14 +1
#407 juhul vähemalt 4 pilti!