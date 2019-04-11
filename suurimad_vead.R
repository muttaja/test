#suurimad vead

dp0 = kp.result.prop
dp =  kp.result.prop #proportsioonide põhjal hetkel
#krt siin id-d järelikult valed!





#MIS SIIN VALESTI ON ID-DEGA!?!?!?






data_p = taks.info[taks.info$aproovitykk_id %in% sidxx,] #sellega langeb kokku!
dp = merge(dp, data_puud, by = "aproovitykk_id")
require(plyr)
dp = join(dp, data_puud, by = "aproovitykk_id")

dp = merge(dp, taks.info, by = "aproovitykk_id")


dp = merge(kb1, taks.info, by = "aproovitykk_id", all.x = T)

dev.off()
par(mfrow = c(2,2))
plot(dp[,11],dp[,2])
plot(dp[,12],dp[,3])
plot(dp[,13],dp[,4])
plot(dp[,14],dp[,5])

mand.p0 = dp0[,1]
mand.p = dp[,2]
mand.t = dp[,11]
mand.t1 = data_p[,7]

plot(mand.t, mand.t1) #need on samad
plot(mand.p, mand.p0) #erinev järjekord; esimesed 6 klapivad!!!

id0 = dp0[,5]
id1 = dp[,1]
id2 = data_p$aproovitykk_id
#lihtsalt vale ID on knn-i väljundil!



dev.off()
par(mfrow = c(2,2))
plot(data_p[,7],dp[,1])
plot(data_p[,8],dp[,2])
plot(data_p[,9],dp[,3])
plot(data_p[,10],dp[,4])


data_p[1:2,]
# aproovitykk_id arv_maht_es ARV_VMA  ARV_VKU   ARV_VKS  ARV_VXX MA   KU   KS   XX
# 4          34094     85.2529       0 2.557587  41.77392 40.92139  0 0.03 0.49 0.48
# 8          34102    178.2790       0 5.348370 171.14784  1.78279  0 0.03 0.96 0.01

dp[1:2,]

#enne merge:
# MA         KU        KS         XX aproovitykk_id
# 1 0.05234516 0.07603549 0.4652403 0.40637904          34094
# 2 0.18524092 0.15901826 0.6228166 0.03292427          34102

# aproovitykk_id       MA.x       KU.x      KS.x       XX.x 
# 1          34094 0.05234516 0.07603549 0.4652403 0.40637904      0 0.03 0.49 0.48
# 2          34102 0.18524092 0.15901826 0.6228166 0.03292427     0 0.03 0.96 0.01

taks.info[taks.info$aproovitykk_id %in% c(34094,34102),]

#aproovitykk_id arv_maht_es ARV_VMA  ARV_VKU   ARV_VKS  ARV_VXX MA   KU   KS   XX
#4          34094     85.2529       0 2.557587  41.77392 40.92139  0 0.03 0.49 0.48
#8          34102    178.2790       0 5.348370 171.14784  1.78279  0 0.03 0.96 0.01
#kokku panemisel ikka mingi probleem!???




sqrt(mean((puud_true[,1]-dp[,1])**2))
sqrt(mean((puud_true[,2]-dp[,2])**2))
sqrt(mean((puud_true[,3]-dp[,3])**2))
sqrt(mean((puud_true[,4]-dp[,4])**2))
sqrt((sum((puud_true[,1:4]-dp[,1:4])**2))/dim(dp)[1]/4) #0.1808612


vead = sqrt((rowSums((puud_true[,1:4]-dp[,1:4])**2))/4); 
dp$viga = vead
hist(dp$viga)

vead.data = cbind(puud_true[,1:4],dp)

vead.data$v2 = sqrt((rowSums((vead.data[,1:4]-vead.data[,5:8])**2))/4)
mean(vead.data$v2) #aga see pole võrreldav siiski

