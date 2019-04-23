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




sqrt(mean((dp[,2]-dp[,11])**2))
sqrt(mean((dp[,3]-dp[,12])**2))
sqrt(mean((dp[,4]-dp[,13])**2))
sqrt(mean((dp[,5]-dp[,14])**2))
sqrt((sum((dp[,2:5]-dp[,11:14])**2))/dim(dp)[1]/4) #0.1808612


vead = sqrt((rowSums((dp[,11:14]-dp[,2:5])**2))/4); 
dp$viga = vead
hist(dp$viga)

vead.data = merge(dp, taks.info, by = "aproovitykk_id", all.x = T)

#vead.data$v2 = sqrt((rowSums((vead.data[,1:4]-vead.data[,5:8])**2))/4)
#mean(vead.data$v2) #aga see pole võrreldav siiski

