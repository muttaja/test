#tõusunurk

dp = kp.result.epa

m1 = lm(dp[,1] ~puud_true[,1])
dp[,1] = dp[,1] / m1$coefficients[2]

m2 = lm(dp[,2] ~puud_true[,2])
dp[,2] = dp[,2] / m2$coefficients[2]

m3 = lm(dp[,3] ~puud_true[,3])
dp[,3] = dp[,3] / m3$coefficients[2]

m4 = lm(dp[,4] ~puud_true[,4])
dp[,4] = dp[,4] / m4$coefficients[2]

dp[,1:4] = dp[,1:4] / rowSums(dp[,1:4])

sqrt(mean((puud_true[,1]-dp[,1])**2))
sqrt(mean((puud_true[,2]-dp[,2])**2))
sqrt(mean((puud_true[,3]-dp[,3])**2))
sqrt(mean((puud_true[,4]-dp[,4])**2))

dev.off()
par(mfrow = c(2,2))
plot(puud_true[,1],dp[,1])
plot(puud_true[,2],dp[,2])
plot(puud_true[,3],dp[,3])
plot(puud_true[,4],dp[,4])
setwd("C:/Users/Mats/Documents/Kool/MAKATÖÖ/TEST/test")
load("knn_pbp_lbl_landsat_sentinel.RData",verbose = T)
load("taks_info.RData")
df = knn.pbp.lbl.ls; df = merge(df,taks.info,by="aproovitykk_id",all.x = T)
head(df)

df = df[,c(1:5)]
df = df %>% group_by(aproovitykk_id) %>% summarise_all(funs(mean))
df = merge(df,taks.info,by="aproovitykk_id",all.x = T)
#lm(y - a*x ~ 1)

m1 = lm(MA.x ~ MA, data = df)
p1 = (df$MA.x - m1$coefficients[1])/m1$coefficients[2];p1=p1-min(p1);p1=p1/max(p1)
m2 = lm(KU.x ~ KU, data = df)
p2 = (df$KU.x - m2$coefficients[1])/m2$coefficients[2];p2=p2-min(p2);p2=p2/max(p2)
m3 = lm(KS.x ~  KS, data = df)
p3 = (df$KS.x - m3$coefficients[1])/m3$coefficients[2];p3=p3-min(p3);p3=p3/max(p3)
m4 = lm(MUU ~ XX ,data = df)
p4 = (df$MUU - m4$coefficients[1])/m4$coefficients[2];p4=p4-min(p4);p4=p4/max(p4)

df[,2] = p1;df[,3] = p2;df[,4] = p3;df[,5] = p4
df[,2:5] = df[,2:5] / rowSums(df[,2:5])

dp = df
dev.off()
par(mfrow = c(2,2))
plot(dp[,11],dp[,2], xlab = "Mänd", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369), pch = 16)
abline(lm(dp[,2] ~ dp[,11]))
plot(dp[,12],dp[,3], xlab = "Kuusk", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369), pch = 16)
abline(lm(dp[,3] ~ dp[,12]))
plot(dp[,13],dp[,4], xlab = "Kask", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369), pch = 16)
abline(lm(dp[,4] ~ dp[,13]))
plot(dp[,14],dp[,5], xlab = "Muu", ylab = "RMSE", xlim = c(0,1), ylim = c(0,1), col = rgb(red = 0, green = 0, blue = 0, alpha = 0.369), pch = 16)
abline(lm(dp[,5] ~ dp[,14]))





