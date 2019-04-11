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