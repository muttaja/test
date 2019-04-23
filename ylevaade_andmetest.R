#ülevaade andmetest


data = read.csv(file = "d506_100.csv")
vars = names(data)[c(3:38,47:60)]
sidxx = data$aproovitykk_id
tvmaht = 2 #ehk siis proportsioonid!
data_puud = taks.info[taks.info$aproovitykk_id %in% sidxx,]
puud_true =  data_puud[,7:10]
data_puud = data_puud[,(4*tvmaht-1):(4*tvmaht+2)]

par(mfrow = c(2,2))
hist(data_puud[,1], xlab = "Männid", main = "")
hist(data_puud[,2], xlab = "Kuused", main = "")
hist(data_puud[,3], xlab = "Kased", main = "")
hist(data_puud[,4], xlab = "Muu", main = "")