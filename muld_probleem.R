#MULD

setwd("A:/MAKA/kagu.andmenaidis")
sat = read.csv("Kagu-Eesti_SMI_prt_pix.csv.sort")

muld = data.frame(muld = sat$muld)
muld$SID = sat$SID
muld$cat = sat$cat

head(muld)

asd1 = muld
asd = asd1 %>% group_by(SID) %>% summarize (muld1 = names(which.max(table(muld))))

muld = merge(muld, asd, by = "SID", all.x = T)
muld1 = muld %>% group_by(SID) %>% sample_n(1)
muld1 = subset(muld1, select = c("SID", "muld1"))


#muld 1-0 tüüpi andmestikuks
muld2 = dcast(muld1,SID~muld1,fun.aggregate = function(x){as.integer(length(x) > 0)})


