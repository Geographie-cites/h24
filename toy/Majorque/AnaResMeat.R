setwd("~/Documents/Eighties-MeatGame")
library(ggplot2)
resMeat = read.csv("result.csv", sep=",", dec=".")
resMeat$index = paste0("p", resMeat$probMeat, 
                       "k", resMeat$k, "n", resMeat$nPeople)
head(resMeat)
dim(resMeat)
summary(resMeat)
p = ggplot(aes(x = k, y = meatersA1, group = probMeat, color = probMeat), data= resMeat)
p + geom_point()

p = ggplot(aes(x = k, y = meatersA2, group = index, color = index), data= resMeat)
p + geom_boxplot()

p = ggplot(aes(x = k, y = meatersImmo, group = probMeat, color = probMeat), data= resMeat)
p + geom_point()




plot(resMeat)

plot(meatersA2 ~ k, data=resMeat, pch=19, col = probMeat)
