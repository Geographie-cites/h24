require("ggplot2")
setwd("~/Downloads")
df = read.csv("unhealthy.csv")
#df
#colnames(df)

p = ggplot(data = df, aes(x = deltaHealth, y = deltaAverageOpinion))
dim(df)

df = df[which(df$evolution.samples > 9),]
summary(df)
dim(df)
p + geom_point(aes(color = healthyDietReward, cex = maxProbaToSwitch))
p + geom_point(aes(color = interpersonalInfluence, cex = inertiaCoefficient))
#p + geom_point(aes(color = healthyDietReward, cex = maxProbaToSwitch))
#p + geom_point(aes(color = interpersonalInfluence, cex = inertiaCoefficient))


m1 = lm(deltaHealth ~ healthyDietReward + maxProbaToSwitch + interpersonalInfluence + 
          inertiaCoefficient, data = df) 
summary(m1)
m2 = lm(deltaAverageOpinion ~ healthyDietReward + maxProbaToSwitch + interpersonalInfluence + 
          inertiaCoefficient, data = df) 
summary(m2)
m3 = lm(deltaMSEOpinion ~ healthyDietReward + maxProbaToSwitch + interpersonalInfluence + 
          inertiaCoefficient, data = df) 
summary(m3)


df[which(df$deltaHealth < -0.45), ]

