library(readr)
library(reshape2)
library(tidyr)
library(maggritr)
library(plyr)
library(dplyr)
library(ggplot2)
library(magrittr)

apparie <- read_csv("apparie.csv",col_names = FALSE) 
voisins <- read_csv("voisins.csv",col_names = FALSE) 
opinion <- read_csv("opinion.csv",col_names = FALSE) 

apparie$X5 <-  gsub("\\[|\\]", "", apparie$X5)
voisins$X5 <- gsub("\\[|\\]", "", voisins$X5)
opinion$X5 <- gsub("\\[|\\]", "", opinion$X5)

l <- strsplit(first(apparie$X5)," " )
sizeSeq <- seq(lengths(l))

dfApparie <- with(apparie, cbind(X1,X2,X3,X4, colsplit(apparie$X5, " ", names = sizeSeq) ))

dfVoisins <- with(voisins, cbind(X1,X2,X3,X4, colsplit(voisins$X5, " ", names = sizeSeq) ))

dfOpinion <- with(opinion, cbind(X1,X2,X3,X4, colsplit(opinion$X5, " ", names = sizeSeq) ))

names(dfApparie)[1] <- paste("who")
names(dfApparie)[2] <- paste("age")
names(dfApparie)[3] <- paste("sex")
names(dfApparie)[4] <- paste("edu")

names(dfVoisins)[1] <- paste("who")
names(dfVoisins)[2] <- paste("age")
names(dfVoisins)[3] <- paste("sex")
names(dfVoisins)[4] <- paste("edu")

names(dfOpinion)[1] <- paste("who")
names(dfOpinion)[2] <- paste("age")
names(dfOpinion)[3] <- paste("sex")
names(dfOpinion)[4] <- paste("edu")

gDfApparie <- gather(dfApparie, time, nb, 5:64) 
gDfOpinion <- gather(dfOpinion, time, ValueOpinion, 5:64) 
gDfVoisins <- gather(dfVoisins, time, nbVoisins, 5:64) 

gdfFinal <- join(gDfApparie, gDfOpinion, by=c("who","age","sex","edu","time"), type="inner")
gdfFinal <- join(gdfFinal, gDfVoisins, by=c("who","age","sex","edu","time"), type="inner")
gdfFinal$time <- as.numeric(gdfFinal$time)
  
#gdFinalSubset <- gdfFinal[gdfFinal$time < 20.0,]

gdFinalByAge <- gdfFinal %>% group_by(age,time) %>%
  summarise_each(funs(mean), ValueOpinion)

gdFinalBySex <- gdfFinal %>% group_by(sex,time) %>%
  summarise_each(funs(mean), ValueOpinion)

gdFinalByEdu <- gdfFinal %>% group_by(edu,time) %>%
  summarise_each(funs(mean), ValueOpinion)

p <- ggplot(data=gdFinalByEdu, aes(x=time, y=ValueOpinion, group=edu, fill=edu, colour=as.character(edu)))
p <- p + geom_line(size=1) #show.legend = FALSE
p


