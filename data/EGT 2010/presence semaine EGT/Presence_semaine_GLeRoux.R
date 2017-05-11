library("plyr")
library("dplyr")
library("lubridate")
library("foreach")
#library("iterators")
memory.limit(size = 20000)
load("Donnees R/deplacements_semaine.RData")
load("Donnees R/personnes_semaine.RData")

##########################################################################
# Transformer table déplacements en table présence

###############

#identifiant unique
deplacements_semaine$ID_pers <- ifelse(nchar(deplacements_semaine$np)==1,paste(deplacements_semaine$nquest,deplacements_semaine$np,sep="0"),paste(deplacements_semaine$nquest,deplacements_semaine$np,sep=""))
personnes_semaine$ID_pers <- ifelse(nchar(personnes_semaine$np)==1,paste(personnes_semaine$nquest,personnes_semaine$np,sep="0"),paste(personnes_semaine$nquest,personnes_semaine$np,sep=""))

# récupère des infos sur la table initiale et correction sur l'ordre des déplacements
deplacements_semaine$orh[deplacements_semaine$ID_pers=="9210106101" & deplacements_semaine$nd=="3"] <- 12
deplacements_semaine$destm[deplacements_semaine$ID_pers=="7703180101" & deplacements_semaine$nd=="4"] <- 35
deplacements_semaine$orm[deplacements_semaine$ID_pers=="7716204102" & deplacements_semaine$nd=="5"] <- 55
deplacements_semaine$orm[deplacements_semaine$ID_pers=="9408275101" & deplacements_semaine$nd=="2"] <- 20
deplacements_semaine$destm[deplacements_semaine$ID_pers=="9408275101" & deplacements_semaine$nd=="2"] <- 40
deplacements_semaine$orm[deplacements_semaine$ID_pers=="9408275101" & deplacements_semaine$nd=="3"] <- 40
deplacements_semaine$destm[deplacements_semaine$ID_pers=="9408275102" & deplacements_semaine$nd=="1"] <- 22
deplacements_semaine$destm[deplacements_semaine$ID_pers=="9414903103" & deplacements_semaine$nd=="2"] <- 30
deplacements_semaine$destm[deplacements_semaine$ID_pers=="7501035102" & deplacements_semaine$nd=="5"] <- 50
deplacements_semaine$desth[deplacements_semaine$ID_pers=="7501035102" & deplacements_semaine$nd=="5"] <- 20
deplacements_semaine$destm[deplacements_semaine$ID_pers=="9101018101" & deplacements_semaine$nd=="12"] <- 38
deplacements_semaine$destm[deplacements_semaine$ID_pers=="9103006102" & deplacements_semaine$nd=="1"] <- 10
deplacements_semaine$destm[deplacements_semaine$ID_pers=="9103006102" & deplacements_semaine$nd=="2"] <- 15
deplacements_semaine$orm[deplacements_semaine$ID_pers=="9111108102" & deplacements_semaine$nd=="3"] <- 25
deplacements_semaine$orm[deplacements_semaine$ID_pers=="9204257101" & deplacements_semaine$nd=="3"] <- 56
deplacements_semaine$orh[deplacements_semaine$ID_pers=="9204257101" & deplacements_semaine$nd=="3"] <- 17
deplacements_semaine$destm[deplacements_semaine$ID_pers=="9204257101" & deplacements_semaine$nd=="3"] <- 0
deplacements_semaine$destm[deplacements_semaine$ID_pers=="9308041102" & deplacements_semaine$nd=="1"] <- 10
deplacements_semaine$destm[deplacements_semaine$ID_pers=="9511901101" & deplacements_semaine$nd=="3"] <- 35
deplacements_semaine$destm[deplacements_semaine$ID_pers=="9511901101" & deplacements_semaine$nd=="6"] <- 40
deplacements_semaine$destm[deplacements_semaine$ID_pers=="7806453101" & deplacements_semaine$nd=="1"] <- 35
deplacements_semaine$destm[deplacements_semaine$ID_pers=="7502188101" & deplacements_semaine$nd=="1"] <- 52
deplacements_semaine$destm[deplacements_semaine$ID_pers=="9414193101" & deplacements_semaine$nd=="1"] <- 19
deplacements_semaine$destm[deplacements_semaine$ID_pers=="7502335102" & deplacements_semaine$nd=="5"] <- 35
deplacements_semaine$destm[deplacements_semaine$ID_pers=="7812166102" & deplacements_semaine$nd=="2"] <- 35
deplacements_semaine$destm[deplacements_semaine$ID_pers=="9511045101" & deplacements_semaine$nd=="4"] <- 45
deplacements_semaine$destm[deplacements_semaine$ID_pers=="7507105101" & deplacements_semaine$nd=="1"] <- 26
deplacements_semaine$destm[deplacements_semaine$ID_pers=="7507105103" & deplacements_semaine$nd=="1"] <- 26
deplacements_semaine$destm[deplacements_semaine$ID_pers=="7509141101" & deplacements_semaine$nd=="4"] <- 10
deplacements_semaine$orm[deplacements_semaine$ID_pers=="7703128101" & deplacements_semaine$nd=="3"] <- 10
deplacements_semaine$orm[deplacements_semaine$ID_pers=="7703428101" & deplacements_semaine$nd=="2"] <- 45
deplacements_semaine$destm[deplacements_semaine$ID_pers=="7705103102" & deplacements_semaine$nd=="5"] <- 35
deplacements_semaine$destm[deplacements_semaine$ID_pers=="7705103104" & deplacements_semaine$nd=="5"] <- 35
deplacements_semaine$destm[deplacements_semaine$ID_pers=="7806413101" & deplacements_semaine$nd=="6"] <- 45
deplacements_semaine$destm[deplacements_semaine$ID_pers=="7808040101" & deplacements_semaine$nd=="1"] <- 15
deplacements_semaine$destm[deplacements_semaine$ID_pers=="9106017102" & deplacements_semaine$nd=="4"] <- 30
deplacements_semaine$destm[deplacements_semaine$ID_pers=="9107056101" & deplacements_semaine$nd=="1"] <- 27
deplacements_semaine$destm[deplacements_semaine$ID_pers=="9111093101" & deplacements_semaine$nd=="3"] <- 32
deplacements_semaine$destm[deplacements_semaine$ID_pers=="9112094102" & deplacements_semaine$nd=="5"] <- 40
deplacements_semaine$destm[deplacements_semaine$ID_pers=="9112094103" & deplacements_semaine$nd=="6"] <- 40
deplacements_semaine$destm[deplacements_semaine$ID_pers=="9112094104" & deplacements_semaine$nd=="3"] <- 40
deplacements_semaine$orm[deplacements_semaine$ID_pers=="9204164101" & deplacements_semaine$nd=="2"] <- 35
deplacements_semaine$destm[deplacements_semaine$ID_pers=="9309257102" & deplacements_semaine$nd=="1"] <- 30
deplacements_semaine$destm[deplacements_semaine$ID_pers=="9309257102" & deplacements_semaine$nd=="5"] <- 25
deplacements_semaine$destm[deplacements_semaine$ID_pers=="9309257102" & deplacements_semaine$nd=="6"] <- 35
deplacements_semaine$destm[deplacements_semaine$ID_pers=="9307901105" & deplacements_semaine$nd=="2"] <- 15
deplacements_semaine$destm[deplacements_semaine$ID_pers=="9410094101" & deplacements_semaine$nd=="3"] <- 40
deplacements_semaine$destm[deplacements_semaine$ID_pers=="9411234101" & deplacements_semaine$nd=="1"] <- 45
deplacements_semaine$destm[deplacements_semaine$ID_pers=="9411234101" & deplacements_semaine$nd=="10"] <- 35
deplacements_semaine$destm[deplacements_semaine$ID_pers=="9502279101" & deplacements_semaine$nd=="4"] <- 5
deplacements_semaine$destm[deplacements_semaine$ID_pers=="9511901101" & deplacements_semaine$nd=="3"] <- 35
deplacements_semaine$destm[deplacements_semaine$ID_pers=="9410215102" & deplacements_semaine$nd=="5"] <- 11
deplacements_semaine$destm[deplacements_semaine$ID_pers=="7514446101" & deplacements_semaine$nd=="1"] <- 34
deplacements_semaine$destm[deplacements_semaine$ID_pers=="7706282101" & deplacements_semaine$nd=="1"] <- 40
deplacements_semaine$destm[deplacements_semaine$ID_pers=="9403813101" & deplacements_semaine$nd=="2"] <- 34
deplacements_semaine$destm[deplacements_semaine$ID_pers=="9406114102" & deplacements_semaine$nd=="2"] <- 8
deplacements_semaine$destm[deplacements_semaine$ID_pers=="9209304101" & deplacements_semaine$nd=="2"] <- 35
deplacements_semaine$destm[deplacements_semaine$ID_pers=="7510518101" & deplacements_semaine$nd=="3"] <- 50
deplacements_semaine$destm[deplacements_semaine$ID_pers=="7703394101" & deplacements_semaine$nd=="1"] <- 30



deplacements_semaine <- filter(deplacements_semaine, ID_pers!="9302190101")
deplacements_semaine <- arrange(deplacements_semaine, ID_pers, orh,orm)
#suppression des observations hors fenetre 4h-4H
deplacements_semaine <- filter(deplacements_semaine, orh>=4 | desth>=4)
deplacements_semaine <- filter(deplacements_semaine, orh<=28 | desth<=28)
deplacements_semaine$destm[deplacements_semaine$desth>=28] <- 0
deplacements_semaine$desth[deplacements_semaine$desth>=28] <- 28
deplacements_semaine$destm[deplacements_semaine$orh<4] <- 0
deplacements_semaine$desth[deplacements_semaine$orh<4] <- 4

# on recalcule les durées
deplacements_semaine$heure_fin <- ifelse(deplacements_semaine$desth>23, as.character.Date(ISOdatetime(2010,1,2,deplacements_semaine$desth-24,deplacements_semaine$destm,0)),
                                         as.character.Date(ISOdatetime(2010,1,1,deplacements_semaine$desth,deplacements_semaine$destm,0)) )
deplacements_semaine$heure_deb <- ifelse(deplacements_semaine$orh>23, as.character.Date(ISOdatetime(2010,1,2,deplacements_semaine$orh-24,deplacements_semaine$orm,0)),
                                         as.character.Date(ISOdatetime(2010,1,1,deplacements_semaine$orh,deplacements_semaine$orm,0)) )
deplacements_semaine$duree <- as.numeric(difftime(ymd_hms(deplacements_semaine$heure_fin, truncated=3),ymd_hms(deplacements_semaine$heure_deb, truncated=3), units= "mins"))


#suppression des individus avec des heures manquantes
IDhNA <- data.frame(ID_pers = character(length(unique(deplacements_semaine$ID_pers[is.na(deplacements_semaine$orh)==FALSE & is.na(deplacements_semaine$desth)==FALSE]))))
IDhNA$ID_pers <- unique(deplacements_semaine$ID_pers[is.na(deplacements_semaine$orh)==FALSE & is.na(deplacements_semaine$desth)==FALSE])
deplacements_semaine <-  merge(x = deplacements_semaine, y=IDhNA, by = "ID_pers", all = FALSE)
deplacements_semaine <- arrange(deplacements_semaine, ID_pers, orh,orm)

# nbre d observation                         
deplacements_sem_GRP<- group_by(.data = deplacements_semaine, ID_pers)
nobs <- as.data.frame(summarize(deplacements_sem_GRP, nobs = n()))
deplacements_sem_GRP <- merge(x = deplacements_sem_GRP, y = nobs, by = "ID_pers", all =  TRUE)

###############
# cree la table de presence
presence_semaine <- data.frame(nquest = character(0), np = character(0), id_ordre = integer(0), poidsp = numeric(0), code_car = character(0), code_com = character(0), code_sec = character(0),
                               heure_deb = character(0), heure_fin = character(0), duree_presence = numeric(0), 
                               motif_presence = integer(0), stringsAsFactors = FALSE )

# remplissage de la table, on crée des dataframe temporaire pour chaque individu qu'on ajoute progressivement à la base
# on découpe en 6 fois la démarche pour avoir une mémoire vive suffisante
VecID <- unique(deplacements_sem_GRP$ID_pers)
VecID1 <- VecID[1:5000]
VecID2 <- VecID[5001:10000]
VecID3 <- VecID[10001:15000]
VecID4 <- VecID[15001:20000]
VecID5 <- VecID[20001:25000]
VecID6 <- VecID[25001:length(VecID)]

presence_semaine1 <- data.frame(nquest = character(0), np = character(0), id_ordre = integer(0), poidsp = numeric(0), code_car = character(0), code_com = character(0), code_sec = character(0),
                                heure_deb = character(0), heure_fin = character(0), duree_presence = numeric(0), 
                                motif_presence = integer(0),  stringsAsFactors = FALSE )

invisible(foreach (ind = VecID1, .verbose = FALSE) %do% {
  
  depsem_ind <- filter(deplacements_sem_GRP, deplacements_sem_GRP$ID_pers == ind)
  nobs <- depsem_ind$nobs[1]
  
  prsem_ind <- data.frame(nquest = character(nobs+1), np = character(nobs+1), id_ordre = integer(nobs+1), poidsp = numeric(nobs+1), code_car = character(nobs+1), code_com = character(nobs+1), code_sec = character(nobs+1),
                          heure_deb = character(nobs+1), heure_fin = character(nobs+1), duree_presence = numeric(nobs+1), 
                          motif_presence = integer(nobs+1),  stringsAsFactors = FALSE )
  
  prsem_ind[,1] <- rep(depsem_ind$nquest[1], nobs+1)
  prsem_ind[,2] <- rep(depsem_ind$np[1], nobs+1)
  prsem_ind[,3] <- 1:(nobs+1)
  prsem_ind[,4] <- rep(depsem_ind$poidsp[1], nobs+1)
  prsem_ind[1,5] <- depsem_ind$orc[1]
  prsem_ind[1,6] <- depsem_ind$orcomm[1]
  prsem_ind[1,7] <- depsem_ind$orsect[1]
  prsem_ind[1,8] <- as.character.Date(ISOdatetime(2010,1,1,4,0,0))
  prsem_ind[nobs+1,9] <- as.character.Date(ISOdatetime(2010,1,2,4,0,0))
  prsem_ind[1,11] <- depsem_ind$ormot_h9[1]
  
  
  for (i in 1:nobs){
    prsem_ind[i+1,5] <- depsem_ind$destc[i]
    prsem_ind[i+1,6] <- depsem_ind$destcomm[i]
    prsem_ind[i+1,7] <- depsem_ind$destsect[i]
    prsem_ind[i+1,8] <- ifelse(depsem_ind$desth[i]>23, as.character.Date(ISOdatetime(2010,1,2,depsem_ind$desth[i]-24,depsem_ind$destm[i],0)),
                               as.character.Date(ISOdatetime(2010,1,1,depsem_ind$desth[i],depsem_ind$destm[i],0)) )
    prsem_ind[i,9] <- ifelse(depsem_ind$orh[i]>23, as.character.Date(ISOdatetime(2010,1,2,depsem_ind$orh[i]-24,depsem_ind$orm[i],0)),
                             as.character.Date(ISOdatetime(2010,1,1,depsem_ind$orh[i],depsem_ind$orm[i],0)) )
    prsem_ind[i+1,11] <- depsem_ind$destmot_h9[i]
  }
  
  presence_semaine1 <-rbind(presence_semaine1, prsem_ind)
})
save(presence_semaine1, file = "Donnees R/presence_semaine_part1.RData")
presence_semaine1 <- NULL
presence_semaine2 <- data.frame(nquest = character(0), np = character(0), id_ordre = integer(0), poidsp = numeric(0), code_car = character(0), code_com = character(0), code_sec = character(0),
                                heure_deb = character(0), heure_fin = character(0), duree_presence = numeric(0), 
                                motif_presence = integer(0), stringsAsFactors = FALSE )

invisible(foreach (ind = VecID2, .verbose = FALSE) %do% {
  
  depsem_ind <- filter(deplacements_sem_GRP, deplacements_sem_GRP$ID_pers == ind)
  nobs <- depsem_ind$nobs[1]
  
  prsem_ind <- data.frame(nquest = character(nobs+1), np = character(nobs+1), id_ordre = integer(nobs+1), poidsp = numeric(nobs+1), code_car = character(nobs+1), code_com = character(nobs+1), code_sec = character(nobs+1),
                          heure_deb = character(nobs+1), heure_fin = character(nobs+1), duree_presence = numeric(nobs+1), 
                          motif_presence = integer(nobs+1), stringsAsFactors = FALSE )
  
  prsem_ind[,1] <- rep(depsem_ind$nquest[1], nobs+1)
  prsem_ind[,2] <- rep(depsem_ind$np[1], nobs+1)
  prsem_ind[,3] <- 1:(nobs+1)
  prsem_ind[,4] <- rep(depsem_ind$poidsp[1], nobs+1)
  prsem_ind[1,5] <- depsem_ind$orc[1]
  prsem_ind[1,6] <- depsem_ind$orcomm[1]
  prsem_ind[1,7] <- depsem_ind$orsect[1]
  prsem_ind[1,8] <- as.character.Date(ISOdatetime(2010,1,1,4,0,0))
  prsem_ind[nobs+1,9] <- as.character.Date(ISOdatetime(2010,1,2,4,0,0))
  prsem_ind[1,11] <- depsem_ind$ormot_h9[1]
  
  for (i in 1:nobs){
    prsem_ind[i+1,5] <- depsem_ind$destc[i]
    prsem_ind[i+1,6] <- depsem_ind$destcomm[i]
    prsem_ind[i+1,7] <- depsem_ind$destsect[i]
    prsem_ind[i+1,8] <- ifelse(depsem_ind$desth[i]>23, as.character.Date(ISOdatetime(2010,1,2,depsem_ind$desth[i]-24,depsem_ind$destm[i],0)),
                               as.character.Date(ISOdatetime(2010,1,1,depsem_ind$desth[i],depsem_ind$destm[i],0)) )
    prsem_ind[i,9] <- ifelse(depsem_ind$orh[i]>23, as.character.Date(ISOdatetime(2010,1,2,depsem_ind$orh[i]-24,depsem_ind$orm[i],0)),
                             as.character.Date(ISOdatetime(2010,1,1,depsem_ind$orh[i],depsem_ind$orm[i],0)) )
    prsem_ind[i+1,11] <- depsem_ind$destmot_h9[i]
  }
  presence_semaine2 <-rbind(presence_semaine2, prsem_ind)
})
save(presence_semaine2,  file = "Donnees R/presence_semaine_part2.RData")
presence_semaine2 <- NULL
presence_semaine3 <- data.frame(nquest = character(0), np = character(0), id_ordre = integer(0), poidsp = numeric(0), code_car = character(0), code_com = character(0), code_sec = character(0),
                                heure_deb = character(0), heure_fin = character(0), duree_presence = numeric(0), 
                                motif_presence = integer(0), stringsAsFactors = FALSE )

invisible(foreach (ind = VecID3, .verbose = FALSE) %do% {
  
  depsem_ind <- filter(deplacements_sem_GRP, deplacements_sem_GRP$ID_pers == ind)
  nobs <- depsem_ind$nobs[1]
  
  prsem_ind <- data.frame(nquest = character(nobs+1), np = character(nobs+1), id_ordre = integer(nobs+1), poidsp = numeric(nobs+1), code_car = character(nobs+1), code_com = character(nobs+1), code_sec = character(nobs+1),
                          heure_deb = character(nobs+1), heure_fin = character(nobs+1), duree_presence = numeric(nobs+1), 
                          motif_presence = integer(nobs+1), stringsAsFactors = FALSE )
  
  prsem_ind[,1] <- rep(depsem_ind$nquest[1], nobs+1)
  prsem_ind[,2] <- rep(depsem_ind$np[1], nobs+1)
  prsem_ind[,3] <- 1:(nobs+1)
  prsem_ind[,4] <- rep(depsem_ind$poidsp[1], nobs+1)
  prsem_ind[1,5] <- depsem_ind$orc[1]
  prsem_ind[1,6] <- depsem_ind$orcomm[1]
  prsem_ind[1,7] <- depsem_ind$orsect[1]
  prsem_ind[1,8] <- as.character.Date(ISOdatetime(2010,1,1,4,0,0))
  prsem_ind[nobs+1,9] <- as.character.Date(ISOdatetime(2010,1,2,4,0,0))
  prsem_ind[1,11] <- depsem_ind$ormot_h9[1]
  
  for (i in 1:nobs){
    prsem_ind[i+1,5] <- depsem_ind$destc[i]
    prsem_ind[i+1,6] <- depsem_ind$destcomm[i]
    prsem_ind[i+1,7] <- depsem_ind$destsect[i]
    prsem_ind[i+1,8] <- ifelse(depsem_ind$desth[i]>23, as.character.Date(ISOdatetime(2010,1,2,depsem_ind$desth[i]-24,depsem_ind$destm[i],0)),
                               as.character.Date(ISOdatetime(2010,1,1,depsem_ind$desth[i],depsem_ind$destm[i],0)) )
    prsem_ind[i,9] <- ifelse(depsem_ind$orh[i]>23, as.character.Date(ISOdatetime(2010,1,2,depsem_ind$orh[i]-24,depsem_ind$orm[i],0)),
                             as.character.Date(ISOdatetime(2010,1,1,depsem_ind$orh[i],depsem_ind$orm[i],0)) )
    prsem_ind[i+1,11] <- depsem_ind$destmot_h9[i]
  }
  presence_semaine3 <-rbind(presence_semaine3, prsem_ind)
})
save(presence_semaine3,  file = "Donnees R/presence_semaine_part3.RData")
presence_semaine3 <- NULL
presence_semaine4 <- data.frame(nquest = character(0), np = character(0), id_ordre = integer(0), poidsp = numeric(0), code_car = character(0), code_com = character(0), code_sec = character(0),
                                heure_deb = character(0), heure_fin = character(0), duree_presence = numeric(0), 
                                motif_presence = integer(0), stringsAsFactors = FALSE )

invisible(foreach (ind = VecID4, .verbose = FALSE) %do% {
  
  depsem_ind <- filter(deplacements_sem_GRP, deplacements_sem_GRP$ID_pers == ind)
  nobs <- depsem_ind$nobs[1]
  
  prsem_ind <- data.frame(nquest = character(nobs+1), np = character(nobs+1), id_ordre = integer(nobs+1), poidsp = numeric(nobs+1), code_car = character(nobs+1), code_com = character(nobs+1), code_sec = character(nobs+1),
                          heure_deb = character(nobs+1), heure_fin = character(nobs+1), duree_presence = numeric(nobs+1), 
                          motif_presence = integer(nobs+1),  stringsAsFactors = FALSE )
  
  prsem_ind[,1] <- rep(depsem_ind$nquest[1], nobs+1)
  prsem_ind[,2] <- rep(depsem_ind$np[1], nobs+1)
  prsem_ind[,3] <- 1:(nobs+1)
  prsem_ind[,4] <- rep(depsem_ind$poidsp[1], nobs+1)
  prsem_ind[1,5] <- depsem_ind$orc[1]
  prsem_ind[1,6] <- depsem_ind$orcomm[1]
  prsem_ind[1,7] <- depsem_ind$orsect[1]
  prsem_ind[1,8] <- as.character.Date(ISOdatetime(2010,1,1,4,0,0))
  prsem_ind[nobs+1,9] <- as.character.Date(ISOdatetime(2010,1,2,4,0,0))
  prsem_ind[1,11] <- depsem_ind$ormot_h9[1]
  
  for (i in 1:nobs){
    prsem_ind[i+1,5] <- depsem_ind$destc[i]
    prsem_ind[i+1,6] <- depsem_ind$destcomm[i]
    prsem_ind[i+1,7] <- depsem_ind$destsect[i]
    prsem_ind[i+1,8] <- ifelse(depsem_ind$desth[i]>23, as.character.Date(ISOdatetime(2010,1,2,depsem_ind$desth[i]-24,depsem_ind$destm[i],0)),
                               as.character.Date(ISOdatetime(2010,1,1,depsem_ind$desth[i],depsem_ind$destm[i],0)) )
    prsem_ind[i,9] <- ifelse(depsem_ind$orh[i]>23, as.character.Date(ISOdatetime(2010,1,2,depsem_ind$orh[i]-24,depsem_ind$orm[i],0)),
                             as.character.Date(ISOdatetime(2010,1,1,depsem_ind$orh[i],depsem_ind$orm[i],0)) )
    prsem_ind[i+1,11] <- depsem_ind$destmot_h9[i]
  }
  presence_semaine4 <-rbind(presence_semaine4, prsem_ind)
})
save(presence_semaine4,  file = "Donnees R/presence_semaine_part4.RData")
presence_semaine4 <- NULL
presence_semaine5 <- data.frame(nquest = character(0), np = character(0), id_ordre = integer(0), poidsp = numeric(0), code_car = character(0), code_com = character(0), code_sec = character(0),
                                heure_deb = character(0), heure_fin = character(0), duree_presence = numeric(0), 
                                motif_presence = integer(0), stringsAsFactors = FALSE )

invisible(foreach (ind = VecID5, .verbose = FALSE) %do% {
  
  depsem_ind <- filter(deplacements_sem_GRP, deplacements_sem_GRP$ID_pers == ind)
  nobs <- depsem_ind$nobs[1]
  
  prsem_ind <- data.frame(nquest = character(nobs+1), np = character(nobs+1), id_ordre = integer(nobs+1), poidsp = numeric(nobs+1), code_car = character(nobs+1), code_com = character(nobs+1), code_sec = character(nobs+1),
                          heure_deb = character(nobs+1), heure_fin = character(nobs+1), duree_presence = numeric(nobs+1), 
                          motif_presence = integer(nobs+1), stringsAsFactors = FALSE )
  
  prsem_ind[,1] <- rep(depsem_ind$nquest[1], nobs+1)
  prsem_ind[,2] <- rep(depsem_ind$np[1], nobs+1)
  prsem_ind[,3] <- 1:(nobs+1)
  prsem_ind[,4] <- rep(depsem_ind$poidsp[1], nobs+1)
  prsem_ind[1,5] <- depsem_ind$orc[1]
  prsem_ind[1,6] <- depsem_ind$orcomm[1]
  prsem_ind[1,7] <- depsem_ind$orsect[1]
  prsem_ind[1,8] <- as.character.Date(ISOdatetime(2010,1,1,4,0,0))
  prsem_ind[nobs+1,9] <- as.character.Date(ISOdatetime(2010,1,2,4,0,0))
  prsem_ind[1,11] <- depsem_ind$ormot_h9[1]
  
  for (i in 1:nobs){
    prsem_ind[i+1,5] <- depsem_ind$destc[i]
    prsem_ind[i+1,6] <- depsem_ind$destcomm[i]
    prsem_ind[i+1,7] <- depsem_ind$destsect[i]
    prsem_ind[i+1,8] <- ifelse(depsem_ind$desth[i]>23, as.character.Date(ISOdatetime(2010,1,2,depsem_ind$desth[i]-24,depsem_ind$destm[i],0)),
                               as.character.Date(ISOdatetime(2010,1,1,depsem_ind$desth[i],depsem_ind$destm[i],0)) )
    prsem_ind[i,9] <- ifelse(depsem_ind$orh[i]>23, as.character.Date(ISOdatetime(2010,1,2,depsem_ind$orh[i]-24,depsem_ind$orm[i],0)),
                             as.character.Date(ISOdatetime(2010,1,1,depsem_ind$orh[i],depsem_ind$orm[i],0)) )
    prsem_ind[i+1,11] <- depsem_ind$destmot_h9[i]
  }
  presence_semaine5 <-rbind(presence_semaine5, prsem_ind)
})
save(presence_semaine5,  file = "Donnees R/presence_semaine_part5.RData")
presence_semaine5 <- NULL
presence_semaine6 <- data.frame(nquest = character(0), np = character(0), id_ordre = integer(0), poidsp = numeric(0), code_car = character(0), code_com = character(0), code_sec = character(0),
                                heure_deb = character(0), heure_fin = character(0), duree_presence = numeric(0), 
                                motif_presence = integer(0),stringsAsFactors = FALSE )

invisible(foreach (ind = VecID6, .verbose = FALSE) %do% {
  
  depsem_ind <- filter(deplacements_sem_GRP, deplacements_sem_GRP$ID_pers == ind)
  nobs <- depsem_ind$nobs[1]
  
  prsem_ind <- data.frame(nquest = character(nobs+1), np = character(nobs+1), id_ordre = integer(nobs+1), poidsp = numeric(nobs+1), code_car = character(nobs+1), code_com = character(nobs+1), code_sec = character(nobs+1),
                          heure_deb = character(nobs+1), heure_fin = character(nobs+1), duree_presence = numeric(nobs+1), 
                          motif_presence = integer(nobs+1), stringsAsFactors = FALSE )
  
  prsem_ind[,1] <- rep(depsem_ind$nquest[1], nobs+1)
  prsem_ind[,2] <- rep(depsem_ind$np[1], nobs+1)
  prsem_ind[,3] <- 1:(nobs+1)
  prsem_ind[,4] <- rep(depsem_ind$poidsp[1], nobs+1)
  prsem_ind[1,5] <- depsem_ind$orc[1]
  prsem_ind[1,6] <- depsem_ind$orcomm[1]
  prsem_ind[1,7] <- depsem_ind$orsect[1]
  prsem_ind[1,8] <- as.character.Date(ISOdatetime(2010,1,1,4,0,0))
  prsem_ind[nobs+1,9] <- as.character.Date(ISOdatetime(2010,1,2,4,0,0))
  prsem_ind[1,11] <- depsem_ind$ormot_h9[1]
  
  for (i in 1:nobs){
    prsem_ind[i+1,5] <- depsem_ind$destc[i]
    prsem_ind[i+1,6] <- depsem_ind$destcomm[i]
    prsem_ind[i+1,7] <- depsem_ind$destsect[i]
    prsem_ind[i+1,8] <- ifelse(depsem_ind$desth[i]>23, as.character.Date(ISOdatetime(2010,1,2,depsem_ind$desth[i]-24,depsem_ind$destm[i],0)),
                               as.character.Date(ISOdatetime(2010,1,1,depsem_ind$desth[i],depsem_ind$destm[i],0)) )
    prsem_ind[i,9] <- ifelse(depsem_ind$orh[i]>23, as.character.Date(ISOdatetime(2010,1,2,depsem_ind$orh[i]-24,depsem_ind$orm[i],0)),
                             as.character.Date(ISOdatetime(2010,1,1,depsem_ind$orh[i],depsem_ind$orm[i],0)) )
    prsem_ind[i+1,11] <- depsem_ind$destmot_h9[i]
  }
  presence_semaine6 <-rbind(presence_semaine6, prsem_ind)
})
save(presence_semaine6,  file = "Donnees R/presence_semaine_part6.RData")

load("Donnees R/presence_semaine_part1.RData")
load("Donnees R/presence_semaine_part2.RData")
load("Donnees R/presence_semaine_part3.RData")
load("Donnees R/presence_semaine_part4.RData")
load("Donnees R/presence_semaine_part5.RData")
load("Donnees R/presence_semaine_part6.RData")

# on récupère les individus qui ne se sont pas déplacés ou hors IDF
presence_semaine_nondep <- filter(personnes_semaine, personnes_semaine$nondepl>1 & personnes_semaine$nondepl<=7)
presence_semaine_nondep <- select(presence_semaine_nondep, nquest, np, poidsp, code_car=resc, code_com=rescomm, code_sec=ressect)
presence_semaine_nondep$id_ordre <- 1
presence_semaine_nondep$heure_deb <- as.character.Date(ISOdatetime(2010,1,1,4,0,0))
presence_semaine_nondep$heure_fin <- as.character.Date(ISOdatetime(2010,1,2,4,0,0))
presence_semaine_nondep$duree_presence <- 24*60
presence_semaine_nondep$motif_presence <- 1
presence_semaine_horsIDF <- filter(personnes_semaine, personnes_semaine$nondep==8)
presence_semaine_horsIDF <- select(presence_semaine_horsIDF, nquest, np, poidsp)
presence_semaine_horsIDF$id_ordre <- 1
presence_semaine_horsIDF$code_car <- "999999"
presence_semaine_horsIDF$code_com <- "99999"
presence_semaine_horsIDF$code_sec <- "9999"
presence_semaine_horsIDF$heure_deb <- as.character.Date(ISOdatetime(2010,1,1,4,0,0))
presence_semaine_horsIDF$heure_fin <- as.character.Date(ISOdatetime(2010,1,2,4,0,0))
presence_semaine_horsIDF$duree_presence <- 0
presence_semaine_horsIDF$motif_presence <- 99

#on récupère les deplacements
nobs <- length(deplacements_semaine$nquest)
presence_semaine_dep <- data.frame(nquest = character(nobs), np = character(nobs), id_ordre = integer(nobs), poidsp = numeric(nobs), code_car = character(nobs), code_com = character(nobs), code_sec = character(nobs),
                                   heure_deb = character(nobs), heure_fin = character(nobs), duree_presence = numeric(nobs), 
                                   motif_presence = integer(nobs), stringsAsFactors = FALSE )
presence_semaine_dep$nquest <- deplacements_semaine$nquest
presence_semaine_dep$np <- deplacements_semaine$np
presence_semaine_dep$poidsp <- deplacements_semaine$poidsp
presence_semaine_dep$code_car <- "888888"
presence_semaine_dep$code_com <- "88888"
presence_semaine_dep$code_sec <- "8888"
presence_semaine_dep$motif_presence <- 88
presence_semaine_dep$heure_fin[deplacements_semaine$desth>23 & is.na(deplacements_semaine$desth)==FALSE] <- as.character.Date(ISOdatetime(2010,1,2,deplacements_semaine$desth[deplacements_semaine$desth>23 & is.na(deplacements_semaine$desth)==FALSE]-24,deplacements_semaine$destm[deplacements_semaine$desth>23 & is.na(deplacements_semaine$desth)==FALSE],0))
presence_semaine_dep$heure_fin[deplacements_semaine$desth<24 & is.na(deplacements_semaine$desth)==FALSE] <- as.character.Date(ISOdatetime(2010,1,1,deplacements_semaine$desth[deplacements_semaine$desth<24 & is.na(deplacements_semaine$desth)==FALSE],deplacements_semaine$destm[deplacements_semaine$desth<24 & is.na(deplacements_semaine$desth)==FALSE],0))
presence_semaine_dep$heure_deb[deplacements_semaine$orh>23 & is.na(deplacements_semaine$orh)==FALSE] <- as.character.Date(ISOdatetime(2010,1,2,deplacements_semaine$orh[deplacements_semaine$orh>23 & is.na(deplacements_semaine$orh)==FALSE]-24,deplacements_semaine$orm[deplacements_semaine$orh>23 & is.na(deplacements_semaine$orh)==FALSE],0))
presence_semaine_dep$heure_deb[deplacements_semaine$orh<24 & is.na(deplacements_semaine$orh)==FALSE] <- as.character.Date(ISOdatetime(2010,1,1,deplacements_semaine$orh[deplacements_semaine$orh<24 & is.na(deplacements_semaine$orh)==FALSE],deplacements_semaine$orm[deplacements_semaine$orh<24 & is.na(deplacements_semaine$orh)==FALSE],0))

presence_semaine <-rbind(presence_semaine1, presence_semaine2,presence_semaine3,presence_semaine4,presence_semaine5,presence_semaine6,presence_semaine_nondep, presence_semaine_horsIDF, presence_semaine_dep)
presence_semaine <- arrange(presence_semaine, nquest, np, heure_deb, heure_fin)

## on supprime les artefacts de construction 4h-4h
presence_semaine <- filter(presence_semaine, presence_semaine$heure_deb!=as.character.Date(ISOdatetime(2010,1,1,4,0,0))|presence_semaine$heure_fin!=as.character.Date(ISOdatetime(2010,1,1,4,0,0)))
presence_semaine <- filter(presence_semaine, presence_semaine$heure_deb!=as.character.Date(ISOdatetime(2010,1,2,4,0,0))|presence_semaine$heure_fin!=as.character.Date(ISOdatetime(2010,1,2,4,0,0)))

## calcul des durées de présence
presence_semaine$duree_presence <- as.numeric(difftime(ymd_hms(presence_semaine$heure_fin, truncated=3),ymd_hms(presence_semaine$heure_deb, truncated=3), units= "mins"))


###########
## on r?cup?re des donn?es socio-d?mo

###table individu
#sexe
#age
#niveau d'?ducation
#cat?gorie socio-pro
presence_semaine <- left_join(x = presence_semaine, y=select(personnes_semaine, nquest, np, sexe, age, dipl,  cs8, cs24l), by = c("nquest", "np"))

############ calcul de la distance au lieu de résidence (centroide du carreau)
##corrections
presence_semaine$code_car[presence_semaine$code_car=="641771g"] <- "641771G"
presence_semaine$code_car[presence_semaine$code_car=="822312g"] <- "822312G"
presence_semaine$code_car[presence_semaine$code_car=="583149g"] <- "583149G"
presence_semaine$code_car[presence_semaine$code_car=="555488e"] <- "555488E"
presence_semaine$code_car[presence_semaine$code_car=="583422h"] <- "583422H"

# on attribue les coordonnées des carreaux à partir d'une table créée sous ArcGis
carrEGT_centroides <- read.csv2("Donnees TXT/carrEGT_centroides.csv")
presence_semaine$ID_pers <- ifelse(nchar(presence_semaine$np)==1,paste(presence_semaine$nquest,presence_semaine$np,sep="0"),paste(presence_semaine$nquest,presence_semaine$np,sep=""))
presence_semaine <- merge(presence_semaine, select(carrEGT_centroides, carr100m_7,POINT_X, POINT_Y) , by.x="code_car", by.y = "carr100m_7", all.x = TRUE, all.y = FALSE)
# on récupère le carreau de résidence
residences <- filter(presence_semaine, motif_presence==1)
residences <- group_by(.data = residences, ID_pers)
residences <- as.data.frame(summarize(residences, POINT_X_RES = first(POINT_X), POINT_Y_RES = first(POINT_Y)))
presence_semaine <- merge(presence_semaine, residences , by= "ID_pers", all.x = TRUE, all.y = FALSE)
#Calcul de la distance activité résidence avec sp
library("sp")
p1 = SpatialPoints(filter(presence_semaine, is.na(POINT_X)==FALSE &  is.na(POINT_X_RES)==FALSE)[,18:19])
p2 = SpatialPoints(filter(presence_semaine, is.na(POINT_X)==FALSE &  is.na(POINT_X_RES)==FALSE)[,20:21])
proj4string(p1) = CRS("+init=epsg:27572")
proj4string(p2) = CRS("+init=epsg:27572")
presence_semaine$Dist_Act_Resid[is.na(presence_semaine$POINT_X)==FALSE &  is.na(presence_semaine$POINT_X_RES)==FALSE] <- as.vector(spDists(x = p1 , y = p2, diagonal = TRUE))
presence_semaine <- arrange(presence_semaine, ID_pers, heure_deb)

save(presence_semaine, file = "Donnees R/presence_semaine_prJV.RData")
write.csv2(presence_semaine, "Sorties TXT/presence_semaine_prJV.csv", row.names = FALSE)
