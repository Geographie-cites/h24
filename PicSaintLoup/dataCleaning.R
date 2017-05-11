library("foreign")
library("readstata13")
setwd("~/Documents/h24/Data_clem")

bases = list.files()

# commencer par matcher les individus au sein d'une meme base-annee

b96_demo= read.dta13("Bsn96_CNRS_sociodem_alimhorsConso.dta")
b96_conso= read.dta13("Bsn96_CNRS_FL_pour_evol.dta")
b96 = data.frame(b96_demo, b96_conso[match(b96_demo$ident, b96_conso$ident),])
b02_demo= read.dta13("Bsn02_CNRS_sociodem_alimhorsConso.dta")
b02_conso= read.dta13("Bsn02_CNRS_FL_pour_evol.dta")
b02 = data.frame(b02_demo, b02_conso[match(b02_demo$q2, b02_conso$q2),])
b08_demo= read.dta13("Bsn08_CNRS_sociodem_alimhorsConso.dta")
b08_conso= read.dta13("Bsn08_CNRS_FL_pour_evol.dta")
b08_bio = read.dta13("BSN08_bio.dta")
b08 = data.frame(b08_demo, b08_conso[match(b08_demo$ident, b08_conso$ident),])
b08 = data.frame(b08, b08_bio[match(b08$ident, b08_bio$ident),])

#noms originaux de variables
# identifiants

ID = c("ident", "q2", "ident")
ponderation_annee = c("poidsr", "poids", "redri2")
# pour la france entiere par annee
ponderation_init = c("poidsr", "poids18", "redrg")
# pour la france entiere de l'annee 1996

# socio-demo

sexe = c("q66", "q91", "sex")
# homme = 1, femme = 2
age = c("ageb","q92","age")
#penser a exclure les <18 ans
edu = c("q227", "q367", "q831")
# regrouper les categories selon la pop synthetique
pro = c("q228", "situi", "q109")
# a regrouper en 4 categories
income = c("q240", "q369uc","revuc")
# reconstruire les tranches en 2002 et 2008 sur le format 1996
# 1996: <1500, 1500-2499, 2500-3999, 4000-6599, 6600-7999, 
# 8000-9999, 10k-13k, 13k-16k, 16k-20k, 20k-30k, 30k-50k, >50k
size_com_res = c("q21", "q19", "catagg")
# 1 = rural, 2 = <20k hab, 3 = 20-100, 4 = 100-200, 5 = >200, 6 = "agglomeration parisienne" (??)
region = c("q25", "q26", "dep")


head(bd)
deps_idf = c(75, 77, 78, 91, 92, 93, 94, 95)

# contexte repas

breakfast_take = c("q78", "q119", "q134")
# utiliser les variables suivantes quand breakfast_take == yes
breakfast_location = c("q79", "q120", "q135")
breakfast_partner = c("q80i", "q121i", "q137_")
# plusieurs reponses possibles sauf "1"
lunch_take = c("q99", "q143", "q224")
# utiliser les variables suivantes quand lunch_take == yes
lunch_location = c("q100", "q144", "q225")
lunch_partner = c("q101i","q145i","q227_")
# plusieurs reponses possibles sauf "1"
dinner_take = c("q143", "q191", "q441")
# utiliser les variables suivantes quand dinner_take == yes
dinner_location = c("q144", "q192", "q442")
dinner_partner = c("q145i", "q193i", "q444_")
# plusieurs reponses possibles sauf "1"

# attitude / opinion

influence_foyer_menu = c("q198s1", "q253s1", "q694_1")
# echelle likert a binariser apres analyse de la distribution
influence_sante_menu = c("q198s2", "q253s2", "q694_2")
# echelle likert a binariser apres analyse de la distribution
influence_budget_menu = c("q198s3", "q253s3", "q694_3")
# echelle likert a binariser apres analyse de la distribution
influence_temps_menu = c("q198s4", "q253s4", "q694_4")
# echelle likert a binariser apres analyse de la distribution

alimentation_equilibre = c("q199", "q254", "q695")

alimentation_info = c("q205", "q257", "q698")

opinion_suffisamment_fruit = c(NA, "q270", "q709")
opinion_suffisamment_legume = c(NA, "q268", "q707")

# conso

conso_bio = c("q192s11", "q247s18", "q679_5")
# lickert en 9 categories

conso_5_fruit_legume = c("pr_frlgr2", "pr_frlgr2", "pr_frlegevr2")
petit_conso_fruit = c("pr_fruipc", "pr_fruipc", "pr_frevpc")
# <=1 fruit la veille
petit_conso_legume = c("pr_legpc", "pr_legpc", "pr_legevpc")
# <=1 legume la veille





# selectionner / renommer les variables interessantes de maniere similaire pour les 3 annees


for(a in 1:3){
  if (a == 1) bd = b96
  if (a == 2) bd = b02
  if (a == 3) bd = b08
  bd$ID = bd[,ID[a]]
  bd$pond_annee = bd[,ponderation_annee[a]]
  bd$pond_96 = bd[,ponderation_init[a]]
  bd$sex = as.numeric(bd[,sexe[a]])
  bd$pro = as.numeric(bd[,pro[a]])
  if (a == 1) {
    bd$pro = ifelse(bd$pro %in% 1:3, 1, #situation de travail
                     ifelse(bd$pro %in% c(4, 5, 13), 2, # situation de chomage
                        ifelse(bd$pro == 14, 3, # situation de etude
                            ifelse(bd$pro %in% c(6:12, 15, 16), 4, NA)))) # situation d'inactivite
  }
  if (a == 2) {
    bd$pro = ifelse(bd$pro %in% 1:2, 1, #situation de travail
                    ifelse(bd$pro == 3, 2, # situation de chomage
                           ifelse(bd$pro == 4, 3, # situation de etude
                                  ifelse(bd$pro %in% c(5,6), 4, NA)))) # situation d'inactivite
  }
  if (a == 3) {
    bd$pro = ifelse(bd$pro %in% 1:5, 1, #situation de travail
                    ifelse(bd$pro == 9, 2, # situation de chomage
                           ifelse(bd$pro %in% c(6, 12), 3, # situation de etude
                                  ifelse(bd$pro %in% c(7, 8, 10, 11, 13), 4, NA)))) # situation d'inactivite
  }
  # 1 = low education, 2 = middle education, 3 = high education
  
   bd$age_continu = bd[,age[a]]
  bd$age_3cat = ifelse(bd$age_continu %in% 15:29, 1,
                       ifelse(bd$age_continu %in% 30:59, 2,
                              ifelse(bd$age_continu >= 60, 3,0)))
  bd$adult = ifelse(bd$age_continu >= 15, 1, 0)
  
  if (a == 1) membres = bd[,paste0("q70s", 1:8)]
  if (a == 2) membres = bd[,paste0("q102s", 1:12)]
  if (a == 3) membres = bd[,paste0("q66_", 1:13)]
  # compter le nombre d'enfants
  enfants = ifelse(membres > 0 & membres <= 15,1,0)
  bd$n_enfants = rowSums(enfants, na.rm = T)
   bd$edu = as.numeric(bd[,edu[a]])
   if (a == 1) {
     bd$educ = ifelse(bd$edu %in% 1:5, 1,
                      ifelse(bd$edu %in% 6:8, 2,
                             ifelse(bd$edu %in% 9:11, 3, NA)))
   }
   if (a == 2) {
     bd$educ = ifelse(bd$edu %in% 1:5, 1,
                      ifelse(bd$edu %in% 6:9, 2,
                             ifelse(bd$edu %in% 10:13, 3, NA)))
   }
   if (a == 3) {
     bd$educ = ifelse(bd$edu %in% 1:5, 1,
                      ifelse(bd$edu %in% 6:9, 2,
                             ifelse(bd$edu %in% 10:13, 3, NA)))
   }
   # 1 = low education, 2 = middle education, 3 = high education
   
 
    bd$income = bd[,income[a]]
    
   
    
    if (a == 1) {
      bd$revenus = ifelse(bd$income %in% 1:3, 1,
                       ifelse(bd$income %in% 4:6, 2,
                          ifelse(bd$income %in% 7:8, 3,
                              ifelse(bd$income %in% 9:12, 4, NA))))
    }
    if (a == 2) {
      bd$revenus = ifelse(bd$income < 4000, 1,
                       ifelse(bd$income >= 4000 & bd$income < 10000, 2,
                         ifelse(bd$income >= 10000 &  bd$income < 16000, 3,
                              ifelse(bd$income >= 16000, 4, NA))))
    }
 
    if (a == 3) {
      bd$income = bd$income * 12 #### /!\ on pense que la donnee initiale est mensuelle mais resultats douteux
      bd$revenus = ifelse(bd$income < 4000, 1,
                          ifelse(bd$income >= 4000 & bd$income < 10000, 2,
                                 ifelse(bd$income >= 10000 &  bd$income < 16000, 3,
                                        ifelse(bd$income >= 16000, 4, NA))))
    }
   bd$commune = as.numeric(bd[,size_com_res[a]])
   bd$rural = ifelse(bd$commune == 1, 1, 0)
  
   bd$reg = as.numeric(bd[,region[a]])
   if (a %in% 1:2) {
     bd$idf = ifelse(bd$reg == 9, 1, 0) # 1 if 
   }
    if (a == 3) {
      bd$idf = ifelse(bd$reg %in% deps_idf, 1, 0)
    }
   

  bd$breakfast_take = as.numeric(bd[,breakfast_take[a]])
  bd$breakfast_yes = ifelse(bd$breakfast_take == 1, 1, 0) 
  bd$lunch_take = as.numeric(bd[,lunch_take[a]])
  bd$lunch_yes = ifelse(bd$lunch_take == 1, 1, 0) 
  bd$dinner_take = as.numeric(bd[,dinner_take[a]])
  bd$dinner_yes = ifelse(bd$dinner_take == 1, 1, 0) 
  
  bd$breakfast_location = bd[,breakfast_location[a]]
  bd$lunch_location = bd[,lunch_location[a]]
  bd$dinner_location = bd[,dinner_location[a]]
  
  bd$breakfast_alone = as.numeric(bd[,paste0(breakfast_partner[a],1)])
  if (a %in% 2) bd$breakfast_alone = ifelse(bd$breakfast_alone == 2, 0, bd$breakfast_alone)
  bd$lunch_alone = as.numeric(bd[,paste0(lunch_partner[a],1)])
  if (a %in% 2) bd$lunch_alone = ifelse(bd$lunch_alone == 2, 0, bd$lunch_alone)
  bd$dinner_alone = as.numeric(bd[,paste0(dinner_partner[a],1)])
  if (a %in% 2) bd$dinner_alone = ifelse(bd$dinner_alone == 2, 0, bd$dinner_alone)
  
  bd$breakfast_family = as.numeric(bd[,paste0(breakfast_partner[a],2)])
  if (a %in% 2) bd$breakfast_family = ifelse(bd$breakfast_family == 2, 0, bd$breakfast_family)
  bd$lunch_family = as.numeric(bd[,paste0(lunch_partner[a],2)])
  if (a %in% 2) bd$lunch_family = ifelse(bd$lunch_family == 2, 0, bd$lunch_family)
  bd$dinner_family = as.numeric(bd[,paste0(dinner_partner[a],2)])
  if (a %in% 2) bd$dinner_family = ifelse(bd$dinner_family == 2, 0, bd$dinner_family)
  
  bd$breakfast_friend = as.numeric(bd[,paste0(breakfast_partner[a],3)])
  if (a %in% 2) bd$breakfast_friend = ifelse(bd$breakfast_friend == 2, 0, bd$breakfast_friend)
  bd$lunch_friend = as.numeric(bd[,paste0(lunch_partner[a],3)])
  if (a %in% 2) bd$lunch_friend = ifelse(bd$lunch_friend == 2, 0, bd$lunch_friend)
  bd$dinner_friend = as.numeric(bd[,paste0(dinner_partner[a],3)])
  if (a %in% 2) bd$dinner_friend = ifelse(bd$dinner_friend == 2, 0, bd$dinner_friend)
  
  bd$breakfast_colleague = as.numeric(bd[,paste0(breakfast_partner[a],4)])
  if (a %in% 2) bd$breakfast_colleague = ifelse(bd$breakfast_colleague == 2, 0, bd$breakfast_colleague)
  bd$lunch_colleague = as.numeric(bd[,paste0(lunch_partner[a],4)])
  if (a %in% 2) bd$lunch_colleague = ifelse(bd$lunch_colleague == 2, 0, bd$lunch_colleague)
  bd$dinner_colleague = as.numeric(bd[,paste0(dinner_partner[a],4)])
  if (a %in% 2) bd$dinner_colleague = ifelse(bd$dinner_colleague == 2, 0, bd$dinner_colleague)
  
  bd$influ_menu_foyer = as.numeric(bd[,influence_foyer_menu[a]])
  bd$influ_menu_foyer = ifelse(bd$influ_menu_foyer %in% 1:2, 1, #concerne
                               ifelse( bd$influ_menu_foyer %in% 3:4, 0, NA)) # pas concerne
  bd$influ_menu_sante = as.numeric(bd[,influence_sante_menu[a]])
  bd$influ_menu_sante = ifelse(bd$influ_menu_sante %in% 1:2, 1, 
                               ifelse(  bd$influ_menu_sante %in% 3:4, 0, NA))
  bd$influ_menu_budget = as.numeric(bd[,influence_budget_menu[a]])
  bd$influ_menu_budget = ifelse(bd$influ_menu_budget %in% 1:2, 1, 
                                ifelse(  bd$influ_menu_budget %in% 3:4, 0, NA))
  bd$influ_menu_temps = as.numeric(bd[,influence_temps_menu[a]])
  bd$influ_menu_temps = ifelse(bd$influ_menu_temps %in% 1:2, 1, 
                               ifelse(   bd$influ_menu_temps %in% 3:4, 0, NA))
  
  bd$alimentation_equilibre = as.numeric(bd[,alimentation_equilibre[a]]) # 1 = tres equilibre, 4 = pas du tout equilibre
  bd$alimentation_equilibre = ifelse(bd$alimentation_equilibre  == 5, NA,bd$alimentation_equilibre)
  
  bd$alimentation_info = as.numeric(bd[,alimentation_info[a]]) # 1 = tres bien informe, 4 = tres mal informe
  bd$alimentation_info = ifelse(bd$alimentation_info  == 5, NA, bd$alimentation_info) # beaucoup de non-question/non-reponse
  
  if (a == 1) {
    bd$suf_leg_oui = NA
    bd$suf_fruit_oui = NA
   } else {
    bd$suf_leg_oui = as.numeric(bd[,opinion_suffisamment_legume[a]])
    bd$suf_leg_oui = ifelse(bd$suf_leg_oui == 1, 1,
                         ifelse(bd$suf_leg_oui ==2, 0, NA))
    bd$suf_fruit_oui = as.numeric(bd[,opinion_suffisamment_fruit[a]])
    bd$suf_fruit_oui = ifelse(bd$suf_fruit_oui == 1, 1,
                            ifelse(bd$suf_fruit_oui ==2, 0, NA))
  }
    
  bd$freq_bio = as.numeric(bd[,conso_bio[a]]) # 8 = jamais bio, 1 = 3x par jour ou +
  bd$freq_bio = ifelse(bd$freq_bio == 9, NA, bd$freq_bio)
  bd$jamais_bio = ifelse(bd$freq_bio == 8, 1,
                            ifelse(bd$freq_bio %in% 1:7, 0, NA))
  
  bd$fruit_leg_5 = as.numeric(bd[,conso_5_fruit_legume[a]]) # 1 = en mange 5 par jour, 0 = moins de 5
  bd$petit_conso_fruit = as.numeric(bd[,petit_conso_fruit[a]]) # 1 = petit consommateur
  bd$petit_conso_leg = as.numeric(bd[,petit_conso_legume[a]])
  
  
  if (a == 2) {
    bd$n_par_jour = ifelse(bd$q267s1 > 0, bd$q267s1, NA)
    bd$n_par_semaine = ifelse(bd$q267s2 > 0, bd$q267s2 / 7, NA)
    bd$n_par_mois = ifelse(bd$q267s3 > 0, bd$q267s3 / 30, NA)
    bd$opinion_n = ifelse(!is.na(bd$n_par_jour), bd$n_par_jour,
                          ifelse(!is.na(bd$n_par_semaine), bd$n_par_semaine,
                                 ifelse(!is.na(bd$n_par_mois), bd$n_par_mois, NA)))
      bd$opinion_index = ifelse(bd$opinion_n >= 5, 1, bd$opinion_n / 5)
  }
  if (a %in% c(1,3)) {
    bd$opinion_index = NA
  }
  
  
  if (a == 1) bd$annee = 1996
  if (a == 2) bd$annee = 2002
  if (a == 3)  bd$annee = 2008
  
  
bd_clean = bd[,c("ID", "pond_annee", "pond_96", "sex", "age_continu", "age_3cat",
                 "adult", "n_enfants", "educ", "revenus", "commune", "rural", "idf", "pro",
                 "breakfast_yes", "lunch_yes", "dinner_yes", 
                 "breakfast_location", "lunch_location", "dinner_location",
                 "breakfast_alone", "lunch_alone", "dinner_alone",
                 "breakfast_family", "lunch_family", "dinner_family",
                 "breakfast_friend", "lunch_friend", "dinner_friend",
                 "breakfast_colleague", "lunch_colleague", "dinner_colleague",
                 "influ_menu_foyer", "influ_menu_sante", "influ_menu_budget", "influ_menu_temps",
                 "alimentation_equilibre", "alimentation_info", "suf_leg_oui", "suf_fruit_oui",
                 "freq_bio", "jamais_bio", "fruit_leg_5", "petit_conso_fruit", "petit_conso_leg",
                 "opinion_index", "annee")]

  if (a == 1) bd96_clean = bd_clean
  if (a == 2) bd02_clean = bd_clean 
  if (a == 3) bd08_clean = bd_clean
}


# integrer les 3 annees dans la meme base.
bd_evol = rbind(bd96_clean, bd02_clean, bd08_clean)
write.csv(bd_evol, "bsn_96_02_08_harmonisee.csv")
# selectionner uniquement les majeurs et non-ruraux 
bd_evol_select = bd_evol[bd_evol$adult == 1,]
bd_evol_select = bd_evol_select[bd_evol_select$rural != 1 | bd_evol_select$idf == 1 ,]
bd_evol_select$category = paste(bd_evol_select$sex, bd_evol_select$age_3cat, bd_evol_select$educ, sep="_")
write.csv(bd_evol_select, "bsn_96_02_08_harmonisee_subset.csv")

str(bd_evol_select)

mean(bd_evol_select[bd_evol_select$educ == 1, "opinion_index"], na.rm = T)
mean(bd_evol_select[bd_evol_select$fruit_leg_5 == 0, "opinion_index"], na.rm = T)
hist(bd_evol_select[bd_evol_select$category == "1_1_1", "opinion_index"])


contrainte_foyer = prop.table(table(bd_evol_select$category, bd_evol_select$influ_menu_foyer), margin = 1)[,"1"]
contrainte_budget = prop.table(table(bd_evol_select$category, bd_evol_select$influ_menu_budget), margin = 1)[,"1"]
contrainte_temps = prop.table(table(bd_evol_select$category, bd_evol_select$influ_menu_temps), margin = 1)[,"1"]

meanNoNA = function(x){
  y = mean(x, na.rm = T)
  return(y)
}
sumNoNA = function(x){
  y = sum(x, na.rm = T)
  return(y)
}

conso_5_1996 = prop.table(table(bd_evol_select[bd_evol_select$annee == 1996, "category"], bd_evol_select[bd_evol_select$annee == 1996, "fruit_leg_5"]), margin = 1)[,"1"]
conso_5_2002 = prop.table(table(bd_evol_select[bd_evol_select$annee == 2002, "category"], bd_evol_select[bd_evol_select$annee == 2002, "fruit_leg_5"]), margin = 1)[,"1"]
conso_5_2008 = prop.table(table(bd_evol_select[bd_evol_select$annee == 2008, "category"], bd_evol_select[bd_evol_select$annee == 2008, "fruit_leg_5"]), margin = 1)[,"1"]
conso_5_all_years = prop.table(table(bd_evol_select$category, bd_evol_select$fruit_leg_5), margin = 1)[,"1"]

bd_evol_select$breakfast_ext = ifelse(bd_evol_select$breakfast_alone == 0 & 
                                      bd_evol_select$breakfast_family == 0, 1, 0)
bd_evol_select$lunch_ext = ifelse(bd_evol_select$lunch_alone == 0 &
                                      bd_evol_select$lunch_family == 0, 1, 0)
bd_evol_select$dinner_ext = ifelse(bd_evol_select$dinner_alone == 0 &
                                      bd_evol_select$dinner_family == 0, 1, 0)

social_context_breakfast = prop.table(table(bd_evol_select$category, bd_evol_select$breakfast_ext), margin = 1)[,"1"]
social_context_lunch = prop.table(table(bd_evol_select$category, bd_evol_select$lunch_ext), margin = 1)[,"1"]
social_context_dinner = prop.table(table(bd_evol_select$category, bd_evol_select$dinner_ext), margin = 1)[,"1"]

bd_evol_select$n = 1
n = table(bd_evol_select$category, bd_evol_select$n)[,"1"]
n_1996 = table(bd_evol_select[bd_evol_select$annee == 1996, "category"], bd_evol_select[bd_evol_select$annee == 1996, "n"])[,"1"]
n_2002 = table(bd_evol_select[bd_evol_select$annee == 2002, "category"], bd_evol_select[bd_evol_select$annee == 2002, "n"])[,"1"]
n_2008 = table(bd_evol_select[bd_evol_select$annee == 2008, "category"], bd_evol_select[bd_evol_select$annee == 2008, "n"])[,"1"]

sub2002 = bd_evol_select[bd_evol_select$annee == 2002,]

sub2002$opinion_index_q1 = ifelse(sub2002$opinion_index >= 0 & sub2002$opinion_index < 0.2,1,0)
sub2002$opinion_index_q2 = ifelse(sub2002$opinion_index >= 0.2 & sub2002$opinion_index < 0.4,1,0)
sub2002$opinion_index_q3 = ifelse(sub2002$opinion_index >= 0.4 & sub2002$opinion_index < 0.6,1,0)
sub2002$opinion_index_q4 = ifelse(sub2002$opinion_index >= 0.6 & sub2002$opinion_index < 0.8,1,0)
sub2002$opinion_index_q5 = ifelse(sub2002$opinion_index >= 0.8 & sub2002$opinion_index <= 1,1,0)
opinion_index_2002 =  aggregate(sub2002[,paste0("opinion_index_q", 1:5)], by = list(sub2002$category), FUN = sumNoNA)
opinion_index_2002$tot = opinion_index_2002$opinion_index_q1 + opinion_index_2002$opinion_index_q2 +
                            opinion_index_2002$opinion_index_q3 + opinion_index_2002$opinion_index_q4 +
                            opinion_index_2002$opinion_index_q5 
Init_categories = cbind(n, n_1996, n_2002, n_2008, 
                        conso_5_1996, conso_5_2002, conso_5_2008, conso_5_all_years,
                        contrainte_foyer, contrainte_budget, contrainte_temps,
                        social_context_breakfast, social_context_lunch,
                        social_context_dinner)
Init_categories = cbind(Init_categories[1:24,],opinion_index_2002)

Init_categories[,paste0("opinion_index_q", 1:5)] = Init_categories[,paste0("opinion_index_q", 1:5)] / Init_categories$tot
Init_categories$tot = NULL

write.csv(Init_categories, "initialisation_distribution_par_cat.csv")

# 
# bd_evol_select$fruit_leg_5
# plot(fruit_leg_5 ~ age_3cat , data = bd_evol_select)
# 
# hist(bd_evol_select$suf_leg_oui)
# 
# table(bd_evol_select$influ_menu_budget, bd_evol_select$revenus)
# table(bd_evol_select$influ_menu_sante, bd_evol_select$revenus)
# table(bd_evol_select$influ_menu_foyer, bd_evol_select$revenus)
# table(bd_evol_select$influ_menu_temps, bd_evol_select$revenus)
# 
# hist(bd_evol_select[bd_evol_select$influ_menu_budget == 1, "revenus"], main = "Influence Budget")
# hist(bd_evol_select[bd_evol_select$influ_menu_budget == 0, "revenus"], main = "Pas d'Influence Budget")
# hist(bd_evol_select[bd_evol_select$influ_menu_sante == 1, "revenus"], main = "Influence Sante")
# hist(bd_evol_select[bd_evol_select$influ_menu_sante == 0, "revenus"], main = "Pas d'Influence Sante")
# hist(bd_evol_select[bd_evol_select$influ_menu_foyer == 1, "revenus"], main = "Influence Foyer")
# hist(bd_evol_select[bd_evol_select$influ_menu_foyer == 0, "revenus"], main = "Pas d'Influence Foyer")
# hist(bd_evol_select[bd_evol_select$influ_menu_temps == 1, "revenus"], main = "Influence Temps")
# hist(bd_evol_select[bd_evol_select$influ_menu_temps == 0, "revenus"], main = "Pas d'Influence Temps")
# 
# hist(bd_evol_select[bd_evol_select$influ_menu_budget == 1, "educ"], main = "Influence Budget")
# hist(bd_evol_select[bd_evol_select$influ_menu_budget == 0, "educ"], main = "Pas d'Influence Budget")
# hist(bd_evol_select[bd_evol_select$influ_menu_sante == 1, "educ"], main = "Influence Sante")
# hist(bd_evol_select[bd_evol_select$influ_menu_sante == 0, "educ"], main = "Pas d'Influence Sante")
# hist(bd_evol_select[bd_evol_select$influ_menu_foyer == 1, "educ"], main = "Influence Foyer")
# hist(bd_evol_select[bd_evol_select$influ_menu_foyer == 0, "educ"], main = "Pas d'Influence Foyer")
# hist(bd_evol_select[bd_evol_select$influ_menu_temps == 1, "educ"], main = "Influence Temps")
# hist(bd_evol_select[bd_evol_select$influ_menu_temps == 0, "educ"], main = "Pas d'Influence Temps")
# 
#   hist(bd_evol_select[bd_evol_select$fruit_leg_5 == 1, "revenus"], main = "5")
# hist(bd_evol_select[bd_evol_select$fruit_leg_5 == 0, "revenus"], main = "<5")
# hist(bd_evol_select[bd_evol_select$fruit_leg_5 == 1, "educ"], main = "5")
# hist(bd_evol_select[bd_evol_select$fruit_leg_5 == 0, "educ"], main = "<5")
# 
# hist(bd_evol_select[bd_evol_select$jamais_bio == 1, "revenus"], main = "Jamais bio")
# hist(bd_evol_select[bd_evol_select$jamais_bio == 0, "revenus"], main = "Parfois bio")
# hist(bd_evol_select[bd_evol_select$jamais_bio == 1, "educ"], main = "Jamais bio")
# hist(bd_evol_select[bd_evol_select$jamais_bio == 0, "educ"], main = "Parfois bio")
# 
# 
# hist(bd_evol_select[bd_evol_select$suf_leg_oui == 1, "fruit_leg_5"], main = "consommation suffisante")
# hist(bd_evol_select[bd_evol_select$suf_leg_oui == 0, "fruit_leg_5"], main = "consommation insuffisante")
# 
# table(bd_evol_select$suf_leg_oui, bd_evol_select$fruit_leg_5)
# chisq.test(bd_evol_select$suf_leg_oui, bd_evol_select$fruit_leg_5)
# table(bd_evol_select$suf_fruit_oui, bd_evol_select$fruit_leg_5)
# chisq.test(bd_evol_select$suf_fruit_oui, bd_evol_select$fruit_leg_5)
# 
#   table(bd_evol_select$alimentation_info, bd_evol_select$alimentation_equilibre)
# summary(lm(bd_evol_select$alimentation_info ~ bd_evol_select$alimentation_equilibre))
# 
# str(bd_evol_select)
# 
# library(ggplot2)
# 
# ggplot(bd_evol_select, aes(x = annee, y = jamais_bio, group = educ, fill = educ)) + geom_point() + geom_line()
# 
#   table(bd_evol_select$fruit_leg_5, bd_evol_select$annee)
#   summary(lm(bd_evol_select$educ ~ bd_evol_select$revenus))
#   
#   summary(as.factor(bd_evol_select[bd_evol_select$annee == 1996, "category"]))
#  
#    
#   
# prop.table(table(bd_evol_select[bd_evol_select$annee == 2008, "fruit_leg_5"], bd_evol_select[bd_evol_select$annee == 2008, "category"]), margin = 2)
# 
# bd_evol_select$breakfast_ext = ifelse(bd_evol_select$breakfast_alone == 0 & 
#                                         bd_evol_select$breakfast_family == 0, 1, 0)
# 
# bd_evol_select$lunch_ext = ifelse(bd_evol_select$lunch_alone == 0 & 
#                                         bd_evol_select$lunch_family == 0, 1, 0)
# 
# bd_evol_select$dinner_ext = ifelse(bd_evol_select$dinner_alone == 0 & 
#                                         bd_evol_select$dinner_family == 0, 1, 0)
# 
# 
# bd_evol_select$incoherence = ifelse(bd_evol_select$fruit_leg_5 == 0, 
#                                   ifelse(bd_evol_select$suf_fruit_oui == 1 | 
#                                            bd_evol_select$suf_leg_oui == 1, 1, 0),0)
# 
# bd_evol_select$contrainte = ifelse(bd_evol_select$influ_menu_foyer == 1 | 
#                                     bd_evol_select$influ_menu_budget == 1 | 
#                                         bd_evol_select$influ_menu_temps == 1, 1, 0)
# 
# 
# 
# 
# 
# 
# 
# 
# 
