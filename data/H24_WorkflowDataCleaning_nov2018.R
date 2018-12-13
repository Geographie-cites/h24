library("foreign")
library("readstata13")
library("dplyr")

setwd("/Users/clementinecottineau/Desktop/bureau/CSV_Travail/")

meanNoNA = function(x){
  y = mean(x, na.rm = T)
  return(y)
}
sumNoNA = function(x){
  y = sum(x, na.rm = T)
  return(y)
}



# Matching health data tables by year
# b96_demo= read.dta13("Bsn96_CNRS_sociodem_alimhorsConso.dta")
# b96_cons= read.dta13("Bsn96_CNRS_FL_pour_evol.dta")
b96_demo= read.csv("Bsn96_CNRS_sociodem_alimhorsConso.csv")
b96_cons= read.csv("Bsn96_CNRS_FL_pour_evol.csv")
b96 = data.frame(b96_demo, b96_cons[match(b96_demo$ident, b96_cons$ident),])

# b02_demo= read.dta13("Bsn02_CNRS_sociodem_alimhorsConso.dta")
# b02_cons= read.dta13("Bsn02_CNRS_FL_pour_evol.dta")
b02_demo= read.csv("Bsn02_CNRS_sociodem_alimhorsConso.csv")
b02_cons= read.csv("Bsn02_CNRS_FL_pour_evol.csv")
b02 = data.frame(b02_demo, b02_cons[match(b02_demo$q2, b02_cons$q2),])

# b08_demo= read.dta13("Bsn08_CNRS_sociodem_alimhorsConso.dta")
# b08_cons= read.dta13("Bsn08_CNRS_FL_pour_evol.dta")
# b08_organic = read.dta13("BSN08_bio.dta")
b08_demo= read.csv("Bsn08_CNRS_sociodem_alimhorsConso.csv")
b08_cons= read.csv("Bsn08_CNRS_FL_pour_evol.csv")
b08_organic = read.csv("BSN08_bio.csv")
b08 = data.frame(b08_demo, b08_cons[match(b08_demo$ident, b08_cons$ident),])
b08 = data.frame(b08, b08_organic[match(b08$ident, b08_organic$ident),])

# list of original variable names (from Barometre Sante Nutrition surveys)
ID = c("ident", "q2", "ident")
weight_by_year = c("poidsr", "poids", "redri2")
# weight for all France by year
initial_weight = c("poidsr", "poids18", "redrg")
# weight for all France in 1996


# socio-demo variables

sex = c("q66", "q91", "sex")
# male = 1, female = 2
age = c("ageb","q92","age")
# in round numbers
edu = c("q227", "q367", "q831")
# original grouping
pro = c("q228", "situi", "q109")
# original grouping
income = c("q240", "q369uc","revuc")
# Brackets in 1996 (Francs)
# <1500, 1500-2499, 2500-3999, 4000-6599, 6600-7999, 
# 8000-9999, 10k-13k, 13k-16k, 16k-20k, 20k-30k, 30k-50k, >50k
size_com_res = c("q21", "q19", "catagg")
# 1 = rural, 2 = <20k residents, 3 = 20-100, 4 = 100-200, 5 = >200, 6 = "agglomeration parisienne" ()
region = c("q25", "q26", "dep")
deps_idf = c(75, 77, 78, 91, 92, 93, 94, 95)
# id of districts included in the Paris region to aggregate


# social context of meals

breakfast_take = c("q78", "q119", "q134")
# = yes when the person had breakfast
breakfast_location = c("q79", "q120", "q135")
breakfast_partner = c("q80i", "q121i", "q137_")
# multiple answer possible
lunch_take = c("q99", "q143", "q224")
lunch_location = c("q100", "q144", "q225")
lunch_partner = c("q101i","q145i","q227_")
dinner_take = c("q143", "q191", "q441")
dinner_location = c("q144", "q192", "q442")
dinner_partner = c("q145i", "q193i", "q444_")


# opinion

impact_habits = c("q198s1", "q253s1", "q694_1")
# question: Do your household habits impact the composition of your meal
# answer in Lickert scale: very much, rather, rather not, not at all.
impact_health = c("q198s2", "q253s2", "q694_2")
# question: Do health considerations impact the composition of your meal
# answer in Lickert scale
impact_budget = c("q198s3", "q253s3", "q694_3")
# question: Does your budget impact the composition of your meal
# answer in Lickert scale
impact_time = c("q198s4", "q253s4", "q694_4")
# question: Does time impact the composition of your meal
# answer in Lickert scale

healthy_diet = c("q199", "q254", "q695")
info_about_diet = c("q205", "q257", "q698")
enough_fruit = c(NA, "q270", "q709")
enough_vegetable = c(NA, "q268", "q707")


# consumption

organic_consumption = c("q192s11", "q247s18", "q679_5")
# Lickert scale in 9 categories

cons_5aday = c("pr_frlgr2", "pr_frlgr2", "pr_frlegevr2")
# did the person eat 5 fruit and veg a day the day before the survey (reconstructed var)
# 1 = yes, 5 or more
# 2 = no, less than 5 a day
low_consumption_fruit = c("pr_fruipc", "pr_fruipc", "pr_frevpc")
# did the person eat 0 or 1 fruit the day before the survey (reconstructed var)
# 1 = yes
# 2 = no
low_consumption_veg = c("pr_legpc", "pr_legpc", "pr_legevpc")
# did the person eat 0 or 1 veg the day before the survey (reconstructed var)
# 1 = yes
# 2 = no

# Select, harmonise and rename variables of interest for all 3 years
for(a in 1:3){ # 1 = 1996, 2= 2002, 3=2008
  if (a == 1) bd = b96
  if (a == 2) bd = b02
  if (a == 3) bd = b08
  bd$ID = bd[,ID[a]]
  bd$pond_annee = bd[,weight_by_year[a]]
  bd$pond_96 = bd[,initial_weight[a]]
  bd$sex = as.numeric(bd[,sex[a]])
  bd$pro = as.numeric(bd[,pro[a]])
  if (a == 1) {
    bd$pro = ifelse(bd$pro %in% 1:3, 1, # active
                     ifelse(bd$pro %in% c(4, 5, 13), 2, # unemployed
                        ifelse(bd$pro == 14, 3, # studying
                            ifelse(bd$pro %in% c(6:12, 15, 16), 4, NA)))) #not active
  }
  if (a == 2) {
    bd$pro = ifelse(bd$pro %in% 1:2, 1,# active
                    ifelse(bd$pro == 3, 2, # unemployed
                           ifelse(bd$pro == 4, 3, # studying
                                  ifelse(bd$pro %in% c(5,6), 4, NA)))) #not active
  }
  if (a == 3) {
    bd$pro = ifelse(bd$pro %in% 1:5, 1, # active
                    ifelse(bd$pro == 9, 2, # unemployed
                           ifelse(bd$pro %in% c(6, 12), 3, # studying
                                  ifelse(bd$pro %in% c(7, 8, 10, 11, 13), 4, NA)))) #not active
  }
  
  bd$age_continuous = bd[,age[a]]
  bd$age_3cat = ifelse(bd$age_continuous %in% 15:29, 1,
                       ifelse(bd$age_continuous %in% 30:59, 2,
                              ifelse(bd$age_continuous >= 60, 3,0)))
  bd$adult = ifelse(bd$age_continuous >= 15, 1, 0)
  
  if (a == 1) household = bd[,paste0("q70s", 1:8)]
  if (a == 2) household = bd[,paste0("q102s", 1:12)]
  if (a == 3) household = bd[,paste0("q66_", 1:13)]
  # compter le nombre d'child
  child = ifelse(household > 0 & household <= 15,1,0)
  bd$n_child = rowSums(child, na.rm = T)
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
      bd$income_4cat = ifelse(bd$income %in% 1:3, 1,
                       ifelse(bd$income %in% 4:6, 2,
                          ifelse(bd$income %in% 7:8, 3,
                              ifelse(bd$income %in% 9:12, 4, NA))))
    }
    if (a == 2) {
      bd$income_4cat = ifelse(bd$income < 4000, 1, #### /!\ Careful! Probably not harmonised between francs and euros
                       ifelse(bd$income >= 4000 & bd$income < 10000, 2,
                         ifelse(bd$income >= 10000 &  bd$income < 16000, 3,
                              ifelse(bd$income >= 16000, 4, NA))))
    }
 
    if (a == 3) {
      bd$income = bd$income * 12 #### /!\ Careful! Probably not harmonised between francs and euros
      bd$income_4cat = ifelse(bd$income < 4000, 1,
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
  
  # if the person didn't share their meal with someone other than their family
  bd$breakfast_ext = ifelse(bd$breakfast_alone == 0 & 
                              bd$breakfast_family == 0, 1, 0)
  bd$lunch_ext = ifelse(bd$lunch_alone == 0 &
                          bd$lunch_family == 0, 1, 0)
  bd$dinner_ext = ifelse(bd$dinner_alone == 0 &
                           bd$dinner_family == 0, 1, 0)
  
  
  bd$impact_habits = as.numeric(bd[,impact_habits[a]])
  bd$impact_habits = ifelse(bd$impact_habits %in% 1:2, 1, #concerne
                               ifelse( bd$impact_habits %in% 3:4, 0, NA)) # pas concerne
  bd$impact_health = as.numeric(bd[,impact_health[a]])
  bd$impact_health = ifelse(bd$impact_health %in% 1:2, 1, 
                               ifelse(  bd$impact_health %in% 3:4, 0, NA))
  bd$impact_budget = as.numeric(bd[,impact_budget[a]])
  bd$impact_budget = ifelse(bd$impact_budget %in% 1:2, 1, 
                                ifelse(  bd$impact_budget %in% 3:4, 0, NA))
  bd$impact_time = as.numeric(bd[,impact_time[a]])
  bd$impact_time = ifelse(bd$impact_time %in% 1:2, 1, 
                               ifelse(   bd$impact_time %in% 3:4, 0, NA))
  
  bd$healthy_diet = as.numeric(bd[,healthy_diet[a]]) # 1 = very balanced, 4 = not at all balanced
  bd$healthy_diet = ifelse(bd$healthy_diet  == 5, NA,bd$healthy_diet)
  
  bd$info_about_diet = as.numeric(bd[,info_about_diet[a]]) # 1 = very well informed, 4 = not informed at all
  bd$info_about_diet = ifelse(bd$info_about_diet  == 5, NA, bd$info_about_diet) # many NA
  
  if (a == 1) {
    bd$enough_veg_yes = NA
    bd$enough_fruit_yes = NA
   } else {
    bd$enough_veg_yes = as.numeric(bd[,enough_vegetable[a]])
    bd$enough_veg_yes = ifelse(bd$enough_veg_yes == 1, 1,
                         ifelse(bd$enough_veg_yes ==2, 0, NA))
    bd$enough_fruit_yes = as.numeric(bd[,enough_fruit[a]])
    bd$enough_fruit_yes = ifelse(bd$enough_fruit_yes == 1, 1,
                            ifelse(bd$enough_fruit_yes ==2, 0, NA))
  }
    
  bd$freq_organic_consumption = as.numeric(bd[,organic_consumption[a]]) # 8 = jamais bio, 1 = 3x par jour ou +
  bd$freq_organic_consumption = ifelse(bd$freq_organic_consumption == 9, NA, bd$freq_organic_consumption)
  bd$never_organic = ifelse(bd$freq_organic_consumption == 8, 1,
                            ifelse(bd$freq_organic_consumption %in% 1:7, 0, NA))
  
  bd$cons_5aday = as.numeric(bd[,cons_5aday[a]]) # 1 = en mange 5 par jour, 0 = moins de 5
  bd$low_consumption_fruit = as.numeric(bd[,low_consumption_fruit[a]]) # 1 = petit consommateur
  bd$low_consumption_veg = as.numeric(bd[,low_consumption_veg[a]])
  
  summary(as.factor(bd$n_per_day))
  if (a == 2) {
    bd$n_per_day = ifelse(bd$q267s1 > 0, bd$q267s1, NA)
    bd$n_per_week = ifelse(bd$q267s2 > 0, bd$q267s2 / 7, NA)
    bd$n_per_month = ifelse(bd$q267s3 > 0, bd$q267s3 / 30, NA)
    bd$opinion_n = ifelse(!is.na(bd$n_per_day), bd$n_per_day,
                          ifelse(!is.na(bd$n_per_week), bd$n_per_week,
                                 ifelse(!is.na(bd$n_per_month), bd$n_per_month, NA)))
      bd$opinion_index = ifelse(bd$opinion_n >= 5, 1, bd$opinion_n / 5)
  }
  if (a %in% c(1,3)) {
    bd$opinion_index = NA
  }

  
  
  
  if (a == 1) bd$annee = 1996
  if (a == 2) bd$annee = 2002
  if (a == 3)  bd$annee = 2008
  
  
bd_clean = bd[,c("ID", "pond_annee", "pond_96", "sex", "age_continuous", "age_3cat",
                 "adult", "n_child", "educ", "income_4cat", "commune", "rural", "idf", "pro",
                 "breakfast_yes", "lunch_yes", "dinner_yes", 
                 "breakfast_location", "lunch_location", "dinner_location",
                 "breakfast_ext", "lunch_ext", "dinner_ext",
                 "impact_habits", "impact_health", "impact_budget", "impact_time",
                 "healthy_diet", "info_about_diet", "enough_veg_yes", "enough_fruit_yes",
                 "freq_organic_consumption", "never_organic", "cons_5aday", "low_consumption_fruit", "low_consumption_veg",
                 "opinion_index", "annee")]

  if (a == 1) bd96_clean = bd_clean
  if (a == 2) bd02_clean = bd_clean 
  if (a == 3) bd08_clean = bd_clean
}


# Integrate 3 database-year / 2 database-year in one
bd_evol = rbind(bd96_clean, bd02_clean, bd08_clean)
bd_evol$category = paste(bd_evol$sex, bd_evol$age_3cat, bd_evol$educ, sep="_")
bd_evol$category_an = paste( bd_evol$annee,bd_evol$sex, bd_evol$age_3cat, bd_evol$educ, sep="_")
bd_evol$categshort = paste( bd_evol$annee, bd_evol$sex, bd_evol$age_3cat, sep="_")
bd_evol$consumption_fruitandveg_SupUn = ifelse (bd_evol$low_consumption_fruit==1 & bd_evol$low_consumption_veg==1,0,1)

write.csv(bd_evol, "bsn_96_02_08_harmonised.csv")


bd_evol_0208 = bd_evol[bd_evol$annee != 1996,]
write.csv(bd_evol_0208, "bsn_02_08_harmonised.csv")


# Select only respondents over 15 year old, not in rural municipalities outside the Paris region, excluing NA.
bd_evol_select = bd_evol[bd_evol$adult == 1,]
bd_evol_select = bd_evol_select[bd_evol_select$rural != 1 | bd_evol_select$idf == 1 ,]
bd_evol_select = bd_evol_select[!is.na(bd_evol_select$educ),]

write.csv(bd_evol_select, "bsn_96_02_08_harmonised_subset.csv")

bd_evol_0208_select = bd_evol_0208[bd_evol_0208$adult == 1,]
bd_evol_0208_select = bd_evol_0208_select[bd_evol_0208_select$rural != 1 | bd_evol_0208_select$idf == 1 ,]
bd_evol_0208_select = bd_evol_0208_select[!is.na(bd_evol_0208_select$educ),]

write.csv(bd_evol_select, "bsn_02_08_harmonised_subset.csv")


########  define initialisation variables by category : en supprimant 1996
habit_constraint = prop.table(table(bd_evol_0208_select$category, bd_evol_0208_select$impact_habits), margin = 1)[,"1"]
budget_constraint = prop.table(table(bd_evol_0208_select$category, bd_evol_0208_select$impact_budget), margin = 1)[,"1"]
time_constraint = prop.table(table(bd_evol_0208_select$category, bd_evol_0208_select$impact_time), margin = 1)[,"1"]

#habit_constraint_2002 = prop.table(table(bd_evol_0208_select[bd_evol_0208_select$annee == 2002, "category"], bd_evol_0208_select[bd_evol_0208_select$annee == 2002, "impact_habits"]), margin = 1)[,"1"]
#budget_constraint_2002 = prop.table(table(bd_evol_0208_select[bd_evol_0208_select$annee == 2002, "category"], bd_evol_0208_select[bd_evol_0208_select$annee == 2002, "impact_budget"]), margin = 1)[,"1"]
#time_constraint_2002 = prop.table(table(bd_evol_0208_select[bd_evol_0208_select$annee == 2002, "category"], bd_evol_0208_select[bd_evol_0208_select$annee == 2002, "impact_time"]), margin = 1)[,"1"]

conso_5_2002 = prop.table(table(bd_evol_0208_select[bd_evol_0208_select$annee == 2002, "category"], bd_evol_0208_select[bd_evol_0208_select$annee == 2002, "cons_5aday"]), margin = 1)[,"1"]
conso_5_2008 = prop.table(table(bd_evol_0208_select[bd_evol_0208_select$annee == 2008, "category"], bd_evol_0208_select[bd_evol_0208_select$annee == 2008, "cons_5aday"]), margin = 1)[,"1"]
conso_5_2002_2008 = prop.table(table(bd_evol_0208_select$category, bd_evol_0208_select$cons_5aday), margin = 1)[,"1"]


social_context_breakfast = prop.table(table(bd_evol_0208_select$category, bd_evol_0208_select$breakfast_ext), margin = 1)[,"1"]
social_context_lunch = prop.table(table(bd_evol_0208_select$category, bd_evol_0208_select$lunch_ext), margin = 1)[,"1"]
social_context_dinner = prop.table(table(bd_evol_0208_select$category, bd_evol_0208_select$dinner_ext), margin = 1)[,"1"]

#social_context_breakfast_2002 = prop.table(table(bd_evol_0208_select[bd_evol_0208_select$annee == 2002, "category"], bd_evol_0208_select[bd_evol_0208_select$annee == 2002, "breakfast_ext"]), margin = 1)[,"1"]
#social_context_lunch_2002 = prop.table(table(bd_evol_0208_select[bd_evol_0208_select$annee == 2002, "category"], bd_evol_0208_select[bd_evol_0208_select$annee == 2002, "lunch_ext"]), margin = 1)[,"1"]
#social_context_dinner_2002 = prop.table(table(bd_evol_0208_select[bd_evol_0208_select$annee == 2002, "category"], bd_evol_0208_select[bd_evol_0208_select$annee == 2002, "dinner_ext"]), margin = 1)[,"1"]

bd_evol_0208_select$n = 1
n = table(bd_evol_0208_select$category, bd_evol_0208_select$n)[,"1"]
n_2002 = table(bd_evol_0208_select[bd_evol_0208_select$annee == 2002, "category"], bd_evol_0208_select[bd_evol_0208_select$annee == 2002, "n"])[,"1"]
n_2008 = table(bd_evol_0208_select[bd_evol_0208_select$annee == 2008, "category"], bd_evol_0208_select[bd_evol_0208_select$annee == 2008, "n"])[,"1"]


sub2002H = bd_evol_0208_select[bd_evol_0208_select$annee == 2002 & bd_evol_0208_select$cons_5aday == 1,]
sub2002U = bd_evol_0208_select[bd_evol_0208_select$annee == 2002 & bd_evol_0208_select$cons_5aday == 0,]

sub2002H$opinion_index_Hq1 = ifelse(sub2002H$opinion_index >= 0 & sub2002H$opinion_index < 0.2,1,0)
sub2002H$opinion_index_Hq2 = ifelse(sub2002H$opinion_index >= 0.2 & sub2002H$opinion_index < 0.4,1,0)
sub2002H$opinion_index_Hq3 = ifelse(sub2002H$opinion_index >= 0.4 & sub2002H$opinion_index < 0.6,1,0)
sub2002H$opinion_index_Hq4 = ifelse(sub2002H$opinion_index >= 0.6 & sub2002H$opinion_index < 0.8,1,0)
sub2002H$opinion_index_Hq5 = ifelse(sub2002H$opinion_index >= 0.8 & sub2002H$opinion_index <= 1,1,0)

sub2002U$opinion_index_Uq1 = ifelse(sub2002U$opinion_index >= 0 & sub2002U$opinion_index < 0.2,1,0)
sub2002U$opinion_index_Uq2 = ifelse(sub2002U$opinion_index >= 0.2 & sub2002U$opinion_index < 0.4,1,0)
sub2002U$opinion_index_Uq3 = ifelse(sub2002U$opinion_index >= 0.4 & sub2002U$opinion_index < 0.6,1,0)
sub2002U$opinion_index_Uq4 = ifelse(sub2002U$opinion_index >= 0.6 & sub2002U$opinion_index < 0.8,1,0)
sub2002U$opinion_index_Uq5 = ifelse(sub2002U$opinion_index >= 0.8 & sub2002U$opinion_index <= 1,1,0)

opinion_index_2002H =  aggregate(sub2002H[,paste0("opinion_index_Hq", 1:5)], by = list(sub2002H$category), FUN = sumNoNA)
opinion_index_2002U =  aggregate(sub2002U[,paste0("opinion_index_Uq", 1:5)], by = list(sub2002U$category), FUN = sumNoNA)
opinion_index_2002H$totH = opinion_index_2002H$opinion_index_Hq1 + opinion_index_2002H$opinion_index_Hq2 +
  opinion_index_2002H$opinion_index_Hq3 + opinion_index_2002H$opinion_index_Hq4 +
  opinion_index_2002H$opinion_index_Hq5 
opinion_index_2002U$totU = opinion_index_2002U$opinion_index_Uq1 + opinion_index_2002U$opinion_index_Uq2 +
  opinion_index_2002U$opinion_index_Uq3 + opinion_index_2002U$opinion_index_Uq4 +
  opinion_index_2002U$opinion_index_Uq5 


Init_categories = cbind(n, n_2002, n_2008, 
                        conso_5_2002, conso_5_2008, conso_5_2002_2008,
                        habit_constraint, budget_constraint, time_constraint,
                        social_context_breakfast, social_context_lunch, social_context_dinner)

Init_categories = cbind(Init_categories,opinion_index_2002H,opinion_index_2002U)

Init_categories[,paste0("opinion_index_Hq", 1:5)] = Init_categories[,paste0("opinion_index_Hq", 1:5)] / Init_categories$totH
Init_categories[,paste0("opinion_index_Uq", 1:5)] = Init_categories[,paste0("opinion_index_Uq", 1:5)] / Init_categories$totU
Init_categories$totH = NULL
Init_categories$totU = NULL
category_var = data.frame(1:18)
category_var$Sex = ifelse(substr(rownames(Init_categories), 1, 1) == "1", 1, 2)
category_var$Age = ifelse(substr(rownames(Init_categories), 3, 3) == "1", 1,
                          ifelse(substr(rownames(Init_categories), 3, 3) == "2", 2, 3))
category_var$Edu = ifelse(substr(rownames(Init_categories), 5, 5) == "1", 1,
                          ifelse(substr(rownames(Init_categories), 5, 5) == "2", 2, 3))
category_var$X1.18 = NULL
Init_categories = cbind(category_var,Init_categories)
Init_categories$Group.1 = NULL
rownames(Init_categories) = NULL
write.csv(Init_categories, "initialisation_distribution_per_cat_2002_2008.csv")


########  define initialisation variables by category : avec initialisation avec 1996
habit_constraint = prop.table(table(bd_evol_select$category, bd_evol_select$impact_habits), margin = 1)[,"1"]
budget_constraint = prop.table(table(bd_evol_select$category, bd_evol_select$impact_budget), margin = 1)[,"1"]
time_constraint = prop.table(table(bd_evol_select$category, bd_evol_select$impact_time), margin = 1)[,"1"]

conso_5_1996 = prop.table(table(bd_evol_select[bd_evol_select$annee == 1996, "category"], bd_evol_select[bd_evol_select$annee == 1996, "cons_5aday"]), margin = 1)[,"1"]
conso_5_2002 = prop.table(table(bd_evol_select[bd_evol_select$annee == 2002, "category"], bd_evol_select[bd_evol_select$annee == 2002, "cons_5aday"]), margin = 1)[,"1"]
conso_5_2008 = prop.table(table(bd_evol_select[bd_evol_select$annee == 2008, "category"], bd_evol_select[bd_evol_select$annee == 2008, "cons_5aday"]), margin = 1)[,"1"]
conso_5_all_years = prop.table(table(bd_evol_select$category, bd_evol_select$cons_5aday), margin = 1)[,"1"]

social_context_breakfast = prop.table(table(bd_evol_select$category, bd_evol_select$breakfast_ext), margin = 1)[,"1"]
social_context_lunch = prop.table(table(bd_evol_select$category, bd_evol_select$lunch_ext), margin = 1)[,"1"]
social_context_dinner = prop.table(table(bd_evol_select$category, bd_evol_select$dinner_ext), margin = 1)[,"1"]

bd_evol_select$n = 1
n = table(bd_evol_select$category, bd_evol_select$n)[,"1"]
n_1996 = table(bd_evol_select[bd_evol_select$annee == 1996, "category"], bd_evol_select[bd_evol_select$annee == 1996, "n"])[,"1"]
n_2002 = table(bd_evol_select[bd_evol_select$annee == 2002, "category"], bd_evol_select[bd_evol_select$annee == 2002, "n"])[,"1"]
n_2008 = table(bd_evol_select[bd_evol_select$annee == 2008, "category"], bd_evol_select[bd_evol_select$annee == 2008, "n"])[,"1"]


sub2002H = bd_evol_select[bd_evol_select$annee == 2002 & bd_evol_select$cons_5aday == 1,]
sub2002U = bd_evol_select[bd_evol_select$annee == 2002 & bd_evol_select$cons_5aday == 0,]

sub2002H$opinion_index_Hq1 = ifelse(sub2002H$opinion_index >= 0 & sub2002H$opinion_index < 0.2,1,0)
sub2002H$opinion_index_Hq2 = ifelse(sub2002H$opinion_index >= 0.2 & sub2002H$opinion_index < 0.4,1,0)
sub2002H$opinion_index_Hq3 = ifelse(sub2002H$opinion_index >= 0.4 & sub2002H$opinion_index < 0.6,1,0)
sub2002H$opinion_index_Hq4 = ifelse(sub2002H$opinion_index >= 0.6 & sub2002H$opinion_index < 0.8,1,0)
sub2002H$opinion_index_Hq5 = ifelse(sub2002H$opinion_index >= 0.8 & sub2002H$opinion_index <= 1,1,0)

sub2002U$opinion_index_Uq1 = ifelse(sub2002U$opinion_index >= 0 & sub2002U$opinion_index < 0.2,1,0)
sub2002U$opinion_index_Uq2 = ifelse(sub2002U$opinion_index >= 0.2 & sub2002U$opinion_index < 0.4,1,0)
sub2002U$opinion_index_Uq3 = ifelse(sub2002U$opinion_index >= 0.4 & sub2002U$opinion_index < 0.6,1,0)
sub2002U$opinion_index_Uq4 = ifelse(sub2002U$opinion_index >= 0.6 & sub2002U$opinion_index < 0.8,1,0)
sub2002U$opinion_index_Uq5 = ifelse(sub2002U$opinion_index >= 0.8 & sub2002U$opinion_index <= 1,1,0)

opinion_index_2002H =  aggregate(sub2002H[,paste0("opinion_index_Hq", 1:5)], by = list(sub2002H$category), FUN = sumNoNA)
opinion_index_2002U =  aggregate(sub2002U[,paste0("opinion_index_Uq", 1:5)], by = list(sub2002U$category), FUN = sumNoNA)
opinion_index_2002H$totH = opinion_index_2002H$opinion_index_Hq1 + opinion_index_2002H$opinion_index_Hq2 +
  opinion_index_2002H$opinion_index_Hq3 + opinion_index_2002H$opinion_index_Hq4 +
  opinion_index_2002H$opinion_index_Hq5 
opinion_index_2002U$totU = opinion_index_2002U$opinion_index_Uq1 + opinion_index_2002U$opinion_index_Uq2 +
  opinion_index_2002U$opinion_index_Uq3 + opinion_index_2002U$opinion_index_Uq4 +
  opinion_index_2002U$opinion_index_Uq5 


Init_categories = cbind(n, n_1996, n_2002, n_2008, 
                        conso_5_1996, conso_5_2002, conso_5_2008, conso_5_all_years,
                        habit_constraint, budget_constraint, time_constraint,
                        social_context_breakfast, social_context_lunch,
                        social_context_dinner)

Init_categories = cbind(Init_categories,opinion_index_2002H,opinion_index_2002U)

Init_categories[,paste0("opinion_index_Hq", 1:5)] = Init_categories[,paste0("opinion_index_Hq", 1:5)] / Init_categories$totH
Init_categories[,paste0("opinion_index_Uq", 1:5)] = Init_categories[,paste0("opinion_index_Uq", 1:5)] / Init_categories$totU
Init_categories$totH = NULL
Init_categories$totU = NULL
category_var = data.frame(1:18)
category_var$Sex = ifelse(substr(rownames(Init_categories), 1, 1) == "1", 1, 2)
category_var$Age = ifelse(substr(rownames(Init_categories), 3, 3) == "1", 1,
                          ifelse(substr(rownames(Init_categories), 3, 3) == "2", 2, 3))
category_var$Edu = ifelse(substr(rownames(Init_categories), 5, 5) == "1", 1,
                          ifelse(substr(rownames(Init_categories), 5, 5) == "2", 2, 3))
category_var$X1.18 = NULL
Init_categories = cbind(category_var,Init_categories)
Init_categories$Group.1 = NULL
rownames(Init_categories) = NULL
write.csv(Init_categories, "initialisation_distribution_per_cat.csv")






##ANALYSEJULIE


########## 5 fruits et l?gumes ################

##Evolution sur les trois ans -et par cat?gorie (Cinq fruits et l?gumes)

tab_cons_5aday<-table(bd_evol_select$cons_5aday, bd_evol_select$annee)
prop.table(tab_cons_5aday,margin=2)
prop.table(table(bd_evol_select[bd_evol_select$annee == 2008, "cons_5aday"], bd_evol_select[bd_evol_select$annee == 2008, "category"]), margin = 2)
prop.table(table(bd_evol_select[bd_evol_select$annee == 2002, "cons_5aday"], bd_evol_select[bd_evol_select$annee == 2002, "category"]), margin = 2)
prop.table(table(bd_evol_select[bd_evol_select$annee == 1996, "cons_5aday"], bd_evol_select[bd_evol_select$annee == 1996, "category"]), margin = 2)


##Measure of social inequality (Cinq fruits et l?gumes) - population distribution 1996 / 2002 / 2008


PrepSocIneq0 <- bd_evol_select %>%
  group_by(category_an, categshort, annee, sex, age_3cat, educ, cons_5aday) %>%
  summarise(Tot = n(), Tot_pd96 = sum (pond_96), Tot_pdan = sum (pond_annee)) 

PrepSocIneq1 <-reshape2::dcast(data = PrepSocIneq0, 
                                    formula = category_an + categshort + annee + sex + age_3cat + educ  ~ cons_5aday, 
                                    value.var = "Tot",fun.aggregate = sum)
  colnames(PrepSocIneq1)[c(7,8)]<- c("NBunhealthy", "NBhealthy")

PrepSocIneq2 <-reshape2::dcast(data = PrepSocIneq0, 
                                   formula = category_an   ~ cons_5aday, 
                                   value.var = "Tot_pd96",fun.aggregate = sum)
  colnames(PrepSocIneq2)[c(2,3)]<- c("NBunhealthy_pd96", "NBhealthy_pd96")

PrepSocIneq3 <-reshape2::dcast(data = PrepSocIneq0, 
                                    formula = category_an ~ cons_5aday, 
                                    value.var = "Tot_pdan",fun.aggregate = sum)
  colnames(PrepSocIneq3)[c(2,3)]<- c("NBunhealthy_pdan", "NBhealthy_pdan")

PrepSocIneq4 <- full_join(PrepSocIneq1, PrepSocIneq2, by = "category_an")
PrepSocIneq <- full_join(PrepSocIneq4, PrepSocIneq3, by = "category_an")
rm (PrepSocIneq0,PrepSocIneq1, PrepSocIneq2, PrepSocIneq3, PrepSocIneq4)


PrepSocIneq$Tot <- PrepSocIneq$NBhealthy+PrepSocIneq$NBunhealthy
PrepSocIneq$Tot_pd96 <- PrepSocIneq$NBhealthy_pd96+PrepSocIneq$NBunhealthy_pd96
PrepSocIneq$Tot_pdan <- PrepSocIneq$NBhealthy_pdan+PrepSocIneq$NBunhealthy_pdan
##PrepSocIneq$Pop2012IDF <- c (0.032803071,0.018921943,0.016376739,0.131945784,0.081081621,0.076213344,0.052093723,0.032139249,0.032265131,0.032832828,0.022836182,0.015121341,0.135555969,0.099735879,0.068748432,0.065459075,0.049409044,0.036460647)
PrepSocIneq$Pop2012IDF <- c (0.032918436, 0.018984796, 0.016421921, 0.131994532, 0.081230533, 0.076242946,0.051952471,0.032066553,0.032317892,0.032815059,0.022885573,0.015088061, 0.135362868, 0.099710323,0.068636633,0.065394212,0.049427567,0.036549624)
Tot2012IDF <- 8164270

head(PrepSocIneq)
df <- PrepSocIneq[,c('annee', 'sex', 'age_3cat', 'educ', 'NBhealthy', 'Tot', 'Pop2012IDF')]
colnames(df) <- c('year', 'sex', 'age', 'edu', 'healthy', 'totSurvey', 'PondIDF')
df$shareHealthy <- df$healthy / df$totSurvey
df$totIDF <- df$PondIDF * Tot2012IDF
df$healthyIDF <- df$totIDF * df$shareHealthy
df$unhealthyIDF <- df$totIDF - df$healthyIDF

guido <- function(nedu1, nedu2, nedu3, nhedu1, nhedu2, nhedu3){
  n <- nedu1 + nedu2 + nedu3
  rankEdu3 <- nedu3/2
  rankEdu2 <- (2*nedu3 + nedu2)/2
  rankEdu1 <- (n + nedu3 + nedu2)/2
  zing <- function(r){
    ((n + 1) / 2) - r
  }
 e <- (8/(n^2))*(zing(rankEdu3)*nhedu3 + zing(rankEdu2)*nhedu2 + zing(rankEdu1)*nhedu1)
 return(e)
}

y=2002
for (y in c(1996, 2002, 2008)){
  taby <- df[which(df$year == y),]
  nedu1 <- sum(taby[which(taby$edu == 1),"totIDF"])
  nedu2 <- sum(taby[which(taby$edu == 2),"totIDF"])
  nedu3 <- sum(taby[which(taby$edu == 3),"totIDF"])
  nhedu1 <- sum(taby[which(taby$edu == 1),"healthyIDF"])
  nhedu2 <- sum(taby[which(taby$edu == 2),"healthyIDF"])
  nhedu3 <- sum(taby[which(taby$edu == 3),"healthyIDF"])
  e <- guido(nedu1, nedu2, nedu3, nhedu1, nhedu2, nhedu3)
  print(paste(nedu1, nedu2, nedu3, nhedu1, nhedu2, nhedu3))
  print(paste(y, e))
}

(nhedu1+nhedu2+nhedu3)/Tot2012IDF



PrepSocIneq$PCThealthy <- ifelse (PrepSocIneq$NBhealthy==0, 
                                       (PrepSocIneq$NBhealthy+1) / (PrepSocIneq$Tot+1),
                                       PrepSocIneq$NBhealthy / PrepSocIneq$Tot)
PrepSocIneq$PCThealthy_pd96 <- ifelse (PrepSocIneq$NBhealthy_pd96==0, 
                                  (PrepSocIneq$NBhealthy_pd96+1) / (PrepSocIneq$Tot_pd96+1),
                                  PrepSocIneq$NBhealthy_pd96 / PrepSocIneq$Tot_pd96)

PrepSocIneq$PCThealthy_pdan <- ifelse (PrepSocIneq$NBhealthy_pdan==0, 
                                       (PrepSocIneq$NBhealthy_pdan+1) / (PrepSocIneq$Tot_pdan+1),
                                       PrepSocIneq$NBhealthy_pdan / PrepSocIneq$Tot_pdan)                                     

PrepSocIneq$PCThealthy_Educ3 <- ifelse (PrepSocIneq$educ==3, PrepSocIneq$PCThealthy,NA)
PrepSocIneq$PCThealthy_pd96_Educ3 <- ifelse (PrepSocIneq$educ==3, PrepSocIneq$PCThealthy_pd96,NA)
PrepSocIneq$PCThealthy_pdan_Educ3 <- ifelse (PrepSocIneq$educ==3, PrepSocIneq$PCThealthy_pdan, NA)   

PrepSocIneq$PCThealthy_Educ1 <- ifelse (PrepSocIneq$educ==1, PrepSocIneq$PCThealthy,NA)
PrepSocIneq$PCThealthy_pd96_Educ1 <- ifelse (PrepSocIneq$educ==1, PrepSocIneq$PCThealthy_pd96,NA)
PrepSocIneq$PCThealthy_pdan_Educ1 <- ifelse (PrepSocIneq$educ==1, PrepSocIneq$PCThealthy_pdan, NA) 

CategTot <- PrepSocIneq %>%
  group_by(categshort) %>%
  summarise(annee = mean (annee),
            Tot = sum (Tot), Tot_pd96 = sum (Tot_pd96), Tot_pdan = sum (Tot_pdan),
            Pop2012IDF = sum(Pop2012IDF),
            NBhealthy  = sum (NBhealthy), NBhealthy_pd96  = sum (NBhealthy_pd96), NBhealthy_pdan  = sum (NBhealthy_pdan),
            PCThealthy_Educ3 = sum (PCThealthy_Educ3, na.rm = TRUE), 
            PCThealthy_Educ1= sum (PCThealthy_Educ1, na.rm = TRUE),
            PCThealthy_pd96_Educ3= sum (PCThealthy_pd96_Educ3, na.rm = TRUE),
            PCThealthy_pd96_Educ1= sum (PCThealthy_pd96_Educ1, na.rm = TRUE),
            PCThealthy_pdan_Educ3= sum (PCThealthy_pdan_Educ3, na.rm = TRUE),
            PCThealthy_pdan_Educ1= sum (PCThealthy_pdan_Educ1, na.rm = TRUE))
            
CategTot$ratioEduc_PCThealthy <- CategTot$PCThealthy_Educ3/ CategTot$PCThealthy_Educ1
CategTot$ratioEduc_PCThealthy_pd96 <- CategTot$PCThealthy_pd96_Educ3/ CategTot$PCThealthy_pd96_Educ1
CategTot$ratioEduc_PCThealthy_pdan <- CategTot$PCThealthy_pdan_Educ3/ CategTot$PCThealthy_pdan_Educ1

CategTot$ratioEduc_PCThealthy_Tot <- CategTot$ratioEduc_PCThealthy * CategTot$Tot
CategTot$ratioEduc_PCThealthy_pd96_Totpd96 <- CategTot$ratioEduc_PCThealthy_pd96 * CategTot$Tot_pd96
CategTot$ratioEduc_PCThealthy_pdan_Totpdan <- CategTot$ratioEduc_PCThealthy_pdan * CategTot$Tot_pdan

CategTot$ratioEduc_PCThealthy_popIDF2012 <- CategTot$ratioEduc_PCThealthy * CategTot$Pop2012IDF
CategTot$ratioEduc_PCThealthy_pdan_popIDF2012 <- CategTot$ratioEduc_PCThealthy_pdan * CategTot$Pop2012IDF


FIN <- CategTot %>%
  group_by(annee) %>% 
  summarise (Tot = sum (Tot), Tot_pd96 = sum (Tot_pd96), Tot_pdan = sum (Tot_pdan),
             NBhealthy  = sum (NBhealthy), NBhealthy_pd96  = sum (NBhealthy_pd96), NBhealthy_pdan  = sum (NBhealthy_pdan),
             SocIneq = sum (ratioEduc_PCThealthy_Tot / Tot),
             SocIneq_pd96 = sum (ratioEduc_PCThealthy_pd96_Totpd96 / Tot_pd96),
             SocIneq_pdan = sum (ratioEduc_PCThealthy_pdan_Totpdan /Tot_pdan),
             SocIneq_popIDF2012 = sum (ratioEduc_PCThealthy_popIDF2012),
             SocIneq_pdan_popIDF2012 = sum (ratioEduc_PCThealthy_pdan_popIDF2012))





######## RELATION OPINION / CONSO Fruits et l?gumes

ValueOpinion2002 <- bd_evol_select %>% 
  filter(annee==2002) %>% 
  group_by (cons_5aday) %>% 
  summarise(NB = n(), moyopi = mean (opinion_index, na.rm=TRUE) ) 

Tab2002 <- bd_evol_select %>% 
  filter(annee==2002) 
LM <-lm (opinion_index ~ cons_5aday, data = Tab2002)
anova (LM)

zut <-table(Tab$opinion_index, Tab$fruit_leg_5)
dim (zut)
aggregate(Tab$opinion_index, list(Tab$fruit_leg_5), mean, na.rm=TRUE)
aggregate(Tab$opinion_index, list(Tab$fruit_leg_5), length, na.rm=TRUE )
aggregate(x ~ opinion_index + fruit_leg_5, data=Tab, length)

aggregate(Tab$opinion_index + Tab$fruit_leg_5, length, na.rm=TRUE)
aggregate(frequency(Tab$opinion_index, Tab$fruit_leg_5, na.rm=TRUE))
lm (opinion_index ~ fruit_leg_5, data = Tab)
lmOp <- lm (opinion_index ~ fruit_leg_5, data = Tab)
anova (lmOp)







# bd_evol_select$cons_5aday
# plot(cons_5aday ~ age_3cat , data = bd_evol_select)
# 
# mean(bd_evol_select[bd_evol_select$educ == 1, "opinion_index"], na.rm = T)
# mean(bd_evol_select[bd_evol_select$cons_5aday == 0, "opinion_index"], na.rm = T)
# hist(bd_evol_select[bd_evol_select$category == "1_1_1", "opinion_index"])

# hist(bd_evol_select$enough_veg_yes)
# 
# table(bd_evol_select$impact_budget, bd_evol_select$income_4cat)
# table(bd_evol_select$impact_health, bd_evol_select$income_4cat)
# table(bd_evol_select$impact_habits, bd_evol_select$income_4cat)
# table(bd_evol_select$impact_time, bd_evol_select$income_4cat)
# 
# hist(bd_evol_select[bd_evol_select$impact_budget == 1, "income_4cat"], main = "Influence Budget")
# hist(bd_evol_select[bd_evol_select$impact_budget == 0, "income_4cat"], main = "Pas d'Influence Budget")
# hist(bd_evol_select[bd_evol_select$impact_health == 1, "income_4cat"], main = "Influence Sante")
# hist(bd_evol_select[bd_evol_select$impact_health == 0, "income_4cat"], main = "Pas d'Influence Sante")
# hist(bd_evol_select[bd_evol_select$impact_habits == 1, "income_4cat"], main = "Influence Foyer")
# hist(bd_evol_select[bd_evol_select$impact_habits == 0, "income_4cat"], main = "Pas d'Influence Foyer")
# hist(bd_evol_select[bd_evol_select$impact_time == 1, "income_4cat"], main = "Influence Temps")
# hist(bd_evol_select[bd_evol_select$impact_time == 0, "income_4cat"], main = "Pas d'Influence Temps")
# 
# hist(bd_evol_select[bd_evol_select$impact_budget == 1, "educ"], main = "Influence Budget")
# hist(bd_evol_select[bd_evol_select$impact_budget == 0, "educ"], main = "Pas d'Influence Budget")
# hist(bd_evol_select[bd_evol_select$impact_health == 1, "educ"], main = "Influence Sante")
# hist(bd_evol_select[bd_evol_select$impact_health == 0, "educ"], main = "Pas d'Influence Sante")
# hist(bd_evol_select[bd_evol_select$impact_habits == 1, "educ"], main = "Influence Foyer")
# hist(bd_evol_select[bd_evol_select$impact_habits == 0, "educ"], main = "Pas d'Influence Foyer")
# hist(bd_evol_select[bd_evol_select$impact_time == 1, "educ"], main = "Influence Temps")
# hist(bd_evol_select[bd_evol_select$impact_time == 0, "educ"], main = "Pas d'Influence Temps")
# 
#   hist(bd_evol_select[bd_evol_select$cons_5aday == 1, "income_4cat"], main = "5")
# hist(bd_evol_select[bd_evol_select$cons_5aday == 0, "income_4cat"], main = "<5")
# hist(bd_evol_select[bd_evol_select$cons_5aday == 1, "educ"], main = "5")
# hist(bd_evol_select[bd_evol_select$cons_5aday == 0, "educ"], main = "<5")
# 
# hist(bd_evol_select[bd_evol_select$never_organic == 1, "income_4cat"], main = "Jamais bio")
# hist(bd_evol_select[bd_evol_select$never_organic == 0, "income_4cat"], main = "Parfois bio")
# hist(bd_evol_select[bd_evol_select$never_organic == 1, "educ"], main = "Jamais bio")
# hist(bd_evol_select[bd_evol_select$never_organic == 0, "educ"], main = "Parfois bio")
# 
# 
# hist(bd_evol_select[bd_evol_select$enough_veg_yes == 1, "cons_5aday"], main = "consommation suffisante")
# hist(bd_evol_select[bd_evol_select$enough_veg_yes == 0, "cons_5aday"], main = "consommation insuffisante")
# 
# table(bd_evol_select$enough_veg_yes, bd_evol_select$cons_5aday)
# chisq.test(bd_evol_select$enough_veg_yes, bd_evol_select$cons_5aday)
# table(bd_evol_select$enough_fruit_yes, bd_evol_select$cons_5aday)
# chisq.test(bd_evol_select$enough_fruit_yes, bd_evol_select$cons_5aday)
# 
#   table(bd_evol_select$info_about_diet, bd_evol_select$healthy_diet)
# summary(lm(bd_evol_select$info_about_diet ~ bd_evol_select$healthy_diet))
# 
# str(bd_evol_select)
# 
# library(ggplot2)
# 
# ggplot(bd_evol_select, aes(x = annee, y = never_organic, group = educ, fill = educ)) + geom_point() + geom_line()
# 
#   table(bd_evol_select$cons_5aday, bd_evol_select$annee)
#   summary(lm(bd_evol_select$educ ~ bd_evol_select$income_4cat))
#   
#   summary(as.factor(bd_evol_select[bd_evol_select$annee == 1996, "category"]))
#  
#    
#   
# prop.table(table(bd_evol_select[bd_evol_select$annee == 2008, "cons_5aday"], bd_evol_select[bd_evol_select$annee == 2008, "category"]), margin = 2)
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
# bd_evol_select$incoherence = ifelse(bd_evol_select$cons_5aday == 0, 
#                                   ifelse(bd_evol_select$enough_fruit_yes == 1 | 
#                                            bd_evol_select$enough_veg_yes == 1, 1, 0),0)
# 
# bd_evol_select$contrainte = ifelse(bd_evol_select$impact_habits == 1 | 
#                                     bd_evol_select$impact_budget == 1 | 
#                                         bd_evol_select$impact_time == 1, 1, 0)
# 
# 
# 
# 
# 
# 
# 
# 
# 


