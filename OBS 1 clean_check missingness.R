#Ontario birth study (OBS) data
#cleaning and missingness

library(tidyverse)
library(dplyr)
library(readr)
library(summarytools)
library(tidyr)


#import dataframes with 24 and 54 month outcomes and maternal/child descriptives


#load 24 mo data (ASQ, MCHAT)

#check duplicates
obs_elig$duplicate <- ifelse(duplicated(obs_elig$id) | duplicated(obs_elig$id, fromLast = TRUE), 1, 0)
obs_dupes <- subset(obs_elig, select = c("id", "kid_id", "duplicate"), duplicate == 1, )
print(obs_dupes)
#0 duplicates
#777 ppts


#check freq across vars
freq(obs_elig$age)
freq(obs_elig$childhood_swear) 
freq(obs_elig$Score_communication) 
freq(obs_elig$Pointlook)


#recode empty MCHAT strings as NA
obs_2 <- obs_2 %>% 
  mutate(Pointlook = case_when(
    Pointlook == "" ~ NA,
    TRUE ~ Pointlook)) %>% 
  mutate(Finger = case_when(
    Finger == "" ~ NA,
    TRUE ~ Finger)) %>% 
  mutate(Interestchildren = case_when(
    Interestchildren == "" ~ NA,
    TRUE ~ Interestchildren)) %>% 
  mutate(Smile = case_when(
    Smile == "" ~ NA,
    TRUE ~ Smile)) %>% 
  mutate(Upset = case_when(
    Upset == "" ~ NA,
    TRUE ~ Upset)) %>% 
  mutate(Lookeye = case_when(
    Lookeye == "" ~ NA,
    TRUE ~ Lookeye)) %>% 
  mutate(Turnhead = case_when(
    Turnhead == "" ~ NA,
    TRUE ~ Turnhead)) %>% 
  mutate(Understand = case_when(
    Understand == "" ~ NA,
    TRUE ~ Understand)) %>% 
  mutate(Lookface = case_when(
    Lookface == "" ~ NA,
    TRUE ~ Lookface)) %>% 
  mutate(Movement = case_when(
    Movement == "" ~ NA,
    Movement == "y" ~ "Yes",
    TRUE ~ Movement))

#update mchat > numerical
#item 1_Pointlook
obs_2 <- obs_2 %>% 
  mutate(Pointlook = case_when(
    Pointlook == "Yes" ~ "0",       
    Pointlook == "No" ~ "1",    
    TRUE ~ Pointlook  # Keeps original value if it doesn't match any case
  ))

#item 2_Deaf **reverse scored**
obs_2 <- obs_2 %>% 
  mutate(Deaf = case_when(
    Deaf == "Yes" ~ "1",   
    Deaf == "No" ~ "0",    
    TRUE ~ Deaf  
  ))

#item 3_Pretend
obs_2 <- obs_2 %>% 
  mutate(Pretend = case_when(
    Pretend == "Yes" ~ "0",      
    Pretend == "No" ~ "1",    
    TRUE ~ Pretend  
  ))

#item 4_Climb
obs_2 <- obs_2 %>% 
  mutate(Climb = case_when(
    Climb == "Yes" ~ "0",     
    Climb == "No" ~ "1",    
    TRUE ~ Climb  
  ))

#item 5_Finger **reverse scored**
obs_2 <- obs_2 %>% 
  mutate(Finger = case_when(
    Finger == "Yes" ~ "1",      
    Finger == "No" ~ "0",   
    TRUE ~ Finger  
  ))

#item 6_Pointask
obs_2 <- obs_2 %>% 
  mutate(Pointask = case_when(
    Pointask == "Yes" ~ "0",    
    Pointask == "No" ~ "1",    
    TRUE ~ Pointask  
  ))

#item 7_Pointshow
obs_2 <- obs_2 %>% 
  mutate(Pointshow = case_when(
    Pointshow == "Yes" ~ "0",     
    Pointshow == "No" ~ "1",  
    TRUE ~ Pointshow
  ))

#item 8_Interestchildren
obs_2 <- obs_2  %>% 
  mutate(Interestchildren = case_when(
    Interestchildren == "Yes" ~ "0",       
    Interestchildren == "No" ~ "1",     
    TRUE ~ Interestchildren 
  ))

#item 9_Showbring
obs_2 <- obs_2 %>% 
  mutate(Showbring = case_when(
    Showbring == "Yes" ~ "0",   
    Showbring == "No" ~ "1",   
    TRUE ~ Showbring 
  ))

#item 10_Respondname
obs_2 <- obs_2 %>% 
  mutate(Respondname = case_when(
    Respondname == "Yes" ~ "0", 
    Respondname == "No" ~ "1",   
    TRUE ~ Respondname
  ))

#item 11_Smile
obs_2 <- obs_2 %>% 
  mutate(Smile = case_when(
    Smile == "Yes" ~ "0",   
    Smile == "No" ~ "1",  
    TRUE ~ Smile 
  ))

#item 12_Upset **reverse scored**
obs_2 <- obs_2 %>% 
  mutate(Upset = case_when(
    Upset == "Yes" ~ "1",    
    Upset == "No" ~ "0", 
    TRUE ~ Upset  
  ))

#item 13_Walk
obs_2 <- obs_2 %>% 
  mutate(Walk = case_when(
    Walk == "Yes" ~ "0",    
    Walk == "No" ~ "1",    
    TRUE ~ Walk 
  ))

#item 14_Lookeye
obs_2 <- obs_2 %>% 
  mutate(Lookeye = case_when(
    Lookeye == "Yes" ~ "0",    
    Lookeye == "No" ~ "1",  
    TRUE ~ Lookeye  
  ))

#item 15_Copyyou
obs_2 <- obs_2 %>% 
  mutate(Copyyou = case_when(
    Copyyou == "Yes" ~ "0",   
    Copyyou == "No" ~ "1", 
    TRUE ~ Copyyou  
  ))

#item 16_Turnhead
obs_2 <- obs_2 %>% 
  mutate(Turnhead = case_when(
    Turnhead == "Yes" ~ "0",   
    Turnhead == "No" ~ "1",     
    TRUE ~ Turnhead 
  ))

#item 17_Getwatch
obs_2 <- obs_2 %>% 
  mutate(Getwatch = case_when(
    Getwatch == "Yes" ~ "0", 
    Getwatch == "No" ~ "1",  
    TRUE ~ Getwatch 
  ))

#item 18_Understand
obs_2 <- obs_2 %>% 
  mutate(Understand = case_when(
    Understand == "Yes" ~ "0",   
    Understand == "No" ~ "1",
    TRUE ~ Understand 
  ))

#item 19_Lookface
obs_2 <- obs_2 %>% 
  mutate(Lookface = case_when(
    Lookface == "Yes" ~ "0",   
    Lookface == "No" ~ "1",   
    TRUE ~ Lookface 
  ))

#item 20_Movement
obs_2 <- obs_2 %>% 
  mutate(Movement = case_when(
    Movement == "Yes" ~ "0",      
    Movement == "No" ~ "1",     
    TRUE ~ Movement 
  ))

print(sapply(obs_2, class))
#MCHAT scores are currently characters, need to change to numeric before analyses

obs_2$Pointlook = as.numeric(obs_2$Pointlook)
obs_2$Deaf = as.numeric(obs_2$Deaf)
obs_2$Pretend = as.numeric(obs_2$Pretend)
obs_2$Climb = as.numeric(obs_2$Climb)                             
obs_2$Finger = as.numeric(obs_2$Finger)
obs_2$Pointask = as.numeric(obs_2$Pointask)                                 
obs_2$Pointshow = as.numeric(obs_2$Pointshow)                                 
obs_2$Interestchildren = as.numeric(obs_2$Interestchildren)                                 
obs_2$Showbring = as.numeric(obs_2$Showbring)                                 
obs_2$Respondname = as.numeric(obs_2$Respondname)                                 
obs_2$Smile = as.numeric(obs_2$Smile)                                 
obs_2$Upset = as.numeric(obs_2$Upset)                                 
obs_2$Walk = as.numeric(obs_2$Walk)                                 
obs_2$Lookeye = as.numeric(obs_2$Lookeye)                                 
obs_2$Copyyou = as.numeric(obs_2$Copyyou)                                 
obs_2$Turnhead = as.numeric(obs_2$Turnhead)                                 
obs_2$Getwatch = as.numeric(obs_2$Getwatch)                                 
obs_2$Understand = as.numeric(obs_2$Understand)                                 
obs_2$Lookface = as.numeric(obs_2$Lookface)                                 
obs_2$Movement = as.numeric(obs_2$Movement)   

#update sex as 0/1
obs_2 <- obs_2 %>% 
  mutate(Male = case_when(
    sex == "Female" ~ "0",     
    sex == "Male" ~ "1",  
    TRUE ~ sex
  ))

##update covariate # option to be NA > including prefer not to answer/dont know
library(naniar)

#marital 7=prefer not to answer
obs_2 <- obs_2 %>% 
  replace_with_na(replace = list(marital = 7))

#education 24=prefer
obs_2 <- obs_2 %>% 
  replace_with_na(replace = list(education = 24))

#baby_mother_born 21=dk 22=prefer
obs_2 <- obs_2 %>% 
  replace_with_na(replace = list(baby_mother_born = 21)) %>% 
  replace_with_na(replace = list(baby_mother_born = 22))

#curr_empl_stat 8=prefer
obs_2 <- obs_2 %>%
  replace_with_na(replace = list(curr_empl_stat = 8))

#hh_inc 9=dk 10=prefer
obs_2 <- obs_2 %>%
  replace_with_na(replace = list(hh_inc = 9)) %>% 
  replace_with_na(replace = list(hh_inc = 10))


#ethnic background 17=dk 18=prefer but its all diff columns AND can select 2+ 
#so remove 17/18 and make all ethn_bkgr values in those columns NA

#filter the rows with ethn_bkgr___17|ethn_bkgr___18 = 1
obs_ethn <- obs_2 %>% 
  select(id, ethn_bkgr___17, ethn_bkgr___18) %>% 
  filter(ethn_bkgr___17 == 1 | ethn_bkgr___18 == 1)

#clear ethn values for id's with missing ethn
obs_2 <- obs_2 %>% 
  mutate(across(starts_with("ethn_bkgr___"), 
                ~ ifelse(id %in% c("91202352", "91202462", "91202763", "91201411", "91201853", "91202605", "91202961"), NA, .)))

obs_2 <- subset(obs_2, select = -c(ethn_bkgr___17, ethn_bkgr___18))

#mother born
obs_2 <- obs_2 %>%
  mutate(mother_born_not_Canada = case_when(
    baby_mother_born == 1 ~ 0,
    baby_mother_born %in% 2:20 ~ 1,  # This includes all values from 2 to 20
    baby_mother_born %in% 21:22 ~ NA,  # This includes values 21 and 22
    TRUE ~ baby_mother_born  # Keeps original value if it doesn't match any case
  ))

#race > dummy code ethnic_background variable
obs_2 <- obs_2 %>% 
  mutate(race = case_when(
    ethn_bkgr___15 == 1 & ethn_bkgr___10 == 1 ~ 0,  # White and Jewish
    ethn_bkgr___15 == 1 ~ 0,                     # White only
    (ethn_bkgr___1 == 1 | ethn_bkgr___2 == 1 | ethn_bkgr___3 == 1 | ethn_bkgr___4 == 1 | 
       ethn_bkgr___5 == 1 | ethn_bkgr___6 == 1 | ethn_bkgr___7 == 1 | ethn_bkgr___8 == 1 | 
       ethn_bkgr___9 == 1 | ethn_bkgr___11 == 1 | ethn_bkgr___12 == 1 | ethn_bkgr___13 == 1 | 
       ethn_bkgr___14 == 1 | ethn_bkgr___16 == 1 | (ethn_bkgr___10 == 1 & ethn_bkgr___15 != 1)) ~ 1,  # Non-white or Jewish not white
    TRUE ~ NA_real_  # Assign NA to those who do not fall into any category 
  ))

#hh_inc_strat
obs_2 <- obs_2 %>%
  mutate(hh_inc_lower150 = case_when(
    hh_inc %in% 7:8 ~ 0,   #Higher income %150k+
    hh_inc < 7 ~ 1,        #Lower income <$150k
    hh_inc %in% 9:10 ~ NA, #Don't know or Prefer not to answer 
  ))

#education
obs_2 <- obs_2 %>%
  mutate(education_less_bach_grad = case_when(
    education == 22 ~ 1,     #Bachelor's
    education == 25 ~ 2,    #Graduate degree 
    education < 22 | education == 23 ~ 0,     # Less than a Bachelor's degree
    education == 24 ~ NA_real_ # None or Prefer not to answer
  ))


#employment status
obs_2 <- obs_2 %>%
  mutate(curr_empl_full_part_unempl = case_when(
    curr_empl_stat == 1 ~ 0,       #full-time
    curr_empl_stat == 2 ~ 1,     #part-time
    curr_empl_stat %in% 3:7 ~ 2,  #unemployed
    curr_empl_stat %in% 9 ~ 2,    #unemployed
    curr_empl_stat == 8 ~ NA,      #prefer not to answer
    TRUE ~ curr_empl_stat  # Keeps original value if it doesn't match any case
  ))

obs_2 <- obs_2 %>% 
  relocate(mother_born_not_Canada, .after = baby_mother_born) %>% 
  relocate(race, .before = ethn_bkgr___1) %>%
  relocate(hh_inc_lower150, .after = hh_inc) %>%
  relocate(education_less_bach_grad, .after = education) %>%
  relocate(curr_empl_full_part_unempl, .after = curr_empl_stat)


#create sum scores (ACEs, outcomes)
#aces
obs_2 <- obs_2 %>% 
  mutate(childhood_swear = case_when( #swear yes=1, no=2
    childhood_swear == 1 ~ 1,
    childhood_swear == 2 ~ 0,
    TRUE ~ NA_real_ )) %>% 
  mutate(childhood_push = case_when( #push yes=3, no=4
    childhood_push == 3 ~ 1,
    childhood_push == 4 ~ 0,
    TRUE ~ NA_real_ )) %>% 
  mutate(childhood_touch = case_when( #touch yes=5, 6=no
    childhood_touch == 5 ~ 1,
    childhood_touch == 6 ~ 0,
    TRUE ~ NA_real_ )) %>% 
  mutate(childhood_divorce = case_when( #divorce yes=5, no=6
    childhood_divorce == 5 ~ 1,
    childhood_divorce == 6 ~ 0,
    TRUE ~ NA_real_ )) %>%   
  mutate(childhood_mother = case_when( #mother yes=5, no=6
    childhood_mother == 5 ~ 1,
    childhood_mother == 6 ~ 0,
    TRUE ~ NA_real_ )) %>%
  mutate(childhood_alcohol = case_when( #alcohol yes=5, no=6
    childhood_alcohol == 5 ~ 1,
    childhood_alcohol == 6 ~ 0,
    TRUE ~ NA_real_ )) %>%
  mutate(childhood_suicide = case_when( #suicide yes=1, no=2
    childhood_suicide == 1 ~ 1,
    childhood_suicide == 2 ~ 0,
    TRUE ~ NA_real_ )) %>% 
  mutate(childhood_prison = case_when( #prison yes=1, no=2
    childhood_prison == 1 ~ 1,
    childhood_prison == 2 ~ 0,
    TRUE ~ NA_real_ ))

obs_2 <- obs_2 %>% 
  mutate(aces = childhood_swear + childhood_push + childhood_touch + childhood_mother +
           childhood_divorce + childhood_alcohol + childhood_suicide + childhood_prison) %>% 
  mutate(abuse = childhood_swear + childhood_push + childhood_touch + childhood_mother) %>% 
  mutate(hd = childhood_divorce + childhood_alcohol + childhood_suicide + childhood_prison)

obs_2 <- obs_2 %>% 
  relocate(hd, .after = childhood_prison) %>% 
  relocate(abuse, .after = childhood_prison) %>% 
  relocate(aces, .after = childhood_prison)

#mchat
obs_2 <- obs_2 %>% 
  mutate(mchat = Pointlook + Deaf + Pretend + Climb + Finger + Pointask +
           Pointask + Interestchildren + Showbring + Respondname +
           Smile + Upset + Walk + Lookeye + Copyyou + Turnhead +
           Getwatch + Understand + Lookface + Movement)
obs_2 <- obs_2 %>% 
  relocate(mchat, .after = Movement) %>% 
  relocate(GA, .after = weight_g)


#================================================================================
#load 54 month data (NIH toolboxes)

#check duplicates
obs_clean_2$duplicate <- ifelse(duplicated(obs_clean_2$id) | duplicated(obs_clean_2$id, fromLast = TRUE), 1, 0)
obs_dupes <- subset(obs_clean_2, select = c("id", "kid_id", "duplicate"), duplicate == 1, )
#0 duplicates
#2177 ppts

#check freq across vars

freq(obs_elig_54$age) 
freq(obs_elig_54$childhood_swear) 
freq(obs_elig_54$vocab_agecorrected)
freq(obs_elig_54$cognitioncomp_agecorrected)
freq(obs_elig_54$anxietysummary_fullycorrected) 

#marital 7=prefer not to answer
obs_2 <- obs_2 %>% 
  replace_with_na(replace = list(marital = 7))

#education 24=prefer
obs_2 <- obs_2 %>% 
  replace_with_na(replace = list(education = 24))

#baby_mother_born 21=dk 22=prefer
obs_2 <- obs_2 %>% 
  replace_with_na(replace = list(baby_mother_born = 21)) %>% 
  replace_with_na(replace = list(baby_mother_born = 22))

#curr_empl_stat 8=prefer
obs_2 <- obs_2 %>%
  replace_with_na(replace = list(curr_empl_stat = 8))

#hh_inc 9=dk 10=prefer
obs_2 <- obs_2 %>%
  replace_with_na(replace = list(hh_inc = 9)) %>% 
  replace_with_na(replace = list(hh_inc = 10))


#ethnic background 17=dk 18=prefer but its all diff columns AND can select 2+ 
#so remove 17/18 and make all ethn_bkgr values in those columns NA

#filter the rows with ethn_bkgr___17|ethn_bkgr___18 = 1
obs_ethn <- obs_2 %>% 
  select(id, ethn_bkgr___17, ethn_bkgr___18) %>% 
  filter(ethn_bkgr___17 == 1 | ethn_bkgr___18 == 1)


#mother born
obs_2 <- obs_2 %>%
  mutate(mother_born_not_Canada = case_when(
    baby_mother_born == 1 ~ 0,
    baby_mother_born %in% 2:20 ~ 1,  # This includes all values from 2 to 20
    baby_mother_born %in% 21:22 ~ NA,  # This includes values 21 and 22
    TRUE ~ baby_mother_born  # Keeps original value if it doesn't match any case
  ))

#race > dummy code ethnic_background variable
obs_2 <- obs_2 %>% 
  mutate(race = case_when(
    ethn_bkgr___15 == 1 & ethn_bkgr___10 == 1 ~ 0,  # White and Jewish
    ethn_bkgr___15 == 1 ~ 0,                     # White only
    (ethn_bkgr___1 == 1 | ethn_bkgr___2 == 1 | ethn_bkgr___3 == 1 | ethn_bkgr___4 == 1 | 
       ethn_bkgr___5 == 1 | ethn_bkgr___6 == 1 | ethn_bkgr___7 == 1 | ethn_bkgr___8 == 1 | 
       ethn_bkgr___9 == 1 | ethn_bkgr___11 == 1 | ethn_bkgr___12 == 1 | ethn_bkgr___13 == 1 | 
       ethn_bkgr___14 == 1 | ethn_bkgr___16 == 1 | (ethn_bkgr___10 == 1 & ethn_bkgr___15 != 1)) ~ 1,  # Non-white or Jewish not white
    TRUE ~ NA_real_  # Assign NA to those who do not fall into any category 
  ))

#hh_inc_strat
obs_2 <- obs_2 %>%
  mutate(hh_inc_lower150 = case_when(
    hh_inc %in% 7:8 ~ 0,   #Higher income %150k+
    hh_inc < 7 ~ 1,        #Lower income <$150k
    hh_inc %in% 9:10 ~ NA, #Don't know or Prefer not to answer 
  ))

#education
obs_2 <- obs_2 %>%
  mutate(education_less_bach_grad = case_when(
    education == 22 ~ 1,     #Bachelor's
    education == 25 ~ 2,    #Graduate degree 
    education < 22 | education == 23 ~ 0,     # Less than a Bachelor's degree
    education == 24 ~ NA_real_ # None or Prefer not to answer
  ))


#employment status
obs_2 <- obs_2 %>%
  mutate(curr_empl_full_part_unempl = case_when(
    curr_empl_stat == 1 ~ 0,       #full-time
    curr_empl_stat == 2 ~ 1,     #part-time
    curr_empl_stat %in% 3:7 ~ 2,  #unemployed
    curr_empl_stat %in% 9 ~ 2,    #unemployed
    curr_empl_stat == 8 ~ NA,      #prefer not to answer
    TRUE ~ curr_empl_stat  # Keeps original value if it doesn't match any case
  ))


#create sum scores (ACEs, outcomes)

#aces
obs_2 <- obs_2 %>% 
  mutate(childhood_swear = case_when( #swear yes=1, no=2
    childhood_swear == 1 ~ 1,
    childhood_swear == 2 ~ 0,
    TRUE ~ NA_real_ )) %>% 
  mutate(childhood_push = case_when( #push yes=3, no=4
    childhood_push == 3 ~ 1,
    childhood_push == 4 ~ 0,
    TRUE ~ NA_real_ )) %>% 
  mutate(childhood_touch = case_when( #touch yes=5, 6=no
    childhood_touch == 5 ~ 1,
    childhood_touch == 6 ~ 0,
    TRUE ~ NA_real_ )) %>% 
  mutate(childhood_divorce = case_when( #divorce yes=5, no=6
    childhood_divorce == 5 ~ 1,
    childhood_divorce == 6 ~ 0,
    TRUE ~ NA_real_ )) %>%   
  mutate(childhood_mother = case_when( #mother yes=5, no=6
    childhood_mother == 5 ~ 1,
    childhood_mother == 6 ~ 0,
    TRUE ~ NA_real_ )) %>%
  mutate(childhood_alcohol = case_when( #alcohol yes=5, no=6
    childhood_alcohol == 5 ~ 1,
    childhood_alcohol == 6 ~ 0,
    TRUE ~ NA_real_ )) %>%
  mutate(childhood_suicide = case_when( #suicide yes=1, no=2
    childhood_suicide == 1 ~ 1,
    childhood_suicide == 2 ~ 0,
    TRUE ~ NA_real_ )) %>% 
  mutate(childhood_prison = case_when( #prison yes=1, no=2
    childhood_prison == 1 ~ 1,
    childhood_prison == 2 ~ 0,
    TRUE ~ NA_real_ ))

obs_2 <- obs_2 %>% 
  mutate(aces = childhood_swear + childhood_push + childhood_touch + childhood_mother +
           childhood_divorce + childhood_alcohol + childhood_suicide + childhood_prison) %>% 
  mutate(abuse = childhood_swear + childhood_push + childhood_touch + childhood_mother) %>% 
  mutate(hd = childhood_divorce + childhood_alcohol + childhood_suicide + childhood_prison)



#now have clean datasets in terms of 1 child per mother, no duplicates, all eligible for OBS-kids per 24 and 54 month df


#====================================================
#check missingness by outcomes 

#ASQ
obs <- obs %>%
  mutate(asq_missing = 
           if_else( if_all(c(Score_communication, Score_gross, Score_fine, 
                             Score_probsolve, Score_social), is.na), 
                    1, #all missing
                    0  #(partial) present
           ) )



#mchat
obs <- obs %>% 
  mutate(mchat_missing = 
           if_else( if_all(c(Pointlook, Deaf, Pretend, Climb, Finger, Pointask, Pointshow, Interestchildren, 
                             Showbring, Respondname, Smile, Upset, Walk, Lookeye, Copyyou, Turnhead,
                             Getwatch, Understand, Lookface, Movement), is.na),
                    1, #all missing
                    0  #(partial) present
           )
  )

#now for all participants I have 2 groups, those with missing asq / mchat data (=1) and those with partial/full (=0)

#ASQ

#categorical: Male, education_less_bach_grad, mother_born_not_Canada, 
#             employ, hh_inc_lower150, race
#continuous: age, GA, weight_g, age_24mo


#Chi-square tests

table_sex <- table(obs_24$asq_missing, obs_24$Male)
chisq.test(table_sex) #p = .03

table_education <- table(obs_24$asq_missing, obs_24$education)
chisq.test(table_education) #p = .44

table_mother <- table(obs_24$asq_missing, obs_24$mother_born_not_Canada)
chisq.test(table_mother) #p = .15

table_employ <- table(obs_24$asq_missing, obs_24$employ)
chisq.test(table_employ) #p = .26

table_income <- table(obs_24$asq_missing, obs_24$hh_inc_lower150)
chisq.test(table_income) #p = .36

table_race <- table(obs_24$asq_missing, obs_24$race)
chisq.test(table_race) #p = .71


#t-tests

t.test(age ~ mchat_missing, data = obs_24) #p value: .86

t.test(GA ~ asq_missing, data = obs_24) #p value: .63

t.test(weight_g ~ asq_missing, data = obs_24) #p value: .96

t.test(age_24mo ~ asq_missing, data = obs_24) #p value: .11

#MCHAT
table_sex <- table(obs_24$mchat_missing, obs_24$Male)
chisq.test(table_sex) #p = .04

table_education <- table(obs_24$mchat_missing, obs_24$education)
chisq.test(table_education) #p = .16

table_mother <- table(obs_24$mchat_missing, obs_24$mother_born_not_Canada)
chisq.test(table_mother) #p = .10

table_employ <- table(obs_24$mchat_missing, obs_24$employ)
chisq.test(table_employ) #p = .32

table_income <- table(obs_24$mchat_missing, obs_24$hh_inc_lower150)
chisq.test(table_income) #p = .39

table_race <- table(obs_24$mchat_missing, obs_24$race)
chisq.test(table_race) #p = .88


#t-tests

t.test(age ~ mchat_missing, data = obs_24) #p value: .87

t.test(GA ~ mchat_missing, data = obs_24) #p value: .76

t.test(weight_g ~ mchat_missing, data = obs_24) #NA
fisher.test(table(obs_24$asq_missing, obs_24$weight_g)) #p value = .06*

t.test(age_24mo ~ mchat_missing, data = obs_24) #p value: .37



#54months missingness checks
obs <- obs %>% 
  mutate(
    missing = case_when(
      # Case 1: All 7 columns are NA
      rowSums(!is.na(select(., vocab_agecorrected, flanker_agecorrected, picturesequence_agecorrected, cognitioncomp_agecorrected,
                            anxietysummary_fullycorrected, negpsychsummary_fullycorrected, wellbeingsummary_fullycorrected))) == 0 ~ 1,
      
      # Case 2: All 4 cognitive columns are NA, but at least one emotional column has data
      rowSums(!is.na(select(., vocab_agecorrected, flanker_agecorrected, picturesequence_agecorrected, cognitioncomp_agecorrected))) == 0 &
        rowSums(!is.na(select(., anxietysummary_fullycorrected, negpsychsummary_fullycorrected, wellbeingsummary_fullycorrected))) > 0 ~ 2,
      
      # Case 0: At least one of the 7 columns has data
      TRUE ~ 0
    )
  )


#test 0 v 1 and 0 v 2
#0 = non-missing
#1 = attrition, missing bc of 
#2 = COVID, missing bc of

#categorical: Male, education_less_bach_grad, mother_born_not_Canada, 
#             employ, hh_inc_lower150, race
#continuous: age, GA, weight_g, age_54

group_0_1 <- obs_54 %>% filter(missing %in% c(0, 1))
group_0_2 <- obs_54 %>% filter(missing %in% c(0, 2))

#chi 0 v 1
table_sex <- table(group_0_1$missing, group_0_1$Male)
chisq.test(table_sex) #p = .98

table_education <- table(group_0_1$missing, group_0_1$education)
chisq.test(table_education) #p = .29

table_mother <- table(group_0_1$missing, group_0_1$mother_born_not_Canada)
chisq.test(table_mother) #p = .57

table_employ <- table(group_0_1$missing, group_0_1$employ)
chisq.test(table_employ) #p = .84

table_income <- table(group_0_1$missing, group_0_1$hh_inc_lower150)
chisq.test(table_income) #p = .85

table_race <- table(group_0_1$missing, group_0_1$race)
chisq.test(table_race) #p = .90

#chi 0 v 2
table_sex <- table(group_0_2$missing, group_0_2$Male)
chisq.test(table_sex) #p = .92

table_education <- table(group_0_2$missing, group_0_2$education)
chisq.test(table_education) #p = .03*

table_mother <- table(group_0_2$missing, group_0_2$mother_born_not_Canada)
chisq.test(table_mother) #p = .34

table_employ <- table(group_0_2$missing, group_0_2$employ)
chisq.test(table_employ) #p = .57

table_income <- table(group_0_2$missing, group_0_2$hh_inc_lower150)
chisq.test(table_income) #p = .23

table_race <- table(group_0_2$missing, group_0_2$race)
chisq.test(table_race) #p = .31

#ttests
t.test(age ~ factor(missing), data = group_0_1) #.002**
t.test(age ~ factor(missing), data = group_0_2) #.002**

t.test(GA ~ factor(missing), data = group_0_1) #.62
t.test(GA ~ factor(missing), data = group_0_2) #.10

t.test(weight_g ~ factor(missing), data = group_0_1) #.30
t.test(weight_g ~ factor(missing), data = group_0_2) #.43

t.test(age_54 ~ factor(missing), data = group_0_1) # < .001
t.test(age_54 ~ factor(missing), data = group_0_2) # < .001





