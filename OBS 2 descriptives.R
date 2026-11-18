#Ontario birth study (OBS) data
#descriptives

library(tidyverse)
library(dplyr)
library(readr)
library(summarytools)
library(tidyr)


#fetal sex
freq(obs$Male)

#fetal weight
mean(obs$ weight, na.rm = TRUE)
sd(obs$weight, na.rm = TRUE) 


#age at Ax
mean(obs$age_24mo, na.rm = TRUE) #26.20 (months)
sd(obs$age_24mo, na.rm = TRUE) #1.40

#maternal age
freq(obs$age) #NA = 16, 2%
mean(obs$age, na.rm = TRUE)   #M = 33.85
sd(obs$age, na.rm = TRUE)    #SD = 3.7

#mother born
freq(obs$mother_born_not_Canada) #outside Canada = 1
#1 = 219 = 28%
#0 = 543 = 70%  
#NA = 15 = 2%

#race
freq(obs$race) #non-White = 1
#1 = 254 = 33%
#0 = 503 = 65%
#NA = 20 = 2%

#hh_inc
freq(obs$hh_inc_lower150) #<150k = 1
#1 = 285 = 37%
#0 = 449 = 58%
#NA = 43 = 5%

#education
freq(obs$education_less_bach_grad) #1 = bach, 2 = graduate, 0 = less than bach
#1 = bach = 382 = 49%
#2 = grad = 306 = 39%
#0 = less than bach = 73 = 9%
#NA = 16 = 2%

freq(obs$education)
#0 = 73
#1 = 688 = 89%


#employment 
freq(obs$curr_empl_full_part_unempl) #1 = part-time, 2 = unemployed, 0 = full-time
#0 = full time = 661, 85%
#1 = part time = 47, 6%
#2 = unemployed = 54, 7%
#NA = 15, 2%

freq(obs$employ)
#0 = 661
#1 = 101 13%

##ACEs descriptives, 1 = yes

freq(obs$childhood_swear)
#0 = 681 88%  
#1 =   89 11%
#NA =  7

freq(obs$childhood_push)
#0 = 725 93%  
#1 =   45 6%
#NA = 7

freq(obs$childhood_touch)
#0 = 734 94%  
#1 =   38 5%
#NA = 5

freq(obs$childhood_mother)
#0 = 741 95%  
#1 =  28 4% 
#NA = 8 1%

freq(obs$childhood_divorce)
#0 =  589 76% 
#1 =   183 24%
#NA = 5

freq(obs$childhood_alcohol)
#0 = 670 86%   
#1 =   103 13%
#NA = 4

freq(obs$childhood_suicide)
#0 = 621 80%   
#1 =  153 20% 
#NA = 3

freq(obs$childhood_prison)
#0 = 766 99%   
#1 = 7 1%   
#NA = 4 <1%

#ACE descriptives, #need to do after confirm CFA model

##MCHAT descriptives
mean(obs$mchat, na.rm = TRUE)
sd(obs$mchat, na.rm = TRUE) 

freq(obs$mchat, order = "freq")
#total score 0-2: Typical     
#3-7: Monitoring zone         
#8-20: Below cutoff         
#NA

##ASQ descriptives
mean(obs$Score_communication, na.rm = TRUE)   #M = 55.13
sd(obs$Score_communication, na.rm = TRUE)    #SD = 9.77

mean(obs$Score_gross, na.rm = TRUE)   #M = 52.63
sd(obs$Score_gross, na.rm = TRUE)    #SD = 9.14

mean(obs$Score_fine, na.rm = TRUE)   #M = 51.75
sd(obs$Score_fine, na.rm = TRUE)    #SD = 10.27

mean(obs$Score_probsolve, na.rm = TRUE)   #M = 52.87
sd(obs$Score_probsolve, na.rm = TRUE)    #SD = 9.09

mean(obs$Score_social, na.rm = TRUE)   #M = 50.29
sd(obs$Score_social, na.rm = TRUE)    #SD = 9.11

###ASQ by cut-offs
#Score_communication  39+ above / 25.18 - 38 monitor / 0 - 25.17 below
Score_comm_24 <- subset(ASQ24_1, select = c(Score_communication))
Score_comm_24$Score_communication = as.numeric(Score_comm_24$Score_communication)

breaks <- c(0, 25.17, 38, 61)             #0-25.17, 25.18-38, 39+
labels <- c("below", "monitor", "above")

comm_score_groups <- cut(Score_comm_24$Score_communication, breaks = breaks, labels = labels, include.lowest = T)
comm_cutoffs <- data.frame(Scores = Score_comm_24$Score_communication , Category = comm_score_groups)
freq(comm_cutoffs$Category)

#Score_gross 46+ above / 38.09 - 45 monitor / 0 - 38.08 below
Score_gross_24 <- subset(ASQ24_1, select = c(Score_gross))
Score_gross_24$Score_gross = as.numeric(Score_gross_24$Score_gross)

breaks <- c(0, 38.08, 45, 61)
labels <- c("below", "monitor", "above")

gross_score_groups <- cut(Score_gross_24$Score_gross, breaks = breaks, labels = labels, include.lowest = T)
gross_cutoffs <- data.frame(Scores = Score_gross_24$Score_gross , Category = gross_score_groups)
freq(gross_cutoffs$Category)

#Score_fine 43+ above / 35.16 - 42 monitor / 0 - 35.15 below
Score_fine_24 <- subset(ASQ24_1, select = c(Score_fine))
Score_fine_24$Score_gross = as.numeric(Score_fine_24$Score_fine)

breaks <- c(0, 35.15, 42, 61)
labels <- c("below", "monitor", "above")

fine_score_groups <- cut(Score_fine_24$Score_fine, breaks = breaks, labels = labels, include.lowest = T)
fine_cutoffs <- data.frame(Scores = Score_fine_24$Score_fine , Category = fine_score_groups)
freq(fine_cutoffs$Category)

#Score_probsolve 39+ above / 29.78 - 38 monitor / 0 - 29.77 below
Score_probsolve_24 <- subset(ASQ24_1, select = c(Score_probsolve))
Score_probsolve_24$Score_probsolve_24= as.numeric(Score_probsolve_24$Score_probsolve)

breaks <- c(0, 29.77, 38, 61)
labels <- c("below", "monitor", "above")

probsolve_score_groups <- cut(Score_probsolve_24$Score_probsolve, breaks = breaks, labels = labels, include.lowest = T)
probsolve_cutoffs <- data.frame(Scores = Score_probsolve_24$Score_probsolve , Category = probsolve_score_groups)
freq(probsolve_cutoffs$Category)


#Score_social 41+ above / 31.54 - 40 monitor / 0 - 31.53 below
Score_social_24 <- subset(ASQ24_1, select = c(Score_social))
Score_social_24$Score_social = as.numeric(Score_social_24$Score_social)

breaks <- c(0, 31.53, 40, 61)
labels <- c("below", "monitor", "above")

social_score_groups <- cut(Score_fine_24$Score_fine, breaks = breaks, labels = labels, include.lowest = T)
social_cutoffs <- data.frame(Scores = Score_social_24$Score_social , Category = social_score_groups)
freq(social_cutoffs$Category)





###ASQ27_1

#Score_communication  37+ above / 24.02 - 36 monitor/ 0 - 24.01 below
Score_comm_27 <- subset(ASQ27_1, select = c(Score_communication))
Score_comm_27$Score_communication = as.numeric(Score_comm_27$Score_communication)

breaks <- c(0, 24.01, 36, 61)
labels <- c("below", "monitor", "above")

comm_score_groups <- cut(Score_comm_27$Score_communication, breaks = breaks, labels = labels, include.lowest = T)
comm_cutoffs27 <- data.frame(Scores = Score_comm_27$Score_communication , Category = comm_score_groups)
freq(comm_cutoffs27$Category)

#Score_gross 39+ above / 28.01 - 38  monitor / 0 - 28 below
Score_gross_27 <- subset(ASQ27_1, select = c(Score_gross))
Score_gross_27$Score_gross = as.numeric(Score_gross_27$Score_gross)

breaks <- c(0, 28, 38, 61)
labels <- c("below", "monitor", "above")

gross_score_groups <- cut(Score_gross_27$Score_gross, breaks = breaks, labels = labels, include.lowest = T)
gross_cutoffs27 <- data.frame(Scores = Score_gross_27$Score_gross , Category = gross_score_groups)
freq(gross_cutoffs27$Category)

#Score_fine 31+ above / 18.42 - 30 monitor / 0 - 18.41 below
Score_fine_27 <- subset(ASQ27_1, select = c(Score_fine))
Score_fine_27$Score_fine = as.numeric(Score_fine_27$Score_fine)

breaks <- c(0, 18.41, 30, 61)
labels <- c("below", "monitor", "above")

fine_score_groups <- cut(Score_fine_27$Score_fine, breaks = breaks, labels = labels, include.lowest = T)
fine_cutoffs27 <- data.frame(Scores = Score_fine_27$Score_fine , Category = fine_score_groups)
freq(fine_cutoffs27$Category)

#Score_probsolve 38+ above / 27.62 - 37 monitor / 0 - 27.61 below
Score_probsolve_27 <- subset(ASQ27_1, select = c(Score_probsolve))
Score_probsolve_27$Score_probsolve = as.numeric(Score_probsolve_27$Score_probsolve)

breaks <- c(0, 27.61, 37, 61)
labels <- c("below", "monitor", "above")

probsolve_score_groups <- cut(Score_probsolve_27$Score_probsolve, breaks = breaks, labels = labels, include.lowest = T)
probsolve_cutoffs27 <- data.frame(Scores = Score_probsolve_27$Score_probsolve , Category = probsolve_score_groups)
freq(probsolve_cutoffs27$Category)


#Score_social 36+ above / 25.31 - 35 monitor / 0 - 25.30 below
Score_social_27 <- subset(ASQ27_1, select = c(Score_social))
Score_social_27$Score_social = as.numeric(Score_social_27$Score_social)

breaks <- c(0, 25.3, 35, 61)
labels <- c("below", "monitor", "above")

social_score_groups <- cut(Score_fine_27$Score_fine, breaks = breaks, labels = labels, include.lowest = T)
social_cutoffs27 <- data.frame(Scores = Score_social_27$Score_social , Category = social_score_groups)
freq(social_cutoffs27$Category)


############

#54mo descriptives

obs <- read.csv("C:\\Users\\cloud\\OneDrive - University of Toronto\\Research\\Data\\obs 54mo_cleaned_w missingness code.csv")
#1131 ppt, 61 v

obs <- subset(obs, select = -c (X))

#same covariates code as above (see below NIH)
#plus NIH

#NIH COG
mean(obs$vocab_agecorrected, na.rm = TRUE) #M = 108.51
sd(obs$vocab_agecorrected, na.rm = TRUE) #SD = 15.17
freq(obs$vocab_agecorrected, order = "freq") #NA = 449

mean(obs$flanker_agecorrected, na.rm = TRUE) #M = 108.27
sd(obs$flanker_agecorrected, na.rm = TRUE) #SD = 11.19
freq(obs$flanker_agecorrected, order = "freq") #NA = 457

mean(obs$DCCS_agecorrected, na.rm = TRUE) #M = 104.61
sd(obs$DCCS_agecorrected, na.rm = TRUE) #SD = 12.52
freq(obs$DCCS_agecorrected, order = "freq") #NA = 458

mean(obs$picturesequence_agecorrected, na.rm = TRUE) #M = 116.15
sd(obs$picturesequence_agecorrected, na.rm = TRUE) #SD = 22.52
freq(obs$picturesequence_agecorrected, order = "freq") #NA = 487

mean(obs$cognitioncomp_agecorrected, na.rm = TRUE) #M = 114.93
sd(obs$cognitioncomp_agecorrected, na.rm = TRUE) #SD = 15.21
freq(obs$cognitioncomp_agecorrected, order = "freq") #NA = 493

mean(obs$anxietysummary_fullycorrected, na.rm = TRUE) #M = 57.59
sd(obs$anxietysummary_fullycorrected, na.rm = TRUE) #SD = 10.63
freq(obs$anxietysummary_fullycorrected, order = "freq") #NA = 206

mean(obs$negpsychsummary_fullycorrected, na.rm = TRUE) #M = 55.41
sd(obs$negpsychsummary_fullycorrected, na.rm = TRUE) #SD = 9.15
freq(obs$negpsychsummary_fullycorrected, order = "freq") #NA = 206

mean(obs$wellbeingsummary_fullycorrected, na.rm = TRUE) #M = 45.95
sd(obs$wellbeingsummary_fullycorrected, na.rm = TRUE) #SD = 9.43
freq(obs$wellbeingsummary_fullycorrected, order = "freq") #NA = 207

#income
#1, Less than $10,000 | 2, $10,000 to $24,999 | 3, $25,000 to $49,999 | 
#4, $50,000 to $74,999 | 5, $75,000 to $99,999 | 6, $100,000 to $149, 999 | 
#7, $150,000 to $199,999 | 8, $200,000 or more | 9, Don't know | 10, Prefer not to answer

freq(obs_2$hh_inc)


#ethnicity 
#1, Aboriginal - First Nations | 2, Aboriginal - Metis | 3, Aboriginal - Inuit | 
#4, Arab (e.g. Egypt, Iraq, Jordan, Lebanon) | 5, Black - North American region | 
#6, Black - African region | 7, Black - Caribbean region | 8, East Asian (e.g. China, Japan, Korea, Taiwan) | 
#9, Filipino | 10, Jewish | 11, Latin American/Hispanic | 12, South Asian (e.g. India, Sri Lanka, Pakistan, Bangladesh) | 
#13, Southeast Asian (e.g. Malaysian, Indonesia, Vietnam) | 14, West Asian (e.g. Turkey, Iran, Afghanistan) | 
#15, White (European descent) | 16, Other ethnic group (not listed above) | 17, Don't know | 18, Prefer not to answer

freq(obs_2$ethn_bkgr___1) 
freq(obs_2$ethn_bkgr___2) 
freq(obs_2$ethn_bkgr___3) 
freq(obs_2$ethn_bkgr___4) 
freq(obs_2$ethn_bkgr___5) 
freq(obs_2$ethn_bkgr___6) 
freq(obs_2$ethn_bkgr___7) 
freq(obs_2$ethn_bkgr___8) 
freq(obs_2$ethn_bkgr___9) 
freq(obs_2$ethn_bkgr___10) 
freq(obs_2$ethn_bkgr___11) 
freq(obs_2$ethn_bkgr___12) 
freq(obs_2$ethn_bkgr___13) 
freq(obs_2$ethn_bkgr___14) 
freq(obs_2$ethn_bkgr___15) 
freq(obs_2$ethn_bkgr___16)

# Define ethnicity columns
ethn_cols <- paste0("ethn_bkgr___", 1:18)

# Asian group columns
asian_cols <- c("ethn_bkgr___8",   # East Asian
                "ethn_bkgr___12",  # South Asian
                "ethn_bkgr___13")  # Southeast Asian

obs_2_counts <- obs_2 %>%
  mutate(
    # Treat columns 17 and 18 as missing
    across(c("ethn_bkgr___17", "ethn_bkgr___18"), ~ NA_real_),
    
    # Count number of non-missing ethnicity endorsements
    n_endorsed = rowSums(across(all_of(ethn_cols)) == 1, na.rm = TRUE),
    
    # Identify Missing category:
    # - 17 or 18 originally = 1 (now NA)
    # - OR all 18 columns are NA or 0
    missing_flag = if_else(
      # all NA or 0 across all 18 columns
      rowSums(across(all_of(ethn_cols)), na.rm = TRUE) == 0,
      TRUE, FALSE
    ),
    
    # Group 1: White (European descent)
    # Includes White-only, White+Jewish, White+anything
    group_white = ethn_bkgr___15 == 1,
    
    # Group 2: East, South, or Southeast Asian
    group_asian = rowSums(across(all_of(asian_cols)) == 1, na.rm = TRUE) > 0,
    
    # Final mutually exclusive category
    ethnicity_group = case_when(
      missing_flag ~ "Missing",
      group_white ~ "White (European Descent)",
      group_asian ~ "East/South/Southeast Asian",
      TRUE ~ "Other and Mixed"
    )
  ) %>%
  count(ethnicity_group)

###

###54 mo covariates

#fetal sex
freq(obs$Male) #male = 1

#fetal weight
mean(obs$ weight, na.rm = TRUE) 
sd(obs$weight, na.rm = TRUE) 

#age at Ax
mean(obs$age_54, na.rm = TRUE) #(months)
sd(obs$age_54, na.rm = TRUE) 

#maternal age
freq(obs$age)
mean(obs$age, na.rm = TRUE) 
sd(obs$age, na.rm = TRUE)

#mother born
freq(obs$mother_born_not_Canada) #outside Canada = 1

#race
freq(obs$race) #non-White = 1

#hh_inc
freq(obs$hh_inc_lower150) #<150k = 1

#education
freq(obs$education_less_bach_grad) #1 = bach, 2 = graduate, 0 = less than bach

#employment 
freq(obs$curr_empl_full_part_unempl) #1 = part-time, 2 = unemployed, 0 = full-time



##ACEs descriptives, 1 = yes

freq(obs$childhood_swear)
#0 = 998 88%%  
#1 = 124 11%
#NA =  9 1%

freq(obs$childhood_push)
#0 = 1060 94% 
#1 =  62 5%
#NA = 9 1%

freq(obs$childhood_touch)
#0 = 1062 94%  
#1 =62 5%
#NA = 7 1%

freq(obs$childhood_mother)
#0 = 1074 95%
#1 =  46 4%
#NA = 11 1%

freq(obs$childhood_divorce)
#0 = 845 75%
#1 = 278 25%
#NA = 8 1%

freq(obs$childhood_alcohol)
#0 = 986 87%
#1 = 136 12%
#NA = 9 1%

freq(obs$childhood_suicide)
#0 = 914 81%   
#1 = 209 18%
#NA = 8 2%

freq(obs$childhood_prison)
#0 = 1107 98%  
#1 = 15 1%  
#NA = 9 1%

