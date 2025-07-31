rm(list=ls())

library(tidyverse)
#read in data file
df <- read.csv("path_to_file",header = TRUE, sep = ",")
dim(df)
#remove invalid records
df <- df %>% filter(str_detect(subject_id, "^111-"))
table(df$subject_type)
#keep mdd cases only
df <- filter(df,subject_type == 1) # proband == 1
dim(df)
#exclude medication naive from dataset
df <- filter(df,medication_ever == 1) # yes == 1
dim(df)
# probands currently using meds
df <- filter(df,curr_med_depr == 1) # yes == 1 
dim(df)
# whose current meds record is availble
df <- filter(df,medication_list_1 != "")
dim(df)

########################################
###                                  ###
###   making unique drug list        ###
###                                  ###
########################################


# recoding drugs
df <- df %>% mutate(across(all_of(current_drugs_col), ~case_when(
  . == 2  ~ "Amitriptyline", . == 1  ~ "Agomelatine", . == 4  ~ "Bupropion", . == 5  ~ "Citalopram",
  . == 6  ~ "Clomipramine", . == 8  ~ "Desvenlafaxine", . == 101  ~ "Dosulepin", . == 9  ~ "Doxepin",
  . == 97  ~ "Dosulepin", . == 10  ~ "Duloxetine", . == 11  ~ "Escitalopram", . == 12 ~ "Fluovoxamine",
  . == 13  ~ "Fluoxetine", . == 14  ~ "Imipramine", . == 21  ~ "Mirtazapine", . == 22  ~ "Moclobemide",
  . == 25  ~ "Nortriptyline", . == 104  ~ "Olazapine+Fluoxetine", . == 26  ~ "Paroxetine", . == 31  ~ "Selegiline",
  . == 33  ~ "Sertraline", . == 36  ~ "Trazodone", . == 39  ~ "Vilazodone", . == 38  ~ "Venlafaxine",
  . == 41  ~ "Vortioxetine", . == 102  ~ "Buspirone", . == 103  ~ "Etifoxine HCL", . == 106  ~ "Propranolol",
  . == 105  ~ "Pregabalin", . == 62  ~ "Carbamazepine", . == 64  ~ "Gabapentin",  . == 65  ~ "Lamotrigine",
  . == 66  ~ "Lithium", . == 69  ~ "Topiramate", . == 71  ~ "Valproic Acid", . == 42  ~ "Aripiprazole",
  . == 45  ~ "Chlorpromazine", . == 46  ~ "Clozapine", . == 47  ~ "Fluphenazine", . == 48  ~ "Haloperidol",
  . == 51  ~ "Lurasidone", . == 108  ~ "Levosulpiride", . == 52  ~ "Olanzapine", . == 53  ~ "Paliperidone",
  . == 55  ~ "Prochlorperazine", . == 56  ~ "Quetiapine", . == 57  ~ "Risperidone", . == 60  ~ "Trifluoperazine",
  . == 61  ~ "Ziprasidone", . == 73  ~ "Alprazolam", . == 96  ~ "Bromazepam",
  . == 75  ~ "Chlordiazepoxide", . == 95  ~ "Clobazam", . == 63  ~ "Clonazepam", . == 76  ~ "Clorazepate",
  . == 77  ~ "Diazepam", . == 79  ~ "Estazolam", . == 82  ~ "Lorazepam", . == 91  ~ "Triazolam",
  . == 94  ~ "Procyclidine", . == 109  ~ "Betahistine", . == 72  ~ "Memantine", 
  . == 80 ~ "Eszopiclone",. == 93 ~ "Zolpidem", TRUE  ~ NA_character_ )))

#generating unique drug list
current_drugs_col <- c("medication_list_1","medication_list_2","medication_list_3","medication_list_4",
                       "medication_list_5","medication_list_6", "medication_list_7")
current_drugs <- df[,current_drugs_col]
length(current_drugs)
# unlisting the vectors with durgs columns
current_drugs <- unlist(df[,current_drugs_col])
length(current_drugs)

# removing spaces or any NA values from list
current_drugs <- current_drugs[!is.na(current_drugs) & current_drugs != ""]
# get unique drugs list
current_drugs_unique <- unique(current_drugs)
length(current_drugs_unique)

# write unique drugs list out
write.table(current_drugs_unique, "current_drugs_unique_2.txt", quote = FALSE, row.names = FALSE, col.names = FALSE)

# classification of drugs into different classes

# ADs
Antidepressants = c("Amitriptyline","Agomelatine", "Bupropion", "Citalopram", "Clomipramine", "Desvenlafaxine", "Dosulepin", "Doxepin","Dothiepin (dosulpin)", "Duloxetine", "Escitalopram","Fluovoxamine",
                    "Fluoxetine", "Imipramine", "Mirtazapine", "Moclobemide", "Nortriptyline", "Olazapine+Fluoxetine",
                    "Paroxetine", "Selegiline", "Sertraline" , "Trazodone", "Vilazodone", "Venlafaxine", "Vortioxetine") # 25
# ANX
anxiolytics <- c("Buspirone","Etifoxine HCL","Propranolol", "Pregabalin") # 4

# MS
mood_stabilizers <- c("Carbamazepine", "Gabapentine", "Lamotrigine", "Lithium","Topiramate", "Valproic Acid") #6

# AP
antipsychotics <- c("Aripiprazole","Chlorpromazine", "Clozapine", "Fluphenazine", "Haloperidol",
                    "Lurasidone" ,"Levosulpiride", "Olanzapine", "Olazapine+Fluoxetine", "Paliperidone", "Prochlorperazine",
                    "Quetiapine","Risperidone", "Trifluoperazine", "Ziprasidone") # 15
# BDZ
benzodiazepines <- c("Alprazolam", "Bromazepam", "Chlordiazepoxide", "Clobazam", "Clonazepam",
                     "Clorazepate", "Diazepam", "Estazolam", "Lorazepam", "Triazolam") # 10

# S
Non_BDZ_Sleep_Agents <- c("Eszopiclone","Zolpidem") #2

#####################################################
###                                               ###
### making individual coulumns for every drug     ###
### Making number of drugs column for every group ###
###                                               ###
#####################################################


# current drugs dose columns
current_meds_dose <- c("medication_dose_1","medication_dose_2","medication_dose_3", "medication_dose_4",
                       "medication_dose_5","medication_dose_6","medication_dose_7")

# loop for drugs and doses
for (DRUG in current_drugs_unique[!is.na(current_drugs_unique)]) {
  # Create a column indicating the presence of the drug
  df[[DRUG]] <- as.integer(rowSums(df[current_drugs_col] == DRUG, na.rm = TRUE) > 0)
  # Initialize the dose column with NA values
  df[paste(DRUG, "dose.mg", sep = "_")] <- NA
  # For each drug column, check if the drug is present and then extract the dose
  for (i in 1:length(current_drugs_col)) {
    rows_with_drug <- which(df[[current_drugs_col[i]]] == DRUG)
    if (length(rows_with_drug) > 0) {
      # Convert the dose to numeric type
      df[rows_with_drug, paste(DRUG, "dose.mg", sep = "_")] <- as.numeric(df[rows_with_drug, current_meds_dose[i]])
    }
  }
}

## adding new columns to the data
# number of ADs presecribed in current meds
df$Current_AD_number <- rowSums(sapply(df[current_drugs_col], function(x) x %in% Antidepressants))

# AD present or not in current meds?
df <- df %>% mutate(AD_current_meds = ifelse(Current_AD_number > 0 , "Yes", "No"))

# number of MS presecribed in current meds
df$Current_MS_number <- rowSums(sapply(df[current_drugs_col], function(x) x %in% mood_stabilizers))

# MS present or not?
df <- df %>% mutate(MS_current_meds = ifelse(Current_MS_number > 0 , "Yes", "No"))

# number of AP presecribed in current meds
df$Current_AP_number <- rowSums(sapply(df[current_drugs_col], function(x) x %in% antipsychotics))

# AP present or not?
df <- df %>% mutate(AP_current_meds = ifelse(Current_AP_number > 0 , "Yes", "No"))

# for BDZ
df$Current_bdz_number <- rowSums(sapply(df[current_drugs_col], function(x) x %in% benzodiazepines))

df$BDZ_current_meds <- ifelse(apply(df[, current_drugs_col], 1, function(x) any(x %in% benzodiazepines)),"Yes","No")
# for anxiolytics
df$Current_anxiolytics_number <- rowSums(sapply(df[current_drugs_col], function(x) x %in% anxiolytics))
df$anxiolytics_current_meds <- ifelse(apply(df[, current_drugs_col], 1, function(x) any(x %in% anxiolytics)),"Yes","No")

# augmentation therapy , 
df <- df %>% mutate(augmentation_therapy = ifelse((AD_current_meds == "Yes" & MS_current_meds == "Yes") | (AD_current_meds == "Yes" & AP_current_meds == "Yes"), "Yes","No"))
# combination therapy , 
df <- df %>% mutate(combination_therapy = ifelse(Current_AD_number > 1,"Yes","No"))

# either combo or augmentation therapy
df <- df %>% mutate(combo_OR_augmentation = ifelse((augmentation_therapy == "Yes" | combination_therapy == "Yes"),"Yes", "No"))

# AD with BDZ ,
df <- df %>% mutate(AD_with_BDZ = ifelse((Current_AD_number > 0 & BDZ_current_meds == "Yes"), "Yes","No"))
# AD with anxiolytics
df <- df %>% mutate(AD_with_anxiolytics = ifelse((Current_AD_number > 0 & anxiolytics_current_meds == "Yes"), "Yes","No"))

# either combo or augmentation therapy or anxiolytics or BDZ with AD
df <- df %>% mutate(either_com_aug_anxiolytic_bdz_withAD = ifelse(augmentation_therapy == "Yes" | combination_therapy == "Yes" |
                                                      AD_with_anxiolytics == "Yes" | AD_with_BDZ == "Yes","Yes", "No"))


########################################
###                                  ###
### transforming some of variables   ###
###                                  ###
########################################

# transforming symptom change after treatment, to factor
df$sympt_change_after_treat <- factor(df$sympt_change_after_treat, levels = c(1,6,2,5,3,4),
              labels= c("complete remission",
                        "episodic symptoms relapse after I quit medications",
                        "partly relieved",
                        "episodic symptoms come and go even if I am taking medications",
                        "minimally relieved",
                        "no relief at all"), ordered = TRUE)

# dichotomizing the overall change after treatment
df$overall_change_after_treatment_binary <- ifelse(
  df$sympt_change_after_treat %in%
    c("complete remission","episodic symptoms relapse after I quit medications",
      "partly relieved","episodic symptoms come and go even if I am taking medications"), "significant change", 
  ifelse(
    df$sympt_change_after_treat %in%
      c("minimally relieved","no relief at all"),"No/minimal change",
    NA                                 
  ) 
)
# tabulate
table(df$overall_change_after_treatment_binary)

# Medication Adherence questions

# for forming into a score recode item 1 , after recoding yes is now scored '0'

df <- df %>% mutate(compliance___1 =ifelse(compliance___1 == 0 ,1,0))

# compliance items, adding them up gives us a score, higher the score poor the compliance
compliance_meds_items <- c("compliance___1","compliance___2","compliance___3","compliance___4")
df$compliance_meds_score <- rowSums(df[,compliance_meds_items])


# overall duration converted to weeks , months multiplied by 4.2 , years by 52,,
df$antidepress_how_long <- as.numeric(df$antidepress_how_long)
df$antidepress_how_long[df$antidepress_how_long == 888] <- NA # 888 is unknown

# duration of use was noted in weeks, months,or years, we convert them all to weeks to bring them all to one standard unit
df <- df %>%
  mutate(
    treatment_duration_converted_weeks = ifelse(
      antidepress_measure == 0,
      antidepress_how_long * 1,
      ifelse(
        antidepress_measure == 1,
        antidepress_how_long * 4.2,
        ifelse(
          antidepress_measure == 2,
          antidepress_how_long * 52,
          NA
        )
      )
    )
  )



##  converting VARAIbles to factors or to numerics as apropriate

# age of onset
df$age_onset <- as.numeric(df$age_onset)
# age
df$screener_age <- as.numeric(df$screener_age)

# duration of illness , this variable notes it down in weeks only upto 99
df$duration_of_illness <- as.numeric(df$dip_op08_duration)

# calculating illness duration from onset age and age ****
# illness_ duration 
df$duration_illness_years <- df$screener_age-df$age_onset # (this is done to calculate illnes duration in years)
# now that in some cases illness duration is less than an year ,this leads to 'zero' from above subtraction. so we use illness-duration in weeks column to
# to replace these 0s with values and converted to years
df <- df %>% mutate(duration_illness_years = ifelse(duration_illness_years == 0, duration_of_illness/52, duration_illness_years))
df$duration_illness_years <- round(df$duration_illness_years,2)


############
## Family history of psychiatric disorders , yes= 1 , no = 2
############

table(df$fam_hist_schiz)
table(df$fam_hist_depr)
table(df$fam_hist_bip)
table(df$fam_hist_anx)
table(df$fam_hist_ocd)
table(df$fam_hist_other)

df <- df %>% mutate(psychiatric_fam_hx_combined = ifelse(fam_hist_schiz == 1 |
                                                           fam_hist_depr == 1 | 
                                                           fam_hist_bip == 1 |
                                                           fam_hist_anx == 1 |
                                                           fam_hist_ocd == 1 |
                                                           fam_hist_other == 1 , 1,0))

# factorizing fam_hx
df$psychiatric_fam_hx_combined <- factor(df$psychiatric_fam_hx_combined, levels=c(0,1),labels=c("No","Yes"))

#########################
# Suicidal symptoms score
########################
df <- df %>% mutate(
  SBQ_item_1_score = ifelse(sbq_suicide_idea_behav == 1, 1,
                            ifelse(sbq_suicide_idea_behav == 2,2,
                                   ifelse(sbq_suicide_idea_behav 
                                          %in% c(3,4), 3,
                                          ifelse(sbq_suicide_idea_behav 
                                                 %in% c(5,6), 4, NA)
      )
    )
  )
)

# 2nd item
# can be used as such
df$sbq_suicide_thought[is.na(df$sbq_suicide_thought)] <- 0
df$SBQ_item_2_score <- df$sbq_suicide_thought

# 3rd item

df <- df %>% mutate(
  SBQ_item_3_score = ifelse(sbq_suicide_told_smb == 1, 1,
                            ifelse(sbq_suicide_told_smb %in% c(2,3) ,2,
                                   ifelse(sbq_suicide_told_smb %in% c(4,5) ,3,0
      )
    )
  )
)

table(df$SBQ_item_3_score)

SBQ_all_items <- c("SBQ_item_1_score","SBQ_item_2_score","SBQ_item_3_score")
df$SBQ_composite_score <- rowSums(df[,SBQ_all_items],na.rm = TRUE)

# previous suicide attempts
df <- df %>% mutate(
  prevous_suicide_attempt = ifelse(sbq_suicide_idea_behav %in% 
                                     c(5,6), 1, 0))
# factorizing ?
df$prevous_suicide_attempt <- factor(df$prevous_suicide_attempt, levels=c(0,1),
                                     labels=c("No","Yes"))
# intentional self harm
df$self_harming <- factor(df$self_harming, levels = c(0,1), labels=c("No","Yes"))
table(df$self_harming)

#################
# Psychotic symptoms
################

df <- df %>%  mutate(psychotic_sym_Del_Hal = ifelse(delusions == 1 | auditory_hallucinations == 1, 1,
                                                ifelse(delusions == 2 & auditory_hallucinations == 2, 0,NA)))
#factorizing
df$psychotic_sym_Del_Hal <- factor(df$psychotic_sym_Del_Hal, levels=c(0,1), labels=c("No","Yes"))


##############
# psych comorbidities
##############

# anxiety ## 0 = No, Yes = 1

anxiety_symp_all <- c("anxiety_screening","anxiety_1","anxiety_2","anxiety_3") 
df$anxiety_symp_score <- rowSums(df[,anxiety_symp_all], na.rm = TRUE)
# anxiety_presenst
df <- df %>% mutate(anxiety_symptoms = ifelse(anxiety_symp_score == 0, 0,1))
#factorizing
df$anxiety_symptoms <- factor(df$anxiety_symptoms, levels=c(0,1), labels=c("No","Yes"))

# PTSD
ptsd_symp_all <- c("ptsd_flashbacks","ptsd_avoidance","ptsd_problems_sleeping")
df <- df %>% mutate(across(all_of(ptsd_symp_all), ~ ifelse(. == 2,0,.)))
df$ptsd_symp_score <- rowSums(df[,ptsd_symp_all], na.rm = TRUE)
df <- df %>% mutate(ptsd_symptoms=ifelse(ptsd_symp_score > 0,1,0))
df$ptsd_symptoms <- factor(df$ptsd_symptoms, levels=c(0,1), labels=c("No","Yes"))

# Panic attacks
df <- df %>%  mutate(panic_attack_symptoms = ifelse(panic_attack_symptoms == 1, 1,
                                               ifelse(panic_attack_symptoms == 2, 0,NA)))
#factorizing
df$panic_attack_symptoms <- factor(df$panic_attack_symptoms, levels = c(0,1), labels = c("No","Yes"))

# OCD
# OCD symptoms
df <- df %>%  mutate(ocd_symptoms = ifelse(worrying == 1 | ocd_more_than_hour == 1, 1,
                                                    ifelse(worrying == 2 & ocd_more_than_hour == 2, 0,NA)))
#factorizing
df$ocd_symptoms <- factor(df$ocd_symptoms, levels = c(0,1), labels = c("No","Yes"))

# ADHD
adhd_symp_all <- c("adhd_sitting_still","adhd_concentration","adhd_impulsive")
df$adhd_symp_score <- rowSums(df[,adhd_symp_all],na.rm = TRUE)
# ADHD symptom present or not
df <- df %>%  mutate(adhd_symptoms = ifelse(adhd_symp_score > 0 ,1, 0))
#factorizing
df$adhd_symptoms <- factor(df$adhd_symptoms, levels=c(0,1), labels=c("No","Yes"))

## psych comorbidities
df <- df %>% mutate(psychiatric_comorbids = ifelse(anxiety_symptoms == "Yes" |
                                                     panic_attack_symptoms == "Yes"| ocd_symptoms == "Yes" | 
                                                     adhd_symptoms == "Yes" | ptsd_symptoms == "Yes",1,0))

df$psychiatric_comorbids <- factor(df$psychiatric_comorbids, levels=c(0,1), labels=c("No","Yes"))

###############
## nicotine use
##############
tobacco_use <- c("tobacco_ever_used","cig_more_100","cig_everyday","cig_wake_up","cig_dif_quit")
df$tobacco_addiction_score <- rowSums(df[,tobacco_use],na.rm = TRUE)

#############################
## physical condition status
############################
physical_condition_all <- c("stroke","epilepsy_seizures","neurological_disorders","heart_problem",
                            "high_blood_pres","hypothyroid","high_blood_sugar",
                            "sleep_problem","chronic_problems_list___1","chronic_problems_list___2",
                            "chronic_problems_list___3","chronic_problems_list___4","chronic_problems_list___5",
                            "chronic_problems_list___6","chronic_problems_list___8","chronic_problems_list___9",
                            "chronic_problems_list___10","chronic_problems_list___11","chronic_problems_list___12")

df <- df %>% mutate(across(all_of(physical_condition_all), ~ if_else(. == 2, 0, .)))
df$physical_condition_score <- rowSums(df[,physical_condition_all], na.rm=TRUE)
df <- df %>% mutate(physical_condition_status=ifelse(physical_condition_score > 0,1,0))
df$physical_condition_status <- factor(df$physical_condition_status, levels=c(0,1), labels=c("No","Yes"))


##########
# demographics
##########

# Sex
df$screener_sex <- factor(df$screener_sex, levels = c(1,2), labels = c("Male","Female"))
# consangunity
df$related_parents <- factor(df$related_parents, levels = c(1,2,3,4), labels = c("Not related","First cousins","Second cousins","Unknown"))

# Recode 5 and 6 to a single value, e.g., 5
df$province_recode <- ifelse(df$ethnic_self_provincial %in% c(5, 6), 5, df$ethnic_self_provincial)
df$province_recode <- factor(df$province_recode,levels = c(1, 2, 3, 4, 5),labels = c("KPK", "Punjab", "Sindh", "Balochistan", "Other"))

# marital status
df$marital_status <- factor(df$se_married, levels = c(1,2,3,4,5),labels=c("Married","Engaged","Widowed","Divorced/separated","Never married"))
# education
df$education_status <- ifelse(df$se_education_self %in% c(2,3,4,5,6,7),1,0)
df$education_status <- factor(df$education_status, levels=c(0,1), labels = c("No","Yes"))

# home facilities
home_facilities <- c("se_facilities_home___1","se_facilities_home___2","se_facilities_home___3",
                     "se_facilities_home___4","se_facilities_home___5","se_facilities_home___6",
                     "se_facilities_home___7","se_facilities_home___8")

# add to get score
df$home_facilities_total_score <- rowSums(df[,home_facilities], na.rm = TRUE)
table(df$home_facilities_total_score)


##########################
## battering scale scoring
#########################
battering_scale_items <- c("web_1","web_2","web_3","web_4","web_5","web_6","web_7","web_8","web_9","web_10")
df$battering_scale_score <- rowSums(df[,battering_scale_items], na.rm=TRUE)



## stressful life events

lec_items <-c("te_bomb___1","te_bomb___2","te_bomb___3","te_bomb___4","te_bomb___5","te_bomb___6",
              "te_transport_accident___1","te_transport_accident___2","te_transport_accident___3",
              "te_transport_accident___4","te_transport_accident___5","te_transport_accident___6",
              "te_other_serious_accident___1","te_other_serious_accident___2","te_other_serious_accident___3",
              "te_other_serious_accident___4","te_other_serious_accident___5","te_other_serious_accident___6",
              "te_physical_assault___1","te_physical_assault___2","te_physical_assault___3",
              "te_physical_assault___4","te_physical_assault___5","te_physical_assault___6",
              "te_assault_weapon___1","te_assault_weapon___2","te_assault_weapon___3",
              "te_assault_weapon___4","te_assault_weapon___5","te_assault_weapon___6",
              "te_sexual_assault___1","te_sexual_assault___2","te_sexual_assault___3",
              "te_sexual_assault___4","te_sexual_assault___5","te_sexual_assault___6",
              "te_other_unwanted_sex___1","te_other_unwanted_sex___2","te_other_unwanted_sex___3",
              "te_other_unwanted_sex___4","te_other_unwanted_sex___5","te_other_unwanted_sex___6",
              "te_war_zone___1","te_war_zone___2","te_war_zone___3","te_war_zone___4",
              "te_war_zone___5","te_war_zone___6",
              "te_captivity___1","te_captivity___2","te_captivity___3","te_captivity___4",
              "te_captivity___5","te_captivity___6",
              "te_sudden_death___1","te_sudden_death___2","te_sudden_death___3","te_sudden_death___4",
              "te_sudden_death___5","te_sudden_death___6",
              "te_injury_smb_else___1","te_injury_smb_else___2","te_injury_smb_else___3",
              "te_injury_smb_else___4","te_injury_smb_else___5","te_injury_smb_else___6")


happend_to_me <- lec_items[grep("___1", lec_items)]
witnessed_it <- lec_items[grep("___2", lec_items)]
happened_close_fam_friend <- lec_items[grep("___3", lec_items)]
part_of_job <- lec_items[grep("___4", lec_items)]
not_sure <- lec_items[grep("___5", lec_items)]
does_not_apply <- lec_items[grep("___6", lec_items)]

# scoring
df <- df %>% mutate(across(all_of(happend_to_me), ~ ifelse(. == 1,3,.)))
df <- df %>% mutate(across(all_of(witnessed_it), ~ ifelse(. == 1,2,.)))
# happened to close family friend is 1 so no need to recode
df <- df %>% mutate(across(all_of(part_of_job), ~ ifelse(. == 1,2,.)))
df <- df %>% mutate(across(all_of(not_sure), ~ ifelse(. == 1,0,.)))
df <- df %>% mutate(across(all_of(does_not_apply), ~ ifelse(. == 1,0,.)))

df$lec_score <-  rowSums(df[,lec_items], na.rm=TRUE)

######################
# OSlo social support
#####################
osss_items <- c("osss_number_close_people","osss_interest_from_others","osss_help_from_others")
df$OSSS_score <- rowSums(df[,osss_items], na.rm=TRUE)

# changing OSSS score to categorical
df$OSSS_cat <- ifelse(df$OSSS_score %in% c(3,4,5,6,7,8), "poor",
                      ifelse(df$OSSS_score %in% c(9,10,11),"moderate",
                             ifelse(df$OSSS_score %in% c(12,13,14),"strong",NA)))

# chamgimg to factor
df$OSSS_cat <- factor(df$OSSS_cat, levels = c("poor", "moderate", "strong"))
df$OSSS_cat <- relevel(df$OSSS_cat, ref = "poor")


## merging in new variables for Sites
# reading in the clinic info data
df1 <- read.csv("path_to_file", sep =",", header= TRUE)
colnames(df1)[2] ="subject_id"
df1 <- df1[!duplicated(df1$subject_id),]
df <- merge(df, df1[, c('subject_id','Site')], by = 'subject_id')


# creating regional (province) variable
df <- df %>% 
  mutate(region_province = ifelse(Site %in% c("Albari Clinic","Al-Mustafa Clinic","BBH","DHQ Faisalabad","Haji Abdul Qayyum Hospital Sahiwal",
                                              "SZH&MC","Dr. Rasheed Khalid","BVH","SOLACE","The Brain Clinic","Pakistan Institute of Medical Sciences",
                                              "Shakoor Mind Care Institute Hospital","Falah Brain Center","DHQ Sahiwal"), "Punjab",
                                  ifelse(Site %in% c("ATH MTI","Dr. Ashfaq Clinic","Dr. Shah Muhammad","Dr. Nizam Ali Clinic",
                                                     "Ibadat Hospital","MMMTH DIK","Saidu Teaching Hospital","SPC","Dr. Kamran","IPH"), "KP",
                                         ifelse(Site %in% c("Civil Hospital Hyderabad","CJIP","Dr. Raza ur Rehman Clinic","Karwan-e-Hayat",
                                                            "Mukhtar Executive Clinic","Hyderabad Consulting Chamber","JPMC","Mind Health Clinic"), "Sindh",
                                                ifelse(Site %in% c("Akram Hospital","BMC Quetta"), "Balochistan", NA)
          )
        )
     )
  )

# creating setup type
df <- df %>% 
  mutate(setup_type = ifelse(Site %in% c("ATH MTI","BBH","BVH","DHQ Faisalabad","SZH&MC","MMMTH DIK","Pakistan Institute of Medical Sciences","Saidu Teaching Hospital",
                                         "Civil Hospital Hyderabad","CJIP","Karwan-e-Hayat","BMC Quetta"), "Public",
                             ifelse(Site %in% c("Albari Clinic","Al-Mustafa Clinic","Haji Abdul Qayyum Hospital Sahiwal","Dr. Rasheed Khalid","SOLACE","The Brain Clinic",
                                                "Dr. Ashfaq Clinic","Dr. Shah Muhammad","Dr. Nizam Ali Clinic","Ibadat Hospital","SPC","Dr. Kamran","IPH",
                                                "Dr. Raza ur Rehman Clinic","Mukhtar Executive Clinic","Hyderabad Consulting Chamber","Akram Hospital"), "Private",NA)
    )
  )


#############
# TRD Def 3 #
############

# selecting patients with AD in current meds 
TRD_DEF_3_DATASET <- subset(df,AD_current_meds == "Yes")

# only retaining those who had 12 weeks of overall treatmnt duration  
TRD_DEF_3_DATASET <- subset(TRD_DEF_3_DATASET, treatment_duration_converted_weeks >= 12 )

# retaining those whose compliance was good or intermediate
TRD_DEF_3_DATASET <- subset(TRD_DEF_3_DATASET, compliance_meds_score < 3)


# define TRD and NTRD status for def 3
TRD_DEF_3_DATASET <- TRD_DEF_3_DATASET %>%
  mutate(TRD_DEF_3 = ifelse(overall_change_after_treatment_binary == "significant change" , "NTRD", 
                              ifelse(overall_change_after_treatment_binary == "No/minimal change", "TRD", NA)))


TRD_DEF_3_DATASET <- TRD_DEF_3_DATASET[!is.na(TRD_DEF_3_DATASET$TRD_DEF_3), ]



# Code 0 ,1 for regression
TRD_DEF_3_DATASET <- TRD_DEF_3_DATASET %>%
  mutate(TRD_DEF_3 = ifelse(overall_change_after_treatment_binary == "significant change" , 0, 
                             ifelse(overall_change_after_treatment_binary == "No/minimal change", 1, NA)))


##################################
# checking and removing missing data
##################################
selected_vars <- c("subject_id","TRD_DEF_3","age_onset","duration_illness_years","screener_age","psychiatric_fam_hx_combined",
                   "SBQ_composite_score","prevous_suicide_attempt","self_harming","psychiatric_comorbids","psychotic_sym_Del_Hal",
                   "tobacco_addiction_score","physical_condition_status","screener_sex","related_parents","ethnic_self_provincial",
                   "marital_status","education_status","home_facilities_total_score","battering_scale_score",
                   "lec_score","OSSS_cat","Site","setup_type","augmentation_therapy","combination_therapy",
                   "AD_with_BDZ","AD_with_anxiolytics","combo_OR_augmentation")



selected_data <- TRD_DEF_3_DATASET[, selected_vars] # only variables needed for analysis
complete_cases <- selected_data[complete.cases(selected_data),] # removing rows with NAs

save.image("complete_cases.RData") ## contains risk factors and TRD def cleaned data














