#3. Non-imaging confounds (+alcohol variable)
#3-1. Non-imaging confounds without alcohol (BMI=23104, waist/hip ratio=48/49, Systolic BP=4080, Diastolic BP=4079, Income=738, age-completed full time education=845, qualifications=6138, number indicating UKB data-field)

non_imaging_confounds <- ukb_smoking_40k_3 %>% select(n_eid,n_4080_2_0,n_4080_0_0,n_4079_2_0,n_4079_0_0,n_21001_0_0,n_738_2_0,n_738_0_0,n_6138_2_0,n_6138_0_0,n_48_2_0,n_48_0_0,n_49_2_0,n_49_0_0,n_845_2_0,n_845_0_0,n_31_0_0,n_21022_0_0)

non_imaging_confounds$n_845_0_0[non_imaging_confounds$n_845_0_0=="Do not know"] <- NA
non_imaging_confounds$n_845_0_0[non_imaging_confounds$n_845_0_0=="Prefer not to answer"] <- NA
non_imaging_confounds$n_845_0_0[non_imaging_confounds$n_845_0_0=="Never went to school"] <- NA
non_imaging_confounds$n_845_0_0 <- as.numeric(as.character(non_imaging_confounds$n_845_0_0))

#calculate the missing percentage #package MICE
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(non_imaging_confounds,2,pMiss)

# Backfillling with initial visit data
non_imaging_confounds <- non_imaging_confounds %>% mutate(qualification=coalesce(n_6138_2_0,n_6138_0_0),bmi=n_21001_0_0,dbp=coalesce(n_4079_2_0,n_4079_0_0),sbp=coalesce(n_4080_2_0,n_4080_0_0),Income=coalesce(n_738_2_0,n_738_0_0),waist=coalesce(n_48_2_0,n_48_0_0),hip=coalesce(n_49_2_0,n_49_0_0),age_completd_edu = coalesce(n_845_2_0,n_845_0_0),sex=n_31_0_0,age=n_21022_0_0) %>% mutate(waist_hip = waist/hip) #make waist/hip ratio from waist circumference, hip circumference data fields

#Change Income to categorical "numbers" 

#remove do not know and prefer not to answer
non_imaging_confounds$Income[non_imaging_confounds$Income=="Do not know"] <- NA
non_imaging_confounds$Income[non_imaging_confounds$Income=="Prefer not to answer"] <- NA

#from smaller income to bigger income, the numbers 1-5 are assigned 
non_imaging_confounds <- non_imaging_confounds %>% mutate(Income = replace(Income,Income=="Less than 18,000","1")) %>% mutate(Income = replace(Income,Income=="18,000 to 30,999","2")) %>% mutate(Income = replace(Income,Income=="31,000 to 51,999","3")) %>% mutate(Income = replace(Income,Income=="52,000 to 100,000","4")) %>% mutate(Income = replace(Income,Income=="Greater than 100,000","5")) 

#convert to categorical variables with 5 levels 
non_imaging_confounds$Income <- as.factor(as.character(non_imaging_confounds$Income))

#Change qualifications to categorical "numbers" 

#remove none of the above and prefer not to answer
non_imaging_confounds$qualification[non_imaging_confounds$qualification=="None of the above"] <- NA
non_imaging_confounds$qualification[non_imaging_confounds$qualification=="Prefer not to answer"] <- NA

#Average age of completing following education = A levels: 17, college/university: 23, gcse/cse: 15, professional qualifications (nursing/teaching): 22, nvq/hnd/hnc: unknown)
non_imaging_confounds <- non_imaging_confounds %>% mutate(qualification = replace(qualification,qualification=="A levels/AS levels or equivalent","17")) %>% mutate(qualification = replace(qualification,qualification=="CSEs or equivalent","15")) %>% mutate(qualification = replace(qualification,qualification=="Other professional qualifications eg: nursing, teaching","22")) %>% mutate(qualification = replace(qualification,qualification=="O levels/GCSEs or equivalent","15")) %>% mutate(qualification = replace(qualification,qualification=="College or University degree","23")) %>% mutate(qualification = replace(qualification,qualification=="NVQ or HND or HNC or equivalent",NA))

#change qualification column to numeric, this is to impute for age completed full time education
non_imaging_confounds$qualification<- as.numeric(as.character(non_imaging_confounds$qualification))

#impute age_completed full time education with the qualification average age 
non_imaging_confounds <- non_imaging_confounds %>% mutate(age_completed_imputed=coalesce(age_completd_edu,qualification))

pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(non_imaging_confounds,2,pMiss)
#we are still missing 25% -> this will be filled by MICE 

#let's do alcohol for 40k
alcohol_40k <- calculate_drinks(baseline_alcohol_1_19_22)
View(alcohol_40k)
alcohol_40k <- alcohol_40k %>% select(n_eid,week_drinks)

non_imaging_confounds <- merge(non_imaging_confounds,alcohol_40k,by="n_eid")
View(non_imaging_confounds)

non_imaging_confounds_for_impute <- non_imaging_confounds %>% select(n_eid,bmi,dbp,sbp,Income,sex,waist_hip,age_completed_imputed,week_drinks)

non_imaging_confounds_for_smoking <- merge(non_imaging_confounds_for_impute,smoking_eids,by="n_eid")
