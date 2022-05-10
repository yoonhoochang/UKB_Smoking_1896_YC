#3. Smoking variables
#ukb_smoking_40k_3 is the input file with needed variables from both initial and imaging visit (3 means it was extracted in March)
ukb_smoking_40k_3 <- read.csv("~/Desktop/UKB_40K/input/ukb_smoking_40k_3.csv", sep="")
View(ukb_smoking_40k_3)
#N=502,485

ukbio_smoking <- ukb_smoking_40k_3 %>% select(n_eid,n_1239_0_0,n_1249_0_0,n_2644_0_0,n_1239_2_0,n_1249_2_0,n_2644_2_0,n_20161_0_0,n_20161_2_0,n_2897_0_0,n_2897_2_0)
View(ukbio_smoking)

# Ever daily smoked: 1) current daily smoking (1 for n_1239_0_0 and n_1239_2_0), 2) former daily smoking (1 for n_1249_0_0 and n_1249_2_0), 3) current -> former (1 for n_1239_0_0 and 1 for n_1249_2_0), 4) former -> current (1 for n_1249_0_0 and 1 for n_1239_2_0)

current_daily <- ukbio_smoking %>% 
  filter(n_1239_0_0 == "Yes, on most or all days" & n_1239_2_0 == "Yes, on most or all days") %>% mutate(ever_daily_smoked = "1") 

former_daily <- ukbio_smoking %>% 
  filter(n_1249_0_0=="Smoked on most or all days" & n_1249_2_0 == "Smoked on most or all days") %>% mutate(ever_daily_smoked = "1")

current_former_daily <- ukbio_smoking %>% 
  filter(n_1239_0_0 == "Yes, on most or all days" & n_1249_2_0 == "Smoked on most or all days") %>% mutate(ever_daily_smoked = "1")

former_current_daily <- ukbio_smoking %>% 
  filter(n_1249_0_0=="Smoked on most or all days" & n_1239_2_0 == "Yes, on most or all days") %>% mutate(ever_daily_smoked = "1")

ever_daily_smoker <- rbind(current_daily,former_daily,current_former_daily,former_current_daily)

# Never smoked: 1) Never smoked (0 for n_1249_0_0 and n_1249_2_0), 2) Less than 100 (0 for n_2644_0_0 and n_2644_2_0), 3) less than 100 -> never (0 for n_2644_0_0 and 0 for n_1249_2_0), 4) never -> less than 100 (0 for n_1249_0_0 and 0 for n_2644_2_0)

never_smoked <- ukbio_smoking %>% 
  filter(n_1249_0_0=="I have never smoked" & n_1249_2_0 == "I have never smoked") %>% mutate(ever_daily_smoked = "0")

less_than100 <- ukbio_smoking %>% 
  filter(n_2644_0_0=="No" & n_2644_2_0 == "No") %>% mutate(ever_daily_smoked = "0")

less_than100_never <- ukbio_smoking %>% 
  filter(n_2644_0_0=="No" & n_1249_2_0 == "I have never smoked") %>% mutate(ever_daily_smoked = "0")

never_lessthan100 <- ukbio_smoking %>% 
  filter(n_1249_0_0=="I have never smoked" & n_2644_2_0 == "No") %>% mutate(ever_daily_smoked = "0")

never_daily_smoked <- rbind(never_smoked,less_than100,less_than100_never,never_lessthan100)

smoking_stauts <- rbind(ever_daily_smoker,never_daily_smoked) %>% select(n_eid,ever_daily_smoked)
#N=39183

View(smoking_stauts)
ukbio_smoking_subset <- merge(ukbio_smoking,smoking_stauts,by="n_eid")
View(ukbio_smoking_subset)
#We work with 39,583 samples now

#we need age and sex separately for this analysis, so let's extract this woo-hoo 
#match with two versions of IDs 
ukb48123_ukb47267_ID_map_20210830 <- read.csv("~/Desktop/UKB_40K/input/ukb48123_ukb47267_ID_map_20210830.txt", sep="")
age_sex_40k <- Confounds_40k_8.30.21 %>% select(n_eid=eid,X31.0.0,X21003.2.0)
age_sex_40k <- merge(age_sex_40k,ukb48123_ukb47267_ID_map_20210830,by.x="n_eid",by.y="ukb47267_ID_1")

# <<Pack years>>
#Back-filling imaging pack years with initial visit pack years 
#If the cell is NA for imaging, then grab whatever value present from the initial dataset using "coalesce" function in dplyr (https://dplyr.tidyverse.org/reference/coalesce.html)

packyears_backfilled <- ukbio_smoking_subset %>% 
  mutate(packyears_imputed=coalesce(n_20161_2_0,n_20161_0_0))
#drop NAs, check the sample size
packyears_backfilled <- packyears_backfilled %>% drop_na(packyears_imputed) %>% select(n_eid,packyears_imputed) 
View(packyears_backfilled)
#N=10915 

ukbio_smoking_subset <- merge(ukbio_smoking_subset,packyears_backfilled,by="n_eid",all=T)
View(ukbio_smoking_subset)
smoking_eids <- ukbio_smoking_subset %>% select(n_eid)

#time since smoking cessation
#Here, we get the "Age of recruitment from Imaging subset"
#Meaning:
age_imaging <- Confounds_40k_8.30.21 %>% select(n_eid=eid,X21003.2.0)
ukbio_smoking_subset <- merge(ukbio_smoking_subset,ukb48123_ukb47267_ID_map_20210830,by.x="n_eid",by.y="ukb48123_ID_1")
ukbio_smoking_subset <- merge(ukbio_smoking_subset,age_imaging,by.x="ukb47267_ID_1",by.y="n_eid")

# <<Time since smoking cessation>>
# Back-filling imaging of smoking with initial age of smoking 
# Time since smoking cessation: Age - Age stopped smoking 
# Current daily smokers for imaging study (n_1239_0_0 = Yes, on most or all days) have 0 time since smoking cessation 

#removing character values (we only have one) and change the column into numeric, allowing coalesce to work 
ukbio_smoking_subset$n_2897_0_0[ukbio_smoking_subset$n_2897_0_0=="Do not know"] <- ""
ukbio_smoking_subset$n_2897_0_0 <- as.numeric(as.character(ukbio_smoking_subset$n_2897_0_0))

agestop_backfilled <- ukbio_smoking_subset %>% mutate(agestop_imputed=coalesce(n_2897_2_0,n_2897_0_0)) %>% mutate(time_since_cessation = X21003.2.0-agestop_imputed) 

#if they said they are current daily smokers in imaging study (n_1239_0_0=1), then their time since cessation value is 0 
agestop_backfilled <- agestop_backfilled %>% mutate(time_since_cessation = ifelse(n_1239_2_0==1,0,time_since_cessation))

#drop NAs, check the sample size
time_since_smoking_cessation <- agestop_backfilled %>% drop_na(time_since_cessation) %>% select(n_eid,time_since_cessation)
#N=7686 
