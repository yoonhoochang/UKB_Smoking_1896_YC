#1. Imaging confounds (adopted from Vera's pipeline_step_2.Rmd)

library(dplyr) # Run install.packages("tidyverse") to get this
library(tidyr) # Run install.packages("tidyr") to get this
library(ggplot2)
library("reshape") # Run install.packages("reshape") to get this

#import image confounds (Janine's file) and change the name
Confounds_40k_8.30.21 <- read.delim("~/Desktop/UKB_40K/input/Confounds_40k_8-30-21.tsv")
imaging_confounds <- Confounds_40k_8.30.21

#match the IDs
ID_key <- read.csv("~/Desktop/UKB_40K/input/ukb48123_ukb47267_ID_map_20210830.txt", sep="")
ID_key <- dplyr::rename(ID_key, pheno_eid = ukb48123_ID_1, eid = ukb47267_ID_1)

imaging_confounds <- right_join(ID_key, imaging_confounds, by = "eid")
imaging_confounds <- imaging_confounds %>% filter(! is.na(pheno_eid))
imaging_confounds <- imaging_confounds %>% select(! eid) %>% dplyr::rename("n_eid" = "pheno_eid")

imaging_confounds <- imaging_confounds %>%
  dplyr::rename("sex" = "X31.0.0") %>%
  dplyr::rename("date" = "X53.2.0") %>%
  dplyr::rename("site" = "X54.2.0") %>%
  dplyr::rename("age" = "X21003.2.0") %>%
  dplyr::rename("head_size" = "X25000.2.0") %>%
  dplyr::rename("rfMRI_motion" = "X25741.2.0") %>%
  dplyr::rename("tfMRI_motion" = "X25742.2.0") %>%
  dplyr::rename("MHQ_date" = "X20400.0.0")

# Select the confounds to include in the analysis
imaging_confounds <- imaging_confounds %>%
  select(n_eid, sex, date, site, age, head_size, rfMRI_motion, tfMRI_motion)

#Convert dates to a numeric. In r this is the number of days since January 1 1970
#https://statistics.berkeley.edu/computing/faqs/dates-and-times-r

imaging_confounds <- imaging_confounds %>%
  mutate(date = as.Date(date)) %>%
  mutate(date = as.numeric(date))

#Scale the confounds using the median and median absolute deviation * 1.48. Do this for all columns except sex and site. If this is done to sex it turns into NaN and Inf
imaging_confounds <- imaging_confounds %>%
  mutate_at(c("date", "age", "head_size", "rfMRI_motion", "tfMRI_motion"), ~scale(.x, center = median(.x, na.rm = TRUE), scale = (mad(.x, , na.rm = TRUE) * 1.48)))

#Remove outliers greater than 8
imaging_confounds <- imaging_confounds %>%
  mutate(date = ifelse(abs(date) > 8, NA, date)) %>%
  mutate(age = ifelse(abs(age) > 8, NA, age)) %>%
  mutate(head_size = ifelse(abs(head_size) > 8, NA, head_size)) %>%
  mutate(rfMRI_motion = ifelse(abs(rfMRI_motion) > 8, NA, rfMRI_motion)) %>%
  mutate(tfMRI_motion = ifelse(abs(tfMRI_motion) > 8, NA, tfMRI_motion))

#split in sites
unique(imaging_confounds$site)

site1_conf <- imaging_confounds %>%
  filter(site == "11025") %>%
  mutate(site = "site1")

site2_conf <- imaging_confounds %>%
  filter(site == "11026") %>%
  mutate(site = "site2")

site3_conf <- imaging_confounds %>%
  filter(site == "11027") %>%
  mutate(site = "site3")

#Replace all NA and missing with the median for the site
site1_conf <- site1_conf %>%
  mutate(date = ifelse(is.na(date), median(site1_conf$date, na.rm = TRUE), date)) %>%
  mutate(age = ifelse(is.na(age), median(site1_conf$age, na.rm = TRUE), age)) %>%
  mutate(head_size = ifelse(is.na(head_size), median(site1_conf$head_size, na.rm = TRUE), head_size)) %>%
  mutate(rfMRI_motion = ifelse(is.na(rfMRI_motion), median(site1_conf$rfMRI_motion, na.rm = TRUE), rfMRI_motion)) %>%
  mutate(tfMRI_motion = ifelse(is.na(tfMRI_motion), median(site1_conf$tfMRI_motion, na.rm = TRUE), tfMRI_motion))


site2_conf <- site2_conf %>%
  mutate(date = ifelse(is.na(date), median(site2_conf$date, na.rm = TRUE), date)) %>%
  mutate(age = ifelse(is.na(age), median(site2_conf$age, na.rm = TRUE), age)) %>%
  mutate(head_size = ifelse(is.na(head_size), median(site2_conf$head_size, na.rm = TRUE), head_size)) %>%
  mutate(rfMRI_motion = ifelse(is.na(rfMRI_motion), median(site2_conf$rfMRI_motion, na.rm = TRUE), rfMRI_motion)) %>%
  mutate(tfMRI_motion = ifelse(is.na(tfMRI_motion), median(site2_conf$tfMRI_motion, na.rm = TRUE), tfMRI_motion))


site3_conf <- site3_conf %>%
  mutate(date = ifelse(is.na(date), median(site3_conf$date, na.rm = TRUE), date)) %>%
  mutate(age = ifelse(is.na(age), median(site3_conf$age, na.rm = TRUE), age)) %>%
  mutate(head_size = ifelse(is.na(head_size), median(site3_conf$head_size, na.rm = TRUE), head_size)) %>%
  mutate(rfMRI_motion = ifelse(is.na(rfMRI_motion), median(site3_conf$rfMRI_motion, na.rm = TRUE), rfMRI_motion)) %>%
  mutate(tfMRI_motion = ifelse(is.na(tfMRI_motion), median(site3_conf$tfMRI_motion, na.rm = TRUE), tfMRI_motion))

#Calculate the z-scores by site so that mean is zero and sd is 1
site1_conf <- site1_conf %>%
  mutate_at(c("date", "age", "head_size", "rfMRI_motion", "tfMRI_motion"), ~scale(.x, center = TRUE, scale = TRUE))

site2_conf <- site2_conf %>%
  mutate_at(c("date", "age", "head_size", "rfMRI_motion", "tfMRI_motion"), ~scale(.x, center = TRUE, scale = TRUE))

site3_conf <- site3_conf %>%
  mutate_at(c("date", "age", "head_size", "rfMRI_motion", "tfMRI_motion"), ~scale(.x, center = TRUE, scale = TRUE))

#check mean/sd
mean(site1_conf$head_size) #close to 0
sd(site1_conf$head_size)

#age^2 and date^2
site1_conf <- site1_conf %>%
  mutate(age_2 = age^2) %>%
  mutate(date_2 = date^2)

site2_conf <- site2_conf %>%
  mutate(age_2 = age^2) %>%
  mutate(date_2 = date^2)

site3_conf <- site3_conf %>%
  mutate(age_2 = age^2) %>%
  mutate(date_2 = date^2)

#age*sex
site1_conf <- site1_conf %>%
  mutate(sex = scale(sex, center = TRUE, scale = TRUE)) %>%
  mutate(age_sex = age * sex)

site2_conf <- site2_conf %>%
  mutate(sex = scale(sex, center = TRUE, scale = TRUE)) %>%
  mutate(age_sex = age * sex)

site3_conf <- site3_conf %>%
  mutate(sex = scale(sex, center = TRUE, scale = TRUE)) %>%
  mutate(age_sex = age * sex)

#stitch tables together
site1_conf <- site1_conf %>%
  dplyr::rename("site1_sex" = "sex") %>%
  dplyr::rename("site1_date" = "date") %>%
  dplyr::rename("site1_date_2" = "date_2") %>%
  dplyr::rename("site1_age" = "age") %>%
  dplyr::rename("site1_age_2" = "age_2") %>%
  dplyr::rename("site1_age_sex" = "age_sex") %>%
  dplyr::rename("site1_head_size" = "head_size") %>%
  dplyr::rename("site1_rfMRI_motion" = "rfMRI_motion") %>%
  dplyr::rename("site1_tfMRI_motion" = "tfMRI_motion")

site2_conf <- site2_conf %>%
  dplyr::rename("site2_sex" = "sex") %>%
  dplyr::rename("site2_date" = "date") %>%
  dplyr::rename("site2_date_2" = "date_2") %>%
  dplyr::rename("site2_age" = "age") %>%
  dplyr::rename("site2_age_2" = "age_2") %>%
  dplyr::rename("site2_age_sex" = "age_sex") %>%
  dplyr::rename("site2_head_size" = "head_size") %>%
  dplyr::rename("site2_rfMRI_motion" = "rfMRI_motion") %>%
  dplyr::rename("site2_tfMRI_motion" = "tfMRI_motion")


site3_conf <- site3_conf %>%
  dplyr::rename("site3_sex" = "sex") %>%
  dplyr::rename("site3_date" = "date") %>%
  dplyr::rename("site3_date_2" = "date_2") %>%
  dplyr::rename("site3_age" = "age") %>%
  dplyr::rename("site3_age_2" = "age_2") %>%
  dplyr::rename("site3_age_sex" = "age_sex") %>%
  dplyr::rename("site3_head_size" = "head_size") %>%
  dplyr::rename("site3_rfMRI_motion" = "rfMRI_motion") %>%
  dplyr::rename("site3_tfMRI_motion" = "tfMRI_motion")

#mutate to add 0s
site1_conf <- site1_conf %>%
  mutate("site2_sex" = 0) %>%
  mutate("site2_date" = 0) %>%
  mutate("site2_age" = 0) %>%
  mutate("site2_date_2" = 0) %>%
  mutate("site2_age_2" = 0) %>%
  mutate("site2_age_sex" = 0) %>%
  mutate("site2_head_size" = 0) %>%
  mutate("site2_rfMRI_motion" = 0) %>%
  mutate("site2_tfMRI_motion" = 0) %>%
  mutate("site2_age_2" = 0) %>%
  mutate("site2_date_2" = 0) %>%
  mutate("site2_age_sex" = 0) %>%
  mutate("site3_sex" = 0) %>%
  mutate("site3_date" = 0) %>%
  mutate("site3_age" = 0) %>%
  mutate("site3_date_2" = 0) %>%
  mutate("site3_age_2" = 0) %>%
  mutate("site3_age_sex" = 0) %>%
  mutate("site3_head_size" = 0) %>%
  mutate("site3_rfMRI_motion" = 0) %>%
  mutate("site3_tfMRI_motion" = 0) %>%
  mutate("site3_age_2" = 0) %>%
  mutate("site3_date_2" = 0) %>%
  mutate("site3_age_sex" = 0)

site2_conf <- site2_conf %>%
  mutate("site1_sex" = 0) %>%
  mutate("site1_date" = 0) %>%
  mutate("site1_age" = 0) %>%
  mutate("site1_date_2" = 0) %>%
  mutate("site1_age_2" = 0) %>%
  mutate("site1_age_sex" = 0) %>%
  mutate("site1_head_size" = 0) %>%
  mutate("site1_rfMRI_motion" = 0) %>%
  mutate("site1_tfMRI_motion" = 0) %>%
  mutate("site1_age_2" = 0) %>%
  mutate("site1_date_2" = 0) %>%
  mutate("site1_age_sex" = 0) %>%
  mutate("site3_sex" = 0) %>%
  mutate("site3_date" = 0) %>%
  mutate("site3_age" = 0) %>%
  mutate("site3_date_2" = 0) %>%
  mutate("site3_age_2" = 0) %>%
  mutate("site3_age_sex" = 0) %>%
  mutate("site3_head_size" = 0) %>%
  mutate("site3_rfMRI_motion" = 0) %>%
  mutate("site3_tfMRI_motion" = 0) %>%
  mutate("site3_age_2" = 0) %>%
  mutate("site3_date_2" = 0) %>%
  mutate("site3_age_sex" = 0)


site3_conf <- site3_conf %>%
  mutate("site1_sex" = 0) %>%
  mutate("site1_date" = 0) %>%
  mutate("site1_age" = 0) %>%
  mutate("site1_date_2" = 0) %>%
  mutate("site1_age_2" = 0) %>%
  mutate("site1_age_sex" = 0) %>%
  mutate("site1_head_size" = 0) %>%
  mutate("site1_rfMRI_motion" = 0) %>%
  mutate("site1_tfMRI_motion" = 0) %>%
  mutate("site1_age_2" = 0) %>%
  mutate("site1_date_2" = 0) %>%
  mutate("site1_age_sex" = 0) %>%
  mutate("site2_sex" = 0) %>%
  mutate("site2_date" = 0) %>%
  mutate("site2_age" = 0) %>%
  mutate("site2_date_2" = 0) %>%
  mutate("site2_age_2" = 0) %>%
  mutate("site2_age_sex" = 0) %>%
  mutate("site2_head_size" = 0) %>%
  mutate("site2_rfMRI_motion" = 0) %>%
  mutate("site2_tfMRI_motion" = 0) %>%
  mutate("site2_age_2" = 0) %>%
  mutate("site2_date_2" = 0) %>%
  mutate("site2_age_sex" = 0)

#merge everything
processed_confounds <- rbind(site1_conf, site2_conf, site3_conf)
#N=36,896 (without filtering)
