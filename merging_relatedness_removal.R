#Now we need to merge everything: smoking variable, IDPs, non_imaging_confounds, imaging confounds

confounds_final_40k <- merge(non_imaging_confounds_for_smoking_final,processed_confounds,by="n_eid")
#N=29,974
smoking_confounds_final_40k <- merge(ukbio_smoking_subset,confounds_final_40k,by="n_eid")
#N=29,974

smoking_confounds_final_40k <- merge(smoking_confounds_final_40k,ukb48123_ukb47267_ID_map_20210830,by.x="n_eid",by.y="ukb48123_ID_1")

IDP_smoking_confounds_final_40k <- merge(smoking_confounds_final_40k,IDPs_40k_with_rf,by.x="ukb47267_ID_1",by.y="eid")
View(IDP_smoking_confounds_final_40k)
#N=29,695

#We removed the related pairs
eid_40k_kinship <- IDP_smoking_confounds_final_40k %>% select(ukb47267_ID_1,n_eid)

relatedness <- ukb48123_kinship %>% filter(ukb48123_kinship$ID1 %in% eid_40k_kinship$n_eid)

relatedness <- relatedness %>% filter(relatedness$ID2 %in% eid_40k_kinship$n_eid)
