#imputation based on this post (https://datascienceplus.com/handling-missing-data-with-mice-package-a-simple-approach/)
library(mice)
init = mice(non_imaging_confounds_for_smoking,maxit=0)
meth = init$method
predM = init$predictorMatrix

#eid will not be included as a predictor, so I set it to predM = 0
predM[, c("n_eid")]=0

#sex will be skipped, but will be used in the prediction 
meth[c("sex")]=""

#different variables with different method: continuous variables were mainly using norm method, while categorical income was using polyreg (common usages)
meth[c("age_completed_imputed")]="norm"
meth[c("Income")]="polyreg"
meth[c("bmi")]="norm"
meth[c("waist_hip")]="norm"
meth[c("dbp")]="norm"
meth[c("sbp")]="norm"
meth[c("week_drinks")]="norm"

set.seed(103)
imputed = mice(non_imaging_confounds_for_smoking,method=meth,predictorMatrix = predM,m=5)
imputed <- complete(imputed)
sapply(imputed,function(x) sum(is.na(x)))
#okay we have no missing values now

non_imaging_confounds_for_smoking_final <- imputed
