#2. IDPs_40k (from Janine)
IDPs_nonrest_40k <- read.delim("~/Desktop/UKB_40K/input/IDPs_nonrest_40k.tsv")

#2-1. Resting functional IDP groups (ICA6)
subs_netica <- read.csv("~/Desktop/UKB_40K/input/subs_netica.txt", header=FALSE)
#this is a raw output file from matlab scripts adopted from https://github.com/PersonomicsLab/MH_in_UKB/tree/master/CCA

#fix the column names N=37457
subs_netica <- subs_netica %>% select(eid=V1,ICA1=V2,ICA2=V3,ICA3=V4,ICA4=V5,ICA5=V6,ICA6=V7)
