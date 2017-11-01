
############################
## REGRESSION: PTLDS Only ##
############################

## extract covariates

vars = c("state", "group2", "tick", "time_en", "time_tx", 
         "pretx_inax", "pretx_st", "ax_tot", "misdx",
         "age", "gender", "racecat", "vcaus_pe")
temp = d.c[ ,c("pid", vars)]

#############
## 10. Age ##
#############

temp$Age = temp$age

################
## 11. Gender ##
################

temp$Gender = factor(temp$gender, labels=c("Female", "Male"))

##############
## 12. Race ##
##############

temp$Race = factor(temp$racecat, labels=c("Other", "White"))

#################################
## 13. Allergies, Cold, or Flu ##
#################################

temp$Cold.Flu = ifelse(temp$vcaus_pe==1, 1, 0)
temp$Cold.Flu[which(is.na(temp$Cold.Flu))] = 0
temp$Cold.Flu = factor(temp$Cold.Flu, labels=c("Allergy/NA", "Cold/Flu"))

####################
## 1. first layer ##
####################

## lymph

lv = code[which(code$layer==1), 2]
d1 = d[ ,c("alive.lymph", lv, "group", "pid")]
k = 2 # starting index of variables of interest (lv)
k.den = 1 # index of denominator
labs = c("TCR Gamma Delta", "CD4+ T", "CD8+ T", "CD4- CD8-",
         "NK", "CD8+ NK", "CD3-", "B")

coef1 = summary_regression(lv, d1, k, k.den, labs)

#####################
## 2. second layer ##
#####################

## CD4+ T, Memory

lv = c("cd4.pos.naive", 
       "cd4.pos.cm", 
       "cd4.pos.em", 
       "cd4.pos.emra")
d1 = d[ ,c("cd4.pos.t", lv, "group", "pid")]
k = 2 # starting index of variables of interest (lv)
k.den = 1 # index of denominator
labs = c("CD4+ T Naive", "CD4+ T CM", "CD4+ T EM", "CD4+ T EMRA")

coef2 = summary_regression(lv, d1, k, k.den, labs)

## CD4+ T, Th

lv = c("cd4.pos.th1",                       
       "cd4.pos.th2",
       "cd4.pos.th17",                       
       "cd4.pos.th1.17",
       "cd4.pos.th9")
d1 = d[ ,c("cd4.pos.t", lv, "group", "pid")]
k = 2 # starting index of variables of interest (lv)
k.den = 1 # index of denominator
labs = c("CD4+ Th1", "CD4+ Th2", "CD4+ Th17", "CD4+ 1/17", "CD4+ Th9")

coef3 = summary_regression(lv, d1, k, k.den, labs)

## CD8+ T, Memory

lv = c("cd8.pos.naive", 
       "cd8.pos.cm", 
       "cd8.pos.em", 
       "cd8.pos.emra")
d1 = d[ ,c("cd8.pos.t", lv, "group", "pid")]
k = 2 # starting index of variables of interest (lv)
k.den = 1 # index of denominator
labs = c("CD8+ T Naive", "CD8+ T CM", "CD8+ T EM", "CD8+ T EMRA")

coef4 = summary_regression(lv, d1, k, k.den, labs)

## CD8+ T, Th

lv = c("cd8.pos.th1",                       
       "cd8.pos.th2",
       "cd8.pos.th17",                       
       "cd8.pos.th1.17",
       "cd8.pos.th9")
d1 = d[ ,c("cd8.pos.t", lv, "group", "pid")]
k = 2 # starting index of variables of interest (lv)
k.den = 1 # index of denominator
labs = c("CD8+ Th1", "CD8+ Th2", "CD8+ Th17", "CD8+ 1/17", "CD8+ Th9")

coef5 = summary_regression(lv, d1, k, k.den, labs)

