
##########################################
## CHARACTERISTICS TABLE FOR ONLY CASES ##
##########################################

## dataset d.c comes from rcode_0c and rcode_0d, only PTLDS data

##############
## 1. State ##
##############

## list of first 3-digits zipcodes for states

md = c(206:219)
md = md[which(md != 213)]
pa = c(150:196)
de = c(197:199)
nj = c(70:89)

## assign states

d.c$state = NA
d.c$state = cases(
  "Maryland" = d.c$fzip3_dg %in% md, 
  "Pennsylvania" = d.c$fzip3_dg %in% pa,
  "Delaware" = d.c$fzip3_dg %in% de,
  "New Jersey" = d.c$fzip3_dg %in% nj,
  "Other" = !d.c$fzip3_dg %in% c(md, pa, de, nj)
)

table(d.c$state, useNA="ifany")

###############################
## 2. LD presentation groups ##
###############################

d.c$group2 = d.c$presgrp

#############
## 3. Tick ##
#############

d.c$tick = d.c$tickb_lh

#################
## 4. Duration ##
#################

## Duration of illness from onset of PTLDS symptoms to enrollment

d.c$time_en

## Duration of illness from onset of first sign/symptom to start of first 
# recommended antibiotic treatment course

d.c$time_tx

##################################
## 5. Inappropriate Antibiotics ##
##################################

d.c$pretx_inax

#############################################
## 6. Steroids Prior to Proper Antibiotics ##
#############################################

d.c$pretx_st

#####################################################
## 7. Total Antibiotic Exposure from Symptom Onset ##
#####################################################

d.c$ax_tot

################################
## 8. Delayed or Misdiagnosis ##
################################

d.c$misdx

##########################################
## 9. characteristics table: PTLDS only ##
##########################################

vars = c("state", "group2", "tick", "time_en", "time_tx", 
         "pretx_inax", "pretx_st", "ax_tot", "misdx")

tableOne <- CreateTableOne(vars=vars, data=d.c, 
                           factorVars=c("state", "group2", "tick", 
                                        "pretx_inax", "pretx_st", "misdx"))
tab_0a = print(tableOne, nonnormal=c("time_en", "time_tx", "ax_tot"), 
      exact=c("state", "group2", "tick", "pretx_inax", "pretx_st", "misdx"), 
      quote=TRUE, noSpaces=TRUE)
tab_0a = data.frame(tab_0a)

#############################################
## CHARACTERISTICS TABLE: CONTROLS & PTLDS ##
#############################################

## Data for Controls and Cases 

d = d.all

#############
## 10. Age ##
#############

d$Age = d$age

################
## 11. Gender ##
################

d$Gender = factor(d$gender, labels=c("Female", "Male"))

##############
## 12. Race ##
##############

d$Race = factor(d$racecat, labels=c("Other", "White"))

#################################
## 13. Allergies, Cold, or Flu ##
#################################

d$Cold.Flu = ifelse(d$vcaus_pe==1, 1, 0)
d$Cold.Flu[which(is.na(d$Cold.Flu))] = 0
d$Cold.Flu = factor(d$Cold.Flu, labels=c("Allergy/NA", "Cold/Flu"))

###################################################
## 14. characteristics table: controls and PTLDS ##
###################################################

vars = c("Age", "Gender", "Race", "Cold.Flu")

tableOne <- CreateTableOne(vars=vars, data=d, strata="group", 
                           factorVars=c("Gender", "Race", "Cold.Flu"))
tab_0b = print(tableOne, nonnormal=c("Age"), exact=c("Gender", "Race", "Cold.Flu"), 
               quote=TRUE, noSpaces=TRUE)
tab_0b = data.frame(tab_0b)[2:5 ,1:3]

######################
## 15. Missing data ##
######################

## controls

vars = c("Age", "Gender", "Race", "Cold.Flu")
temp = d[which(d$group=="Control"), vars]
apply(temp, MARGIN = 2, FUN = function(v){sum(is.na(v))})

## cases

vars = c("Age", "Gender", "Race", "Cold.Flu")
temp = d[which(d$group=="PTLDSl"), vars]
apply(temp, MARGIN = 2, FUN = function(v){sum(is.na(v))})

## cases
 # pretx_st being NA means no steroids, or no appropriate antibiotics, or both
 # NA in misdx comes from pretx_st

vars = c("state", "group2", "tick", "time_en", "time_tx", 
         "pretx_inax", "pretx_st", "ax_tot", "misdx")
temp = d.c[, vars]
apply(temp, MARGIN = 2, FUN = function(v){sum(is.na(v))})

## conclusion: no missing data in the characteristics table

