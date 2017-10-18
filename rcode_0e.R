
###########
## State ##
###########

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

############################
## LD presentation groups ##
############################

d.c$group2 = d.c$presgrp

##########
## Tick ##
##########

d.c$tick = d.c$tickb_lh

##############
## Duration ##
##############

## Duration of illness from onset of PTLDS symptoms to enrollment

d.c$time_en

## Duration of illness from onset of first sign/symptom to start of first 
# recommended antibiotic treatment course

d.c$time_tx

###############################
## Inappropriate Antibiotics ##
###############################

d.c$pretx_inax

##########################################
## Steroids Prior to Proper Antibiotics ##
##########################################

d.c$pretx_st

##################################################
## Total Antibiotic Exposure from Symptom Onset ##
##################################################

d.c$ax_tot

#############################
## Delayed or Misdiagnosis ##
#############################

d.c$misdx

###########################
## CHARACTERISTICS TABLE ##
###########################

vars = c("state", "group2", "tick", "time_en", "time_tx", 
         "pretx_inax", "pretx_st", "ax_tot", "misdx")

tableOne <- CreateTableOne(vars=vars, data=d.c, 
                           factorVars=c("state", "group2", "tick", 
                                        "pretx_inax", "pretx_st", "misdx"))
print(tableOne, nonnormal=c("time_en", "time_tx", "ax_tot"), 
      exact=c("state", "group2", "tick", "pretx_inax", "pretx_st", "misdx"), 
      quote=TRUE, noSpaces=TRUE)


#################################
## Data for Controls and Cases ##
#################################

d.c = d.o

#########
## Age ##
#########

d.c$Age = d.c$age

############
## Gender ##
############

d.c$Gender = factor(d.c$gender, labels=c("Female", "Male"))

##########
## Race ##
##########

d.c$Race = factor(d.c$racecat, labels=c("Other", "White"))

#############################################
## CHARACTERISTICS TABLE: CONTROLS & CASES ##
#############################################

vars = c("Age", "Gender", "Race")

tableOne <- CreateTableOne(vars=vars, data=d.c, strata="group", 
                           factorVars=c("Gender", "Race"))
print(tableOne, nonnormal=c("Age"), 
      exact=c("Gender", "Race"), 
      quote=TRUE, noSpaces=TRUE)

