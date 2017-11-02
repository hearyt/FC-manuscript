
##################
## 1. load data ##
##################

## SLICE II clinical data

d.c2 = read.csv(paste0(folder, "Other Data/alldata2.2017.06.01.csv")) # clinical data
names(d.c2) = tolower(names(d.c2))

## SLICE III clinical data

d.c3 = read.csv(paste0(folder, "Other Data/alldata3.2017.07.07.csv")) # clinical data
names(d.c3) = tolower(names(d.c3))

#####################
## 2. process data ##
#####################

## SLICE II: extract control data

d.c2$group = substring(d.c2[,1], 1, 2)
d.c2 = d.c2[which(d.c2$group=="09" & d.c2$visit==1.0), ]

## SLICE III: extract group variable

d.c3$group = substring(d.c3[,1], 1, 2)

## remove these pids: '07-195', '07-204', '07-283', '07-297', '07-196'

ind = which(d.c3$pid %in% c("07-195", "07-204", "07-283", "07-297", "07-196"))
d.c3 = d.c3[-ind, ]

## vcaus_pe variable:
 # original question: 17. In the past 10 days, have you had symptoms of a runny nose, sore throat, or cough?
 # original question: 17a. IF YES to 17 →  Do you think these symptoms were caused by:"
 # 1, allergies | 2, cold or flu virus | 3, other
 # variable for 3, other specification: spvcs_pe
 # recode--> allegy=2, cold/flu=1, nothing=NA
 # i manually coded slice II controls to be so
 # now recode slice III

temp = d.c3$vcaus_pe
temp[which(d.c3$vcaus_pe==1)] = 2
temp[which(d.c3$vcaus_pe %in% c(2,3))] = 1
d.c3$vcaus_pe = temp

#####################################
## 3. combine slice II & slice III ##
#####################################

## variables needed 

vars.c = c("pid",
           "flulike_mr", "sero_mr", "edisa_dg", 
           "sterd_bm", "ste1d_bm", "antad_mr",
           "lysy1_mr", "lysy2_mr", "lys1p_mr", "lys1t_mr", "lys2d_mr", "epdat_mr", 
           "rash_mr", "carditis_mr", "neuro_mr", "arthritis_mr", 
           "time_tx", "pretx_inax", 
           "age", "gender",
           "educa_dg", "hispa_dg", "race1_dg", "race2_dg",
           "vcaus_pe", 
           "lys2e_mr", "lys2m_mr", "lys2g_mr", "lys2c_mr",
           "fzip3_dg", 
           "tickb_lh", 
           "time_en", "time_tx",
           "ax_tot") # total number of days of antibiotics exposure, use BM form, ant1t, add all up

## variables in slice II control dataset

vars.c2 = vars.c[which(vars.c %in% names(d.c2))]

## combine

temp1 = d.c3[,vars.c]
temp2 = d.c2[,vars.c2]
d.c = merge(temp1, temp2, all=TRUE)
# d.c$group = substring(d.c$pid, 1, 2)

#######################################
## 8. combine flow and clinical data ##
#######################################

## overlapping pids only

pid = intersect(unique(d$pid), unique(d.c$pid))

## merge flow and clinical data

d.all = merge(d, d.c, by="pid", all=FALSE)

dim(d.all)
length(unique(d.all$pid))
table(d.all$group)

## make control the reference group

d.all$group = factor(d.all$group, labels=c("PTLDS", "Control"))
d.all = within(d.all, group <- relevel(group, ref = "Control"))
