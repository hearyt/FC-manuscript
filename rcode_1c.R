
##################
## 1. load data ##
##################

## SLICE II clinical data

d.c2 = read.csv(paste0(folder, "Other Data/alldata2.2017.06.01.csv")) # clinical data
names(d.c2) = tolower(names(d.c2))

#####################
## 2. process data ##
#####################

## SLICE II: extract control data

d.c2$group = substring(d.c2[,1], 1, 2)
d.c2 = d.c2[which(d.c2$group=="09" & d.c2$visit==1.0), ]

#######################
## 3. extract subset ##
#######################

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
d.c2 = d.c2[,vars.c2]
