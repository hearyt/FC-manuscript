
#########################################
## CLINICAL, ACCESS: SLICE II CONTROLS ##
#########################################

##################
## 1. load data ##
##################

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

vars.c2 = vars.c[which(vars.c %in% names(d.c2))]

## extract subset

d.c2 = d.c2[,vars.c2]
