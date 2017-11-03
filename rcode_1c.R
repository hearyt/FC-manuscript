
#########################
## CLINICAL: SLICE III ##
#########################

##################
## 1. load data ##
##################

d.c3 = read.csv(paste0(folder, "Other Data/alldata3.2017.07.07.csv")) # clinical data
names(d.c3) = tolower(names(d.c3))

#####################
## 2. process data ##
#####################

## SLICE III: extract group variable

d.c3$group = substring(d.c3[,1], 1, 2)

## remove these pids: '07-195', '07-204', '07-283', '07-297', '07-196'

ind = which(d.c3$pid %in% c("07-195", "07-204", "07-283", "07-297", "07-196"))
d.c3 = d.c3[-ind, ]

#######################
## 3. extract subset ##
#######################

## stract subset

vars.c3 = vars.c
d.c3 = d.c3[,vars.c3]
