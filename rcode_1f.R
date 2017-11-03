
##############################
## 1. combine clinical data ##
##############################

d.c = merge(temp1, temp2, all=TRUE)
# d.c$group = substring(d.c$pid, 1, 2)

#####################################
## 2. combine flow & clinical data ##
#####################################

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


## vcaus_pe variable:
# original question: 17. In the past 10 days, have you had symptoms of a runny nose, sore throat, or cough?
# original question: 17a. IF YES to 17 -->  Do you think these symptoms were caused by:"
# 1, allergies | 2, cold or flu virus | 3, other
# variable for 3, other specification: spvcs_pe
# i manually coded slice II controls (alldata2) to be so (there is an error)
# recode--> cold/flu = 1, everything else = 0

temp = d.c3$vcaus_pe
temp[which(d.c3$vcaus_pe==1)] = 2
temp[which(d.c3$vcaus_pe %in% c(2,3))] = 1
d.c3$vcaus_pe = temp


