
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
