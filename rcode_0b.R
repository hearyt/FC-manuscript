
#######################
## 1. load flow data ##
#######################

folder = "~/Box Sync/Ting's Lyme Research Folder 2016/Projects/Flow Data/Datasets/"

## load codebook

code = read.xlsx(paste0(folder, "Other Data/codebook.xlsx"), 
                 sheetIndex = 1, stringsAsFactors = FALSE)

## load flow data

file.list = list.files(path = paste0(folder, "Flow Data"), pattern='*.xlsx')
file.list = paste0(folder, "Flow Data/", file.list)
df.list = lapply(file.list, read.xlsx, sheetIndex = 1, stringsAsFactors = FALSE)

## rename flow dataset columns 

df.list = lapply(df.list, setNames, code[,2])

## remove "bleed.date" column

df.list = lapply(df.list, function(x) { x["bleed.date"] <- NULL; x })

## combine flow datasets

d = bind_rows(df.list)
dim(d)

## remove trailing space or leading space from pid and visit

d$pid = trimws(d$pid, which = "both")
d$visit = trimws(d$visit, which = "both")

## load tracking sheet PIDs

tracking = read.xlsx(paste0(folder, "Other Data/tracking.xlsx"), sheetIndex = 1)

#################################################
## 2. load clinical data: SLICE II & SLICE III ##
#################################################

## load SLICE II clinical data

d.c2 = read.csv(paste0(folder, "Other Data/alldata2.2017.06.01.csv")) # clinical data
names(d.c2) = tolower(names(d.c2))

## load SLICE III clinical data

d.c3 = read.csv(paste0(folder, "Other Data/alldata3.2017.07.07.csv")) # clinical data
names(d.c3) = tolower(names(d.c3))

#####################################################################
## 3. remove values based on Aarti's flow cytometry tracking sheet ##
#####################################################################

## remove 07-196, 07-258 

ind = which(d$pid %in% c("07-192", "07-196", "07-258", "07-323"))
# d = d[-ind, ]

## for the following pids, remove CCR4 related fields

ind = which(d$pid %in% c("07-199",
                         "07-200",
                         "07-201",
                         "07-202",
                         "07-203",
                         "07-205",
                         "07-208",
                         "07-210",
                         "07-212",
                         "07-251",
                         "07-252"))

d[ind ,c(names(d)[grep("th", names(d))])] = NA

## for the following pids, remove CD57 related fields

ind = which(d$pid %in% c("07-193",
                         "07-194",
                         "07-195"))

d[ind ,c(names(d)[grep("cd57", names(d))])] = NA

## for the following pids, remove CXCR5 related fields

ind = which(d$pid %in% c("07-193",
                         "07-194",
                         "07-195"))

d[ind ,c(names(d)[grep("cxcr5", names(d))])] = NA

## for the following pids, remove granulocyte related fields

ind = which(d$pid %in% c("07-195",
                         "07-197",
                         "07-198",
                         "07-228",
                         "07-243",
                         "07-270",
                         "07-274",
                         "07-314",
                         "07-349"))

d[ind ,c(names(d)[grep("gran", names(d))])] = NA

## for the following pids, remove monocyte related fields

ind = which(d$pid %in% c("07-270",
                         "07-274",
                         "07-314"))

d[ind ,c(names(d)[grep("mono", names(d))])] = NA

## for the following pids, no beads, remove custom token

ind = which(d$pid %in% c("07-208",
                         "07-228",
                         "07-243",
                         "07-253",
                         "07-254",
                         "07-255",
                         "07-256"))

d[ind ,c(names(d)[grep("custom.token", names(d))])] = NA

##############################
## 4. data processing: flow ##
##############################

## flow data: extract group variable

d$group = substring(d[,1], 1, 2)

## remove these pids: '07-195', '07-204', '07-283', '07-297', '07-196'

ind = which(d$pid %in% c("07-195", "07-204", "07-283", "07-297", "07-196"))
d = d[-ind, ]

## only keep visit 1 data

d = d[which(d$visit=="V1"), ]
dim(d)
length(unique(d$pid))

## calculate total memory

d$cd4.pos.tm = d$cd4.pos.cm + d$cd4.pos.em + d$cd4.pos.emra
d$cd8.pos.tm = d$cd8.pos.cm + d$cd8.pos.em + d$cd8.pos.emra

## layer info

code$layer = as.numeric(code$layer)

##################################
## 5. data processing: clinical ##
##################################

## SLICE II: extract control data

d.c2$group = substring(d.c2[,1], 1, 2)
d.c2 = d.c2[which(d.c2$group=="09" & d.c2$visit==1.0), ]

## SLICE III: extract group variable

d.c3$group = substring(d.c3[,1], 1, 2)

## remove these pids: '07-195', '07-204', '07-283', '07-297', '07-196'

ind = which(d.c3$pid %in% c("07-195", "07-204", "07-283", "07-297", "07-196"))
d.c3 = d.c3[-ind, ]

## vcaus_pe variable: recode--> allegy=2, cold/flu=1, nothing=NA
 # i manually coded slice II controls to be so
 # now recode slice III

temp = d.c3$vcaus_pe
temp[which(d.c3$vcaus_pe==1)] = 2
temp[which(d.c3$vcaus_pe %in% c(2,3))] = 1
d.c3$vcaus_pe = temp

#################################
## 6. data quality check: flow ##
#################################

## check against tracking sheet

length(unique(tracking$pid))
length(unique(d$pid)) # controls and ptlds
sum( unique(d$pid) %in% tracking$pid )

################################################
## 7. combine clinical data: PTLDS & controls ##
################################################

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
