
##################
## 1. load data ##
##################

folder = "~/Box Sync/Ting's Lyme Research Folder 2016/Projects/Flow Data/Datasets/"

## load flow data

file.list = list.files(path = paste0(folder, "Flow Data"), pattern='*.xlsx')
file.list = paste0(folder, "Flow Data/", file.list)
df.list = lapply(file.list, read.xlsx, sheetIndex = 1, stringsAsFactors = FALSE)

## rename flow dataset columns 

code = read.xlsx(paste0(folder, "Other Data/codebook.xlsx"), 
                 sheetIndex = 1, stringsAsFactors = FALSE) # codebook
df.list = lapply(df.list, setNames, code[,2])

## remove "bleed.date" column

df.list = lapply(df.list, function(x) { x["bleed.date"] <- NULL; x })

## combine flow datasets

d = bind_rows(df.list)
dim(d)

#####################
## 2. process data ##
#####################

## remove trailing space or leading space from pid and visit

d$pid = trimws(d$pid, which = "both")
d$visit = trimws(d$visit, which = "both")

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

###################################################################
## 3. remove values based on Aarti's flow cytometry tracker file ##
###################################################################

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
                         "07-212"))

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

#################################
## 4. data quality check: flow ##
#################################

## load master tracking sheet PIDs

tracking = read.xlsx(paste0(folder, "Other Data/tracking.xlsx"), sheetIndex = 1)

## check against tracking sheet

length(unique(d$pid)) # controls and ptlds
sum( unique(d$pid) %in% tracking$pid )
dim(d)

## save dataset

d.f = d

