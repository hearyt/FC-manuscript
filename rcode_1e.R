
#########################################
## CLINICAL, REDCAP: SLICE II CONTROLS ##
#########################################

##################
## 1. load data ##
##################

token = "883E8E3BDDD28F049F6B2D2C725B94D4"
out = postForm("https://mrprcbcw.hosts.jhmi.edu/redcap/api/",
                token=token,
                content="record",
                type="flat",
                format="csv",
                .opts=curlOptions(ssl.verifypeer=FALSE))

d.r = read.table(file = textConnection(out), header = TRUE, sep = ",", na.strings = "",
                 stringsAsFactors = FALSE)

#####################
## 2. process data ##
#####################

## extract visit 1

d.r = d.r[which(d.r$redcap_event_name == "visit_1_arm_1"), ]

#######################
## 3. extract subset ##
#######################

## extract subset

d.r2 = d.r[ ,vars.c3]

