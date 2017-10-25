
####################
## ALISON'S EDITS ##
####################


##########################################
## 1. select subset needed for analysis ##
##########################################

## select clinical variables 

d.c = d.all[ ,c(names(d.c), "group")]

## select PTLDS patients only (remove controls)

d.c = d.c[which(d.c$group=="07"), ] # PTLDS

####################
## 2. edit values ##
####################

d.c[which(d.c$pid=="07-343"), "flulike_mr"] = 1
d.c[which(d.c$pid=="07-346"), "sero_mr"] = 1
d.c[which(d.c$pid=="07-281"), "edisa_dg"] = 1
d.c[which(d.c$pid=="07-280"), "lys1t_mr"] = 1 # pcr+, more evidence than late lyme IgG

####################################
## 3. steroids before antibiotics ##
####################################
## did you have steoids before the end of appropirate antibiotic treatment (14 days): 0, 1
## antad_mr: first appropriate antibiotic date
## ste1d_bm: date you took steroids since onset of lyme
## pretx_st being NA means no steroids, or no appropriate antibiotics, or both

d.c$ste1d_bm = strptime(d.c$ste1d_bm, format="%m/%d/%y")
d.c$antad_mr = strptime(d.c$antad_mr, format="%m/%d/%y")
d.c$steroid_interval = difftime(d.c$ste1d_bm, d.c$antad_mr, units="days")
d.c$pretx_st = ifelse(d.c$steroid_interval <= 14, 1, 0)

##############################################
## 4. setting diagnosis presentation groups ##
##############################################
## lysy1_mr: first lyme serology --> Yes, No
## lysy2_mr: first POSITIVE lyme serology --> Yes, No
## lys1p_mr: is there a CDC-positive test for EARLY lyme (+ELISA, +WB) --> Yes, No
## lys1t_mr: is there a CDC-positive test for LATE lyme (+IgG) --> Yes, No
## lys2d_mr: first positive lyme serology date
## epdat_mr: lyme episode starting date
## NEW: test_mr: positive lyme serology
## NEW: early_late: positive serology for early or late lyme or both
## NEW: time_sero: if sero positive for lyme, is it outside 30 days after episode started
## comment: first positive lyme serology may not be from CDC approved labs
## comment: therefore, it's possible lysy2_mr=1 but lys1p and lys1t both = 0
## comment: for such subjects, since they are eligible for the study, they must have a rash

# d.c$temp = d.c$lys1p_mr + d.c$lys1t_mr
# d.c[which(d.c$lysy2_mr==1), c("lysy2_mr", "temp", "rash_mr")] # temp=0

## lab serology test results (not necessiary CDC approved)

d.c$test_mr = NA
d.c$test_mr[which(d.c$lysy1_mr == 1 & d.c$lysy2_mr == 0)] = "Negative"
d.c$test_mr[which(d.c$lysy1_mr == 1 & d.c$lysy2_mr == 1)] = "Positive"
d.c$test_mr[which(d.c$lysy1_mr == 0 & d.c$lysy2_mr == 0)] = "Not drawn"

## CDC-positive: early or late or both

d.c$early_late = NA
d.c$early_late[which(d.c$lys1p_mr == 1 & d.c$lys1t_mr == 0)] = "Sero Early"
d.c$early_late[which(d.c$lys1p_mr == 0 & d.c$lys1t_mr == 1)] = "Sero Late"
d.c$early_late[which(d.c$lys1p_mr == 1 & d.c$lys1t_mr == 1)] = "Sero Both"

## was the positive sero test 30 days after lyme episode?
# this is used to differentiate the early vs. late within the Sero Both group

d.c$lys2d_mr = strptime(d.c$lys2d_mr, format="%m/%d/%y")
d.c$epdat_mr = strptime(d.c$epdat_mr, format="%m/%d/%y")
d.c$sero_dur = difftime(d.c$lys2d_mr, d.c$epdat_mr, units="days")
d.c$time_sero = ifelse(d.c$sero_dur > 30, 1, 0)

######################################
## 5. Lyme initial diagnosis groups ##
######################################
## R: rash
## S: serology
## C: carditis
## N: neuro
## A: arthritis
## D: time_sero

## rash 

R = (d.c$rash_mr==1)

## other symptoms 

C = (d.c$carditis_mr==1)
N = (d.c$neuro_mr==1)
A = (d.c$arthritis_mr==1)

## CDC sero results

E = (d.c$early_late=="Sero Early") 
L = (d.c$early_late=="Sero Late")
B = (d.c$early_late=="Sero Both")

## sero test (not necessarily CDC approved) time > 30 days or not
# recruitement is based on CDC approved tests

D = (d.c$time_sero==1)

## group 1: CDC confirmed: Physician-documented erythema migrans rash
## group 2: CDC confirmed: Early objective finding / (+) ELISA/WB
## group 3: CDC confirmed: Late objective finding / (+) IgG-WB
## group 4: CDC probable: Viral-like illness / (+) ELISA/WB
## group 5: CDC probable:  Non-acute patient reported symptoms / (+) IgG-WB
## each group is defined based on a set of subjects, EXCLUDING all previous groups

## no rash, with other symptoms, possibilities
# some possibilities do not happen

P1 = E & A & (!C) & (!N) # ?? not eligible
P2 = E & (!A) & C & (!N) # group 2, early
P3 = E & (!A) & (!C) & N # group 2, early
P4 = E & A & C & (!N) # group 2 early ?? is this okay? yes
P5 = E & A & (!C) & N # group 2 early ?? is this okay? yes, rare
P6 = E & (!A) & C & N # group 2 early

P7 = L & A & (!C) & (!N) # group 3, late
P8 = L & (!A) & C & (!N) # ?? group 2, not sure, early?
P9 = L & (!A) & (!C) & N # group 3, late
P10 = L & A & C & (!N) # group 3 ?? is this okay? yes, rare
P11 = L & A & (!C) & N # group 3
P12 = L & (!A) & C & N # group 3 ?? is this okay? group 2?

P13 = B & A & (!C) & (!N) # group 3
P14 = B & (!A) & C & (!N) # ?? group 2, early
P15 = B & (!A) & (!C) & N # ?? group 2 or 3, depends on time_sero and/or what neuro signs
P16 = B & A & C & (!N) # group 3 ?? is this okay? no, group 2, Johns says Carditis trumps arthritis
P17 = B & A & (!C) & N # group 3
P18 = B & (!A) & C & N # ?? group 2

P19 = E & A & C & N # group 2 ?? is this okay? yes
P20 = L & A & C & N # group 3 ?? is this okay? yes
P21 = B & A & C & N # group 3 ?? is this okay? group 2?

## no rash, without other symptoms, possibilities

P22 = E & (!A) & (!C) & (!N) # group 4 
P23 = L & (!A) & (!C) & (!N) # group 5 
P24 = B & (!D) & (!A) & (!C) & (!N) # group 4
P25 = B & D & (!A) & (!C) & (!N) # group 5

## define groups

d.c$presgrp = NA
d.c$presgrp = cases(
  "1" = R, # if you had a rash 
  "2" = (!R) & ( P2 | P3 | P4 | P5 | P6 | P8 | P12 | P14 | P15 | P16 | P18 | P19 ), 
  "3" = (!R) & ( P7 | P9 | P10 | P11 | P13 | P17 | P20 | P21 ), 
  "4" = (!R) & ( P22 | P24), 
  "5" = (!R) & (P23 | P25) 
)

table(d.c$presgrp, useNA="ifany")
# d.c[which(d.c$pid=="07-255"), "presgrp"] = 2
# d.c[which(d.c$pid=="07-278"), "presgrp"] = 2
# d.c[which(d.c$pid=="07-355"), "presgrp"] = 2
# d.c[which(d.c$pid=="07-367"), "presgrp"] = 2

###############################
## 6. confirmed vs. probable ##
###############################

## rash 

R0 = (d.c$rash_mr==0)

## other symptoms 

C0 = (d.c$carditis_mr==0)
N0 = (d.c$neuro_mr==0)
A0 = (d.c$arthritis_mr==0)

d.c$dxgrp = NA
d.c$dxgrp[which(R | C | N | A)] = 1
d.c$dxgrp[which(R0 & C0 & N0 & A0)] = 0
table(d.c$dxgrp, useNA="ifany") # no missing data

###############################
## 7. mis-diagnosis category ##
###############################
## time_tx: time from beginning of lyme episode to first proper treatment, in days
## pretx_st: steroids before appropriate antibiotics
## pretx_inax: after the onset of lyme episode, 
# presence of inappropriate antibiotics for lyme before appropriate antibiotics

d.c$longdx = ifelse(d.c$time_tx>=30, 1, 0)

G0 = (d.c$longdx==0)
H0 = (d.c$pretx_inax==0)
I0 = (d.c$pretx_st==0)

G = (d.c$longdx==1)
H = (d.c$pretx_inax==1)
I = (d.c$pretx_st==1)

d.c$misdx = NA
d.c$misdx[which(G0 & H0 & I0)] = 0
d.c$misdx[which(G | H | I)] = 1

## tere is missing data in pretx_st
# a=NA, b=1 ==> (a==1) | (b==1) = TRUE

table(d.c$misdx, useNA = "ifany") 

#############################
## 8. education categories ##
#############################

d.o = d.all
table(d.o$educa_dg, useNA="ifany") # no missing data

d.o$educat = NA
d.o$educat[which(d.o$educa_dg %in% c(1, 2, 3, 4))] = 1
d.o$educat[which(d.o$educa_dg %in% c(5))] = 2

########################
## 9. race categories ##
########################

table(d.o$race2_dg, useNA = "ifany")
d.o$racecat = ifelse(d.o$hispa_dg==0 & d.o$race1_dg==1 & is.na(d.o$race2_dg), 1, 0)
table(d.o$racecat, useNA = "ifany")

d.all = d.o

