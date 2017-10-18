
#######################################
## Determine early vs. late vs. both ##
#######################################

## list of variables 

var.list = c("lys2e_mr", "lys2m_mr", "lys2g_mr")
var.list2 = c("elisa", "igm", "igg")
var.date = c("lys2d_mr", "epdat_mr")

## define new variables (try to replicate alison and erica's algorithm)

d.c$lys1p_mr2 = NA # cdc positive early
d.c$lys1t_mr2 = NA # cdc positive late

d.c$early_late2 = NA
d.c$presgrp2 = NA

## generate binary variables indicating + vs. -

d.c$c6 = ifelse(is.na(d.c$lys2c_mr), 0, 1) # non-missing means +
d.c$c6[which(is.na(d.c$lys2c_mr))] = NA # keep NA as NA

d.c$elisa = ifelse(is.na(d.c$lys2e_mr), 0, 1) # non-missing means +
d.c$elisa[which(is.na(d.c$lys2e_mr))] = NA # keep NA as NA

d.c$igm = ifelse(d.c$lys2m_mr>=2, 1, 0)
d.c$igm[which(is.na(d.c$lys2m_mr))] = NA # keep NA as NA

d.c$igg = ifelse(d.c$lys2g_mr>=5, 1, 0)
d.c$igg[which(is.na(d.c$lys2g_mr))] = NA # keep NA as NA

## some missing test is + or -, but the value is missing (from erica)

d.c$elisa[which(d.c$pid %in% c("07-357", "07-413", "07-283"))] = 1
d.c$igm[which(d.c$pid %in% c("07-357"))] = 0
d.c$igm[which(d.c$pid %in% c("07-413"))] = 1
d.c$igg[which(d.c$pid %in% c("07-357", "07-413"))] = 1

## change missing values to be 0: elisa, igm, igg

d.c$elisa[which(is.na(d.c$elisa))] = 0
d.c$igm[which(is.na(d.c$igm))] = 0
d.c$igg[which(is.na(d.c$igg))] = 0

## C6 not missing ==> C6 positive ==> both, elisa, IgM, and IgG do not matter

ind = which(!is.na(d.c$c6))
d.c[ind, "early_late2"] = "Sero Both"
d.c[ind, c("pid", "early_late")] # compare to Alison's data (match after Alison edited 07-401)

## C6 missing ==> look at elisa, IgM, and IgG: define CDC-positive early

E = (d.c$elisa == 1)
M = (d.c$igm == 1)
G = (d.c$igg == 1)

P1 = (!E) & (!M) & (!G)

P2 = E & (!M) & (!G)
P3 = (!E) & M & (!G)
P4a = (!E) & (!M) & G & (d.c$time_sero==0) # no need for time_sero to be 1??
P4b = (!E) & (!M) & G & (d.c$time_sero==1)

P5a = E & M & (!G) & (d.c$time_sero==0)
P5b = E & M & (!G) & (d.c$time_sero==1)
P6 = E & (!M) & G
P7 = (!E) & M & G

P8 = E & M & G

d.c$lys1p_mr2 = ifelse(P5a | P6 | P8, 1, 0)
d.c$lys1p_mr2[which(!is.na(d.c$c6))] = 1 # C6 + ==> early = 1

temp = which( compareNA(d.c$lys1p_mr, d.c$lys1p_mr2) == FALSE )
d.c[temp, c("pid", "lys1p_mr", "lys1p_mr2", "time_sero", var.list2, "c6")]

## C6 missing ==> look at elisa, IgM, and IgG: define CDC-positive late

d.c$lys1t_mr2 = ifelse(P4b | P6 | P7 | P8, 1, 0)
d.c$lys1t_mr2[which(!is.na(d.c$c6))] = 1 # C6 + ==> late = 1

temp = which( compareNA(d.c$lys1t_mr, d.c$lys1t_mr2) == FALSE )
d.c[temp, c("pid", "lys1t_mr", "lys1t_mr2", var.list2, "c6")]
