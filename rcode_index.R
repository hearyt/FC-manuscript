

###############################
## 0. R PACKAGES & FUNCTIONS ##
###############################

source("rcode_0a.R") # r packages
source("rcode_0b.R") # r functions

##################
## 1. LOAD DATA ##
##################

source("rcode_1a.R") # list of clinical variables to extract 
source("rcode_1b.R") # process flow cytometry data 
source("rcode_1c.R") # process clinical data: slice III
source("rcode_1d.R") # process clinical data: slice II Access
source("rcode_1e.R") # process clinical data: slice II REDCap
source("rcode_1f.R") # combine data 

source("rcode_0c.R") # Alison's edits (presgrp: Ting's complete mapping algorithm)
source("rcode_0d.R") # Ting reproduces early, late, early_late
source("rcode_0e.R") # characteristics table - TABLE 1

#######################
## 1. SUMMARIZE DATA ##
#######################

source("rcode_1a.R") # first layer
source("rcode_1b.R") # second layer

###################
## 2. REGRESSION ##
###################

source("rcode_2a.R") # PTLDS vs. Controls
source("rcode_2b.R") # PTLDS only ..... not done yet, reorganize code to generate
                                      # a dataset for PTLDS only


