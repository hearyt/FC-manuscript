
##########################
## second layer: CD4+ T ##
##########################

## CD4+ T, Memory

lv = c("cd4.pos.naive", 
       "cd4.pos.cm", 
       "cd4.pos.em", 
       "cd4.pos.emra")
d1 = d[ ,c("alive.lymph", "cd4.pos.t", lv, "group")]
k = 3 # starting index of variables of interest (lv)
k.parent = 2 # index of the parent node
k.den = 2 # index of denominator
lab.x.tick.marks = c("CD4+ T Naive", "CD4+ T CM", "CD4+ T EM", "CD4+ T EMRA")
lab.y = "% of CD4+ T" # y axis label
bin.width = 0.005 # dot size in figure
caption.fig = "CD4+ T, Memory Subsets" # table and figure caption
caption.tab = "\\% of CD4+ T" # table and figure caption
leg.pos = "none" # legend position

l_2 = summary_data(lv, d1, k, k.parent, k.den, lab.y, bin.width, caption.fig, caption.tab)
apply(d1, MARGIN = 2, FUN = function(v){sum(is.na(v))}) # missing data

## CD4+ T, Th1, Th2, Th17, Th1/17, Th9

lv = c("cd4.pos.th1",                       
       "cd4.pos.th2",
       "cd4.pos.th17",                       
       "cd4.pos.th1.17",
       "cd4.pos.th9")
d1 = d[ ,c("alive.lymph", "cd4.pos.t", "cd4.pos.ccr6.pos", lv, "group")]
k = 4 # starting index of variables of interest (lv)
k.parent = 2 # index of the parent node
k.den = 2 # index of denominator
lab.x.tick.marks = c("CD4+ Th1", "CD4+ Th2", "CD4+ Th17", "CD4+ 1/17", "CD4+ Th9")
lab.y = "% of CD4+ T" # y axis label
bin.width = 0.005 # dot size in figure
caption.fig = "CD4+ Th cells" # table and figure caption
caption.tab = "\\% of CD4+ T" # table and figure caption
leg.pos = "none" # legend position

l_3 = summary_data(lv, d1, k, k.parent, k.den, lab.y, bin.width, caption.fig, caption.tab)
apply(d1, MARGIN = 2, FUN = function(v){sum(is.na(v))}) # missing data

##########################
## second layer: CD8+ T ##
##########################

## CD8+ T, Memory

lv = c("cd8.pos.naive", 
       "cd8.pos.cm", 
       "cd8.pos.em", 
       "cd8.pos.emra")
d1 = d[ ,c("alive.lymph", "cd8.pos.t", lv, "group")]
k = 3 # starting index of variables of interest (lv)
k.parent = 2 # index of the parent node
k.den = 2 # index of denominator
lab.x.tick.marks = c("CD8+ T Naive", "CD8+ T CM", "CD8+ T EM", "CD8+ T EMRA")
lab.y = "% of CD8+ T" # y axis label
bin.width = 0.005 # dot size in figure
caption.fig = "CD8+ T, Memory Subsets" # table and figure caption
caption.tab = "\\% of CD8+ T" # table and figure caption
leg.pos = "none" # legend position

l_4 = summary_data(lv, d1, k, k.parent, k.den, lab.y, bin.width, caption.fig, caption.tab)
apply(d1, MARGIN = 2, FUN = function(v){sum(is.na(v))}) # missing data

## CD8+ T, Th1, Th2, Th17, Th1/17, Th9

lv = c("cd8.pos.th1",                       
       "cd8.pos.th2",
       "cd8.pos.th17",                       
       "cd8.pos.th1.17",
       "cd8.pos.th9")
d1 = d[ ,c("alive.lymph", "cd8.pos.t", "cd8.pos.ccr6.pos", lv, "group")]
k = 4 # starting index of variables of interest (lv)
k.parent = 2 # index of the parent node
k.den = 2 # index of denominator
lab.x.tick.marks = c("CD8+ Th1", "CD8+ Th2", "CD8+ Th17", "CD8+ 1/17", "CD8+ Th9")
lab.y = "% of CD8+ T" # y axis label
bin.width = 0.005 # dot size in figure
caption.fig = "CD8+ Th cells" # table and figure caption
caption.tab = "\\% of CD8+ T" # table and figure caption
leg.pos = "none" # legend position

l_5 = summary_data(lv, d1, k, k.parent, k.den, lab.y, bin.width, caption.fig, caption.tab)
apply(d1, MARGIN = 2, FUN = function(v){sum(is.na(v))}) # missing data

#####################
## combine figures ##
#####################

grid_arrange_shared_legend(l_2[[2]], l_4[[2]], l_3[[2]], l_5[[2]], 
                           nrow = 2, ncol = 2)
