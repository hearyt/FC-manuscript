
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
lab.x.tick.marks = c("CD4+ Naive", "CD4+ CM", "CD4+ EM", "CD4+ EMRA")
lab.y = "% of CD4+ T" # y axis label
bin.width = 0.005 # dot size in figure
caption.fig = "Second Layer, CD4+ T" # table and figure caption
caption.tab = "Second Layer, \\% of CD4+ T" # table and figure caption
leg.pos = "none" # legend position

l_5 = data_summary(lv, d1, k, k.parent, k.den, lab.y, bin.width, caption.fig, caption.tab)

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
caption.fig = "Second Layer, CD4+ T" # table and figure caption
caption.tab = "Second Layer, \\% of CD4+ T" # table and figure caption
leg.pos = "none" # legend position

l_9 = data_summary(lv, d1, k, k.parent, k.den, lab.y, bin.width, caption.fig, caption.tab)

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
lab.x.tick.marks = c("CD8+ Naive", "CD8+ CM", "CD8+ EM", "CD8+ EMRA")
lab.y = "% of CD8+ T" # y axis label
bin.width = 0.005 # dot size in figure
caption.fig = "Second Layer, CD8+ T" # table and figure caption
caption.tab = "Second Layer, \\% of CD8+ T" # table and figure caption
leg.pos = "none" # legend position

l_10 = data_summary(lv, d1, k, k.parent, k.den, lab.y, bin.width, caption.fig, caption.tab)

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
caption.fig = "Second Layer, CD8+ T" # table and figure caption
caption.tab = "Second Layer, \\% of CD8+ T" # table and figure caption
leg.pos = "none" # legend position

l_14 = data_summary(lv, d1, k, k.parent, k.den, lab.y, bin.width, caption.fig, caption.tab)

#####################
## combine figures ##
#####################

grid_arrange_shared_legend(l_5[[2]], l_9[[2]], l_10[[2]], l_14[[2]], 
                           nrow = 2, ncol = 2)
