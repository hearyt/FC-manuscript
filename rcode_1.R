
#################
## first layer ##
#################

## lymph

lv = code[which(code$layer==1), 2]
d1 = d[ ,c("alive.lymph", lv, "group")]
k = 2 # starting index of variables of interest (lv)
k.parent = 1 # index of the parent node
k.den = 1 # index of denominator
lab.x.tick.marks = c("TCR Gamma Delta", "CD4+ T", "CD8+ T", "CD4- CD8-",
                     "NK", "CD8+ NK", "CD3-", "B")
lab.y = "% of Alive Lymphocytes" # y axis label
bin.width = 0.001 # dot size in figure
caption.fig = "First Layer, Lymphocytes" # table and figure caption
caption.tab = "First Layer, % of Lymphocytes" # table and figure caption
leg.pos = "top" # legend position

l_1 = data_summary(lv, d1, k, k.parent, k.den, lab.y, bin.width, caption.fig, caption.tab)
l_1[[2]] # figure

## missing data

apply(d1, MARGIN = 2, FUN = function(v){sum(is.na(v))})
