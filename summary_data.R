
summary_data <- function(lv, d1, k, k.parent, k.den, lab.y, binwidth, caption.fig, caption.tab)
  
{
  #############
  ## data QA ##
  #############
  
  # sum of all children nodes' numbers of events (numerator)
  
  d1$sum = apply(d1[k:(k+length(lv)-1)], MARGIN = 1, FUN=sum)
  
  ## percentage out of parent node
  
  d1$perc = d1$sum / d1[ ,k.parent] * 100
  
  ## summary table
  
  tab1 = summaryfunction(d1$perc, digits=0)
  tab1 = xtable(tab1)
  
  ###########################
  ## data summary: boxplot ##
  ###########################
  
  ## percentage
  
  for (i in k:(k+length(lv)-1))
  {
    d1[ ,i] = d1[ ,i] / d1[ ,k.den]
  }
  
  ## reshape: wide to long
  
  l <- reshape(d1[,k:(k+length(lv))], 
               varying = lv, 
               v.names = "perc",
               timevar = "cell", 
               times = lv, 
               direction = "long")
  
  ## order
  
  l = l[order(l$cell, l$perc), ]
  
  ## convert cell type to factor
  
  l$cell = factor(l$cell, levels=lv)
  
  ## plot
  
  p = ggplot(l, aes(cell, perc, fill=group)) + 
    geom_boxplot(outlier.colour=NA) + 
    geom_dotplot(binaxis='y', stackdir='center', 
                 position=position_dodge(1), binwidth=bin.width) +
    ylab(lab.y) + 
    theme(text = element_text(size=16)) +
    theme(axis.title.x=element_blank()) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_discrete(labels=lab.x.tick.marks) +
    scale_y_continuous(breaks = seq(0,max(d1[,k:(k+length(lv)-1)], na.rm=TRUE),by=0.1), 
                       labels=scales::percent) +
    theme(plot.title = element_text(size=14, face="bold", lineheight=.8, hjust=0.5)) +
    theme(legend.title=element_blank()) +
    theme(legend.position=leg.pos) +
    ggtitle(caption.fig)
  
  #########################
  ## data summary: table ##
  #########################
  
  ## select dataset
  
  temp = subset(d1, select=c(k:(k+length(lv)-1)))
  
  ## multiple percentages by 100 for table presentation
  
  temp = temp * 100
  
  ## combine it with group info
  
  temp = data.frame( cbind(temp, d1$group))
  names(temp)[(length(lv)+1)] = "group"
  
  ## table output
  
  vars = names(temp)[1:length(lv)]
  tab2 = CreateTableOne(vars=vars, strata=c("group"), data=temp)
  tab2 = print(tab2, nonnormal=vars, smd=FALSE, quote=FALSE, noSpaces=TRUE)
  tab2 = data.frame(tab2)[ ,1:3]
  row.names(tab2)[-1] = lv
  #tab2 = xtable( tab2, caption = caption.tab )
  #align(tab2) <- "lccc"
  
  ####################
  ## return results ##
  ####################
  
  return(list(tab1, p, tab2))
  
}