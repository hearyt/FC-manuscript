
#########################
## load all r packages ##
#########################

library(knitr)
library(xlsx)
library(ggplot2)
library(tableone)
library(plyr)
library(dplyr)
library(lme4)
library(party)
library(randomForest)
library(caret)
library(Hmisc)
library(tableone)
library(survival)
library(lattice)
library(cluster) 
library(fpc)
library(NbClust)
library(flexclust)
library(pvclust)
library(ggdendro)
library(plotly)
library(reshape)
library(webshot)
library(xtable)
library(shiny)
library(lmerTest)
library(ordinal)
library(texreg)
library(tidyr)
library(scales)
library(tableone)
library(survival)
library(memisc)

##########################
## function "multiplot" ##
##########################

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

############################################
## function: multiplot with shared legend ##
############################################

library(ggplot2)
library(gridExtra)
library(grid)
grid_arrange_shared_legend <- function(..., nrow = 1, ncol = length(list(...)), position = c("bottom", "right")) {
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position = "none"))
  gl <- c(gl, nrow = nrow, ncol = ncol)
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  grid.newpage()
  grid.draw(combined)
}


#########################
## function "rounding" ##
#########################

rounding <- function(x, digits)
{
  return( formatC(round(x, digits=digits), format="f", digits=digits) )
}

#####################################################
## Summary Function that is compatible with xtable ##
#####################################################

summaryfunction= function (x, digits){
  if( is.numeric(x)!=TRUE) {stop("Supplied X is not numeric")}
  mysummary = data.frame(
    "Min." = rounding( as.numeric( min(x, na.rm=TRUE)), digits=digits ),
    "1st Qu." = rounding( quantile(x, na.rm=TRUE)[2], digits=digits ),
    "Median" = rounding( median(x, na.rm=TRUE), digits=digits ),
    "Mean" = rounding( mean(x, na.rm=TRUE), digits=digits ),
    "3rd Qu." = rounding( quantile(x, na.rm=TRUE)[4], digits=digits ),
    "Max." = rounding( max(x, na.rm=TRUE), digits=digits ),
    row.names=""
    
  )
  names(mysummary) = c("Min.","1st Qu.","Median","Mean","3rd Qu.","Max.")
  return( mysummary )
}

######################
## compare with NAs ##
######################

compareNA <- function(v1,v2) {
  same <- (v1 == v2) | (is.na(v1) & is.na(v2))
  same[is.na(same)] <- FALSE
  return(same)
}

#############################
## function "data_summary" ##
#############################

source("summary_data.R")
source("summary_regression.R")
