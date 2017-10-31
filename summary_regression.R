

summary_regression <- function(lv, d1, k, k.den, labs)
  
{
  ##########################
  ## calculate percentage ##
  ##########################
  
  ## percentage (*100 for better interpretation)
  
  for (i in k:(k+length(lv)-1))
  {
    d1[ ,i] = d1[ ,i] / d1[ ,k.den] *100
  }
  
  ################
  ## regression ##
  ################
  
  ## merge fc data with covariates data
  
  d.reg = merge(d1, temp, by="pid")
  d.reg$pid = NULL
  d.reg$Age = d.reg$Age / 10 # scale age: every 10 year increase in Age is associated with ...

  ## fit models 
  
  n.cov = 4
  coef = numeric(0)
  
  for (i in k:(k+length(lv)-1))
  {
    fit = lm(d.reg[ ,i] ~ group + Age + Gender + Race + Cold.Flu, data=d.reg)
    
    t1 = summary(fit)$coef[2:(n.cov+2), 1] # point estimate
    t2 = summary(fit)$coef[2:(n.cov+2), 4] # p-value

    ind0 = which(t2 < 0.1 & t2 >= 0.05)
    ind1 = which(t2 < 0.05 & t2 >= 0.01)
    ind2 = which(t2 < 0.01 & t2 >= 0.001)
    ind3 = which(t2 < 0.001)
    
    t1 = format(round(t1, 2), nsmall = 2)
    t1[ind0] = paste0(t1[ind0], " .")
    t1[ind1] = paste0(t1[ind1], " *")
    t1[ind2] = paste0(t1[ind2], " **")
    t1[ind3] = paste0(t1[ind3], " ***")
    coef = rbind(coef, t1)
  }
  
  ###########
  ## table ##
  ###########
  
  ## table output
  
  dimnames(coef)[[1]] = labs
  dimnames(coef)[[2]] = c("PTLDS_vs._Control)", "Age_by_10_years", 
                          "Male_vs_Female)", "White_vs_Non-White)", 
                          "Cold_or_Flu_vs._None")
  coef = data.frame(coef)
  
  ####################
  ## return results ##
  ####################
  
  return(coef)
  
}