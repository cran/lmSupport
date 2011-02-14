lm.sumSquares <-
function(model)
#This calculates SS, sr2 (aka eta2, delta R2), and pr2 (aka partial eta2) for all effects in a linear model object.  
#For categorical variables, it calculates these for multi-df effect
#2011-02-03: name change for rows in table, JJC
{
  SST = sum((model$model[,1] - mean(model$model[,1]))^2)  #Total SS for DV
  t1 = Anova(model, type=3)  #Get ANOVA table for type 3 SS
  SSE = t1[nrow(t1),1] 
  nEffects = nrow(t1) - 2

  tSS = matrix(NA,nEffects+2,3)
  rownames(tSS) = c(row.names(t1)[2:(nEffects+1)], 'Residual (SSE)', 'Total (SST)')
  colnames(tSS) = c('SS', 'dR-sqr', 'PRE') 

  tSS[1:(nEffects+1),1] = t1[2:(nEffects+2),1]
  tSS[nEffects+2,1] = SST
  
  tSS[1:nEffects,2] = tSS[1:nEffects,1]/ tSS[nEffects+2,1]  #Calculate SSR/SST
  tSS[1:nEffects,3] = tSS[1:nEffects,1]/ (tSS[1:nEffects,1] + tSS[nEffects+1,1]) #Calculate SSR/(SSR + SSE)
  return(tSS)
}