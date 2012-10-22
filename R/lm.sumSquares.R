lm.sumSquares <-
function(model)
#This calculates SS, sr2 (aka eta2, delta R2), and pr2 (aka partial eta2) for all effects in a linear model object.  
#For categorical variables, it calculates these for multi-df effect
#2011-02-03: name change for rows in table, JJC
#2012-10-03: add intercept, change labels, JJC
{
  
  tANOVA =   Anova(model, type=3)  #Get ANOVA table for type 3 SS
  
  nEffects = nrow(tANOVA) -1
  tSS = matrix(NA,nEffects+2,6)
  rownames(tSS) = c(row.names(tANOVA)[1:(nEffects)], 'Error (SSE)', 'Total (SST)')
  colnames(tSS) = c('SS', 'dR-sqr', 'pEta-sqr', 'df', 'F', 'p-value') 

  #SSE and SST
  SSE = sum(model$residuals^2)   #could also get from tANOVA
  tSS[nEffects+1,1] = SSE
  SST = sum((model$model[,1] - mean(model$model[,1]))^2)  #Total SS for DV
  tSS[nEffects+2,1] = SST
  
  
  #SSR
  tSS[1:nEffects,1] = tANOVA[1:nEffects,1]  #SSR
  
  #dR-sqr
  tSS[1:nEffects,2] = round(tSS[1:nEffects,1] / SST,4) 
  
  #pEta-sqr
  tSS[1:nEffects,3] = round(tSS[1:nEffects,1] / (SSE + tSS[1:nEffects,1]),4)
    
  #df, F and p-values
  tSS[1:(nEffects+1),4] = tANOVA[1:(nEffects+1),2]  #Add df
  tSS[1:nEffects,5] = round(tANOVA[1:nEffects,3],4)  #Add F stats
  tSS[1:nEffects,6] = round(tANOVA[1:nEffects,4],4)  #Add p-values
  

  
  
  
  return(tSS)

}