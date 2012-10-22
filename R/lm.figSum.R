lm.figSum <-
function(TheVar, VarName = '', IDs = NULL, AddRug=TRUE, AddDensity=TRUE, detail=2)
#creates a histogram (using default parameters for hist), with options to include
#rug plot, density plot and univariate descriptives of varying detail
#Revision history
#2011-04-11, fixed bug with double plot of hist, JJC
#2011-08-23:  added ID function with default of no ID, JJC
{
  #print descriptives
  print(paste('Descriptive statistics: ',VarName ,sep=''))
  print(lm.describeData(TheVar,detail))
    
  par(cex.lab=1.5, cex.axis=1.2, lwd=2)  #set graphics parameters for pretty graphs
  HistData = hist(TheVar, main='', xlab=VarName)
  if(AddRug)  rug(TheVar,col='red')
  
  if(AddDensity) 
  {
      DensityData = density(TheVar)
      lines(DensityData$x,DensityData$y * (max(HistData$counts)/max(DensityData$y)), col='blue')  
  }
  
  if (!is.null(IDs)) 
  {
     identify(TheVar,rep(0,length(TheVar)), labels=IDs)
  } 
}