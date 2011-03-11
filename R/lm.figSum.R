lm.figSum <-
function(TheVar, VarName = '', AddRug=TRUE, AddDensity=TRUE, detail=2)
#creates a histogram (using default parameters for hist), with options to include
#rug plot, density plot and univariate descriptives of varying detail
{
  par(cex.lab=1.5, cex.axis=1.2, lwd=2)  #set graphics parameters for pretty graphs
  HistData = hist(TheVar)
  hist(TheVar, main='', xlab=VarName)
  if(AddRug)  rug(TheVar,col='red')
  
  if(AddDensity) 
  {
      DensityData = density(TheVar)
      lines(DensityData$x,DensityData$y * (max(HistData$counts)/max(DensityData$y)), col='blue')  
  }
  lm.describeData(TheVar,detail)  
}