lm.mixedBar <-
function(lModels, data, GroupNames = NULL, VariateNames = NULL, GroupClusters = TRUE, LegendOffset = 0.3)
#lModels is a list with lm models for each within subject variate, ordered as will appear in graph
#data is a data.frame with rows for each "group" to be graphed.  Should include all regressors from the lm objects
#GroupNames are the string names to label groups.  If not provided, will use sequential numbers
#VariateNames are the string names to label model variates.  If not provided, will use DV names from each model
#GroupsClusters indicates clusters of bars should consist of single group (across variates).  If FALSE clusters should consist of single variate (across groups)
#LegendOffset indicates where to put top of legend relative to max y.  Default is Max Y + .3* Max Y
#2010-06-10:  released, JJC
#2010-10-12: updated to use lm.mixedMeans, JJC
{
  GraphData = lm.mixedMeans(lModels, data, GroupNames, VariateNames)
  Means = GraphData$Means
  SE.u = GraphData$SE.u
  SE.l = GraphData$SE.l
  
  if(GroupClusters)  #Default with variates within groups
  {
    Means = t(Means)
    SE.u = t(SE.u)
    SE.l = t(SE.l) 
    BarColors = rainbow(length(lModels)) 
  }else
  {
    BarColors = rainbow(nrow(data)) #Groups within variates
  }
    
  op = par(lwd=3, cex = 1.5, font=2,  cex.axis=1, font.axis=2, cex.lab =1.5, font.lab=2, xpd=TRUE)    
  barplot2(Means, beside = TRUE, ylim = c(0,max(SE.u) + .1 * max(SE.u)),xlab = '', ylab = '', plot.ci =TRUE, ci.l = SE.l, ci.u = SE.u,  ci.lwd = 3, axes=TRUE, col= BarColors)  
  legend(list(x=1,y=(max(SE.u) + LegendOffset * max(SE.u))), colnames(t(Means)), fill=BarColors, cex=0.75)
  par(op)
  
  return(GraphData)   
}