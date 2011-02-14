lm.caseAnalysis <- function(model, Type='RESIDUALS', ID=row.names(model$model))
#Provides diagnositic graphs and visual cut points for identification of points that are univaraite outliers, high leverage, regression outliers, and/or influential
#model: an LM model object
#Type: univariate, hatvalues, residuals, cooksd, influenceplot, dfbetas, covratio
#ID:  Use to identify points.  Default = row.names(model$model).  NULL = no identification
#2010-02-07, JJC
#2010-06-08, updated to new car package.  updated to not require thedata unless needed, JJC
#2020-06-09, modified to include model formula in titles, JJC
{
switch(toupper(Type),

   UNIVARIATE =
   {
      d=model$model
      {Vars = names(d)}
      for(varname in Vars)
      {
          par(cex.lab=1.5, cex.axis=1.2, lwd=2, ask=TRUE)  #set graphics parameters for pretty graphs
          if(is.factor(d[[varname]]))
          {
             plot(d[varname], xlab=varname, ylab = "Frequency")
          } else
          {
             #hist(thedata[[varname]],nclass=n.bins(thedata[[varname]]),xlab=varname, main='Red: Mean +- 3SD; Green: Median +- 2.2IQR')
             hist(d[[varname]],xlab=varname, main='Red: Mean +- 3SD; Green: Median +- 2.2IQR')
             points(d[[varname]],rep(0,length(d[[varname]])),pch = "|", col="blue")
             abline(v=c(-3,0, 3) * sd(d[[varname]]) + mean(d[[varname]]),col='red', lty=c(1,2,1))
             abline(v=c(-2.2,0, 2.2) * IQR(d[[varname]]) + median(d[[varname]]),col='green', lty=c(1,2,1))  #IQR = 1.34896SD for normal distribution
             if (!is.null(ID))   {identify(d[[varname]],rep(0,length(d[[varname]])),labels=ID)}
          }
      }
   },

   HATVALUES =
   {
      #Diagnostics for Leverage (Hat values)
      #NOTE: Mahalanobis distance = (N - 1)(h - 1/N).   SPSS reports centered leverage (h - 1/N)
      #NOTE: 3 * mean(h) is cut for small sample.  2 * mean(h) is cut for large sample
      par(cex.lab=1.5, cex.axis=1.2, lwd=2)  #set graphics parameters for pretty graphs
      TheTitle = paste('Model: ', model$call[2], '\n', 'Small sample cut (green) = 3 * mean(Hat)\nLarge sample cut: 2 * mean(Hat)', sep='')
      hist(hatvalues(model),xlab='Hat Values', main=TheTitle)
      abline(v= c(2,3) * mean(hatvalues(model)),col=c('red', 'green'))
      points(hatvalues(model),rep(0,length(hatvalues(model))),pch = "|", col="blue")
      if (!is.null(ID))   {identify(hatvalues(model),rep(0,length(hatvalues(model))),labels=ID)}
   },

   RESIDUALS =
   {
      #Diagnostics for Regresssion Outliers (studentized residuals;E*i)
      #NOTE:  SPSS calls these Studentized Deleted Residuals.  Cohen calls these Externally Studentized Residual
      #E*i follows t-distribution with N-k-2 dfs
      #outlier.test(model, labels=ID) #to get corrected p-value for worst outlier
      #NOTE: Can get Bonferroni corrected p-value for any Studentized t as: N * 2 * pt(t, N-k-2, lower.tail = FALSE)
      par(cex.lab=1.5, cex.axis=1.2, lwd=2)  #set graphics parameters for pretty graphs
      N=length(rstudent(model))
      k= length(coef(model))-1
      TCut <- qt(p = .025/N, df = N-k-2, lower.tail = FALSE) #Bonferroni corrected t cut for studentized residuals
      TheTitle = paste('Model: ', model$call[2], '\n', 'Bonferroni corrected p < .05 cut-off in red', sep='')
      hist(rstudent(model), xlab='Studentized Residuals', main=TheTitle)
      abline(v= c(-1,1) * TCut,col="red")
      points(rstudent(model),rep(0,length(rstudent(model))),pch = "|", col="blue")
      if (!is.null(ID))   {identify(rstudent(model),rep(0,length(rstudent(model))),labels=ID)}
   },

   COOKSD =
   {
      #Diagnositics for Influence
      #NOTE: Alternative cuts are 4/(N-k-1) & qf(.5,k+1,N-k-1)
      par(cex.lab=1.5, cex.axis=1.2, lwd=2)  #set graphics parameters for pretty graphs
      N=length(cooks.distance(model))
      k= length(coef(model))-1
      TheTitle = paste('Model: ', model$call[2], '\n', '4/(N-k-1) cut-off (red)\nqf(.5,k+1,N-k-1) cut-off (green)', sep='')
      hist(cooks.distance(model), xlab='Cooks d', main=TheTitle)
      abline(v=c((4/(N-k-1)),qf(.5,k+1,N-k-1)) ,col=c('red', 'green'))
      points(cooks.distance(model),rep(0,length(cooks.distance(model))),pch = "|", col="blue")
      if (!is.null(ID))   {identify(cooks.distance(model),rep(0,length(cooks.distance(model))),labels=ID)}
   },

   DFBETAS =
   {
      {Vars = dimnames(dfbetas(model))[[2]]}
      for(varname in Vars)
      {
         par(cex.lab=1.5, cex.axis=1.2, lwd=2, ask=TRUE)  #set graphics parameters for pretty graphs
         TheTitle = paste('Model: ', model$call[2], '\n', 'B= ', coef(model)[varname], sep='')
         hist(dfbetas(model)[,varname],xlab=paste('DFBETAS:', varname), main=TheTitle)
         points(dfbetas(model)[,varname],rep(0,length(dfbetas(model)[,varname])),pch = "|", col="blue")
         abline(v=c(-2, 2), col='red')
         if (!is.null(ID))   {identify(dfbetas(model)[,varname],rep(0,length(dfbetas(model)[,varname])),labels=ID)}
      }
      par(ask=FALSE)
      avPlots(model,intercept=TRUE,id.method='identify', id.n=nrow(dfbetas(model)), labels=ID)
   },

   INFLUENCEPLOT =
   {
      par(cex.lab=1.5, cex.axis=1.2, lwd=2)  #set graphics parameters for pretty graphs
      TheTitle = paste('Influence Bubble plot', '\nModel: ', model$call[2], sep='')
      plot(hatvalues(model),rstudent(model), type='n', xlab='Hat Values', ylab='Studentized Residuals', main=TheTitle )
      cooksize = 10*sqrt(cookd(model))/max(cookd(model))
      points(hatvalues(model), rstudent(model),cex=cooksize)

      N=length(rstudent(model))
      k= length(coef(model))-1
      TCut <- qt(p = .025/N, df = N-k-2, lower.tail = FALSE) #Bonferroni corrected t cut for studentized residuals
      abline(h=c(-1,0, 1) * TCut,col='red', lty=c(1,2,1))
      abline(v=c(1,2,3) * mean(hatvalues(model)),col='red', lty=c(2,1,1))
      if (!is.null(ID))   {identify(hatvalues(model),rstudent(model),labels=ID)}
   },

   COVRATIO =
   {
      #Diagnositics for SE inflation
      N=length(covratio(model))
      k= length(coef(model))-1
      par(cex.lab=1.5, cex.axis=1.2, lwd=2)  #set graphics parameters for pretty graphs
      TheTitle = paste('Model: ', model$call[2], '\n', 'abs((3*(k+1)/N)-1 cut-off in red', sep='')
      hist(covratio(model), xlab='CovRatio', main=TheTitle)
      abline(v=abs((3*(k+1)/N)-1), col="red")
      points(covratio(model),rep(0,length(covratio(model))),pch = "|", col="blue")
      if (!is.null(ID))   {identify(covratio(model),rep(0,length(covratio(model))),labels=ID)}
   },

   {print('Valid options for type: hatvalues, residuals, cooksd, dfbetas, influenceplot, covratio, univariate')}     #OTHERWISE
)#end switch

}
