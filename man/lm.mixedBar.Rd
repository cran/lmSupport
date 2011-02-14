\name{lm.mixedBar}
\alias{lm.mixedBar}
\title{Creates bar graph for mixed model design}
\description{
Creates 'rough' (i.e., not publication quality) graph for examining results form
mixed model design includes factor IV and repeated measures across variates in separate lm models.
}
\usage{
lm.mixedBar(lModels, data, GroupNames = NULL, VariateNames = NULL, GroupClusters = TRUE, LegendOffset = 0.3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lModels}{a list of linear models produced by \code{lm}.  List should be order 
  as will be presented in graph.}
  \item{data}{a data frame with rows/cases to represent regressor coding for 
  each group to be graphed.  Should include all regressors from lm objects.   
  Set control/covariates to 0 with approriate coding.  Can use lm.codeRegressors 
  for factor covariates}
  \item{GroupNames}{a vector of strings with Group names.  Will use 1:NGroups if not included.}
  \item{VariateNames}{a vector of strings with variate (DVs from lModels) names.  
  Will use DV names from lModels if not provided}
  \item{GroupClusters}{Logical; If TRUE (default) clusters of bars should consist 
  of single group (across variates).  If FALSE clusters should consist of 
  single variate (across groups)}
  \item{LegendOffset}{LegendOffset indicates where to put top of legend relative to max y.  
  Default is Max Y + .3* Max Y}
}
\value{
Returns list with group means, SEs, etc as from lm.mixedMeans}
\author{John J. Curtin \email{jjcurtin@wisc.edu}}
\seealso{
lm.mixedMeans, lm.pointEstimates, lm.codeRegressors, predict
}
\examples{
##NEED TWO EXAMPLES (CODED BASED ON GROUPCLUSTER)
}
\keyword{regression}
