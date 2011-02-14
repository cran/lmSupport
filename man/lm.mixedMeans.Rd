\name{lm.mixedMeans}
\alias{lm.mixedMeans}
\title{Creates estimated mean and SE matrices from list of lm models}
\description{
Used to create estimated mean, SE, lower and upper bound matrices for 
linear models based on data provided in dPredict.  Commonly used when list includes models
from individual variates in a repeated measure analysis.   Designed to be used 
with categorical IV (or IVs).  However, will work with quantitative IV as well.
}
\usage{
lm.mixedMeans(lModels, data, GroupNames = NULL, VariateNames = NULL)
}
\arguments{
  \item{lModels}{a list of linear models produced by \code{lm}.  Typical for this
  list to be models with variates from repeated measures design.}
  \item{data}{a data frame with cases that represent groups for prediction.
  Must contain all the regressors for the linear models in lModel.  Can set control/covariate
  regressors to 0 or other constant for main or simple effects at a fixed level of covariate.}
  \item{GroupNames}{Vector of string names for Groups (cases in dPredict).  Used to label rows in 
  estimated mean, SE, etc matrices.   If not provided, will use 1:NGroups.}
  \item{VariateNames}{Vector of string names for DVs for each model in lModel.   
  If not provided, will use DV name from the model itself.}
}
\details{
Control/covariates should be coded such that they can be set to 0 to control but ignore.
Can use lm.codeRegressors() for catetorical control variables.
}
\value{
Returns a list with 4 matrices: Means, SE, SE.l (predicted values for Mean - 1SE) 
and SE.u (predicted value for mean + 1SE).  These matrices are of the form Groups X Variates.
}
\author{John J. Curtin \email{jjcurtin@wisc.edu}}
\seealso{
lm.pointEstimates, lm.codeRegressors, predict
}
\examples{
#NEED TO DEVELOP EXAMPLE BASED ON REPEATED MEASURES DATA
}
\keyword{regression}