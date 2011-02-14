\name{lm.codeRegressors}
\alias{lm.codeRegressors}
\title{Adds mechanical regressors for factor to dataframe as variables}
\description{
Adds new variables/columns in dataframe to represent numeric regressors for a factor.
Factors are coded using their currently defined contrast codes.  
This function is useful for control of a factor covariate when graphing 
and ignoring this factor and/or other lower-level control variables.  
For this purpose, POC coding will typically be set for factor prior to using lm.codeRegressor
}
\usage{
lm.codeRegressors(data, VarName, RegressorNames = NULL)
}
\arguments{
  \item{data}{The dataframe to add regressors
}
  \item{VarName}{Character string name of variable to code regressor for
}
  \item{RegressorNames}{Optional variable names for regressors.}
}
\value{
Returns original data frame (dData) with addition of new regressors.
}
\author{John J. Curtin \email{jjcurtin@wisc.edu}}
\examples{
dNew = Ornstein
contrasts(dNew$nation) = lm.setContrasts(dNew$nation, Type='POC', POCList = list(c(3,-1,-1, -1),c(0,2,-1, -1), c(0,0,1,-1)))
dNew = lm.codeRegressors(dNew,'nation')
}
\keyword{regression}
