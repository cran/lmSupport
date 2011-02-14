\name{lm.deltaR2}
\alias{lm.deltaR2}
\title{F-tests for model R2s and delta R2}
\description{
Calculates F-test to compare two models to determine if model2 R2 is signifcantly 
greater than model 1 R2. Model 1 should contain subset of Model 2 regressors.
Also provides test of R2 for model 1 and model 2.  NOTE: Does not check that model 1
is subset of model 2.  User must use cautiously and verify this themselves.}
\usage{
lm.deltaR2(model1, model2)
}
\arguments{
 \item{model1}{a linear model, produced by \code{lm}.  
 This base model should include a subset of regressors from Model 2}
 \item{model2}{a linear model, produced by \code{lm}}.  This full model should 
 include all regressors from Model 1 plus additional regressors.?
}
\details{
Calculates F test for delta R2.
F =   ((Model2R2 - Model1R2) / (kModel2 - kModel1)  /  ((1- Model2R2) / (N -kModel2 - 1))
ndf = kModel2 - kModel1 
ddf = N -kModel2 - 1
}
\value{
Returns matrix with results for tests of R2 for model1, model2 and test of 
delta R2.
}
\author{John J. Curtin \email{jjcurtin@wisc.edu}}
\examples{
m1 = lm(interlocks~assets, data=Ornstein)
m2 = lm(interlocks~assets+nation, data=Ornstein)
lm.deltaR2(m1,m2)
}
\keyword{regression}
