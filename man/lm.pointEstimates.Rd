\name{lm.pointEstimates}
\alias{lm.pointEstimates}
\title{Provides point estimates and SEs from linear model}
\description{
Calculates point estimates and SEs for all cases in dataframe.
}
\usage{
lm.pointEstimates(model, data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{a linear model, produced by \code{lm}.}
  \item{data}{a dataframe containg cases for predictions.  Must include all 
  regressors from model.}
}
\value{
Returns a cases X 4 matrix.  Col 1= point estimate, Col 2= lwr bound (-1 SE), 
Col 3 = upper bound (+1 SE), Col 4 = SE.
}
\author{John J. Curtin \email{jjcurtin@wisc.edu}}
\seealso{
predict
}
\examples{
##make plot of predicted values with 1SE error bands for CAN
m = lm(interlocks~assets+nation, data=Ornstein) 
dNew = data.frame(assets = seq(1000,100000, by=1000),nation='CAN') 
pNew = lm.pointEstimates(m, dNew)
plot(dNew$assets,pNew[,1], type = 'l', col= 'red', lwd =2, xlab = 'Assets', ylab= 'Interlocks')
lines(dNew$assets,pNew[,2], type = 'l', col= 'gray', lwd =.5)
lines(dNew$assets,pNew[,3], type = 'l', col= 'gray', lwd =.5)
} 
\keyword{regression}
