\name{lm.boxCox}
\alias{lm.boxCox}
\title{Calculates lambda for Box-Cox power transformation}
\description{
Calcluates and plots log-liklihoods lambda for power transformation of response variable.  
Reports chi-square test of lambda <> 1.
All values of Y must > 0 or function will crash.  Add offset to Y if necessary (see example).
Default lambda range is -2 to 2.    Uses boxcox() from \pkg{MASS} package.
}
\usage{
lm.boxCox(model, lambdas = seq(-2, 2, by = 0.1))
}
\arguments{
  \item{model}{an unweighted linear model, produced by \code{lm}.}
  \item{lambdas}{a vector of lambda values to plot.  Default is seq(-2,2,by=0.1)}
}
\references{
Box, G. E. P. & Cox, D. R. (1964). 
An analysis of transformations (with discussion).  
Journal of the Royal Statistical Society, 26, 211-252.
}
\author{John J. Curtin \email{jjcurtin@wisc.edu}}
\seealso{
boxcox() in car package
}
\examples{
m = lm(interlocks + 1 ~ assets+nation, data=Ornstein)
lm.boxCox(m)
}
\keyword{regression}
