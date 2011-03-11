\name{lm.figSum}
\alias{lm.figSum}
\title{Creates histogram, optional rug and density plots, and generates univariate descriptive statistics}
\description{
Represents important aspects of a variable/vector both visually (histogram, rug, and density plots) and with descriptive statistics of varying detail
}
\usage{
lm.figSum(TheVar,  VarName = '', AddRug = TRUE, AddDensity = TRUE, detail = 2)
}
\arguments{
  \item{TheVar}{A variable/vector to visualize}
  \item{VarName}{The variable name of TheVar as string. Default = ''}
  \item{AddRug}{TRUE (default) or FALSE to include rug plot}
  \item{AddDensity}{TRUE (default) or FALSE to include density plot}
  \item{detail}{1-3 of increasing detail for descriptives using lm.describeData()}
}
\value{
returns descriptive statistics table and creates graphic as side effect
}
\author{John J. Curtin \email{jjcurtin@wisc.edu}}
\seealso{
hist(), rug(), density(), lm.describeData(), describe()
}
\examples{  
lm.figSum(Prestige$income, 'Income')
}
\keyword{ descriptives }
