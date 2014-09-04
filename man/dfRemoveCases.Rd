\name{dfRemoveCases}
\alias{dfRemoveCases}
\title{Removes cases from dataframe}
\description{
Removes cases from dataframe.  Cases can be numeric or character.  
If numeric, rownames must be able to be converted to numeric.  
Returns warning if cases not found in dataframe.
}
\usage{
dfRemoveCases(Data, Cases)
}
\arguments{
  \item{Data}{a dataframe}
  \item{Cases}{a vector of numeric or character case IDs}
}
\value{Returns dataframe with cases removed.}
\author{John J. Curtin \email{jjcurtin@wisc.edu}}
\examples{
d = dfRemoveCases(Prestige, c('bookbinders')) #data from car package
d = dfRemoveCases(Davis, c(1,2,4))        #data from car package
}
\keyword{manip}
