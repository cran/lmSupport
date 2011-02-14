\name{lm.sumSquares}
\alias{lm.sumSquares}
\title{Calculates descriptive statistics based on Sums of Squares}
\description{
Calculates unique SSRs, SSE, SST.  Based on these SSs, it calculates 
sr2 (aka eta2, delta R2), and pr2 (aka partial eta2) for all effects in a linear model object.  
For categorical variables, it calculates these for multi-df effect.  
Uses Anova() from car package with Type 3 error
}
\usage{
lm.sumSquares(model)
}
\arguments{
  \item{model}{a linear model, produced by \code{lm}}
}
\value{
Returns table with all indices
}
\author{John J. Curtin \email{jjcurtin@wisc.edu}}
\seealso{
Anova()
}
\examples{
m = lm(interlocks~assets+nation, data=Ornstein)
lm.sumSquares(m)
}
\keyword{regression}
