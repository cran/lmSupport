\name{varDescribeBy}
\alias{varDescribeBy}
\title{Provides common desriptives for single DV by factor(s)}
\description{
Returns Group N's, Missing, Means, SDs, Mins, and Maxs for DV by a list of factors
Removes missing values from calculation of all descriptives (except of course Missing)
}
\usage{
varDescribeBy(DV, IVList)
}
\arguments{
  \item{DV}{dependent variable from data frame}
  \item{IVList}{list of one or more factors from data frame}
}
\value{
Returns list of N, Miss, Mean, SD, Min, Max
}
\author{John J. Curtin \email{jjcurtin@wisc.edu}}
\examples{
varDescribeBy(Adler$rating,list(Adler$expectation,Adler$instruction))
}
\keyword{summary}
