\name{lm.describeData}
\alias{lm.describeData}
\title{Provides typical descriptive statistics for data frame}
\description{
Provides three levels of detail regarding descriptive statistics for a data frame.  
Based on describe() function from psych package
}
\usage{
lm.describeData(data, detail = 2, digits=2)
}
\arguments{
  \item{data}{a data frame}
  \item{detail}{Indicates level of detail for descriptives, 1=minimal, 2=typical (default), 3= detailed}
  \item{digits}{Number of decimal places to display; NULL = display all sig digits.  Default =2.}
}
\value{
Returns table with descriptive statistics rounded to digits.
}
\author{John J. Curtin \email{jjcurtin@wisc.edu}}
\seealso{describe lm.describeGroups describe.by}
\examples{
lm.describeData(Ornstein)
lm.describeData(Ornstein, detail=3)
lm.describeData(Ornstein, detail=2, digits=1)
}
\keyword{manip}