\name{lm.renameVar}
\alias{lm.renameVar}
\title{Rename Variable in Dataframe}
\description{
  Renames a variable in specified dataframe. 
}
\usage{lm.renameVar(data, from, to)}
\arguments{
  \item{data}{a dataframe object}
  \item{from}{original name of variable as string}
  \item{to}{new name of variable as string}
}
\value{
  Returns dataframe with new variable name for specified variable
}
\author{John J. Curtin \email{jjcurtin@wisc.edu}}
\examples{
  d = data.frame(x=1:10, y=11:20)
  names(d)
  d = lm.renameVar(d,'x', 'x1')
  names(d)
}
\keyword{manip}