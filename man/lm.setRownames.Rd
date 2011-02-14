\name{lm.setRownames}
\alias{lm.setRownames}
\title{Sets rownames to SubID}
\description{
Sets the row names of the data frame to the variable name listed as SubID.
SubID should be text name of variable.  
Also keeps number of characters constant by default and removes SubID by default
}
\usage{
lm.setRownames(data, SubID = "SubID", FixedWidth = TRUE, Remove = TRUE)
}
\arguments{
  \item{data}{a data frame with a variable containing subject ID numbers}
  \item{SubID}{Text name of subject ID variable.  Default is SubID}
  \item{FixedWidth}{logical.  If TRUE (default), all rowames will be the same length 
  by padding with leading 0's}
  \item{Remove}{logical.  If TRUE (default), the subject ID variable will be 
  removed from data frame after setting rownames}
}
\value{
Returns data frame with rownames set (and SubID removed if requested)
}
\author{John J. Curtin \email{jjcurtin@wisc.edu}}
\examples{
d <- data.frame(SubID = c(1,2,3,10,20), v1=c(1,2,3,4,5), v2=c(1,NA,NA,2,4), data=1:5)
d=lm.setRownames(d)
}
\keyword{manip}