\name{lm.mergeData}
\alias{lm.mergeData}
\title{Merges two data frames}
\description{
Merges variables from two data frames (dX, dY).  
By default matches on row names but can use other variable names 
in d1 (by.x) and d2 (by.y) as needed.
By default, includes all cases in dX and dY but can limit to 
only matching (all.X=FALSE, all.y=FALSE) or left join (all.y=FALSE) 
or right join (all.x=FALSE).
}
\usage{
lm.mergeData(dX, dY, by.x = 0, by.y = 0, all.x = TRUE, all.y = TRUE)
}
\arguments{
  \item{dX}{first data frame for merge}
  \item{dY}{second data frame for merge}
  \item{by.x}{Name of variable in dX to match cases on.   Column can be specified by
     name or number.  Default is 0 which uses rownames}
  \item{by.y}{Name of variable in dY to match cases on.   Column can be specified by
     name or number.  Default is 0 which uses rownames}
  \item{all.x}{logical; if TRUE, then extra rows will be added to the
          output, one for each row in dX that has no matching row
          in dY.  These rows will have NAs in those columns
          that are usually filled with values from dY.  The
          default is TRUE, so that all rows with data from both
          dX and dY are included in the output. In other words, 
          it is the union of these two dataframes}
  \item{all.y}{analagous to all.x but for dY}
}
\details{
see merge() for more details
}
\value{
Returns merged data frame
}
\author{John J. Curtin \email{jjcurtin@wisc.edu}}
\seealso{
merge
}
\examples{
dX <- data.frame(v1=c(1,2,3,4,5), v2=c(1,NA,NA,2,4), data=1:5)
rownames(dX) = c(1,2,3,4,5)
dY <- data.frame(v3=c(3,2,1,4,15), v4=c(2,4,5,6,7), data=6:10)
rownames(dY) = c(1,2,3,4,6)
dNew = lm.mergeData(dX,dY)
}
\keyword{manip}