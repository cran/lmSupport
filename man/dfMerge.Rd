\name{dfMerge}
\alias{dfMerge}
\title{Merges two data frames}
\description{
Merges variables from two data frames (DataX, DataY).  
By default matches on row names but can use other variable names 
in DataX (ByX) and DataY (ByY) as needed.
By default, includes all cases in DataX and DataY but can limit to 
only matching (AllX=FALSE, AllY=FALSE) or left join (AllY=FALSE) 
or right join (AllX=FALSE).
}
\usage{
dfMerge(DataX, DataY, ByX = 0, ByY = 0, AllX = TRUE, AllY = TRUE)
}
\arguments{
  \item{DataX}{first data frame for merge}
  \item{DataY}{second data frame for merge}
  \item{ByX}{Name of variable in DataX to match cases on.   Column can be specified by
     name or number.  Default is 0 which uses rownames}
  \item{ByY}{Name of variable in DataY to match cases on.   Column can be specified by
     name or number.  Default is 0 which uses rownames}
  \item{AllX}{logical; if TRUE, then extra rows will be added to the
          output, one for each row in DataX that has no matching row
          in DataY  These rows will have NAs in those columns
          that are usually filled with values from dY.  The
          default is TRUE, so that all rows with data from both
          dX and dY are included in the output. In other words, 
          it is the union of these two dataframes}
  \item{AllY}{analagous to AllX but for DataY}
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
dNew = dfMerge(dX,dY)
}
\keyword{manip}