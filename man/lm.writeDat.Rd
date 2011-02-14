\name{lm.writeDat}
\alias{lm.writeDat}
\title{Saves dataframe as tab-delimited text file with typical Curtin lab parameters}
\description{
Saves a dataframe as a tab-delimited data file with standard Curtin lab format.
Will add rownames as a first column in .dat file and label this column with SubID
}
\usage{lm.writeDat(data, file, SubID = "SubID")}
\arguments{
  \item{data}{a dataframe}
  \item{file}{file name for .dat file}
  \item{SubID}{Name for new column with data from rownames.  
  If NULL, rownames will not be added to .dat file)  Default is 'SubID'}
}
\details{
Uses these parameters with write.table
no append, no quote, separator is tab, no rownames, yes for columnns.
}
\value{no return value but creates .dat file in current wd}
\author{John J. Curtin \email{jjcurtin@wisc.edu}}
\seealso{
read.table(), read.delim(), write.table()
}
\examples{
lm.writeDat(Ornstein, file="Test1.dat")
lm.writeDat(Ornstein, file="Test2.dat", SubID = 'ID')
lm.writeDat(Ornstein, file="Test3.dat", SubID = NULL)
}
\keyword{manip}