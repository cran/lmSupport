\name{lm.clearAll}
\alias{lm.clearAll}
\title{Clear workspace, screen/console, and close all graphic devices}
\description{
Clears screen/console, closes all graphic devices, and removes all objects.  
}
\usage{
lm.clearAll()
}
\details{
Requires the RDCOMClient which is not on CRAN.  Can be installed with this code:
Install.packages('RDCOMClient', repos = 'http://www.omegahat.org/R').  
NOTE: This package and therefore lm.clearAll() only works on the Windows platform
}
\references{
Uses code from Philippe Grosjean and Gabor Grothendieck to clear screen
}
\author{John J. Curtin \email{jjcurtin@wisc.edu}}
\seealso{
graphics.off(), rm(), ls()
}
\examples{
lm.clearAll()
}
\keyword{environment}
\keyword{utilities}
