\name{modelPower}
\alias{modelPower}
\title{Calculate power for GLM tests}
\description{
Wrapper to calculate power for tests of paramter estimates or full model in GLM based on Cohen's tables and using pwr.f2.test in pwr packag.  Allows use of partial eta squared or delta R2 rather than just f2 as effect size.   If you provide power, it returns v, if you provide v, it returns power.  You must specific effect size as either f2, partial eta2, or delta R2 with model R2. 
}
\usage{
modelPower(u=NULL, v=NULL, alpha=0.05, power=NULL, f2=NULL, peta2=NULL, dR2=NULL, R2=NULL)}
\arguments{
  \item{u}{numerator degrees of freedom for effect;  PA-PC in model comparision terms}
  \item{v}{denominator degrees of freedom for effect;  N-PA in model comparision terms}
  \item{alpha}{alpha for statistical test}
  \item{power}{power for statistical test}
  \item{f2}{f2 effect size}
  \item{peta2}{partial eta2 effect size}
  \item{dR2}{delta R2 effect size;  if provided must also specify R2}
  \item{R2}{Model R2, only need if using Delta R2 as effect size}
}
\value{
Returns either power or v from analyis.  N* = v + PA
}
\author{John J. Curtin \email{jjcurtin@wisc.edu}}
\seealso{pwr.f2.test}
\examples{
modelPower(u=1, power=.90, peta2=.157)
modelPower(u=1, v=75, peta2=.157)
}
\keyword{manip}