\name{lm.caseAnalysis}
\alias{lm.caseAnalysis}
\title{Provides graphs and/or tests for problematic cases for a linear model}
\description{
Provides diagnositic graphs and visual cut points for identification of points 
that are univaraite outliers, high leverage, regression outliers, and/or influential
}
\usage{
lm.caseAnalysis(model, Type = "RESIDUALS", ID = row.names(model$model))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{a linear model produced by \code{lm}.}
  \item{Type}{Type = c('RESIDUALS', 'UNIVARIATE', 'HATVALUES', 'COOKSD', 'DFBETAS', 'INFLUENCEPLOT' 'COVRATIO')
  RESIDUALS (default) = regression outliers, UNIVARIATE = univariate outliers,
  HATVALUES = leverage, COOKSD = model influence, DFBETAS= individual parameter influence, 
  INFLUENCEPLOT= leverage X influence, COVRATIO = inflation of SEs.}
  \item{ID}{Use to identify points.  Default = row.names(model$model).  NULL = no identification}
}
\references{
Fox, J. (1991). 
Regression diagnostics. SAGE Series (79)
Quantitative Applications in the Social Science.
}
\author{John J. Curtin \email{jjcurtin@wisc.edu}}
\examples{
m = lm(interlocks~assets+nation, data=Ornstein)
lm.caseAnalysis(m,'RESIDUALS')
lm.caseAnalysis(m,'DFBETAS')
}
\keyword{regression}
