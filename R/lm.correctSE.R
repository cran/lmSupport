lm.correctSE <- function(model)
{
  newSEs = sqrt(diag(hccm(model)))
  modelsum = summary(model)
  thetable = modelsum$coefficients
  thetable[,2] = newSEs
  thetable[,3] = thetable[,1] / thetable[,2]
  thetable[,4] = 2*(pt(abs(thetable[,3]), df=modelsum$df[2], lower.tail=FALSE))

  cat('Uncorrected Tests of Coefficients\n\n')
  print(modelsum$coefficients)
  cat('\nWhite (1980) Heteroscedascity-corrected SEs and Tests\n\n')
  print(thetable)
}