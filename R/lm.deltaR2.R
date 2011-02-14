lm.deltaR2 <-
function(model1, model2)
{
  mResults = matrix(NA,3,5,FALSE,dimnames =list(c('Model 1', 'Model 2', 'Delta'), c('R2', 'F', 'ndf', 'ddf', 'p')))
  sse1 = sum(model1$residuals^2)
  sse2 = sum(model2$residuals^2)

  ssr1= sum((model1$fitted.values - mean(model1$fitted.values))^2)
  ssr2= sum((model2$fitted.values - mean(model2$fitted.values))^2)

  #Model 1 stats
  R2_1 = ssr1 / (ssr1 + sse1)
  ndf1 = length(model1$coefficients) -1
  ddf1 =  model1$df.residual  
  F1 = R2_1/ndf1 / ((1- R2_1) / ddf1 )
  p1 = pf(F1, df1=ndf1, df2=ddf1, lower.tail=FALSE)
  mResults[1,1] = R2_1
  mResults[1,2] = F1
  mResults[1,3] = ndf1
  mResults[1,4] = ddf1
  mResults[1,5] = p1

  #Model 2 stats
  R2_2 = ssr2 / (ssr2 + sse2)
  ndf2 = length(model2$coefficients) -1
  ddf2 =  model2$df.residual  
  F2 = R2_2/ndf2 / ((1- R2_2) / ddf2 )
  p2 = pf(F2, df1=ndf2, df2=ddf2, lower.tail=FALSE)  
  mResults[2,1] = R2_2
  mResults[2,2] = F2
  mResults[2,3] = ndf2
  mResults[2,4] = ddf2
  mResults[2,5] = p2  
  
  #Delta R2
  dR2 = R2_2 - R2_1
  ndfd = ndf2-ndf1
  ddfd = ddf2
  Fd =   (dR2/ndfd)  /  ((1- R2_2) / ddfd )
  pd = pf(Fd, df1=ndfd, df2=ddfd, lower.tail=FALSE)
  mResults[3,1] = dR2
  mResults[3,2] = Fd
  mResults[3,3] = ndfd
  mResults[3,4] = ddfd
  mResults[3,5] = pd  
  
  return(mResults)
}