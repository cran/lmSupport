modelCompare <-
function(ModelC, ModelA)
{
  sseC = sum(residuals(ModelC)^2)
  sseA = sum(residuals(ModelA)^2)

  pC = length(coef(ModelC))
  pA = length(coef(ModelA))
  if (!(pA > pC))  stop('Invalid model comparison:  modelA does not have more parameters than modelC')
  
  termsA <- attr(terms(ModelA), "term.labels")
  termsC <- attr(terms(ModelC), "term.labels")
  
  if (!all(termsC %in% termsA))  stop('Invalid model comparison: modelC is not a subset of modelA')

  nC = ModelC$df.residual + pC
  nA = ModelA$df.residual + pA
  if (!(nC == nA))  stop('Invalid model comparison:  ModelA and ModelC have different N')
  
  nDF = pA - pC
  dDF = nA - pA
  FStat=   ((sseC - sseA) / (pA-pC)) / (sseA / (nA-pA))
  
  p = pf(FStat,nDF, dDF, lower.tail = FALSE)
  
  PRE = (sseC - sseA) / sseC
  DeltaR2 = summary(ModelA)$r.squared - summary(ModelC)$r.squared
  
  #print output
  cat('SSE (Compact) = ', sseC, '\n', sep=' ')
  cat('SSE (Augmented) = ', sseA,  '\n', sep=' ')
  cat('Delta R-Squared = ', DeltaR2,  '\n', sep=' ')
  cat('Partial Eta-Squared (PRE) = ', PRE,  '\n', sep=' ')
  cat('F(', nDF, ',', dDF, ') = ', FStat, ', ', 'p = ', p, '\n', sep='')
  
  Results = list(sseC=sseC, sseA=sseA, pC=pC, pA=pA, nDF=nDF, dDF=dDF, Fstat=FStat, p=p,  PRE=PRE, DeltaR2=DeltaR2)
  invisible(Results)  #return but dont print list
}