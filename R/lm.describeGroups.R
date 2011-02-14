lm.describeGroups <- function(DV,IVList)
#Returns Group N's, Missing, Means, SDs, SEs, Mins, and Maxs for DV by IVList
{ 
  N = tapply(DV,IVList, function(x) (sum(!is.na(x))))
  Miss = tapply(DV,IVList, function(x) (sum(is.na(x)))) 
  Mean = tapply(DV, IVList, mean, na.rm=TRUE)
  SD = tapply(DV, IVList, sd, na.rm=TRUE)
  SE = tapply(DV, IVList, function(x)(sd(x,na.rm=TRUE)/sqrt(sum(!is.na(x)))))
  Min = tapply(DV, IVList, min, na.rm=TRUE)
  Max = tapply(DV, IVList, max, na.rm=TRUE)
  return(list(N=N, Miss=Miss, Mean=Mean, SD=SD, SE=SE, Min=Min, Max=Max))
}