lm.removeCases <- function(data,Cases)
#Returns dataframe with Cases (identified by rowname) removed.  generates warning if cases not found
{ 
  if (is.numeric(Cases))  
  {
    d = data[!is.element(as.numeric(rownames(data)),Cases), ]
    FoundIndex =  is.element(Cases,as.numeric(rownames(data)))
    if (sum(FoundIndex) < length(Cases))
    {      
      warning(c('The following cases were not found in dataframe: ', paste(setdiff(Cases,as.numeric(rownames(data))), collapse= ', ')))
    } 
  }  else
  {
      d = data[!is.element(rownames(data),Cases), ]
      FoundIndex =  is.element(Cases,rownames(data))
      if (sum(FoundIndex) < length(Cases))
      {       
        warning(c('The following cases were not found in dataframe: ', paste(setdiff(Cases,rownames(data)), collapse= ', ')))
      } 
  }  
  return(d)
}