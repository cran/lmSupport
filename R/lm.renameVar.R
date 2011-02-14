lm.renameVar <- function(data, from, to)
#2010-10-11: released, JJC
{
  if (!is.element(to,names(data))) names(data)[names(data)==from] = to 
  else stop('Error: Variable name already exists in dataframe')
  
  return(data)
}

