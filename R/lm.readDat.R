lm.readDat <-
function(file, SubID='SubID')
#opens a tab-delimited data file with standard Curtin lab format
#header = TRUE, sep = '\t'
#if variable named SubID (default) or other text supplied by SubID variable is in file,
#row names will be set with this variable and then variable is removed from data frame
#2011-02-03, JJC
{
  dData = read.table(file, header=TRUE, sep='\t')
  if(is.element(SubID, names(dData)))
  {
    dData = lm.setRownames(dData,SubID = SubID)
  }
  return(dData)
}