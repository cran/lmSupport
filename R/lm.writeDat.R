lm.writeDat <-
function(data, file, SubID='SubID')
#writes a data frame as a tab-delimited data file with standard Curtin lab format
#Will add rownames as a variable named SubID
#2011-02-03, JJC
{
  if(!is.element(SubID, names(data)) && !is.null(SubID))
  {
    dNew = as.data.frame(rownames(data))
    names(dNew)[1] = SubID
    dNew[2:(ncol(data)+1)] = data
    data = dNew
  }

  write.table(data, file = file, append = FALSE, quote = FALSE, sep = '\t',
                 eol = "\n", na = "NA", dec = ".", row.names = FALSE,
                 col.names = TRUE)
}