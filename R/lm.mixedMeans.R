lm.mixedMeans <-
function(lModels, data, GroupNames = NULL, VariateNames = NULL)
{
  nGroups = nrow(data)
  if(is.null(GroupNames)) {GroupNames = 1:nGroups}   #If no GroupNames provided use 1:nGroups
  if(length(GroupNames) != nGroups)
  {
    warning('Length of GroupNames != number of groups')
    GroupNames = 1:nGroups
  }
    
  nVariates = length(lModels)
  if((length(VariateNames) != nVariates) & !is.null(VariateNames))  #wrong length and not NULL
  {
    warning('Length of VariateNames != number of variates') #Will later set to model dv names
  }  
      
  pMatrix = list()
  Means = matrix(data=NA, nrow=nGroups, ncol=nVariates, dimnames = list(GroupNames,1:nVariates))
  SE.l =  matrix(data=NA, nrow=nGroups, ncol=nVariates, dimnames = list(GroupNames,1:nVariates)) 
  SE.u =  matrix(data=NA, nrow=nGroups, ncol=nVariates, dimnames = list(GroupNames,1:nVariates))
  SE =  matrix(data=NA, nrow=nGroups, ncol=nVariates, dimnames = list(GroupNames,1:nVariates))
  for (i in 1:length(lModels))
  {
    pMatrix[[i]] = lm.pointEstimates(lModels[[i]],data)
    Means[,i] = pMatrix[[i]][,1]
    
    if(is.null(VariateNames)) {colnames(Means)[i] = strsplit(toString(lModels[[i]]$call[2]), ' ')[[1]][1]} else {colnames(Means)[i] = VariateNames[i]}
    if(is.null(VariateNames)) {colnames(SE.l)[i] = strsplit(toString(lModels[[i]]$call[2]), ' ')[[1]][1]} else {colnames(SE.l)[i] = VariateNames[i]}
    if(is.null(VariateNames)) {colnames(SE.u)[i] = strsplit(toString(lModels[[i]]$call[2]), ' ')[[1]][1]} else {colnames(SE.u)[i] = VariateNames[i]}
    if(is.null(VariateNames)) {colnames(SE)[i] = strsplit(toString(lModels[[i]]$call[2]), ' ')[[1]][1]} else {colnames(SE)[i] = VariateNames[i]}
    
    SE.l[,i] = pMatrix[[i]][,2]                                              
    SE.u[,i] = pMatrix[[i]][,3]
    SE[,i] = pMatrix[[i]][,4] 
  }     
  
  return(list(Means=Means, SE.l=SE.l, SE.u=SE.u, SE=SE))
}