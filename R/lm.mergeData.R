lm.mergeData <-
function(dX, dY, by.x=0, by.y=0, all.x=TRUE, all.y=TRUE)
#merges variables from two data frames (dX, dY).  
#By default matches on row names other variable name in d1 (by.x) and d2 (by.y)
#By default, includes all cases in dX and dY but can  limit to only matching (all.X=FALSE, all.y=FALSE) or left join (all.y=FALSE) or right join (all.x=FALSE) 
#2010-09-15:  Now sorts data frame by numeric row.names before returning.
{
   dXY = merge(dX,dY,by.x=by.x, by.y=by.y, all.x=all.x, all.y = all.y)
   row.names(dXY) = dXY$Row.names
   dXY$Row.names = NULL
   dXY = dXY[sort(as.numeric(row.names(dXY)), index.return=TRUE)$ix,]   
   return(dXY)
}

