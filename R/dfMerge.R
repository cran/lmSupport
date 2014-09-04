dfMerge <-
function(DataX, DataY, ByX=0, ByY=0, AllX=TRUE, AllY=TRUE)
#merges variables from two data frames (dX, dY).  
#By default matches on row names other variable name in d1 (by.x) and d2 (by.y)
#By default, includes all cases in dX and dY but can  limit to only matching (all.X=FALSE, all.y=FALSE) or left join (all.y=FALSE) or right join (all.x=FALSE) 
{
   dXY = merge(x=DataX,y=DataY,by.x=ByX, by.y=ByY, all.x=AllX, all.y =AllY)
   row.names(dXY) = dXY$Row.names
   dXY$Row.names = NULL
      
   return(dXY)
}