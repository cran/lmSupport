lm.pointEstimates <-
function(model, data)
#Calculates point estimates and SEs for rows in NewData
{
    CILevel = 1 - 2 * pt(c(1), df=model$df.residual, lower.tail=FALSE) 
    Predictions = matrix(data =NA,nrow=nrow(data),ncol=4, dimnames = list(1:nrow(data),c('Predicted', 'lwr', 'upr', 'se')))
    Predictions[,1:3] = predict(model,newdata=data, interval= 'confidence', level= CILevel)
    Predictions[,4] = Predictions[,1] - Predictions[,2]    
    return(Predictions)
}