lm.describeData <-
function(data, detail = 2)
{
    t3 = describe(data)
    t3 = data.frame(t3)
    t2 = t3[c(1:5,8,9,11,12)]
    t1 = t3[c(2:4,8,9)]
    return(switch(detail,t1, t2,t3))    
}