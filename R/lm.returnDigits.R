lm.returnDigits <-function(Number, LowerDigit=1, UpperDigit=1)
#returns a subset of digits from a Number.  Number can be numeric or string that can be converted to numeric
#LowerDigit and UpperDigit indcate postition of digits to return (in base 10)
#works with vector
#e.g.,  lm.returnDigits(1234,10,100) returns  23
{
  Number = as.numeric(Number)
  Subset = trunc(Number/LowerDigit) %% (10*UpperDigit/LowerDigit)
  return(Subset)
}

