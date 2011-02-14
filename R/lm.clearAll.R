lm.clearAll <-
function()
#clears screen, closes all graphic devices, and removes all objects
#clear screen will only work on windows
{
  graphics.off()  #close all graphics devices
  rm(list=ls(pos=1), pos=1)   #clear all objects from global environment(pos=1)
			
  #code from Philippe Grosjean and Gabor Grothendieck to clear screen
  #requires the RDCOMClient which is not on CRAN.  Can be installed with this code
  #install.packages("RDCOMClient", repos = "http://www.omegahat.org/R")
  if (.Platform$GUI[1] != "Rgui")  return(invisible(FALSE))  #Only works for RGui
  if ( .Platform$OS.type != "windows")  return(invisible(FALSE))  #Only works for windows OS
  #requires RDCOMClient.   Generate error message with instructions to resolve if it is not installed
  if (!require(RDCOMClient)) stop('Package RDCOMClient is required.\n To install: install.packages("RDCOMClient", repos = "http://www.omegahat.org/R")')   
  wsh <- COMCreate("Wscript.Shell") 
  invisible(wsh$SendKeys("\014"))   
}

