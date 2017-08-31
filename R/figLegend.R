figLegend <- function(x, y=NULL, legend, fill=NULL, border='black', angle=NULL, density=NULL, pch=NULL, leg.cex=NULL, 
                      leg.lty=NULL, leg.lwd=NULL, leg.font=NULL, leg.bty=NULL)
  #wrapper for plot with defaults for our typical use of lines() using 
  #our lab's default figure parameters 
{
  
  #get default if not over-rided by passing in
  if (is.null(leg.cex)) leg.cex = getOption('FigPars')$leg.cex
  if (is.null(leg.lty)) leg.lty = getOption('FigPars')$leg.lty  
  if (is.null(leg.lwd)) leg.lwd = getOption('FigPars')$leg.lwd 
  if (is.null(leg.font)) leg.font = getOption('FigPars')$leg.font
  if (is.null(leg.font)) leg.font = getOption('FigPars')$leg.font
  if (is.null(leg.bty)) leg.bty = getOption('FigPars')$leg.bty

  
  legend(x=x,y=y,legend=legend, fill=fill, border=border, angle=angle, density=density, pch=pch, 
         lty=leg.lty, lwd=leg.lwd, box.lwd=leg.lwd, pt.lwd=leg.lwd, cex=leg.cex, text.font=leg.font, bty=leg.bty)
  
}