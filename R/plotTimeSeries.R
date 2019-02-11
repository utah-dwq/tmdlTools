#' Plot Parameter Concentrations Over Time
#' 
#' This function creates a plot of parameter concentrations over time. Can handle plotting multiple sites over time.
#' @param x A data frame containing a field labeled "E.coli" and another labeled "Date", which has been converted to a date format.
#' @param max_crit Numeric. The maximum criterion for the E.coli dataset, taken from R317-2-14.
#' @export plotTimeSeries
#' @importFrom yarrr piratepal


plotTimeSeries <- function(x,max_crit, yeslines=TRUE, sitepal = "basel"){
  # Get number of sites
  uni.sites <- unique(x$ML_Name)
  colrs <- piratepal(sitepal)
  # Get dates for axis
  max.dat <- max(x$Date)
  min.dat <- min(x$Date)
  max.e <- max(x$E.coli)
  min.e <- min(x$E.coli)
  # Create an empty plot
  plot(1, type="n", xlab="", ylab="MPN/100 mL", xaxt="n", xlim=c(min.dat, max.dat), ylim=c(min.e, max.e))
  axis.Date(1, at=seq(min.dat, max.dat, by="6 months"), format="%m-%Y", las=2, cex=0.8)
  abline(h=max_crit, col="red", lwd=2)
  text(min.dat+10,max_crit-50, "Max Crit")
  site = vector()
  colr = vector()
  # Start plotting
  for(i in 1:length(uni.sites)){
    y = x[x$ML_Name==uni.sites[i],]
    if(yeslines){
      lines(y$E.coli_Geomean~y$Date, lwd=1, lty=1, col=colrs[i])  
    }
    points(y$E.coli_Geomean~y$Date, pch=21, col="black", bg=colrs[i])
    site[i] = as.character(uni.sites[i])
    colr[i] = colrs[i]
  }
  l=legend("topleft",c(site),col="black",pt.bg=c(colrs), pch=21, bty="n", cex=1) 

}