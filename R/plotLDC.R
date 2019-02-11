#' Plot Load Duration Curve
#'
#' This function creates a plot of observed and loading capacities as a function of flow percentile for a given site.
#' @param x A data frame containing a field labeled "observed.loading", "loading.capacity", and "flow.percentile". Generally used on a site level.
#' @export plotLDC



# LDC plot function - used below if plot_it=TRUE
plotLDC <- function(x){
  # Order flow data
  flow.plot <- x[order(x$flow.percentile),]
  # Pull out observed loadings (E.coli data)
  ecoli.loads <- x[!is.na(x$E.coli),]
  spre <- ecoli.loads[ecoli.loads$Season=="Spring",]
  sume <- ecoli.loads[ecoli.loads$Season=="Summer",]
  fale <- ecoli.loads[ecoli.loads$Season=="Fall",]
  wine <- ecoli.loads[ecoli.loads$Season=="Winter",]
  windows()
  plot(1, type="n", xlab="Flow Exceedance Percentile", ylab="E.coli Load (MPN/day)", xlim=c(0, 100), ylim=c(0,max(ecoli.loads$observed.loading)), main=paste("Load Duration Curve:",x$ML_Name[1]))
  abline(v=10, lty=2)
  abline(v=40, lty=2)
  abline(v=60, lty=2)
  abline(v=90, lty=2)
  text(5, max(ecoli.loads$observed.loading)-.2*max(ecoli.loads$observed.loading),"High \n Flows")
  text(25, max(ecoli.loads$observed.loading)-.2*max(ecoli.loads$observed.loading),"Moist \n Conditions")
  text(50, max(ecoli.loads$observed.loading)-.2*max(ecoli.loads$observed.loading),"Mid-Range \n Flows")
  text(75, max(ecoli.loads$observed.loading)-.2*max(ecoli.loads$observed.loading),"Dry \n Conditions")
  text(95, max(ecoli.loads$observed.loading)-.2*max(ecoli.loads$observed.loading),"Low \n Flows")
  lines(flow.plot$loading.capacity~flow.plot$flow.percentile, type="l", col="blue", lwd=2)
  lines(flow.plot$loading.capacity.mos~flow.plot$flow.percentile, col="blue4", lwd=2)
  points(spre$observed.loading~spre$flow.percentile, pch=21, col="black", bg="springgreen3", cex=1.5)
  points(sume$observed.loading~sume$flow.percentile, pch=21, col="black", bg="firebrick3", cex=1.5)
  points(fale$observed.loading~fale$flow.percentile, pch=21, col="black", bg="goldenrod3", cex=1.5)
  points(wine$observed.loading~wine$flow.percentile, pch=21, col="black", bg="seashell4", cex=1.5)
  legend("topright",legend=c("Loading Capacity","Loading Capacity + MOS", "Observed E.coli Loading"), bty="n", col=c("blue","green","black"), lty=c(1,1,NA),lwd=c(2,2,NA),pch=c(NA,NA,21), pt.bg=c(NA,NA,"purple"), cex=1)
}
