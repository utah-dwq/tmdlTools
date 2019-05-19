library(plotly)
plot_ly(x = ~ecoli$Date, y = ~ecoli$E.coli_Geomean, type = "scatter",color = ~ecoli$ML_Name, mode = "lines+markers", legendgroup = "group1")%>%
  add_trace(x = ~flow$Date, y = ~flow$Flow, color = ~flow$ML_Name,opacity = 0.7, yaxis = "y2", legendgroup = "group2")%>%
  layout(xaxis = list(title = "Date"),
         yaxis = list(title = "MPN/100 mL"),
         yaxis2 = list(side = "right", overlaying = "y", title = "Flow (cfs)"))

cols <- function(a) image(1:10, 1, as.matrix(1:10), col=a, axes=FALSE , xlab="", ylab="")
